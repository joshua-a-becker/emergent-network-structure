rm(list=ls());gc()
require(DescTools)
require(tidyverse)

#### PREP GURCAY DATA
question_lookup = read.csv("question_lookup.csv", stringsAsFactors=F, header=F,fileEncoding="UTF-8-BOM") %>%
  `colnames<-`(c("question","true.values")) %>%
  mutate(
    question=tolower(question)
    ,true.values=round(true.values,2)
  )

chats=read.csv("chatlog.csv", stringsAsFactors=F) %>%
  mutate(
    subject.no = as.numeric(sapply(strsplit(Rs,";"), "[", 1))
    , Qs=tolower(Qs)
    , question = unlist(lapply(Qs, FUN=function(x){names(which(sapply(question_lookup$question, grepl, x)))}))
  )


gurc_d <- read.csv("GURCAY_et_al_newDataApr30.csv") %>% 
  group_by(question.no, group) %>% 
  mutate(
    valid = !is.na(est1) & !is.na(est2)
  ) %>% 
  subset(valid) %>% 
  mutate(
    mu1 = mean(est1, na.rm=T)
    , toward_truth = ifelse((est1 < mean(est1) & mu1 <= true.values) | (est1 > mu1 & mu1 >= true.values), "Away","Toward")
    , group_number = group
    , true.values = round(true.values, 2)
  ) %>%
  merge(question_lookup, by="true.values")



chat_sum = chats %>%
  group_by(subject.no, question) %>%
  summarize(
    count_chat = length(Rs)
  ) %>%
  rowwise %>%
  subset(subject.no %in% gurc_d$subject.no) %>%
  mutate(
    group_number = unique(gurc_d$group[gurc_d$subject.no==subject.no])
  ) %>%
  group_by(group_number, question) %>%
  summarize(
    gini = Gini(count_chat)
  )


gurc_aggreg = gurc_d %>% 
  ### retain only people who answered both times
  ### retain only social conditions
  subset(valid & condition!="C") %>%
  group_by(condition, question, group_number) %>%
  summarize(
    N = length(est1)
    , truth=unique(true.values)
    , mu1 = mean(est1/truth)
    , mu2 = mean(est2/truth)
    , med1 = median(est1)
    , err_mu1 = abs(mu1 - 1)
    , err_mu2 = abs(mu2 - 1)
    
    ### change in error of mean
    , change_mu = abs(mu2-mu1)/truth
    , change_err_mu = (err_mu2 - err_mu1)/truth
    
    ###
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_away_truth = mean(toward_truth=="Away")
    , prop_away_truth_round=round(prop_away_truth,1)
    
    ### did the mean improve?
    , mu_improved = ifelse(change_err_mu<0, "Improved", "Worse")
    , dataset="gurcay"
  ) %>%
  merge(chat_sum, by=c("question","group_number"))


cols=c("mu_improved","gini","question","group_number", "prop_away_truth"
       , "prop_away_truth_round","change_mu","dataset")

all_aggreg = rbind(
  gurc_aggreg[,cols] %>% data.frame
) %>%
  mutate(
    gini_round = round(gini, 1)
    , gini_quantile = cut(gini,
                          breaks=quantile(gini, probs=c(0,0.5,1), na.rm=TRUE)
                          , include.lowest=TRUE)
    , prop_quantile = cut(prop_away_truth,
                          breaks=c(0,0.5,1)
                          , include.lowest=TRUE) %>% as.factor
  )

levels(all_aggreg$prop_quantile) = c("Majority\nTruth Side","Majority\nOpposite Side")
levels(all_aggreg$gini_quantile) = c("Low","High")



ggplot(all_aggreg %>% subset(dataset=="gurcay")
       , aes(x=prop_quantile, y=(mu_improved=="Improved")*1
             , color=gini_quantile, group=gini_quantile)) +
  #geom_hline(yintercept=0.5, linetype="dashed") +
  #geom_vline(xintercept=0.5, linetype="dashed")+
  stat_summary(fun.y="mean", geom="point")+
  stat_summary(fun.y="mean", geom="line")+
  labs(y="Probability of Improving", x="", color="Latent\nCentralization\n(Gini)")

ggsave("Evidence for Latent Network Structure.png", width=4, height=2.75, dpi=300)  


### THE KEY INTERACTION

glm(mu_improved=="Improved" ~ gini*prop_away_truth + question, all_aggreg, family="binomial") %>%
  summary

