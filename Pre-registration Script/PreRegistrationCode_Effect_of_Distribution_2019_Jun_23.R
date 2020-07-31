rm(list=ls());gc();
library(readxl)
library(Rmisc)
library(httr)
library(tidyverse)
library(ggplot2)
require(miceadds)



lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
if(!file.exists("lorenz_et_al.xls")) {
  GET(lorenz_url, write_disk(tf <- "lorenz_et_al.xls", overwrite=T))  
}
lorenz2011 <- read_excel("lorenz_et_al.xls") %>%
  mutate(
    pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Numeric"
  ) %>% 
  subset(network=="Decentralized")

gurcay2015 = read.csv("GURCAY_et_al_newDataApr30.csv") %>%
  mutate(
    trial=group
    , dataset="gurcay2015"
    , task=question.no
    , pre_influence=est1
    , post_influence=est2
    , network= fct_recode(condition, "Solo" = "C", "Decentralized" = "I", "Decentralized"="G")
    , communication="Discussion"
    , truth=true.values
  ) %>% 
  subset(network=="Decentralized")


becker2017 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                      , stringsAsFactors=F) %>%
  mutate(
    trial=group_number
    , dataset="becker2017"
    , pre_influence=response_1
    , post_influence=response_3
    , communication="Numeric"
  ) %>% 
  subset(network=="Decentralized")

becker2019 = read.csv(url("https://raw.githubusercontent.com/joshua-a-becker/wisdom-of-partisan-crowds/master/Becker%20Centola%20Porter%20-%20Wisdom%20of%20Partisan%20Crowds%20-%20Supplementary%20Dataset.csv")) %>%
  mutate(
    trial=paste0(set,pair_id,network,experiment,party)
    , dataset="becker2019"
    , pre_influence=response_1
    , post_influence=response_3
    , task=q
    , network= fct_recode(network, "Decentralized" = "Social", "Solo" = "Control")  
    , communication="Numeric"
  ) %>% 
  subset(network=="Decentralized")



cols=c("pre_influence","post_influence","truth","task","trial","network","dataset", "communication")

d = rbind(
  becker2017[,cols]
  , lorenz2011[,cols]
  , becker2019[,cols]
  , gurcay2015[,cols]
)



aggreg = d %>% 
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  group_by(task, trial, network, dataset, communication) %>%
  mutate(
    #  truth=truth/truth
    mu1 = mean(pre_influence)
    , err1 = abs(pre_influence - truth)
    , err2 = abs(post_influence - truth)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  )  %>%
  summarize(
    truth=unique(truth)
    , N = length(pre_influence)
    
    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    
    ## calc median
    , med1 = median(pre_influence)
    , med2 = median(post_influence) 
    
    ## raw change
    , change_mu = abs(mu1-mu2)
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
    
    ## error of median
    , err_med1 = abs(med1 - truth)
    , err_med2 = abs(med2 - truth)
    , change_err_med = mean(err_med2 - err_med1)
    , med_improve = ifelse(change_err_med<0, "Improve","Worse")
    , med_improve = ifelse(change_err_med==0, "Same",med_improve)
    
    ## ind err
    , err_ind1 = mean(err1)
    , err_ind2 = mean(err2)
    , change_err_ind = mean(abs(err2)) - mean(abs(err1))
    , change_ind = mean(abs(pre_influence-post_influence))
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
    
    ## diversity
    , sd1 = sd(pre_influence)
    , sd2 = sd(post_influence)
    , change_sd = sd2-sd1
    
    ## measuring movement of median
    , med_mean_gap_1 = abs(med1-mu1)
    , med_mean_gap_2 = abs(med2-mu1)
    , change_med_gap = med_mean_gap_2 - med_mean_gap_1
    
  ) %>%
  mutate(
    prop_toward_round=round(prop_toward,1)
    #, prop_away_quarters = floor(prop_away_truth*5)/5
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
  ) %>% subset(
    N>4
  )





###
ag_sum = aggreg %>% 
  group_by(prop_toward_round,network,communication) %>%
  summarize(
    N = length(improve)
    ,upper=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[2], NA)
    ,lower=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[1], NA)
    ,improve = mean(improve)
  )


adjust=c(rep(0.07,7), -0.05, rep(0.07, 1))
ggplot(ag_sum %>% subset(communication=="Discussion"), 
       aes(x=prop_toward_round, y=improve)) +
  geom_errorbar(aes(ymin=1-lower, ymax=1-upper), color="red", alpha=0.2, size=6, width=0)+
  geom_point(size=3)+
  geom_hline(yintercept=0.5, linetype="dashed") + 
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_label(aes(label=paste0(N), y=improve+adjust), size=3, label.padding=unit(0.15,"lines"))+
  xlim(c(0,1))+ 
  scale_y_continuous(expand = c(0,0), lim=c(0,1))+
  labs( y=""
        ,x="")



ggplot(ag_sum %>% subset(communication=="Numeric"), 
       aes(x=prop_toward_round, y=improve)) +
  geom_errorbar(aes(ymin=1-lower, ymax=1-upper), color="red", alpha=0.2, size=6, width=0)+
  geom_point(size=3)+
  geom_hline(yintercept=0.5, linetype="dashed") + 
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_label(aes(label=paste0(N), y=improve+0.06), size=3, label.padding=unit(0.15,"lines"))+
  xlim(c(0,1))+ 
  scale_y_continuous(expand = c(0,0), lim=c(0,1))+
  labs( y=""
        ,x="")






d_sum = summarySE(aggreg %>% subset(network=="Decentralized")
                  , measurevar="improve"
                  , groupvars=c("communication","majority")
)

ggplot(d_sum, aes(x=communication, y=improve,color=majority)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci), position=position_dodge(0.5), width=0)+
  geom_label(aes(label=paste0(N), y=improve+0.03), size=1.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))




ag_numeric = subset(aggreg, communication=="Numeric")
ag_discussion = subset(aggreg, communication=="Discussion")

ag_towards = subset(aggreg, prop_toward>0.5)
ag_away = subset(aggreg, prop_toward<0.5)


### MAIN TEST:  DOES PROP_TOWARD MATTER?

## CLUSTER-ROBUST LOGISTIC REGRESSION
miceadds::glm.cluster(
  formula = improve==1 ~ trial + prop_toward
  , data=ag_discussion
  , cluster=ag_discussion$trial
  , family="binomial" 
) %>% summary

miceadds::glm.cluster(
  formula=improve==1 ~ dataset + trial + prop_toward
  , data=ag_numeric
  , cluster = paste0(ag_numeric$trial, ag_numeric$dataset)
  , family="binomial"
) %>% summary 


### DOES NUMERIC IMPROVE OVERALL?
table(ag_numeric$improve) %>%
  prop.test()


### DOES NUMERIC IMPROVE WHEN PHI>0.5?
with(ag_numeric %>% subset(prop_toward>0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test


### DOES NUMERIC IMPROVE WHEN PHI<0.5?
with(ag_numeric %>% subset(prop_toward<0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test


### DOES DISCUSSION IMPROVE WHEN PHI>0.5?
with(ag_discussion %>% subset(prop_toward>0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test


### DOES DISCUSSION IMPROVE WHEN PHI<0.5?
with(ag_discussion %>% subset(prop_toward<0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test


### ARE THE TWO SETS OF TRIALS SIGNIFICANTLY DIFFERENT?
with(ag_discussion %>% subset(prop_toward!=0.5), 
     table(ifelse(improve==1,"Good","Bad"),  ifelse(prop_toward<0.5,"Minority","Majority"))
) %>%
  chisq.test

### ARE THE TWO COMMUNICATION FORMATS DIFFERENT?
### WHEN MAJORITY IS AWAY?
with(aggreg %>% subset(prop_toward<0.5 & network=="Decentralized"), 
     table(ifelse(improve==1,"Good","Bad"), communication)
) %>% 
  chisq.test



### ARE THE TWO COMMUNICATION FORMATS DIFFERENT?
### WHEN MAJORITY IS TOWARDS?
with(aggreg %>% subset(prop_toward>0.5 & network=="Decentralized"), 
     table(ifelse(improve==1,"Good","Bad"), communication)
) %>% 
  prop.test



aggreg$majority = aggreg$prop_toward>0.5
d_sum = summarySE(aggreg %>% subset(network=="Decentralized" & prop_toward!=0.5)
                  , measurevar="improve"
                  , groupvars=c("communication","majority")
                  
)