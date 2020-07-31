require(tidyverse)
require(readxl)
require(httr)

### DOWNLOAD DELPHI DATA

lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
if(!file.exists("Reanalysis Data/lorenz_et_al.xls")) {
  GET(lorenz_url, write_disk(tf <- "Reanalysis Data/lorenz_et_al.xls", overwrite=T))  
}
lorenz2011 <- read_excel("Reanalysis Data/lorenz_et_al.xls") %>%
  mutate(
      pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Delphi"
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(trial, task) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )


if(!file.exists("Reanalysis Data/becker2017.csv.download")){
  becker2017 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                        , stringsAsFactors=F) %>%
    mutate(
      trial=group_number
      , dataset="becker2017"
      , pre_influence=response_1
      , post_influence=response_3
      , communication="Delphi"
    ) %>% 
    subset(network=="Decentralized") %>%
    group_by(trial, task) %>%
    mutate(
      soc_info = mean.neighbor.time1
    )
  
  write.csv(becker2017, "Reanalysis Data/becker2017.csv.download")
} else {
  becker2017 = read.csv("Reanalysis Data/becker2017.csv.download", stringsAsFactors=F)
}
  

if(!file.exists("Reanalysis Data/becker2019.csv.download")){
  
  becker2019 = read.csv(url("https://raw.githubusercontent.com/joshua-a-becker/wisdom-of-partisan-crowds/master/Becker%20Centola%20Porter%20-%20Wisdom%20of%20Partisan%20Crowds%20-%20Supplementary%20Dataset.csv")) %>%
    mutate(
      trial=paste0(set,pair_id,network,experiment,party)
      , dataset="becker2019"
      , pre_influence=response_1
      , post_influence=response_3
      , task=q
      , network= fct_recode(network, "Decentralized" = "Social", "Solo" = "Control")  
      , communication="Delphi"
    ) %>% 
    subset(network=="Decentralized") %>%
    group_by(task, trial) %>%
    mutate(
      soc_info = mean(pre_influence, na.rm=T)
    )
  
  write.csv(becker2019, "Reanalysis Data/becker2019.csv.download")
} else {
  becker2019 = read.csv("Reanalysis Data/becker2019.csv.download", stringsAsFactors=F)
}


#### LOAD GURCAY DATA
question_lookup = read.csv("Reanalysis Data/question_lookup.csv", stringsAsFactors=F, header=F,fileEncoding="UTF-8-BOM") %>%
  `colnames<-`(c("question","true.values")) %>%
  mutate(
    question=tolower(question)
    ,true.values=round(true.values,2)
  )

chats=read.csv("Reanalysis Data/chatlog.csv", stringsAsFactors=F) %>%
  mutate(
    subject.no = as.numeric(sapply(strsplit(Rs,";"), "[", 1))
    , Qs=tolower(Qs)
    , question = unlist(lapply(Qs, FUN=function(x){names(which(sapply(question_lookup$question, grepl, x)))}))
  )


gurc_d <- read.csv("Reanalysis Data/GURCAY_et_al_newDataApr30.csv") %>% 
  subset(condition!="C") %>%
  group_by(question.no, group) %>% 
  mutate(
      valid = !is.na(est1) & !is.na(est2)
    , pre_influence = est1
    , post_influence=est2
  ) %>% 
  subset(valid) %>% 
  mutate(
      group_number = group
    , true.values = round(true.values, 2)
    , truth=true.values
  ) %>%
  merge(question_lookup, by="true.values") %>%
  mutate(
    task = question
    , trial=group_number
  )

chat_sum_individ = chats %>%
  group_by(subject.no, question) %>%
  summarize(
      count_chat = length(Rs)
    , count_words = sum(nchar(Rs))
  ) %>%
  rowwise %>%
  subset(subject.no %in% gurc_d$subject.no) %>%
  mutate(
    group_number = unique(gurc_d$group[gurc_d$subject.no==subject.no])
  )




cols=c("pre_influence","post_influence","truth","task","trial","dataset", "communication","soc_info")

delphi_reanalysis = rbind(
  becker2017[,cols] %>% as.data.frame
  , lorenz2011[,cols] %>% as.data.frame
  , becker2019[,cols] %>% as.data.frame
) %>%
  mutate(
      analysis = "reanalysis"
      , count_chat=NA
      , count_words=NA
  )


disc_reanalysis = gurc_d %>%
  ### some chat groups seem to be missing the chat data.
  merge(
    .  
  , chat_sum_individ
  , by=c("subject.no","question")
  , all.x=T
) %>% mutate(
    err1 = abs(est1-true.values)
  , communication="Discussion"
  , dataset="gurcay2015"
) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence)
  ) %>%
  mutate(
      analysis="reanalysis"
      , count_chat = ifelse(is.na(count_chat), 0, count_chat)
      , count_words = ifelse(is.na(count_chat), 0, count_words)
      
      ### ensure that actual missing data is encoded as NA
      , count_chat = ifelse(!group %in% chat_sum_individ$group_number, NA, count_chat)
      , count_words = ifelse(!group %in% chat_sum_individ$group_number, NA, count_words)
  )
