library(tidyverse, warn.conflicts = F, quietly = T)

delphi_replication = read.csv("Replication Data/delphi_data.csv", stringsAsFactors=F) %>%
  mutate(
      task=question
    , pre_influence=response_1
    , post_influence=response_5
    , communication="Delphi"
    , count_chat = NA
    , count_length = NA
    , analysis="replication"
    , dataset="replication"
  ) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )


### get count chat for discussion data
Sys.setlocale("LC_ALL", "C") # fix a character encoding issue
chat_data_replication = read.csv("Replication Data/chat_data.csv", stringsAsFactors=F) %>%
  group_by(trial, playerId) %>%
  summarize(
      count_chat = sum(!is.na(text))
    , count_length = sum(nchar(text))
    , count_length = ifelse(is.na(count_length), 0, count_length)
  )


disc_replication = read.csv("Replication Data/discussion_data.csv", stringsAsFactors=F) %>%
  mutate(
      task=question
    , pre_influence=initial
    , post_influence=final
    , communication="Discussion"
    , tr=substr(trial, 1, 15)
    , analysis="replication"
    , dataset="replication"
  ) %>%
  merge(chat_data_replication, by=c("playerId","trial")) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )




data_loaded=14159
