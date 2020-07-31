require(tidyverse)

delphi_replication = read.csv("Replication Data/delphi_data.csv", stringsAsFactors=F) %>%
  mutate(
      task=question
    , pre_influence=response_1
    , post_influence=response_5
    , communication="Delphi"
    , count_chat = NA
    , count_words = NA
    , analysis="replication"
    , dataset="replication"
  ) %>%
  group_by(task, trial) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  )


### get count chat for discussion data
chat_data_replication = read.csv("Replication Data/chat_data.csv", stringsAsFactors=F) %>%
  group_by(trial, playerId) %>%
  summarize(
      count_chat = sum(!is.na(text))
    , count_words = sum(nchar(text))
    , count_words = ifelse(is.na(count_words), 0, count_words)
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
