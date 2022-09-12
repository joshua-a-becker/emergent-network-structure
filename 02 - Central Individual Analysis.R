if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}


# 2x2 proportion test
# probability of improvement as a fn of whether most talkative individual 
# is in correct/incorrect direction

# DISCUSSION ONLY

rean_test = aggreg %>%
  subset(analysis=="reanalysis" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()


rep_test = aggreg %>%
  subset(analysis=="replication" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()

rean_test
#diff est
rean_test$estimate[1]-rean_test$estimate[2]

rep_test
#diff
rep_test$estimate[1]-rep_test$estimate[2]


### ANOTHER VERSION WITH WORD COUNT

rean_test_alt = aggreg %>%
  subset(analysis=="reanalysis" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth_length,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()


rep_test_alt = aggreg %>%
  subset(analysis=="replication" & communication=="Discussion") %>%
  with(.,
       table(central_twd_truth_length,improve)[, 2:1]
  ) %>%
  #prop.table(margin=2)
  prop.test()

rean_test_alt
#diff est
rean_test_alt$estimate[1]-rean_test_alt$estimate[2]

rep_test_alt
#diff
rep_test_alt$estimate[1]-rep_test_alt$estimate[2]




## is talkativeness correlated with accuracy?
myd = 
  d_valid %>%
  subset(is.finite(count_chat)
         & communication=="Discussion") %>%
  group_by(task) %>%
  mutate(
    err_quant = cut(err,
                    breaks=quantile(err, probs=seq(0,1,by=0.25)), include.lowest=T) %>% as.numeric
  ) %>%
  group_by(analysis) %>%
  mutate(
    talk_quant = cut(count_chat,
                     breaks=quantile(count_chat, probs=seq(0,1,by=0.25)), include.lowest=T) %>% as.numeric
  )

## reanalysis
myd %>%
  subset(analysis=="reanalysis") %>%
  with(
    cor.test(talk_quant, err_quant)
  )

## replication correlation
myd %>%
  subset(analysis=="replication") %>%
  with(
    cor.test(talk_quant, err_quant)
  )

