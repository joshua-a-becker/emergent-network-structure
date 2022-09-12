if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}



aggreg %>%
  subset(communication=="Discussion") %>%
  ### remove those 5 trials with no chat data
  subset(!is.na(central_twd_truth)) %>%
  mutate(
    #improve = ifelse(improve, "Improve","Worse")
    central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
    #, analysis= ifelse(analysis=="replication","Replication","Reanalysis")
  ) %>% 
  group_by(central_twd_truth, analysis) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(x=central_twd_truth, y=improve#, color=analysis
  )) +
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(0.5)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_y_continuous(labels=pct_labels, lim=c(0.11,0.89))+
  facet_grid(.~analysis)+
  labs(x="Central Node Toward Truth", y="% Improve") +
  theme_test()

ggsave("Figures/Central Node Predicts.png", width=3, height=2)



##### ALTERNATIVE VERSION
##### COUNTING WORDS NOT MESSAGES


aggreg %>%
  subset(communication=="Discussion") %>%
  ### remove those 5 trials with no chat data
  subset(!is.na(central_twd_truth_length)) %>%
  mutate(
    #improve = ifelse(improve, "Improve","Worse")
    central_twd_truth_words = ifelse(central_twd_truth_length, "Toward","Away")
    #, analysis= ifelse(analysis=="replication","Replication","Reanalysis")
  ) %>% 
  group_by(central_twd_truth_length, analysis) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(x=central_twd_truth_length, y=improve#, color=analysis
  )) +
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(0.5)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_y_continuous(labels=pct_labels, lim=c(0.19,0.81))+
  facet_grid(.~analysis)+
  labs(x="Central Node Toward Truth", y="% Improve") +
  theme_test()

ggsave("Figures/Central Node Predicts__count length.png", width=3, height=2)
