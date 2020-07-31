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
     , dataset= ifelse(dataset=="replication","Replication","Reanalysis")
  ) %>% 
  group_by(central_twd_truth, dataset) %>%
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
  scale_y_continuous(labels=pct_labels, lim=c(0.15,0.85))+
  facet_grid(.~dataset)+
  labs(x="Central Node Toward Truth", y="% Improve") +
  theme_test()

ggsave("Figures/Central Node Predicts.png", width=3, height=2)

aggreg %>%
  subset(communication=="Discussion" & analysis=="reanalysis") %>%
  mutate(
    improve = ifelse(improve, "Improve","Worse")
    , central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>%
  tidy_table(central_twd_truth, improve) %>%
  #prop.table(margin=1)
  prop.test
  
  
aggreg %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  mutate(
    improve = ifelse(improve, "Improve","Worse")
    , central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>%
  tidy_table(central_twd_truth, improve) %>%
  #prop.table(margin=1)
  prop.test


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

myd %>%
  subset(analysis=="reanalysis") %>%
    miceadds::glm.cluster(
    formula=talk_quant ~ trial + err_quant
    , data=.
    , cluster = (.)$trial
    ) %>%
  summary
    

myd %>%
  subset(analysis=="replication") %>%
  lm(
    talk_quant ~ err_quant
    , data=.
  ) %>% 
  summary
