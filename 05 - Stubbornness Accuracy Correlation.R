if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}



aggreg %>%
  subset(communication=="Delphi" 
         #& dataset=="replication"
  ) %>%
  ungroup %>%
  mutate(
    taskx=as.factor(as.numeric(as.factor(task))%%2)
    , ax = alpha_cor#round(alpha_cor/1, 1)*1
    , ax = ifelse(ax<0,
                  "<0", ifelse(ax>0, ">0", "=0"))
  ) %>%
  group_by(ax, dataset) %>%
  summarize(
    improv = mean(change_err_mu<0)
    #improv = (change_err_mu/err_mu1)
    #, ax=mean(alpha_cor)
    , n=n()
  ) %>%
  ### remove those 5 trials with no chat data
  ggplot(aes(x=ax, y=improv, shape=dataset)) +
  #xlim(c(-0.5,0.5)) +
  scale_shape_manual(values=c(1,0,8,4))+
  geom_point() +
  geom_hline(yintercept=0.5)+geom_vline(xintercept=0) +
  theme_test() +
  labs(x="Stubbornness/Error Correlation", y="% Improve")

ggsave("Figures/Stub_Acc_Corr.png", width=3, height=3)




rep=aggreg %>%
  subset(communication=="Delphi" 
         & dataset=="replication"
  )


table(ifelse(rep$improve, "improve","worse")
             , 
      ifelse(rep$alpha_cor<0, "r<0","r>0")
             )
