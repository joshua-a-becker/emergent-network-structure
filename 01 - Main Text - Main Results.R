if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(magrittr)
library(miceadds)

my.prop.test = function(tb, margin=NULL) {
  p.tb=prop.table(tb, margin=margin)
  p.ts=prop.test(tb)
  
  return(list(test=p.ts, table=p.tb))
}


myd = aggreg %>%
  mutate(improve=  ifelse(improve==1, "improve","worse")
         , prop=ifelse(prop_toward>0.5,">0.5","<=0.5")
         , replication=ifelse(dataset=="replication","replication","reanalysis")
  )

### Does social inf improve belief accuracy?
myd %>%
  group_by(replication, communication) %>%
  summarize(
      improve = mean(change_err_mu<0)
    , p.val = prop.test(table(change_err_mu<0))$p.val
    , est = prop.test(table(change_err_mu<0))$estimate
    
  )

myd %>%
  group_by(communication,dataset) %>%
  summarize(
    improve = mean(change_err_mu<0)
  )


### Does phi predict outcomes?

# For replication (as pre-registered)
aggreg %>%
  subset(dataset=="replication") %>%
  subset(communication=="Discussion") %>%
  glm(improve ~ task + prop_toward, family="binomial", data=.) %>%
  summary

aggreg %>%
  subset(dataset=="replication") %>%
  subset(communication=="Delphi") %>%
  glm(improve ~ task + prop_toward, family="binomial", data=.) %>%
  summary

# For reanalysis (as pre-registered)
aggreg %>%
  subset(dataset!="replication") %>%
  subset(communication=="Discussion") %>%
  miceadds::glm.cluster(
    formula = improve==1 ~ trial + prop_toward
    , data=.
    , cluster=(.)$trial
    , family="binomial" 
  ) %>% summary

aggreg %>%
  subset(dataset!="replication") %>%
  subset(communication=="Delphi")  %>%
  miceadds::glm.cluster(
    formula = improve==1 ~ dataset + trial + prop_toward
    , data=.
    , cluster=paste0((.)$trial, (.)$dataset)
    , family="binomial" 
  ) %>% summary


### VISUALIZE
aggreg %>% 
  subset(prop_toward!=0.5) %>%
  mutate(
    prop_t = ifelse(prop_toward>0.5, "\u03C6>0.5","\u03C6<0.5")
    , dataset=ifelse(dataset=="replication","Replication","Reanalysis")
  ) %>%
  group_by(dataset, communication, prop_t) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(color=ifelse(communication=="Delphi","Numeric","Discussion"), x=prop_t, y=improve, group=communication)) +
  geom_hline(yintercept=0.5, linetype="dashed", color="#333333") +
  geom_point() + geom_line() +
  #geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1)+
  facet_grid(.~dataset) + 
  labs(x="", y="% Improved", color="") +
  nice_theme()
ggsave("Figures/Main Hypothesis.png", width=4, height=2)

### PROPORTION TEST ON THE LINES IN THE FIG ABOVE
### I.E., WHETHER PHI>0.5 PREDICTS OUTCOMES
### (AS PRE-REGISTERED)
myd %>% 
  mutate(maj = ifelse(prop_toward<0.5,"Away","Toward")) %>%
  group_by(replication, communication) %>%
  subset(prop_toward!=0.5) %>%
  summarize(
    p.val = prop.test(table(maj, improve))$p.val
  )



### evidence of centralization

### pre-registered version.... doesn't include zeros
### b/c gini was calulated only on people *present* in the conversation
### omitting lurkers with opinions, but zero contributions
myd %>%
  subset(replication=="reanalysis") %>%
  glm(improve=="improve" ~ gini_talkativeness_present_only*prop_toward + task, family="binomial", data=.) %>%
  summary


myd %>%
  subset(replication=="replication") %>%
  glm(improve=="improve" ~ gini_talkativeness_present_only*prop_toward + task, family="binomial", data=.) %>%
  summary

### revised version? does include zeros
myd %>%
  subset(replication=="reanalysis") %>%
  glm(improve=="improve" ~ gini_talkativeness*prop_toward + task, family="binomial"
      , data=.) %>%
  summary

myd %>%
  subset(replication=="replication") %>%
  glm(improve=="improve" ~ gini_talkativeness*prop_toward + task, family="binomial"
      , data=.) %>%
  summary





