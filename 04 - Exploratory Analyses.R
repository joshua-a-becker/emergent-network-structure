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

tasks = d_valid %>%
  group_by(task) %>%
  summarize(
    task_phi=mean(toward_truth=="Toward")
  ) %>% 
  arrange(task)


myd = aggreg %>%
  mutate(improve=  ifelse(improve==1, "improve","worse")
         , prop=ifelse(prop_toward>0.5,">0.5","<=0.5")
         , replication=ifelse(dataset=="replication","replication","reanalysis")
  ) %>%
  merge(tasks, by="task")

myd %>%
  subset(replication=="replication") %>%
  mutate(
    task_phi=ifelse(task_phi < 0.5, "away","toward")
  ) %>%
  group_by(task_phi, communication) %>%
  summarize(
    improve=mean(improve=="improve")
    , N=n()
  ) %>%
  subset(N>=5)%>%
  ggplot(aes(x=task_phi, y=improve))+
  geom_point() +
  facet_grid(.~communication)
  theme_test()
  


myd %>%
  subset(replication=="replication") %>%
  subset(!task%in%c("chicago","philly","van gogh", "syria")) %>%
  mutate(
    #task_phi=round(task_phi, 1)
  ) %>%
  group_by(task_phi, communication, task) %>%
  summarize(
    improve=mean(improve=="improve")
    , N=n()
  ) %>%
  subset(N>=5)%>% 
  ggplot(aes(x=task_phi, y=improve, color=communication, group=task))+
  geom_point() + 
  geom_line() +
  theme_test()

myd %>%
  subset(replication=="replication") %>%
  group_by(task, communication) %>%
  summarize(
    task_phi=unique(task_phi)
    , prop_toward=mean(prop_toward)
  ) %>%
  ggplot(aes(x=task_phi, y=prop_toward, color=communication, shape=task)) + 
  scale_shape_manual(values=c(15:25))+
  geom_point(size=5)+
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=0.5) +
  theme_test()


myd %>%
  subset(replication=="replication") %>%
  group_by(task, communication) %>%
  summarize(
    task_phi=unique(task_phi)
    , prop_toward=mean(prop_toward)
    , improve=mean(improve=="improve")
  ) %>%
  ggplot(aes(x=prop_toward, y=improve, color=communication, shape=task)) + 
  scale_shape_manual(values=c(15:25))+
  geom_point(size=5)+
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=0.5) +
  theme_test()




myd %>%
  subset(replication=="replication") %>%
  group_by(task) %>%
  summarize(
      task_phi=unique(task_phi)
    , prop_toward=mean(prop_toward)
    , improve=mean(improve=="improve")
  ) %>%
  ggplot(aes(x=task_phi, y=prop_toward)) + 
  geom_point()+
  geom_hline(yintercept=0.5) +
  geom_vline(xintercept=0.5) +
  theme_test()

aggreg %>%
  mutate(
    maj = ifelse(prop_toward<0.5, "away", "toward")
  ) %>%
  group_by(maj) %>%
  summarize(
    p=prop.test(table(improve))$p.val
    , est=prop.test(table(improve))$estimate
  )
