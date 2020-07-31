if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}


myd = aggreg %>%
  mutate(improve=  ifelse(improve==1, "improve","worse")
         , prop=ifelse(prop_toward>0.5,">0.5","<=0.5")
         , replication=ifelse(dataset=="replication","replication","reanalysis")
         , maj = ifelse(prop_toward<0.5,"Away","Toward")
  )

### ORIGINAL PRE-REGISTRATION (SEE INCLUDED)
### INVOLVED A WHOLE LOT OF PAIRWISE COMPARISONS.
### VERY DESCRIPTIVE, BUT
### NOT REALLY THE BEST PLAN FOR A PAPER.

myd %>%
  subset(prop_toward!=0.5) %>%
  group_by(maj, communication, replication) %>%
  summarize(
      prop.improve=mean(improve=="improve")
    , p.val = prop.test(table(improve=="improve"))$p.val
    #, est = prop.test(table(improve=="improve"))$estimate
  )


### ARE THE TWO COMMUNICATION FORMATS DIFFERENT?
### WHEN MAJORITY IS AWAY?
myd %>% 
  group_by(replication, maj) %>%
  summarize(
    p.val=prop.test(table(improve, communication))$p.val
  )
s