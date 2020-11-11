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
    , xsq = prop.test(table(improve=="improve"))$statistic
    , est = prop.test(table(improve=="improve"))$estimate
  )


### ARE THE TWO COMMUNICATION FORMATS DIFFERENT
### WHEN MAJORITY IS TOWARD/AWAY?
myd %>% 
  subset(prop_toward!=0.5) %>%
  group_by(replication, maj) %>%
  summarize(
    p.val=prop.test(table(communication, improve))$p.val
    , xsq = prop.test(table(communication, improve))$statistic
#    , est1 = prop.test(table(communication, improve))$estimate[1]
#    , est2 = prop.test(table(communication, improve))$estimate[2]
    , delph = mean(improve[communication=="Delphi"]=="improve")
    , disc = mean(improve[communication=="Discussion"]=="improve")
  )
  