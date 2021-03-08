if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(magrittr)
library(miceadds)
library(texreg)

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
    , p.val = binom.test(table(change_err_mu<0))$p.val
    , est = prop.test(table(change_err_mu<0))$estimate
    
  )

myd %>%
  group_by(communication,dataset) %>%
  summarize(
    improve = mean(change_err_mu<0)
  )


### Does phi predict outcomes?

# For replication (as pre-registered)
mod.rep.disc = aggreg %>%
  subset(dataset=="replication") %>%
  subset(communication=="Discussion") %>%
  glm(improve ~ task + prop_toward, family="binomial", data=.)


mod.rep.delph = aggreg %>%
  subset(dataset=="replication") %>%
  subset(communication=="Delphi") %>%
  glm(improve ~ task + prop_toward, family="binomial", data=.)


# For reanalysis (as pre-registered)
mod.rean.disc=aggreg %>%
  subset(dataset!="replication") %>%
  subset(communication=="Discussion") %>%
  miceadds::glm.cluster(
    formula = improve==1 ~ trial + prop_toward
    , data=.
    , cluster=(.)$trial
    , family="binomial" 
  )

mod.rean.delph=aggreg %>%
  subset(dataset!="replication") %>%
  subset(communication=="Delphi")  %>%
  miceadds::glm.cluster(
    formula = improve==1 ~ dataset + trial + prop_toward
    , data=.
    , cluster=paste0((.)$trial, (.)$dataset)
    , family="binomial" 
  )





htmlreg(list(mod.rep.disc, mod.rep.delph, mod.rean.disc, mod.rean.delph)
        , file="Figures/Table A2.html"
        , custom.header=list("Replication"=1:2,"Reanalysis"=3:4)
        ,custom.model.names=c("Disc.","Delphi","Disc.","Delphi")
        , custom.coef.map=list("prop_toward"="&Phi;")
        , caption="Outcome:  Mean Closer to Truth (Yes/No)"
        , caption.above=T
        , stars=c(0.02,0.01,0.001)
)


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
  #subset(prop_toward!=0.5) %>%
  summarize(
    p.val = prop.test(table(maj, improve))$p.val
    , est = prop.test(table(maj, improve))$estimate[1]
  )



### evidence of centralization

### pre-registered version.... doesn't include zeros
### b/c gini was calulated only on people *present* in the conversation
### omitting lurkers with opinions, but zero contributions
cent.rean.orig = myd %>%
  subset(replication=="reanalysis") %>%
  glm(improve=="improve" ~ gini_talkativeness_present_only*prop_toward + task, family="binomial", data=.)

cent.rep.orig = myd %>%
  subset(replication=="replication") %>%
  glm(improve=="improve" ~ gini_talkativeness_present_only*prop_toward + task, family="binomial", data=.)

### revised version? does include zeros
cent.rean.rev = myd %>%
  subset(replication=="reanalysis") %>%
  glm(improve=="improve" ~ gini_talkativeness*prop_toward + task, family="binomial"
      , data=.)

cent.rep.rev = myd %>%
  subset(replication=="replication") %>%
  glm(improve=="improve" ~ gini_talkativeness*prop_toward + task, family="binomial"
      , data=.)





htmlreg(list(cent.rean.orig, cent.rep.orig, cent.rean.rev, cent.rep.rev)
        , file="Figures/Table A3.html"
        , custom.header=list("PreReg."=1:2,"Revised"=3:4)
        ,custom.model.names=c("Rean.","Rep.","Rean.","Rep.")
        , custom.coef.map=list(
            "prop_toward" = "&Phi;"
          , "gini_talkativeness_present_only" = "Gini"
          , "gini_talkativeness" = "Gini"
          , "gini_talkativeness_present_only:prop_toward"="Gini * &Phi;"
          , "gini_talkativeness:prop_toward" = "Gini * &Phi;"
          
          
        )
        , caption="Outcome:  Mean Closer to Truth (Yes/No)"
        , caption.above=T
)



