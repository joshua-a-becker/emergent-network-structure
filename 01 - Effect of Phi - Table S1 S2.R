if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

library(texreg)
library(lme4)

estWithCi = function(mod, param="prop_toward", mult=1.96) {
  c(coef(summary(mod))[param,"Estimate"]
  ,coef(summary(mod))[param,"Estimate"] - coef(summary(mod))[param,"Std. Error"]*mult
  ,coef(summary(mod))[param,"Estimate"] + coef(summary(mod))[param,"Std. Error"]*mult
  ,coef(summary(mod))[param,"z value"]
  )
}

### fit the model
mod1_numeric = aggreg %>% 
  subset(communication=="Delphi") %>%
  glmer(improve ~ prop_toward + (1|dataset), family="binomial", data=.)

mod1_disc = aggreg %>% 
  subset(communication=="Discussion") %>%
  glmer(improve ~ prop_toward + (1|dataset), family="binomial", data=.)

mod2 = aggreg %>% 
  glmer(improve ~ prop_toward*communication + (1|dataset), family="binomial", data=.)


mod1_numeric_rep = aggreg %>% 
  subset(analysis=="replication" & communication=="Delphi") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod1_disc_rep = aggreg %>% 
  subset(analysis=="replication" & communication=="Discussion") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod2_rep = aggreg %>% 
  subset(analysis=="replication") %>%
  glm(improve ~ prop_toward*communication, family="binomial", data=.)



mod1_numeric_rean = aggreg %>% 
  subset(analysis=="reanalysis" & communication=="Delphi") %>%
  glmer(improve ~ prop_toward + (1|dataset), family="binomial", data=.)

mod1_disc_rean = aggreg %>% 
  subset(analysis=="reanalysis" & communication=="Discussion") %>%
  glm(improve ~ prop_toward, family="binomial", data=.)

mod2_rean = aggreg %>% 
  subset(analysis=="reanalysis") %>%
  glmer(improve ~ prop_toward*communication + (1|dataset), family="binomial", data=.)




estWithCi(mod1_disc)
estWithCi(mod1_numeric)
estWithCi(mod2, param="prop_toward:communicationDiscussion")

estWithCi(mod1_disc_rean); nrow(subset(aggreg, analysis=="reanalysis" & communication=="Discussion"))
estWithCi(mod1_disc_rep); nrow(subset(aggreg, analysis=="replication" & communication=="Discussion"))

estWithCi(mod1_numeric_rean);nrow(subset(aggreg, analysis=="reanalysis" & communication=="Delphi"))
estWithCi(mod1_numeric_rep)

estWithCi(mod2_rean, param="prop_toward:communicationDiscussion")
estWithCi(mod2_rep, param="prop_toward:communicationDiscussion")






## TABLE S1
htmlreg( 
  list(mod1_numeric, mod1_disc, mod2)
, file="Figures/Table S1_fixed.html"
#, custom.header=list("Reanalysis"=1:2,"Replication"=3:4)
,custom.model.names=c("Numeric","Discussion", "Interaction")
, custom.coef.map=list(
  "(Intercept)"=NA
  , "prop_toward" = "&Phi;"
  , "communicationDiscussion" = "Mode (Discussion)"
  ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
)
, caption="Improvement as a function of &Phi;"
, caption.above=T
, stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)


## TABLE S2
htmlreg( 
  list(
    mod1_numeric_rean, mod1_disc_rean, mod2_rean
    ,mod1_numeric_rep, mod1_disc_rep, mod2_rep
    
    )
  , file="Figures/Table S2_fixed.html"
  , custom.header=list("Reanalysis"=1:3,"Replication"=4:6)
  ,custom.model.names=c(
    "Delphi","Discussion","Combined"
    ,"Delphi","Discussion","Combined"
    )
  , custom.coef.map=list(
    "(Intercept)"=NA
    , "prop_toward" = "&Phi;"
    , "communicationDiscussion" = "Mode (Discussion)"
    ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
  )
  , caption="Improvement as a function of &Phi;, Separating Reanalysis and Replication"
  , caption.above=T
  , stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)

