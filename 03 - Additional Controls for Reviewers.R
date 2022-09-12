if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
library(lmerTest)
library(texreg)
###########



aggreg = aggreg %>%
  mutate(
    lvar = -1/log(var)
    , lerr = -1/log(ind_err)
  ) 

mod0a = glmer(improve ~ (1|dataset) + lvar, family="binomial", data=aggreg)

mod0b = glmer(improve ~ (1|dataset) + lerr, family="binomial", data=aggreg)

mod1 = glmer(improve ~ (1|dataset) + prop_toward, family="binomial", data=aggreg)

mod2 = glmer(improve ~ (1|dataset) + lvar + prop_toward, family="binomial", data=aggreg)

mod3 = glmer(improve ~ (1|dataset) + lerr + prop_toward, family="binomial", data=aggreg)

mod4 = glmer(improve ~ (1|dataset) + prop_toward*communication, family="binomial", data=aggreg)

mod5 = glmer(improve ~ (1|dataset) + lvar + prop_toward*communication, family="binomial", data=aggreg) 

mod6 = glmer(improve ~ (1|dataset) + lerr + prop_toward*communication, family="binomial", data=aggreg) 


### CONTROLLING WITH INTERACTION 

## TABLE S1
htmlreg( 
  list(#mod0a, mod0b, 
       mod1, mod2, mod3, mod4, mod5, mod6)
, file="Figures/Control_vars.html"
#, custom.header=list("Reanalysis"=1:2,"Replication"=3:4)
#,custom.model.names=c("Numeric","Discussion", "Interaction")
, custom.coef.map=list(
  "(Intercept)"=NA
  , "lvar"="Variance"
  , "lerr"="Individ. Err."
  , "prop_toward" = "&Phi;"
  , "communicationDiscussion" = "Mode (Discussion)"
  ,"prop_toward:communicationDiscussion"="&Phi;*Mode"
)
, caption="Improvement as a function of &Phi;, Controlling for Variance and Individ. Error"
, caption.above=T
, stars=c(0.1, 0.05,0.01,0.001), symbol="&dagger;"
)


cor.test(-(1/aggreg$var), aggreg$prop_toward)
