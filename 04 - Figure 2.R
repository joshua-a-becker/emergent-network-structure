if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

library(MASS)
library(lme4)

aggreg %>% 
  glmer(improve ~ prop_toward*communication + (1|dataset), family="binomial", data=.) %>% 
  summary


### fit the model
estimated_model = aggreg %>% 
  glmer(improve ~ prop_toward*communication + (1|dataset), family="binomial", data=.)

mu_intercept = sum(coef(estimated_model)$dataset[,1]*(table(aggreg$dataset)/nrow(aggreg)))
mod_mu = c(mu_intercept, as.numeric(coef(estimated_model)$dataset[1,2:4]))
### extract uncertainty around coefficients
mod_vcov = vcov(estimated_model)

getSim = function(prop_toward=0.5, commDisc=1, n=10000){
  
  ### draw sample of model coefficients based on uncertainty
  B_samp =  mvrnorm(n = n, mod_mu, mod_vcov)  
  
  samp_effect = B_samp[,"(Intercept)"] + B_samp[,"prop_toward"]*prop_toward +
    B_samp[,"communicationDiscussion"]*commDisc +
    B_samp[,"prop_toward:communicationDiscussion"]*prop_toward*commDisc
  
  
  samp_effect
}

out=data.frame(
    commDisc=numeric()
  , phi=numeric()
  , sd=numeric()
  , est=numeric()
  , fitest=numeric()
)

for(commDisc in c(0,1)){
  for(phi in seq(0,1,by=0.1)) {
    
    est_effect = mu_intercept + 
      coef(estimated_model)$dataset[1,"prop_toward"]*phi +
      coef(estimated_model)$dataset[1,"communicationDiscussion"]*commDisc +
      coef(estimated_model)$dataset[1,"prop_toward:communicationDiscussion"]*commDisc*phi
    
    
    samp_effect=getSim(phi, commDisc)
    out[nrow(out)+1,]=c(  commDisc
                                  , phi
                                  , sd(samp_effect)
                                  , est_effect
                                  , mean(samp_effect)
    )
  }
}


out %>%
  mutate(
     communication=ifelse(commDisc==1,"Discussion","Delphi"), analysis="Replication"
    , conf = as.numeric(sd)*1.96
    , est=as.numeric(est)
    , est_adj = exp(est)/(1+exp(est))
    , upper = exp(est+conf)/(1+exp(est+conf))
    , lower = exp(est-conf)/(1+exp(est-conf))
    , phi=as.numeric(phi)
  ) %>%
  ggplot(aes(x=phi, y=est_adj, color=communication
             , group=communication
             , fill=communication)) +
  geom_line() +
  geom_ribbon(aes(ymax=upper
                  , ymin=lower
                  , color=communication
  ), alpha=0.1, color=NA) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  nice_theme() +
  ylim(c(0,1)) +
  labs(y="Prob. of Improving", x="\u03d5 (Prop. Toward Truth)", fill="", color="") +
  theme(axis.text = element_text(size=7), axis.title = element_text(size = 10))

ggsave("Figures/Fig 2.png", width=2.8, height=1.5)


