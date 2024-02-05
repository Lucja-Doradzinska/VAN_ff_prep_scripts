###################### early VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(early_van_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-early_van_models["erp_amp ~ 1 + masking * emotion * task + (1 + masking * emotion * task | subject)"][[1]]

summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(ev_data, vars = c('task', 'emotion')))
ce_dt <- as.data.frame(ce[['masking']])
ce


hypotheses <- c("maskingunmasked < 0","maskingunmasked + maskingunmasked:emotionfearful < 0",
                "maskingunmasked + maskingunmasked:taskID < 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked = 0",
                
                "maskingunmasked:emotionfearful < 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID = 0", 
                "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "emotionfearful < 0", "emotionfearful + maskingunmasked:emotionfearful < 0", 
                "emotionfearful + emotionfearful:taskID < 0",
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "emotionfearful = 0", "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "taskID < 0", "taskID + emotionfearful:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "taskID = 0", "taskID + emotionfearful:taskID = 0")

hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_bayesian_models.RData")


###################### late VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(late_van_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-late_van_models["erp_amp ~ 1 + masking * emotion * task + (1 + masking * emotion * task | subject)"][[1]]

summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(lv_data, vars = c('task', 'emotion' )))
ce


hypotheses <- c("maskingunmasked < 0","maskingunmasked + maskingunmasked:emotionfearful < 0",
                "maskingunmasked + maskingunmasked:taskID < 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful < 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID = 0",
                
                "emotionfearful < 0", "emotionfearful + emotionfearful:taskID < 0",
                "emotionfearful + maskingunmasked:emotionfearful < 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "emotionfearful = 0", "emotionfearful + emotionfearful:taskID = 0",
                
                "taskID < 0", "taskID + emotionfearful:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0")

hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_bayesian_models.RData")

###################### early occipital VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_early_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(early_van_occ_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-early_van_occ_models["erp_amp ~ 1 + masking * emotion * task + (1 + masking * emotion * task | subject)"][[1]]

summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(ev_occ_data, vars = c('task', 'emotion' )))
ce_dt <- as.data.frame(ce[['masking']])
ce


hypotheses <- c("maskingunmasked > 0","maskingunmasked + maskingunmasked:emotionfearful > 0",
                "maskingunmasked + maskingunmasked:taskID > 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID > 0",
                
                "maskingunmasked:emotionfearful < 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful = 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID = 0",
                "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "emotionfearful < 0", "emotionfearful + emotionfearful:taskID < 0",
                "emotionfearful + maskingunmasked:emotionfearful < 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "emotionfearful = 0", "emotionfearful + emotionfearful:taskID = 0",
                "emotionfearful + maskingunmasked:emotionfearful = 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "taskID < 0", "taskID + emotionfearful:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "taskID = 0", "taskID + emotionfearful:taskID = 0",
                "taskID + maskingunmasked:taskID = 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID = 0")



hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_early_bayesian_models.RData")

###################### late occipital VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_late_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(late_van_occ_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-late_van_occ_models["erp_amp ~ 1 + masking * emotion * task + (1 + masking * emotion * task | subject)"][[1]]
summary(winning_model)

ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(lv_occ_data, vars = c('task', 'emotion' )))
ce_dt <- as.data.frame(ce[['masking']])
ce


hypotheses <- c("maskingunmasked < 0","maskingunmasked + maskingunmasked:emotionfearful < 0",
                "maskingunmasked + maskingunmasked:taskID < 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful < 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "maskingunmasked:emotionfearful = 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID = 0",
                "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "emotionfearful < 0", "emotionfearful + emotionfearful:taskID < 0",
                "emotionfearful + maskingunmasked:emotionfearful < 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "emotionfearful = 0", "emotionfearful + emotionfearful:taskID = 0",
                "emotionfearful + maskingunmasked:emotionfearful = 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID = 0",
                
                "taskID < 0", "taskID + emotionfearful:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID < 0",
                
                "taskID = 0", "taskID + emotionfearful:taskID = 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID = 0")



hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_late_bayesian_models.RData")

###################### early lateralized VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_bayesian_models.RData")


loo_comp <- as.data.frame(loo_compare(lat_early_van_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-lat_early_van_models["erp_amp ~ 1 + masking * side * task + (1 + masking * side * task | subject)"][[1]]

summary(winning_model)

ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(lat_ev_data, vars = c('task', 'side' )))
ce_dt <- as.data.frame(ce[['masking']])
ce

hypotheses <- c("maskingunmasked < 0","maskingunmasked + maskingunmasked:sidecontra < 0",
                "maskingunmasked + maskingunmasked:taskID < 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:sidecontra + maskingunmasked:sidecontra:taskID < 0",
                
                "maskingunmasked = 0",
                
                "maskingunmasked:sidecontra < 0", "maskingunmasked:sidecontra + maskingunmasked:sidecontra:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:sidecontra:taskID < 0", 
                
                "maskingunmasked:sidecontra = 0", "maskingunmasked:sidecontra + maskingunmasked:sidecontra:taskID = 0",
                
                "sidecontra < 0", "sidecontra + sidecontra:taskID < 0",
                "sidecontra + maskingunmasked:sidecontra < 0", 
                "sidecontra + maskingunmasked:sidecontra + sidecontra:taskID + maskingunmasked:sidecontra:taskID < 0",
                
                "sidecontra = 0", "sidecontra + sidecontra:taskID = 0",
                
                "taskID < 0", "taskID + sidecontra:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + sidecontra:taskID + maskingunmasked:sidecontra:taskID < 0",
                
                "taskID = 0", "taskID + sidecontra:taskID = 0")


hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_bayesian_models.RData")

###################### late lateralized VAN evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(lat_late_van_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-lat_late_van_models["erp_amp ~ 1 + masking * side * task + (1 + masking * side * task | subject)"][[1]]

summary(winning_model)

ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(lat_lv_data, vars = c('task' ,'side')))
ce_dt <- as.data.frame(ce[['masking']])
ce

hypotheses <- c("maskingunmasked < 0","maskingunmasked + maskingunmasked:sidecontra < 0",
                "maskingunmasked + maskingunmasked:taskID < 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:sidecontra + maskingunmasked:sidecontra:taskID < 0",
                
                "maskingunmasked:sidecontra < 0", "maskingunmasked:sidecontra + maskingunmasked:sidecontra:taskID < 0",
                "maskingunmasked:taskID < 0", "maskingunmasked:taskID + maskingunmasked:sidecontra:taskID < 0", 
                
                "sidecontra < 0", "sidecontra + sidecontra:taskID < 0",
                "sidecontra + maskingunmasked:sidecontra < 0", 
                "sidecontra + maskingunmasked:sidecontra + sidecontra:taskID + maskingunmasked:sidecontra:taskID < 0",
                
                "sidecontra = 0", "sidecontra + sidecontra:taskID = 0",
                
                "taskID < 0", "taskID + sidecontra:taskID < 0",
                "taskID + maskingunmasked:taskID < 0",
                "taskID + maskingunmasked:taskID + sidecontra:taskID + maskingunmasked:sidecontra:taskID < 0")

hypothesis(winning_model, hypotheses)


save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_bayesian_models.RData")

###################### P3b evaluation ######################
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\EEG\\models\\p3_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(p3_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-p3_models["erp_amp ~ 1 + masking * emotion * task + (1 + masking * emotion * task | subject)"][[1]]

summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'masking', conditions = make_conditions(p3_data, vars = c('task', 'emotion' )))
ce_dt <- as.data.frame(ce[['masking']])
ce


hypotheses <- c("maskingunmasked > 0","maskingunmasked + maskingunmasked:emotionfearful > 0",
                "maskingunmasked + maskingunmasked:taskID > 0",
                "maskingunmasked + maskingunmasked:taskID + maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID > 0",
                
                "maskingunmasked = 0","maskingunmasked + maskingunmasked:emotionfearful = 0",
                "maskingunmasked + maskingunmasked:taskID = 0",
                
                "maskingunmasked:emotionfearful > 0", "maskingunmasked:emotionfearful + maskingunmasked:emotionfearful:taskID > 0",
                "maskingunmasked:taskID > 0", "maskingunmasked:taskID + maskingunmasked:emotionfearful:taskID > 0",
                
                "maskingunmasked:emotionfearful = 0", "maskingunmasked:taskID = 0", 
                
                "emotionfearful > 0", "emotionfearful + emotionfearful:taskID > 0",
                "emotionfearful + maskingunmasked:emotionfearful > 0", 
                "emotionfearful + maskingunmasked:emotionfearful + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID > 0",
                
                "emotionfearful = 0", "emotionfearful + emotionfearful:taskID = 0",
                "emotionfearful + maskingunmasked:emotionfearful = 0",
                
                "taskID > 0", "taskID + emotionfearful:taskID > 0",
                "taskID + maskingunmasked:taskID > 0",
                "taskID + maskingunmasked:taskID + emotionfearful:taskID + maskingunmasked:emotionfearful:taskID > 0")


hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\p3_bayesian_models.RData")

###################### early VAN by response evaluation ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_corr_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(corr_ev_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-corr_ev_models["erp_amp ~ 1 + emotion * response + (1 + emotion * response | subject)"][[1]]
summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'response', conditions = make_conditions(ev_data, vars = c('emotion' )))
ce_dt <- as.data.frame(ce[['response']])

hypotheses <- c("responseincorrect = 0", "responseincorrect + emotionfearful:responseincorrect = 0",
                "emotionfearful = 0")

hypothesis(winning_model, hypotheses)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_corr_bayesian_models.RData")


###################### late VAN by response evaluation ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_corr_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(corr_lv_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

max_model<-corr_lv_models["erp_amp ~ 1 + emotion * response + (1 + emotion * response | subject)"][[1]]
summary(max_model)

ce <-conditional_effects(max_model, effect = 'response', conditions = make_conditions(lv_data, vars = c('emotion' )))
ce_dt <- as.data.frame(ce[['response']])

hypotheses <- c("responseincorrect = 0", "responseincorrect + emotionfearful:responseincorrect = 0",
                "emotionfearful = 0")

hypothesis(max_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_corr_bayesian_models.RData")

###################### early lateralized VAN by response evaluation ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_corr_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(corr_lat_ev_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

max_model<-corr_lat_ev_models["erp_amp ~ 1 + side * response + (1 + side * response | subject)"][[1]]
summary(max_model)

ce <-conditional_effects(max_model, effect = 'response', conditions = make_conditions(lat_ev_data, vars = c('side' )))
ce_dt <- as.data.frame(ce[['response']])

hypotheses <- c("responseincorrect = 0", "responseincorrect + sidecontra:responseincorrect = 0",
                "sidecontra = 0")

hypothesis(max_model, hypotheses)


save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_corr_bayesian_models.RData")


###################### late lateralized VAN by response evaluation ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_corr_bayesian_models.RData")

loo_comp <- as.data.frame(loo_compare(corr_lat_lv_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)


winning_model<-corr_lat_lv_models["erp_amp ~ 1 + side + response + (1 + side * response | subject)"][[1]]
summary(winning_model)

ce_resp <-conditional_effects(winning_model, effect = 'response')
ce_resp_dt <- as.data.frame(ce_resp[['response']])

ce_side <-conditional_effects(winning_model, effect = 'side')
ce_side_dt <- as.data.frame(ce_side[['side']])

hypotheses <- c("responseincorrect = 0", "sidecontra = 0")

hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_corr_bayesian_models.RData")

