
###################### early VAN ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
ev_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_temporal_early_amps_long_format.xlsx")
ev_data = transform(ev_data, emotion=factor(faces, levels = c('neutral', 'fearful')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(ev_data, abs(scale(erp_amp)) > 3)
ev_data = filter(ev_data, abs(scale(erp_amp)) <= 3)

#################### ev models
formulas = get_all_combs(factors = list("masking", "emotion", "task"),
                         outcome = "erp_amp", random = "(1 + masking*emotion*task | subject)")

early_van_models <- list()
early_van_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = ev_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = ev_data ,family = gaussian(link = 'identity'), 
              set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
              sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  early_van_models <- c(early_van_models, list(model))
}

names(early_van_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_bayesian_models.RData")

for (model in early_van_models){
  early_van_loos <-  c(early_van_loos, list(loo(model, cores = 2)))
}

names(early_van_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_bayesian_models.RData")

###################### VAN_late ###################### 
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
lv_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_temporal_late_amps_long_format.xlsx")
lv_data = transform(lv_data, emotion=factor(faces, levels = c('neutral', 'fearful')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(lv_data, abs(scale(erp_amp)) > 3)
lv_data = filter(lv_data, abs(scale(erp_amp)) <= 3)

#################### lv models
formulas = get_all_combs(factors = list("masking", "emotion", "task"),
                         outcome = "erp_amp", random = "(1 + masking*emotion*task | subject)")

late_van_models <- list()
late_van_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lv_data ,family = gaussian(link = 'identity'), 
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lv_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  late_van_models <- c(late_van_models, list(model))
}
names(late_van_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_bayesian_models.RData")

for (model in late_van_models){
  late_van_loos <-  c(late_van_loos, list(loo(model, cores = 2)))
}

names(late_van_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_bayesian_models.RData")


###################### early VAN occipital ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
ev_occ_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_occipital_early_amps_long_format.xlsx")
ev_occ_data = transform(ev_occ_data, emotion=factor(faces, levels = c('neutral', 'fearful')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(ev_occ_data, abs(scale(erp_amp)) > 3)
ev_occ_data = filter(ev_occ_data, abs(scale(erp_amp)) <= 3)

#################### ev occ models
formulas = get_all_combs(factors = list("masking", "emotion", "task"),
                         outcome = "erp_amp", random = "(1 + masking*emotion*task | subject)")

early_van_occ_models <- list()
early_van_occ_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = ev_occ_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = ev_occ_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  early_van_occ_models <- c(early_van_occ_models, list(model))
}

names(early_van_occ_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_early_bayesian_models.RData")

for (model in early_van_occ_models){
  early_van_occ_loos <-  c(early_van_occ_loos, list(loo(model, cores = 2)))
}

names(early_van_occ_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_early_bayesian_models.RData")


###################### late VAN occipital ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
lv_occ_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_occipital_late_amps_long_format.xlsx")
lv_occ_data = transform(lv_occ_data, emotion=factor(faces, levels = c('neutral', 'fearful')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(lv_occ_data, abs(scale(erp_amp)) > 3)
lv_occ_data = filter(lv_occ_data, abs(scale(erp_amp)) <= 3)

#################### lv occ models
formulas = get_all_combs(factors = list("masking", "emotion", "task"),
                         outcome = "erp_amp", random = "(1 + masking*emotion*task | subject)")


late_van_occ_models <- list()
late_van_occ_loos <- list()

for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lv_occ_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(int_model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lv_occ_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  late_van_occ_models <- c(late_van_occ_models, list(model))
}


names(late_van_occ_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_late_bayesian_models.RData")

for (model in late_van_occ_models){
  late_van_occ_loos <-  c(late_van_occ_loos, list(loo(model, cores = 2)))
}

names(late_van_occ_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_late_bayesian_models.RData")


###################### lateralized VAN early ###################### 
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
library(parallel)
options(mc.cores = parallel::detectCores())
source("E:\\BM_functions\\factors.r")

#################### load data
lat_ev_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_early_amps_long_format.xlsx")
lat_ev_data = transform(lat_ev_data, side=factor(side, levels = c('ipsi', 'contra')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(lat_ev_data, abs(scale(erp_amp)) > 3)
lat_ev_data = filter(lat_ev_data, abs(scale(erp_amp)) <= 3)

#################### lat ev models
formulas = get_all_combs(factors = list("masking", "side", "task"),
                         outcome = "erp_amp", random = "(1 + masking*side*task | subject)")

lat_early_van_models <- list()
lat_early_van_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lat_ev_data ,family = gaussian(link = 'identity'), 
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lat_ev_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  lat_early_van_models <- c(lat_early_van_models, list(model))
}

names(lat_early_van_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_bayesian_models.RData")


for (model in lat_early_van_models){
  lat_early_van_loos <-  c(lat_early_van_loos, list(loo(model, cores = 2)))
}

names(lat_early_van_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_bayesian_models.RData")


###################### lateralized VAN late ###################### 
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())
source("E:\\BM_functions\\factors.r")

#################### load data
lat_lv_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_late_amps_long_format.xlsx")
lat_lv_data = transform(lat_lv_data, side=factor(side, levels = c('ipsi', 'contra')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(lat_lv_data, abs(scale(erp_amp)) > 3)
lat_lv_data = filter(lat_lv_data, abs(scale(erp_amp)) <= 3)

#################### lat lv models
formulas = get_all_combs(factors = list("masking", "side", "task"),
                         outcome = "erp_amp", random = "(1 + masking*side*task | subject)")

lat_late_van_models <- list()
lat_late_van_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lat_lv_data ,family = gaussian(link = 'identity'), 
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lat_lv_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  lat_late_van_models <- c(lat_late_van_models, list(model))

}

names(lat_late_van_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_bayesian_models.RData")

for (model in lat_late_van_models){
  lat_late_van_loos <-  c(lat_late_van_loos, list(loo(model, cores = 2)))
}

names(lat_late_van_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_bayesian_models.RData")



###################### P3b ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
p3_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\P3b_amps_long_format.xlsx")
p3_data = transform(p3_data, emotion=factor(faces, levels = c('neutral', 'fearful')), task=factor(task), masking=factor(masking), subject = factor(subject))


#filtering out outliers
outliers <- filter(p3_data, abs(scale(erp_amp)) > 3)
p3_data = filter(p3_data, abs(scale(erp_amp)) <= 3)

#################### p3 models
formulas = get_all_combs(factors = list("masking", "emotion", "task"),
                         outcome = "erp_amp", random = "(1 + masking*emotion*task | subject)")

p3_models <- list()
p3_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = p3_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = p3_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  p3_models <- c(p3_models, list(model))
}

names(p3_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\p3_bayesian_models.RData")

for (model in p3_models){
  p3_loos <-  c(p3_loos, list(loo(model, cores = 2)))
}

names(p3_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\p3_bayesian_models.RData")


###################### early VAN by resp ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
ev_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_temporal_early_amps_long_format.xlsx")
ev_data = transform(ev_data, emotion=factor(faces, levels = c('neutral', 'fearful')), response=factor(response), subject = factor(subject))

#filtering out outliers
outliers <- filter(ev_data, abs(scale(erp_amp)) > 3)
ev_data = filter(ev_data, abs(scale(erp_amp)) <= 3)

ev_data <-filter(ev_data, task == 'ID')
ev_data <-filter(ev_data, masking == 'masked')


#################### models
formulas = get_all_combs(factors = list("emotion", "response"),
                         outcome = "erp_amp", random = "(1 + emotion*response | subject)")

corr_ev_models <- list()
corr_ev_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = ev_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = ev_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  corr_ev_models <- c(corr_ev_models, list(model))
}

names(corr_ev_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_corr_bayesian_models.RData")


########################loos
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_corr_bayesian_models.RData")
for (model in corr_ev_models){
  corr_ev_loos <-  c(corr_ev_loos, list(loo(model, cores = 2)))
}

names(corr_ev_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_corr_bayesian_models.RData")

###################### late VAN by resp ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
lv_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_temporal_late_amps_long_format.xlsx")
lv_data = transform(lv_data, emotion=factor(faces, levels = c('neutral', 'fearful')), response=factor(response), subject = factor(subject))

#filtering out outliers
outliers <- filter(lv_data, abs(scale(erp_amp)) > 3)
lv_data = filter(lv_data, abs(scale(erp_amp)) <= 3)

lv_data <-filter(lv_data, task == 'ID')
lv_data <-filter(lv_data, masking == 'masked')


#################### models
formulas = get_all_combs(factors = list("emotion", "response"),
                         outcome = "erp_amp", random = "(1 + emotion*response | subject)")

corr_lv_models <- list()
corr_lv_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lv_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lv_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  corr_lv_models <- c(corr_lv_models, list(model))
}

names(corr_lv_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_corr_bayesian_models.RData")


########################loos
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_corr_bayesian_models.RData")
for (model in corr_lv_models){
  corr_lv_loos <-  c(corr_lv_loos, list(loo(model, cores = 2)))
}

names(corr_lv_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_corr_bayesian_models.RData")


###################### early lateralized VAN by resp ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
lat_ev_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_early_amps_long_format.xlsx")
lat_ev_data = transform(lat_ev_data, side=factor(side_, levels = c('ipsi', 'contra')), response=factor(response), subject = factor(subject))

#filtering out outliers
outliers <- filter(lat_ev_data, abs(scale(erp_amp)) > 3)
lat_ev_data = filter(lat_ev_data, abs(scale(erp_amp)) <= 3)

lat_ev_data <-filter(lat_ev_data, task == 'ID')
lat_ev_data <-filter(lat_ev_data, masking == 'masked')


#################### models
formulas = get_all_combs(factors = list("side", "response"),
                         outcome = "erp_amp", random = "(1 + side*response | subject)")

corr_lat_ev_models <- list()
corr_lat_ev_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lat_ev_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lat_ev_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  corr_lat_ev_models <- c(corr_lat_ev_models, list(model))
}

names(corr_lat_ev_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_corr_bayesian_models.RData")


########################loos
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_corr_bayesian_models.RData")
for (model in corr_lat_ev_models){
  corr_lat_ev_loos <-  c(corr_lat_ev_loos, list(loo(model, cores = 2)))
}

names(corr_lat_ev_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_corr_bayesian_models.RData")


###################### late lateralized VAN by resp ######################
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")

#################### load data
lat_lv_data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_late_amps_long_format.xlsx")
lat_lv_data = transform(lat_lv_data, side=factor(side_, levels = c('ipsi', 'contra')), response=factor(response), subject = factor(subject))

#filtering out outliers
outliers <- filter(lat_lv_data, abs(scale(erp_amp)) > 3)
lat_lv_data = filter(lat_lv_data, abs(scale(erp_amp)) <= 3)

lat_lv_data <-filter(lat_lv_data, task == 'ID')
lat_lv_data <-filter(lat_lv_data, masking == 'masked')


#################### models
formulas = get_all_combs(factors = list("side", "response"),
                         outcome = "erp_amp", random = "(1 + side*response | subject)")

corr_lat_lv_models <- list()
corr_lat_lv_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = lat_lv_data ,family = gaussian(link = 'identity'),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
    sd_erp<-summary(model)$spec_pars$Estimate
    
  } else{
    model<- brm(formula = bf(f), data = lat_lv_data ,family = gaussian(link = 'identity'), 
                set_prior(paste("normal(0,", 10*sd_erp, ")", sep = ""), class = "b"),
                sample_prior = "yes", chains = 2, iter = 8000, cores = 2, warmup = 4000)
  }
  
  corr_lat_lv_models <- c(corr_lat_lv_models, list(model))
}

names(corr_lat_lv_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_corr_bayesian_models.RData")


########################loos
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())

source("E:\\BM_functions\\factors.r")


load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_corr_bayesian_models.RData")
for (model in corr_lat_lv_models){
  corr_lat_lv_loos <-  c(corr_lat_lv_loos, list(loo(model, cores = 2)))
}

names(corr_lat_lv_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_corr_bayesian_models.RData")


