rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(emmeans)
library(loo)
library(patchwork)

#library(doParallel)
#cores<-detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)
options(mc.cores = parallel::detectCores())
######################  Accuracy ###################### 
#importing data
rm(list=ls())

library(brms)
library(readxl)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())
source("E:\\BM_functions\\factors.r")

data <- read_excel("D:\\ff_experiment\\Results\\behavior\\beh_logs\\raw_beh_long.xlsx")
data = transform(data, masking=factor(masking), target = factor(target, levels = c('neutral', 'fearful')), 
                 dist_face = factor(dist_face, levels = c('neutral', 'fearful')))

data$resp <- 0
data$resp[data$target == "fearful" & data$response == "incorrect"] <- 1
data$resp[data$target == "neutral" & data$response == "correct"] <- 1

data = filter(data, task == 'ID')

formulas = get_all_combs(factors = list("target", "masking", "dist_face"),
                         outcome = "resp", random = "(1 | subject)")

acc_models <- list()
acc_loos <- list()
for (f in formulas){
  if (f == formulas[[1]]){
    model<- brm(formula = bf(f), data = data ,family = bernoulli(link = "probit"),
                sample_prior = "yes", chains = 2,iter = 8000, cores = 2,  warmup = 4000)
    
  } else{
    model<- brm(formula = bf(f), data = data ,family = bernoulli(link = "probit"), prior = set_prior("normal(0, 10)", class = "b"), 
                sample_prior = "yes", chains = 2,iter = 8000, cores = 2,  warmup = 4000)
  }
  
  acc_models <- c(acc_models, list(model))
}

names(acc_models) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\behavior\\models\\acc_dist_face_bayesian_model.RData")


for (model in acc_models){
  acc_loos <-  c(acc_loos, list(loo(model, cores = 2)))
}

names(acc_loos) <- as.character(formulas)
save.image("D:\\ff_experiment\\Results\\behavior\\models\\acc_dist_face_bayesian_model.RData")


######################## evaluation
rm(list=ls())

library(brms)
library(tidyverse)
library(loo)

load("D:\\ff_experiment\\Results\\behavior\\models\\acc_dist_face_bayesian_model.RData")

loo_comp <- as.data.frame(loo_compare(acc_loos))
loo_comp$SEs <- abs(loo_comp$elpd_diff / loo_comp$se_diff)

winning_model<-acc_models["resp ~ 1 + target * masking * dist_face + (1 | subject)"][[1]]

summary(winning_model)
ce <-conditional_effects(winning_model, effect = 'target', conditions = make_conditions(data, vars = c('masking', 'dist_face')))
ce_acc_dt <- as.data.frame(ce$target)

ce


hypotheses <- c("dist_facefearful > 0", "dist_facefearful + targetfearful:dist_facefearful > 0",
                "dist_facefearful + maskingunmasked:dist_facefearful > 0", 
                "dist_facefearful + maskingunmasked:dist_facefearful + targetfearful:dist_facefearful + targetfearful:maskingunmasked:dist_facefearful > 0",
                
                "dist_facefearful = 0", "dist_facefearful + targetfearful:dist_facefearful = 0",
                "dist_facefearful + maskingunmasked:dist_facefearful = 0")

h_tests <- hypothesis(winning_model, hypotheses)

save.image("D:\\ff_experiment\\Results\\behavior\\models\\acc_dist_face_bayesian_model.RData")



################################# PLOT
rm(list=ls())
library(patchwork)
load("D:\\ff_experiment\\Results\\behavior\\models\\acc_dist_face_bayesian_model.RData")

cbPalette <- c("#c33b75ff", "#120d31ff")

ce_acc_dt <- as.data.frame(ce$target)
ce_acc_dt$dist_face <-factor(ce_acc_dt$dist_face, levels = c('neutral', 'fearful'))
ce_acc_masked_dt <- filter(ce_acc_dt, masking == 'masked')
ce_acc_unmasked_dt <- filter(ce_acc_dt, masking == 'unmasked')

plot_masked <- ggplot(ce_acc_masked_dt, aes(x = target, color = dist_face)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="\'neutral\' responses [%]",
                     breaks = seq(0, 1, 0.25), 
                     limits = c(0,1), 
                     expand = c(0,0),
                     labels=c("0", "25", "50", "75", "100")) +
  scale_x_discrete(name ="target",
                   limits=c("neutral","fearful"), expand = c(0.2,0.2))+
  scale_color_manual(name = "second face", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.3,0.7, 0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  #plot.margin = margin(1,1,1,1, "cm")
  ggtitle('masked')
plot_masked

plot_unmasked <- ggplot(ce_acc_unmasked_dt, aes(x = target, color = dist_face)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="\'neutral\' responses [%]",
                     breaks = seq(0, 1, 0.25), 
                     limits = c(0,1), 
                     expand = c(0,0),
                     labels=c("0", "25", "50", "75", "100")) +
  scale_x_discrete(name ="target",
                   limits=c("neutral","fearful"), expand = c(0.2,0.2))+
  scale_color_manual(name = "second face", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.3,0.3, 0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  #plot.margin = margin(1,1,1,1, "cm")
  ggtitle('unmasked')
plot_unmasked

plot_all <- plot_masked / plot_unmasked + 
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
plot_all

plot_all_H <- plot_masked + plot_unmasked  +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
plot_all_H
