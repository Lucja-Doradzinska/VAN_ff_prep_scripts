library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)
library(effectsize)
library(emmeans)
emm_options(lmer.df = "satterthwaite") 
emm_options(lmerTest.limit = 51126)

###################### early VAN ###################### 

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_early_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m + t | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m + t || subject), data=data, 
                             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_reduced_model)
summary(rePCA(uncorr_reduced_model))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
early_VAN_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + task + masking |subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(early_VAN_model)

#checking normality assumptions
qqnorm(resid(early_VAN_model))
qqline(resid(early_VAN_model))


#extracting anova
early_VAN_rand_table <- rand(early_VAN_model) #values are for the model without the listed effect!!!
early_VAN_rand_table

early_VAN_anova_table <- anova(early_VAN_model)
early_VAN_anova_table

early_VAN_masking_task_contr_table = summary(emmeans(early_VAN_model, pairwise~ masking | task, combine = T))$contrasts
early_VAN_masking_task_contr_table

early_VAN_masking_task_emmeans <- summary(emmeans(early_VAN_model, pairwise ~  masking | task, combine = T))$emmeans
early_VAN_masking_task_emmeans


early_VAN_task_emmeans <- summary(emmeans(early_VAN_model, pairwise ~  task, combine = T))$emmeans
early_VAN_task_emmeans


early_VAN_task_contr_table = summary(emmeans(early_VAN_model, pairwise~ task | masking, combine = T))$contrasts
early_VAN_task_contr_table




###################### VAN_late ###################### 

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_late_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m + t | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m + t || subject), data=data, 
                             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_reduced_model)
summary(rePCA(uncorr_reduced_model))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
late_VAN_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + task + masking |subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(VAN_model)

#checking normality assumptions
qqnorm(resid(late_VAN_model))
qqline(resid(late_VAN_model))


#extracting anova
late_VAN_rand_table <- rand(late_VAN_model) #values are for the model without the listed effect!!!
late_VAN_rand_table

late_VAN_anova_table <- anova(late_VAN_model)
late_VAN_anova_table

late_VAN_masking_task_contr_table = summary(emmeans(late_VAN_model, pairwise~ masking | task, combine = T))$contrasts
late_VAN_masking_task_contr_table

late_VAN_masking_task_emmeans <- summary(emmeans(late_VAN_model, pairwise ~  masking | task, combine = T))$emmeans
late_VAN_masking_task_emmeans


late_VAN_masking_emotion_contr_table = summary(emmeans(late_VAN_model, pairwise~ masking | emotion, combine = T))$contrasts
late_VAN_masking_emotion_contr_table

late_VAN_masking_emotion_emmeans <- summary(emmeans(late_VAN_model, pairwise ~  masking | emotion, combine = T))$emmeans
late_VAN_masking_emotion_emmeans


late_VAN_task_contr_table = summary(emmeans(late_VAN_model, pairwise~ task |masking, combine = T))$contrasts
late_VAN_task_contr_table


###################### lateralized VAN early ###################### 
#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_early_amps_long_format.xlsx")
data = transform(data, side=factor(side), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$s <- 0.5
data$s[data$side == "ipsi"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$side) <- rbind(-.5, .5)
colnames(contrasts(data$side)) <- levels(data$side)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + s * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + s * t * m + (1 + s * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t || subject), data=data, 
                             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_reduced_model)
summary(rePCA(uncorr_reduced_model))



#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + s * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
VAN_lat_early_model <- lmer(erp_amp ~ 1 + side * task * masking + (1 + task * masking | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(VAN_lat_early_model)

#checking normality assumptions
qqnorm(resid(VAN_lat_early_model))
qqline(resid(VAN_lat_early_model))


#extracting anova
VAN_lat_early_rand_table <- rand(VAN_lat_early_model) #values are for the model without the listed effect!!!
VAN_lat_early_rand_table

VAN_lat_early_anova_table <- anova(VAN_lat_early_model)
VAN_lat_early_anova_table

VAN_lat_early_masking_task_contr_table = summary(emmeans(VAN_lat_early_model, pairwise~ masking | task, combine = T))$contrasts
VAN_lat_early_masking_task_contr_table

VAN_lat_early_masking_task_emmeans <- summary(emmeans(VAN_lat_early_model, pairwise ~  masking | task, combine = T))$emmeans
VAN_lat_early_masking_task_emmeans


lat_VAN_masking_side_contr_table = summary(emmeans(VAN_lat_early_model, pairwise~ masking | side, combine = T))$contrasts
lat_VAN_masking_side_contr_table

lat_VAN_masking_side_emmeans <- summary(emmeans(VAN_lat_early_model, pairwise ~  masking | side, combine = T))$emmeans
lat_VAN_masking_side_emmeans


lat_VAN_task_contr_table = summary(emmeans(VAN_lat_early_model, pairwise~ task |masking, combine = T))$contrasts
lat_VAN_task_contr_table

lat_VAN_side_contr_table = summary(emmeans(VAN_lat_early_model, pairwise~ side |masking, combine = T))$contrasts
lat_VAN_side_contr_table

lat_VAN_side_emmeans <- summary(emmeans(VAN_lat_early_model, pairwise ~  side, combine = T))$emmeans
lat_VAN_side_emmeans


###################### lateralized VAN late ###################### 
#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\VAN_lat_late_amps_long_format.xlsx")
data = transform(data, side=factor(side), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$s <- 0.5
data$s[data$side == "ipsi"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$side) <- rbind(-.5, .5)
colnames(contrasts(data$side)) <- levels(data$side)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + s * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + s * t * m + (1 + s * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t || subject), data=data, 
                             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_reduced_model)
summary(rePCA(uncorr_reduced_model))



#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + s * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
lat_VAN_model <- lmer(erp_amp ~ 1 + side * task * masking + (1 + task * masking | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(lat_VAN_model)

#checking normality assumptions
qqnorm(resid(lat_VAN_model))
qqline(resid(lat_VAN_model))


#extracting anova
lat_VAN_rand_table <- rand(lat_VAN_model) #values are for the model without the listed effect!!!
lat_VAN_rand_table

lat_VAN_anova_table <- anova(lat_VAN_model)
lat_VAN_anova_table

lat_VAN_masking_task_contr_table = summary(emmeans(lat_VAN_model, pairwise~ masking | task, combine = T))$contrasts
lat_VAN_masking_task_contr_table

lat_VAN_masking_task_emmeans <- summary(emmeans(lat_VAN_model, pairwise ~  masking | task, combine = T))$emmeans
lat_VAN_masking_task_emmeans


lat_VAN_masking_side_contr_table = summary(emmeans(lat_VAN_model, pairwise~ masking | side, combine = T))$contrasts
lat_VAN_masking_side_contr_table

lat_VAN_masking_side_emmeans <- summary(emmeans(lat_VAN_model, pairwise ~  masking | side, combine = T))$emmeans
lat_VAN_masking_side_emmeans


lat_VAN_task_contr_table = summary(emmeans(lat_VAN_model, pairwise~ task |masking, combine = T))$contrasts
lat_VAN_task_contr_table

lat_VAN_side_contr_table = summary(emmeans(lat_VAN_model, pairwise~ side |masking, combine = T))$contrasts
lat_VAN_side_contr_table







###################### P3 ###################### effects of masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\P3b_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
P3_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking * task| subject), data=data, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(P3_model)

#checking normality assumptions
qqnorm(resid(P3_model))
qqline(resid(P3_model))


#extracting anova
P3_rand_table <- rand(P3_model) 
P3_rand_table

P3_anova_table <- anova(P3_model)
P3_anova_table


P3_emotion_contr_table = summary(emmeans(P3_model, pairwise~ emotion | masking * task, combine = T))$contrasts
P3_emotion_contr_table
p.adjust(P3_emotion_contr_table$p.value, method = 'holm')

P3_emmeans <- summary(emmeans(P3_model, pairwise~emotion | masking * task, combine = T))$emmeans
P3_emmeans

P3_task_contr_table = summary(emmeans(P3_model, pairwise~ task | masking, combine = T))$contrasts
P3_task_contr_table

P3_masking_contr_table = summary(emmeans(P3_model, pairwise~ masking | emotion * task, combine = T))$contrasts
P3_masking_contr_table
p.adjust(P3_masking_contr_table$p.value, method = 'holm')

P3_task_emmeans <- summary(emmeans(P3_model, pairwise~task|masking, combine = T))$emmeans
P3_task_emmeans

