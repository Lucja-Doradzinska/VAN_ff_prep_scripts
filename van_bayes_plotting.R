rm(list=ls())
library(tidyverse)
library(patchwork)

load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_early_bayesian_models.RData")
#load("D:\\ff_experiment\\Results\\EEG\\models\\van_temp_late_bayesian_models.RData")
#load("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_early_bayesian_models.RData")
#load("D:\\ff_experiment\\Results\\EEG\\models\\van_occ_late_bayesian_models.RData")
#load("D:\\ff_experiment\\Results\\EEG\\models\\p3_bayesian_models.RData")

cbPalette <- c("#fb8961ff", "#762181ff")


ce_dt <- as.data.frame(ce[['masking']])
ce_irrel_dt <- filter(ce_dt, task == 'DP')
ce_rel_dt <- filter(ce_dt, task == 'ID')

plot_irrel <- ggplot(ce_irrel_dt, aes(x = emotion, color = masking)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="ERP values [μV]",
                     breaks = seq(0, 8, 2), #(-6, 2, 2), #,(-4, 4, 2) #(-4, 4, 2),(-3, 9, 3)
                     limits = c(0, 8), 
                     expand = c(0,0),
                     labels=c("0", "2", "4", "6", "8")) +
  scale_x_discrete(name ="emotion",
                   limits=c("neutral","fearful"), expand = c(0.2,0.2))+
  scale_color_manual(name = "masking", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.7,0.3, 0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  ggtitle('irrelevant')
plot_irrel

plot_rel <- ggplot(ce_rel_dt, aes(x = emotion, color = masking)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="ERP values [μV]",
                     breaks = seq(0, 8, 2), #(-6, 2, 2), #,(-4, 4, 2) #(-4, 4, 2),(-3, 9, 3)
                     limits = c(0, 8), 
                     expand = c(0,0),
                     labels=c("0", "2", "4", "6", "8")) +
  scale_x_discrete(name ="emotion",
                   limits=c("neutral","fearful"), expand = c(0.2,0.2))+
  scale_color_manual(name = "masking", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.3,0.3, 0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  ggtitle('relevant')
plot_rel

plot_all_H <- plot_irrel + plot_rel  +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
plot_all_H


load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_early_bayesian_models.RData")
#load("D:\\ff_experiment\\Results\\EEG\\models\\van_lat_late_bayesian_models.RData")

cbPalette <- c("#fb8961ff", "#762181ff")

ce_dt <- as.data.frame(ce[['masking']])
ce_irrel_dt <- filter(ce_dt, task == 'DP')
ce_rel_dt <- filter(ce_dt, task == 'ID')

plot_irrel <- ggplot(ce_irrel_dt, aes(x = side, color = masking)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="ERP values [μV]",
                     breaks = seq(-2,6, 2), 
                     limits = c(-2, 6), 
                     expand = c(0,0),
                     labels=c("-2", "0", "2", "4", "6")) +
  scale_x_discrete(name ="side",
                   limits=c("ipsi","contra"), expand = c(0.2,0.2))+
  scale_color_manual(name = "masking", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.7, 0.3,0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  ggtitle('irrelevant')
plot_irrel

plot_rel <- ggplot(ce_rel_dt, aes(x = side, color = masking)) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__),width=0, linewidth=1, position = position_dodge(width=0.4)) +
  geom_point(aes(y=estimate__), size=2, position = position_dodge(width=0.4)) +
  scale_y_continuous(name ="ERP values [μV]",
                     breaks = seq(-2,6, 2), 
                     limits = c(-2, 6), 
                     expand = c(0,0),
                     labels=c("-2", "0", "2", "4", "6")) +
  scale_x_discrete(name ="side",
                   limits=c("ipsi","contra"), expand = c(0.2,0.2))+
  scale_color_manual(name = "masking", values = cbPalette) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "gray79"), panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'gray79'), 
        panel.grid.minor = element_blank(), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(margin=margin(0,0,20,0)),
        plot.margin = margin(0.3,0.3,0.3, 0.3, "cm"), legend.position="bottom",
        legend.text=element_text(size=11)) +
  ggtitle('relevant')
plot_rel

plot_all_H <- plot_irrel + plot_rel  +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
plot_all_H
