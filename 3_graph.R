###########################################################
# Description:
# This script processes simulated and experimental data to
# create the graphs of the paper
###########################################################

# load necessaries libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)
library(patchwork)

# --------------------------------------------------------
# SIMULATION DATA IMPORT
# --------------------------------------------------------

# Set working directory (adjust to your system)
setwd("/home/bernard-costa/Documents/0_github")

# import simulation data
sim_output1 <- read_delim("2_1_sim_output1.csv",
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(mean_response = col_number(), 
                                           accuracy = col_number(), pr_shift = col_number(), 
                                           pr_shift_win = col_number(), pr_shift_lose = col_number(), 
                                           lag = col_number(), auto_correlation = col_number(), 
                                           cross_correlation = col_number()), trim_ws = TRUE)

sim_output2 <- read_delim("2_1_sim_output2.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(cross_valor = col_number()), 
                          trim_ws = TRUE)

sim_output3 <- read_delim("2_1_sim_output3.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(cross_valor = col_number()), 
                          trim_ws = TRUE)


# group, filter and summarize simulation data
sim_mean <- sim_output1 %>%
  filter(type!="SURROGATE") %>%
  group_by(behaviour,sequence,type) %>%
  summarise(
    count = n(),
    meanRES=mean(mean_response,na.rm=TRUE),
    sdRES=sd(mean_response,na.rm=TRUE)
  )

sim_acc <- sim_output1 %>%
  filter(type!="INPUT") %>%
  group_by(behaviour,sequence,type) %>%
  summarise(
    count = n(),
    meanACC=mean(accuracy,na.rm=TRUE),
    sdACC=sd(accuracy,na.rm=TRUE)
  )

sim_auto_M0<- sim_output1 %>%
  filter(type!="INPUT") %>%
  filter(sequence=="M0") %>%
  group_by(behaviour,sequence,type,lag) %>%
  summarise(
    count = n(),
    meanAUTOM0=mean(auto_correlation,na.rm=TRUE),
    sdAUTOM0=sd(auto_correlation,na.rm=TRUE)
  )

sim_auto_M2<- sim_output1 %>%
  filter(type!="INPUT") %>%
  filter(sequence=="M2") %>%
  group_by(behaviour,sequence,type,lag) %>%
  summarise(
    count = n(),
    meanAUTOM2=mean(auto_correlation,na.rm=TRUE),
    sdAUTOM2=sd(auto_correlation,na.rm=TRUE)
  )

sim_cross_M0<- sim_output1 %>%
  filter(type!="INPUT") %>%
  filter(sequence=="M0") %>%
  group_by(behaviour,sequence,type,lag) %>%
  summarise(
    count = n(),
    meanCROSSM0=mean(cross_correlation,na.rm=TRUE),
    sdCROSSM0=sd(cross_correlation,na.rm=TRUE)
  )

sim_cross_M2<- sim_output1 %>%
  filter(type!="INPUT") %>%
  filter(sequence=="M2") %>%
  group_by(behaviour,sequence,type,lag) %>%
  summarise(
    count = n(),
    meanCROSSM2=mean(cross_correlation,na.rm=TRUE),
    sdCROSSM2=sd(cross_correlation,na.rm=TRUE)
  )

sim_prshift <- sim_output1 %>%
  filter(type!="INPUT") %>%
  group_by(behaviour,sequence,type) %>%
  summarise(
    count = n(),
    meanSHIFT=mean(pr_shift,na.rm=TRUE),
    sdSHIFT=sd(pr_shift,na.rm=TRUE)
  )

sim_prshift_win <- sim_output1 %>%
  filter(type!="INPUT") %>%
  group_by(behaviour,sequence,type) %>%
  summarise(
    count = n(),
    meanWIN=mean(pr_shift_win,na.rm=TRUE),
    sdWIN=sd(pr_shift_win,na.rm=TRUE)
  )

sim_prshift_lose<- sim_output1 %>%
  filter(type!="INPUT") %>%
  group_by(behaviour,sequence,type) %>%
  summarise(
    count = n(),
    meanLOSE=mean(pr_shift_lose,na.rm=TRUE),
    sdLOSE=sd(pr_shift_lose,na.rm=TRUE)
  )

sim_scatterM0 <- sim_output1 %>%
  filter(lag=="0") %>%
  filter(type=="OUTPUT") %>%
  filter(sequence=="M0")

sim_scatterM2 <- sim_output1 %>%
  filter(lag=="0") %>%
  filter(type=="OUTPUT") %>%
  filter(sequence=="M2")

sim_rec<- sim_output3 %>%
  filter(sequence=="M2") %>%
  filter(type=="OUTPUT") %>%
  group_by(behaviour,sequence,type,element) %>%
  summarise(
    count = n(),
    meanREC=mean(rec,na.rm=TRUE),
    sdREC=sd(rec,na.rm=TRUE)
  )
# --------------------------------------------------------
# PLOTS: simulation
# --------------------------------------------------------

# plots mean response
plot1 = ggplot(subset(sim_mean, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(sequence, meanRES, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanRES, ymin=meanRES-sdRES, ymax=meanRES+sdRES,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='ALTERNATION',y='MEAN RESPONSE') +
  geom_point(aes(sequence, meanRES, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_mean, behaviour %in% 'WSLS')) +
  geom_line(aes(sequence, meanRES, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanRES, ymin=meanRES-sdRES, ymax=meanRES+sdRES,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='WSLS',y=NULL) +
  geom_point(aes(sequence, meanRES, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_mean, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(sequence, meanRES, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanRES, ymin=meanRES-sdRES, ymax=meanRES+sdRES,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='PERSEVERATION',y=NULL) +
  geom_point(aes(sequence, meanRES, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_mean, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(sequence, meanRES, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanRES, ymin=meanRES-sdRES, ymax=meanRES+sdRES,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='PROB. MATCHING',y=NULL) +
  geom_point(aes(sequence, meanRES, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_mean, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(sequence, meanRES, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanRES, ymin=meanRES-sdRES, ymax=meanRES+sdRES,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='IDEAL PREDICTOR',y=NULL) +
  geom_point(aes(sequence, meanRES, color=type)) +
  theme(legend.title = element_blank(),legend.position = c(0.5, 0.2)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)
                                                                                                                      

# plots accuracy
plot1 = ggplot(subset(sim_acc, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(sequence, meanACC, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanACC, ymin=meanACC-sdACC, ymax=meanACC+sdACC,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='ALTERNATION',y='ACCURACY') +
  geom_point(aes(sequence, meanACC, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_acc, behaviour %in% 'WSLS')) +
  geom_line(aes(sequence, meanACC, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanACC, ymin=meanACC-sdACC, ymax=meanACC+sdACC,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='WSLS',y=NULL) +
  geom_point(aes(sequence, meanACC, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_acc, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(sequence, meanACC, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanACC, ymin=meanACC-sdACC, ymax=meanACC+sdACC,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='PERSEVERATION',y=NULL) +
  geom_point(aes(sequence, meanACC, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_acc, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(sequence, meanACC, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanACC, ymin=meanACC-sdACC, ymax=meanACC+sdACC,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='PROB. MATCHING',y=NULL) +
  geom_point(aes(sequence, meanACC, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_acc, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(sequence, meanACC, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanACC, ymin=meanACC-sdACC, ymax=meanACC+sdACC,color=type), width=.1) +
  ylim(0.4,1) +
  labs(x='IDEAL PREDICTOR',y=NULL) +
  geom_point(aes(sequence, meanACC, color=type)) +
  theme(legend.title = element_blank(),legend.position = c(0.5, 0.2)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)

# plots autocorrelation
plot1 = ggplot(subset(sim_auto_M0,behaviour %in% 'ALTERNATION')) +
  geom_line(aes(lag, meanAUTOM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM0, ymin=meanAUTOM0-sdAUTOM0, ymax=meanAUTOM0+sdAUTOM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTOM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_auto_M2, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(lag, meanAUTOM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM2, ymin=meanAUTOM2-sdAUTOM2, ymax=meanAUTOM2+sdAUTOM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanAUTOM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_auto_M0,behaviour %in% 'WSLS')) +
  geom_line(aes(lag, meanAUTOM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM0, ymin=meanAUTOM0-sdAUTOM0, ymax=meanAUTOM0+sdAUTOM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTOM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_auto_M2, behaviour %in% 'WSLS')) +
  geom_line(aes(lag, meanAUTOM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM2, ymin=meanAUTOM2-sdAUTOM2, ymax=meanAUTOM2+sdAUTOM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanAUTOM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_auto_M0,behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(lag, meanAUTOM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM0, ymin=meanAUTOM0-sdAUTOM0, ymax=meanAUTOM0+sdAUTOM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTOM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot6 = ggplot(subset(sim_auto_M2, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(lag, meanAUTOM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM2, ymin=meanAUTOM2-sdAUTOM2, ymax=meanAUTOM2+sdAUTOM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanAUTOM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot7 = ggplot(subset(sim_auto_M0,behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(lag, meanAUTOM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM0, ymin=meanAUTOM0-sdAUTOM0, ymax=meanAUTOM0+sdAUTOM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTOM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot8 = ggplot(subset(sim_auto_M2, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(lag, meanAUTOM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM2, ymin=meanAUTOM2-sdAUTOM2, ymax=meanAUTOM2+sdAUTOM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanAUTOM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot9 = ggplot(subset(sim_auto_M0,behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(lag, meanAUTOM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM0, ymin=meanAUTOM0-sdAUTOM0, ymax=meanAUTOM0+sdAUTOM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTOM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot10 = ggplot(subset(sim_auto_M2, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(lag, meanAUTOM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanAUTOM2, ymin=meanAUTOM2-sdAUTOM2, ymax=meanAUTOM2+sdAUTOM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanAUTOM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'ALTERNATION',theme=theme(plot.title=element_text(hjust=0.5)))
plot3 + plot4+ plot_annotation(title = 'WIN-STAY/LOSE-SHIFT',theme=theme(plot.title=element_text(hjust=0.5)))
plot5 + plot6+ plot_annotation(title = 'PERSEVERATION',theme=theme(plot.title=element_text(hjust=0.5)))
plot7 + plot8+ plot_annotation(title = 'PROBABILITY MATCHING',theme=theme(plot.title=element_text(hjust=0.5)))
plot9 + plot10+ plot_annotation(title = 'IDEAL PREDICTOR',theme=theme(plot.title=element_text(hjust=0.5)))

# plots cross-corralation
plot1 = ggplot(subset(sim_cross_M0,behaviour %in% 'ALTERNATION')) +
  geom_line(aes(lag, meanCROSSM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM0, ymin=meanCROSSM0-sdCROSSM0, ymax=meanCROSSM0+sdCROSSM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSSM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_cross_M2, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(lag, meanCROSSM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM2, ymin=meanCROSSM2-sdCROSSM2, ymax=meanCROSSM2+sdCROSSM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanCROSSM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_cross_M0,behaviour %in% 'WSLS')) +
  geom_line(aes(lag, meanCROSSM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM0, ymin=meanCROSSM0-sdCROSSM0, ymax=meanCROSSM0+sdCROSSM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSSM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_cross_M2, behaviour %in% 'WSLS')) +
  geom_line(aes(lag, meanCROSSM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM2, ymin=meanCROSSM2-sdCROSSM2, ymax=meanCROSSM2+sdCROSSM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanCROSSM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_cross_M0,behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(lag, meanCROSSM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM0, ymin=meanCROSSM0-sdCROSSM0, ymax=meanCROSSM0+sdCROSSM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSSM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot6 = ggplot(subset(sim_cross_M2, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(lag, meanCROSSM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM2, ymin=meanCROSSM2-sdCROSSM2, ymax=meanCROSSM2+sdCROSSM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanCROSSM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot7 = ggplot(subset(sim_cross_M0,behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(lag, meanCROSSM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM0, ymin=meanCROSSM0-sdCROSSM0, ymax=meanCROSSM0+sdCROSSM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSSM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot8 = ggplot(subset(sim_cross_M2, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(lag, meanCROSSM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM2, ymin=meanCROSSM2-sdCROSSM2, ymax=meanCROSSM2+sdCROSSM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanCROSSM2, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot9 = ggplot(subset(sim_cross_M0,behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(lag, meanCROSSM0, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM0, ymin=meanCROSSM0-sdCROSSM0, ymax=meanCROSSM0+sdCROSSM0,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M0',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSSM0, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot10 = ggplot(subset(sim_cross_M2, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(lag, meanCROSSM2, group=type,colour = type)) +
  geom_errorbar(aes(lag, meanCROSSM2, ymin=meanCROSSM2-sdCROSSM2, ymax=meanCROSSM2+sdCROSSM2,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='M2',y=NULL) +
  geom_point(aes(lag, meanCROSSM2, color=type)) +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'ALTERNATION',theme=theme(plot.title=element_text(hjust=0.5)))
plot3 + plot4+ plot_annotation(title = 'WIN-STAY/LOSE-SHIFT',theme=theme(plot.title=element_text(hjust=0.5)))
plot5 + plot6+ plot_annotation(title = 'PERSEVERATION',theme=theme(plot.title=element_text(hjust=0.5)))
plot7 + plot8+ plot_annotation(title = 'PROBABILITY MATCHING',theme=theme(plot.title=element_text(hjust=0.5)))
plot9 + plot10+ plot_annotation(title = 'IDEAL PREDICTOR',theme=theme(plot.title=element_text(hjust=0.5)))

# plots shift
plot1 = ggplot(subset(sim_prshift, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(sequence, meanSHIFT, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanSHIFT, ymin=meanSHIFT-sdSHIFT, ymax=meanSHIFT+sdSHIFT,color=type), width=.1) +
  ylim(0,1) +
  labs(x='ALTERNATION',y='Pr(Shift)') +
  geom_point(aes(sequence, meanSHIFT, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_prshift, behaviour %in% 'WSLS')) +
  geom_line(aes(sequence, meanSHIFT, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanSHIFT, ymin=meanSHIFT-sdSHIFT, ymax=meanSHIFT+sdSHIFT,color=type), width=.1) +
  ylim(0,1) +
  labs(x='WSLS',y=NULL) +
  geom_point(aes(sequence, meanSHIFT, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_prshift, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(sequence, meanSHIFT, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanSHIFT, ymin=meanSHIFT-sdSHIFT, ymax=meanSHIFT+sdSHIFT,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PERSEVERATION',y=NULL) +
  geom_point(aes(sequence, meanSHIFT, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_prshift, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(sequence, meanSHIFT, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanSHIFT, ymin=meanSHIFT-sdSHIFT, ymax=meanSHIFT+sdSHIFT,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PROB. MATCHING',y=NULL) +
  geom_point(aes(sequence, meanSHIFT, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_prshift, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(sequence, meanSHIFT, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanSHIFT, ymin=meanSHIFT-sdSHIFT, ymax=meanSHIFT+sdSHIFT,color=type), width=.1) +
  ylim(0,1) +
  labs(x='IDEAL PREDICTOR',y=NULL) +
  geom_point(aes(sequence, meanSHIFT, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)

# pr shift win
plot1 = ggplot(subset(sim_prshift_win, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(sequence, meanWIN, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanWIN, ymin=meanWIN-sdWIN, ymax=meanWIN+sdWIN,color=type), width=.1) +
  ylim(0,1) +
  labs(x='ALTERNATION',y='Pr(Shift|win)') +
  geom_point(aes(sequence, meanWIN, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_prshift_win, behaviour %in% 'WSLS')) +
  geom_line(aes(sequence, meanWIN, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanWIN, ymin=meanWIN-sdWIN, ymax=meanWIN+sdWIN,color=type), width=.1) +
  ylim(0,1) +
  labs(x='WSLS',y=NULL) +
  geom_point(aes(sequence, meanWIN, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_prshift_win, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(sequence, meanWIN, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanWIN, ymin=meanWIN-sdWIN, ymax=meanWIN+sdWIN,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PERSEVERATION',y=NULL) +
  geom_point(aes(sequence, meanWIN, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_prshift_win, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(sequence, meanWIN, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanWIN, ymin=meanWIN-sdWIN, ymax=meanWIN+sdWIN,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PROB. MATCHING',y=NULL) +
  geom_point(aes(sequence, meanWIN, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_prshift_win, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(sequence, meanWIN, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanWIN, ymin=meanWIN-sdWIN, ymax=meanWIN+sdWIN,color=type), width=.1) +
  ylim(0,1) +
  labs(x='IDEAL PREDICTOR',y=NULL) +
  geom_point(aes(sequence, meanWIN, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)

# pr shift lose
plot1 = ggplot(subset(sim_prshift_lose, behaviour %in% 'ALTERNATION')) +
  geom_line(aes(sequence, meanLOSE, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanLOSE, ymin=meanLOSE-sdLOSE, ymax=meanLOSE+sdLOSE,color=type), width=.1) +
  ylim(0,1) +
  labs(x='ALTERNATION',y='Pr(Shift|lose)') +
  geom_point(aes(sequence, meanLOSE, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(sim_prshift_lose, behaviour %in% 'WSLS')) +
  geom_line(aes(sequence, meanLOSE, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanLOSE, ymin=meanLOSE-sdLOSE, ymax=meanLOSE+sdLOSE,color=type), width=.1) +
  ylim(0,1) +
  labs(x='WSLS',y=NULL) +
  geom_point(aes(sequence, meanLOSE, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot3 = ggplot(subset(sim_prshift_lose, behaviour %in% 'PERSEVERATION')) +
  geom_line(aes(sequence, meanLOSE, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanLOSE, ymin=meanLOSE-sdLOSE, ymax=meanLOSE+sdLOSE,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PERSEVERATION',y=NULL) +
  geom_point(aes(sequence, meanLOSE, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot4 = ggplot(subset(sim_prshift_lose, behaviour %in% 'PROBABILITYMATCHING')) +
  geom_line(aes(sequence, meanLOSE, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanLOSE, ymin=meanLOSE-sdLOSE, ymax=meanLOSE+sdLOSE,color=type), width=.1) +
  ylim(0,1) +
  labs(x='PROB. MATCHING',y=NULL) +
  geom_point(aes(sequence, meanLOSE, color=type)) +
  theme(legend.title = element_blank(),legend.position = c(0.5, 0.1))  +
  gghighlight(use_direct_label = FALSE)

plot5 = ggplot(subset(sim_prshift_lose, behaviour %in% 'IDEALPREDICTOR')) +
  geom_line(aes(sequence, meanLOSE, group=type,colour = type)) +
  geom_errorbar(aes(sequence, meanLOSE, ymin=meanLOSE-sdLOSE, ymax=meanLOSE+sdLOSE,color=type), width=.1) +
  ylim(0,1) +
  labs(x='IDEAL PREDICTOR',y=NULL) +
  geom_point(aes(sequence, meanLOSE, color=type)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot3 + plot4 + plot5 + plot_layout(ncol = 5)

# scaterplot shift M0
ggplot(subset(sim_scatterM0), aes(x=pr_shift_win, y=pr_shift_lose, group=behaviour, colour = behaviour)) + 
  xlim(0,1) + ylim(0,1) +
  labs(x='Pr(shift|win)',y='Pr(shift|lose)') +
  geom_vline(xintercept=0.5,linetype='dashed') +
  geom_hline(yintercept=0.5,linetype='dashed') + 
  annotate('text', x=0.15, y=0.6, label= 'Win-stay/lose-shift') +
  annotate('text', x=0.85, y=0.6, label= 'Win-shift/lose-shift') +
  annotate('text', x=0.15, y=0.4, label= 'Win-stay/lose-stay') +
  annotate('text', x=0.85, y=0.4, label= 'Win-shift/lose-stay') +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  geom_point()

# scaterplot shift M2
ggplot(subset(sim_scatterM2), aes(x=pr_shift_win, y=pr_shift_lose, group=behaviour, colour = behaviour)) + 
  xlim(0,1) + ylim(0,1) +
  labs(x='Pr(shift|win)',y='Pr(shift|lose)') +
  geom_vline(xintercept=0.5,linetype='dashed') +
  geom_hline(yintercept=0.5,linetype='dashed') + 
  annotate('text', x=0.15, y=0.6, label= 'Win-stay/lose-shift') +
  annotate('text', x=0.85, y=0.6, label= 'Win-shift/lose-shift') +
  annotate('text', x=0.15, y=0.4, label= 'Win-stay/lose-stay') +
  annotate('text', x=0.85, y=0.4, label= 'Win-shift/lose-stay') +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  geom_point()

# Plot markov reconstruction M2
plot1 = ggplot(subset(sim_rec)) +
  geom_line(aes(element, meanREC, group=behaviour,colour = behaviour)) +
  geom_errorbar(aes(element, meanREC, ymin=meanREC-sdREC, ymax=meanREC+sdREC,color=behaviour), width=.1) +
  ylim(-0.02,1) +
  labs(x='',y='') +
  geom_point(aes(element, meanREC, color=behaviour)) +
  theme(legend.title = element_blank(),legend.position = c(0.15, 0.2)) +
  gghighlight(use_direct_label = FALSE)
plot1

# --------------------------------------------------------
# EXPERIMENTAL DATA IMPORT
# --------------------------------------------------------

data_output1 <- read_delim("2_3_data_output1.csv",
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(mean_response = col_number(), 
                                           accuracy = col_number(), pr_shift = col_number(), 
                                           pr_shift_win = col_number(), pr_shift_lose = col_number(), 
                                           lag = col_number(), auto_correlation = col_number(), 
                                           cross_correlation = col_number()), trim_ws = TRUE)

data_output2 <- read_delim("2_3_data_output2.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(cross = col_number()), 
                          trim_ws = TRUE)

data_output3 <- read_delim("2_3_data_output3.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(cross = col_number()), 
                          trim_ws = TRUE)

# group and summarize collected data
data_mean_acc <- data_output1 %>%
  filter(sequence == "OUTPUT") %>%
  pivot_longer(
    cols = c(mean_response, accuracy),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(measure,
                     "mean_response" = "MEAN RESPONSE",
                     "accuracy" = "ACCURACY")
  ) %>%
  group_by(local, group, measure) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    ic95=sd/sqrt(count)*1.96,
    .groups = "drop"
  )

data_auto <- data_output1 %>%
  filter(sequence!="INPUT") %>%
  group_by(local,sequence,group,lag) %>% 
  summarise(
    count = n(),
    meanAUTO=mean(auto_correlation,na.rm=TRUE),
    sdAUTO=sd(auto_correlation,na.rm=TRUE),
    ic95AUTO=sdAUTO/sqrt(count)*1.96
  )

data_cross <- data_output1 %>%
  filter(sequence!="INPUT") %>%
  group_by(local,sequence,group,lag) %>% 
  summarise(
    count = n(),
    meanCROSS=mean(cross_correlation,na.rm=TRUE),
    sdCROSS=sd(cross_correlation,na.rm=TRUE),
    ic95CROSS=sdCROSS/sqrt(count)*1.96
  )

data_shift <- data_output1 %>%
  filter(sequence == "OUTPUT") %>%
  pivot_longer(
    cols = c(pr_shift, pr_shift_win,pr_shift_lose),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(measure,
                     "pr_shift" = "Pr(shift)",
                     "pr_shift_win" = "Pr(shift|win)",
                     "pr_shift_lose" = "Pr(shift|lose)")
  ) %>%
  group_by(local, group, measure) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    ic95=sd/sqrt(count)*1.96,
    .groups = "drop"
  )

sim_scatter_exp1 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="Oxford")

sim_scatter_exp2 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="USP")

data_rec  <- data_output3 %>%
  filter(local=="USP") %>%
  unite("group_type", group, sequence, sep = "-") %>%
  select(local, group_type, element, rec) %>%
  group_by(group_type,element) %>% 
  summarise(
    count = n(),
    meanREC=mean(rec,na.rm=TRUE),
    sdREC=sd(rec,na.rm=TRUE),
    ic95REC=sdREC/sqrt(count)*1.96
  )

# --------------------------------------------------------
# EXPERIMENTAL DATA GRAPHS
# --------------------------------------------------------

# mean response and accuracy for exp I and II
plot1 = ggplot(subset(data_mean_acc, local %in% 'Oxford')) +
  geom_line(aes(group, mean, group=measure,colour = measure)) +
  geom_errorbar(aes(group, mean, ymin=mean-ic95, ymax=mean+ic95,color=measure), width=.1) +
  ylim(0,1) +
  labs(x='EXPERIMENT I',y=NULL) +
  geom_point(aes(group, mean, color=measure)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_mean_acc, local %in% 'USP')) +
  geom_line(aes(group, mean, group=measure,colour = measure)) +
  geom_errorbar(aes(group, mean, ymin=mean-ic95, ymax=mean+ic95,color=measure), width=.1) +
  ylim(0,1) +
  labs(x='EXPERIMENT II',y=NULL) +
  geom_point(aes(group, mean, color=measure)) +
  theme(legend.title = element_blank(),legend.position = c(0.5, 0.1))  +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot_layout(ncol = 2)

# autocorrelation exp I
plot1 = ggplot(subset(data_auto, local %in% 'Oxford' & group %in% 'WIN')) +
  geom_line(aes(lag, meanAUTO, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTO, ymin=meanAUTO-ic95AUTO, ymax=meanAUTO+ic95AUTO,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='WIN',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTO, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_auto, local %in% 'Oxford' & group %in% 'LOSE')) +
  geom_line(aes(lag, meanAUTO, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTO, ymin=meanAUTO-ic95AUTO, ymax=meanAUTO+ic95AUTO,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='LOSE',y=NULL) +
  geom_point(aes(lag, meanAUTO, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'EXPERIMENT I',theme=theme(plot.title=element_text(hjust=0.5)))

# autocorrelation exp II
plot1 = ggplot(subset(data_auto, local %in% 'USP' & group %in% 'WIN')) +
  geom_line(aes(lag, meanAUTO, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTO, ymin=meanAUTO-ic95AUTO, ymax=meanAUTO+ic95AUTO,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='WIN',y='AUTOCORRELATION') +
  geom_point(aes(lag, meanAUTO, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_auto, local %in% 'USP' & group %in% 'LOSE')) +
  geom_line(aes(lag, meanAUTO, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanAUTO, ymin=meanAUTO-ic95AUTO, ymax=meanAUTO+ic95AUTO,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='LOSE',y=NULL) +
  geom_point(aes(lag, meanAUTO, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'EXPERIMENT II',theme=theme(plot.title=element_text(hjust=0.5)))


# cross-correlation exp I
plot1 = ggplot(subset(data_cross, local %in% 'Oxford' & group %in% 'WIN')) +
  geom_line(aes(lag, meanCROSS, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSS, ymin=meanCROSS-ic95CROSS, ymax=meanCROSS+ic95CROSS,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='WIN',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSS, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_cross, local %in% 'Oxford' & group %in% 'LOSE')) +
  geom_line(aes(lag, meanCROSS, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSS, ymin=meanCROSS-ic95CROSS, ymax=meanCROSS+ic95CROSS,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='LOSE',y=NULL) +
  geom_point(aes(lag, meanCROSS, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'EXPERIMENT I',theme=theme(plot.title=element_text(hjust=0.5)))

# cross-correlation exp II
plot1 = ggplot(subset(data_cross, local %in% 'USP' & group %in% 'WIN')) +
  geom_line(aes(lag, meanCROSS, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSS, ymin=meanCROSS-ic95CROSS, ymax=meanCROSS+ic95CROSS,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='WIN',y='CROSS-CORRELATION') +
  geom_point(aes(lag, meanCROSS, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_cross, local %in% 'USP' & group %in% 'LOSE')) +
  geom_line(aes(lag, meanCROSS, group=type ,colour = type)) +
  geom_errorbar(aes(lag, meanCROSS, ymin=meanCROSS-ic95CROSS, ymax=meanCROSS+ic95CROSS,color=type), width=.1) +
  ylim(-1,1) +
  labs(x='LOSE',y=NULL) +
  geom_point(aes(lag, meanCROSS, color=type)) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2+ plot_annotation(title = 'EXPERIMENT II',theme=theme(plot.title=element_text(hjust=0.5)))

# pr(shift), pr(shift|win) and pr(shift|lose) for exp I and II
plot1 = ggplot(subset(data_shift, local %in% 'Oxford')) +
  geom_line(aes(group, mean, group=measure,colour = measure)) +
  geom_errorbar(aes(group, mean, ymin=mean-ic95, ymax=mean+ic95,color=measure), width=.1) +
  ylim(0,1) +
  labs(x='EXPERIMENT I',y=NULL) +
  geom_point(aes(group, mean, color=measure)) +
  theme(legend.position='none') +
  gghighlight(use_direct_label = FALSE)

plot2 = ggplot(subset(data_shift, local %in% 'USP')) +
  geom_line(aes(group, mean, group=measure,colour = measure)) +
  geom_errorbar(aes(group, mean, ymin=mean-ic95, ymax=mean+ic95,color=measure), width=.1) +
  ylim(0,1) +
  labs(x='EXPERIMENT II',y=NULL) +
  geom_point(aes(group, mean, color=measure)) +
  theme(legend.title = element_blank(),legend.position = c(0.5, 0.1))  +
  gghighlight(use_direct_label = FALSE)

plot1 + plot2 + plot_layout(ncol = 2)

# scaterplot shift Exp I
ggplot(subset(sim_scatter_exp1), aes(x=pr_shift_win, y=pr_shift_lose, group=group, colour = group)) + 
  xlim(0,1) + ylim(0,1) +
  labs(x='Pr(shift|win)',y='Pr(shift|lose)') +
  geom_vline(xintercept=0.5,linetype='dashed') +
  geom_hline(yintercept=0.5,linetype='dashed') + 
  annotate('text', x=0.15, y=0.6, label= 'Win-stay/lose-shift') +
  annotate('text', x=0.85, y=0.6, label= 'Win-shift/lose-shift') +
  annotate('text', x=0.15, y=0.4, label= 'Win-stay/lose-stay') +
  annotate('text', x=0.85, y=0.4, label= 'Win-shift/lose-stay') +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  geom_point()

# scaterplot shift Exp II
ggplot(subset(sim_scatter_exp2), aes(x=pr_shift_win, y=pr_shift_lose, group=group, colour = group)) + 
  xlim(0,1) + ylim(0,1) +
  labs(x='Pr(shift|win)',y='Pr(shift|lose)') +
  geom_vline(xintercept=0.5,linetype='dashed') +
  geom_hline(yintercept=0.5,linetype='dashed') + 
  annotate('text', x=0.15, y=0.6, label= 'Win-stay/lose-shift') +
  annotate('text', x=0.85, y=0.6, label= 'Win-shift/lose-shift') +
  annotate('text', x=0.15, y=0.4, label= 'Win-stay/lose-stay') +
  annotate('text', x=0.85, y=0.4, label= 'Win-shift/lose-stay') +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.2)) +
  geom_point()

# Plot markov reconstruction M2 Exp II
plot1 = ggplot(subset(data_rec)) +
  geom_line(aes(element, meanREC, group=group_type,colour = group_type)) +
  geom_errorbar(aes(element, meanREC, ymin=meanREC-ic95REC, ymax=meanREC+ic95REC,color=group_type), width=.1) +
  ylim(-0.02,1) +
  labs(x='',y='') +
  geom_point(aes(element, meanREC, color=group_type)) +
  theme(legend.title = element_blank(),legend.position = c(0.15, 0.2)) +
  gghighlight(use_direct_label = FALSE)
plot1