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
library(effectsize)
library(afex)

# --------------------------------------------------------
# EXPERIMENTAL DATA IMPORT
# --------------------------------------------------------

# Set working directory (adjust to your system)
setwd("/home/bernard-costa/Documents/0_github")

data_output1 <- read_delim("2_5_data_output1.csv",
                           delim = "\t", escape_double = FALSE, 
                           col_types = cols(mean_response = col_number(), 
                                            accuracy = col_number(), pr_shift = col_number(), 
                                            pr_shift_win = col_number(), pr_shift_lose = col_number(), 
                                            lag = col_number(), auto_correlation = col_number(), 
                                            cross_correlation = col_number()), trim_ws = TRUE)

data_output2 <- read_delim("2_5_data_output2.csv", 
                           delim = "\t", escape_double = FALSE, 
                           col_types = cols(cross = col_number()), 
                           trim_ws = TRUE)

data_output3 <- read_delim("2_5_data_output3.csv", 
                           delim = "\t", escape_double = FALSE, 
                           col_types = cols(cross = col_number()), 
                           trim_ws = TRUE)

# group and summarize collected data
# converting to factors
data_output1$name <- factor(data_output1$name)
data_output1$local <- factor(data_output1$local)
data_output1$group <- factor(data_output1$group)
data_output1$sequence  <- factor(data_output1$sequence)
data_output1$lag   <- factor(data_output1$lag)

#mean response & accuracy manova exp I and II
data_manova_exp1 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="Oxford")
manova_exp1 <- manova(cbind(mean_response, accuracy) ~ group, data = data_manova_exp1)
summary(manova_exp1, test = "Wilks")
eta_squared(manova_exp1, partial = TRUE)

data_manova_exp2 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="USP")
manova_exp2 <- manova(cbind(mean_response, accuracy) ~ group, data = data_manova_exp2)
summary(manova_exp2, test = "Wilks")
eta_squared(manova_exp2, partial = TRUE)

# anova autocorrelation exp I and II
data_anova_exp1 <- data_output1 %>%
  filter(lag!="-5" & lag!="-4" & lag!="-3" & lag!="-2" & lag!="-1" & lag!="0") %>%
  filter(sequence!="INPUT") %>%
  filter(local=="Oxford") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanAUTO=mean(auto_correlation,na.rm=TRUE)
  )
anova_exp1 <- aov_ez(id = "name", dv = "meanAUTO", data = data_anova_exp1,
  within = c("sequence", "lag"),
  between = "group")
anova(anova_exp1, correction = "none")
eta_squared(anova_exp1, partial = TRUE)

data_anova_exp2 <- data_output1%>%
  filter(lag!="-5" & lag!="-4" & lag!="-3" & lag!="-2" & lag!="-1" & lag!="0") %>%
  filter(sequence!="INPUT") %>%
  filter(local=="USP") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanAUTO=mean(auto_correlation,na.rm=TRUE)
  )
anova_exp2 <- aov_ez(id = "name", dv = "meanAUTO", data = data_anova_exp2,
                within = c("sequence", "lag"),
                between = "group")
anova(anova_exp2, correction = "none")
eta_squared(anova_exp2, partial = TRUE)

# anova cross-correlation exp I and II - all lags
data_anova_exp1 <- data_output1 %>%
  filter(sequence!="INPUT") %>%
  filter(local=="Oxford") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanCROSS=mean(cross_correlation,na.rm=TRUE)
  )
anova_exp1 <- aov_ez(id = "name", dv = "meanCROSS", data = data_anova_exp1,
                within = c("sequence", "lag"),
                between = "group")
anova(anova_exp1, correction = "none")
eta_squared(anova_exp1, partial = TRUE)

data_anova_exp2 <- data_output1 %>%
  filter(sequence!="INPUT") %>%
  filter(local=="USP") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanCROSS=mean(cross_correlation,na.rm=TRUE)
  )
anova_exp2 <- aov_ez(id = "name", dv = "meanCROSS", data = data_anova_exp2,
                within = c("sequence", "lag"),
                between = "group")
anova(anova_exp2, correction = "none")
eta_squared(anova_exp2, partial = TRUE)

# anova cross-correlation exp I and II - positive lags
data_anova_exp1 <- data_output1 %>%
  filter(lag!="-5" & lag!="-4" & lag!="-3" & lag!="-2" & lag!="-1") %>%
  filter(sequence!="INPUT") %>%
  filter(local=="Oxford") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanCROSS=mean(cross_correlation,na.rm=TRUE)
  )
anova_exp1 <- aov_ez(id = "name", dv = "meanCROSS", data = data_anova_exp1,
                within = c("sequence", "lag"),
                between = "group")
anova(anova_exp1, correction = "none")
eta_squared(anova_exp1, partial = TRUE)

data_anova_exp2 <- data_output1[c(1:4,10:12)] %>%
  filter(lag!="-5" & lag!="-4" & lag!="-3" & lag!="-2" & lag!="-1") %>%
  filter(sequence!="INPUT") %>%
  filter(local=="USP") %>%
  group_by(name,group,sequence,lag) %>%
  summarise(
    count = n(),
    meanCROSS=mean(cross_correlation,na.rm=TRUE)
  )
anova_exp2 <- aov_ez(id = "name", dv = "meanCROSS", data = data_anova_exp2,
                within = c("sequence", "lag"),
                between = "group")
anova(anova_exp2, correction = "none")
eta_squared(anova_exp2, partial = TRUE)

#shift manova exp I and II
data_manova_exp1 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="Oxford")
manova_exp1 <- manova(cbind(pr_shift,pr_shift_win,pr_shift_lose) ~ group, data = data_manova_exp1)
summary(manova_exp1, test = "Wilks")
eta_squared(manova_exp1, partial = TRUE)

data_manova_exp2 <- data_output1 %>%
  filter(lag=="0") %>%
  filter(sequence=="OUTPUT") %>%
  filter(local=="USP")
manova_exp2 <- manova(cbind(pr_shift,pr_shift_win,pr_shift_lose) ~ group, data = data_manova_exp2)
summary(manova_exp2, test = "Wilks")
eta_squared(manova_exp2, partial = TRUE)

#markov reconstruction exp I and II
markov_0 <- data_output2 %>%
  filter(sequence!="OUTPUT") %>%
  filter(local=="Oxford") %>%
  group_by(group,sequence,element) %>%
  summarise(
    count = n(),
    meanREC=mean(rec,na.rm=TRUE),
    sdREC=sd(rec,na.rm=TRUE),
    ic95REC=sdREC/sqrt(count)*1.96,
    int_neg_1=meanREC-ic95REC,
    int_pos_1=meanREC+ic95REC,
    int_pos_11=1-int_pos_1,
    int_neg_11=1-int_neg_1
  )

markov_2 <- data_output3 %>%
  filter(sequence!="OUTPUT") %>%
  filter(local=="USP") %>%
  group_by(group,sequence,element) %>%
  summarise(
    count = n(),
    meanREC=mean(rec,na.rm=TRUE),
    sdREC=sd(rec,na.rm=TRUE),
    ic95REC=sdREC/sqrt(count)*1.96,
    int_neg_1=meanREC-ic95REC,
    int_pos_1=meanREC+ic95REC,
    int_pos_11=1-int_pos_1,
    int_neg_11=1-int_neg_1
  )
