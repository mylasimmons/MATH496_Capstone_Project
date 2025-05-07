library(tidyverse)
library(forcats)
source("C:/Users/mysty/OneDrive/MylaSimmons/MylaSimmons/capstone-project/code/.Rproj.user/recode.fun.R")

dq_df <- read_csv("C:/Users/mysty/OneDrive/MylaSimmons/MylaSimmons/capstone-project/code/Misuse of AI-RR-Student-Survey (Direct Questioning)_last.csv")


# Cleaning/Wrangling
dq_df <- dq_df[-(which(dq_df$DistributionChannel == "preview")),]
dq_df <- dq_df[-(which(dq_df$Q2 == "I don't consent to participate in this study")),]
dq_df <- dq_df[-c(1:2),]
dq_df <- dq_df %>% mutate(Q3 = ifelse(Q3 == "Prefer not to say",NA,Q3))
dq_df <- dq_df %>% mutate(Q5 = ifelse(Q5 == "Don’t know" | Q5 == "Prefer not to say",NA,Q5))
dq_df <- dq_df %>% rename(gender = Q3, HS_GPA = Q5, classification = Q6, social_class = Q8)
dem_cols <- c("gender","HS_GPA","classification","social_class")
dq_df[,dem_cols] <- lapply(dq_df[,dem_cols], factor)
dq_df$HS_GPA <- fct_collapse(dq_df$HS_GPA, "<3.00" = c("Below 1.00","1.00 – 1.99","2.00 – 2.99"))
dq_df$social_class <- fct_collapse(dq_df$social_class, "Above Middle Class" = c("Upper Middle Class","Wealthy Class"))


dq_df <- dq_df %>% mutate(first_gen = recode.fun(Q4, scale = 15),
                          STEM = recode.fun(Q7, scale = 16))
dq_df <- dq_df %>% mutate(study_hrs = recode.fun(Q9, scale = 1),
                          sleep_hrs = recode.fun(Q10, scale = 2),
                          work_hrs = recode.fun(Q11, scale = 3),
                          extracurr_hrs = recode.fun(Q12, scale = 4),
                          time_management = recode.fun(Q13, scale = 5),
                          resources = recode.fun(Q14, scale = 6),
                          compl_time = recode.fun(Q15, scale = 6))
dq_df <- dq_df %>% mutate(ai = recode.fun(Q16, scale = 6),
                          paid = recode.fun(Q17, scale = 6),
                          online = recode.fun(Q18, scale = 6),
                          gc = recode.fun(Q19, scale = 6))
dq_df <- dq_df %>% mutate(sn_ai = recode.fun(Q20, scale = 8),
                          sn_paid = recode.fun(Q21, scale = 8),
                          sn_online = recode.fun(Q22, scale = 8),
                          sn_gc = recode.fun(Q23, scale = 8))
dem_cols2 <- c("first_gen","STEM")
dq_df[,dem_cols2] <- lapply(dq_df[,dem_cols2], factor)


# Visualizations
dq_df1 <- na.omit(dq_df)

ggplot(dq_df1, aes(gender, fill = gender)) +
  geom_bar(show.legend = F) +
  xlab("Gender") +
  ylab("Frequency") +
  ggtitle("Gender Distribution of Survey Participants (DQ)")
ggsave("DQ_Gender.png", width = 5, height = 5, unit = "in")

ggplot(dq_df1, aes(first_gen, fill = first_gen)) +
  geom_bar(show.legend = F) +
  xlab("First Generation") +
  ylab("Frequency") +
  ggtitle("First Generation College Student Distribution of Survey participants (DQ)")
ggsave("DQ_FirstGen.png", width = 6.5, height = 6.5, unit = "in")

ggplot(dq_df1, aes(HS_GPA, fill = HS_GPA)) +
  geom_bar(show.legend = F) +
  xlab("High School GPA") +
  ylab("Frequency") +
  ggtitle("High School GPA Distribution of Survey Participants (DQ)")
ggsave("DQ_HSGPA.png", width = 5.25, height = 5, unit = "in")

ordered_class <- c("Freshman","Sophomore","Junior","Senior")
ggplot(dq_df1, aes(x = factor(classification,ordered_class), fill = classification)) +
  geom_bar(show.legend = F) +
  xlab("Classification") +
  ylab("Frequency") +
  ggtitle("Classification Distribution of Survey Participants (DQ)")
ggsave("DQ_Class.png", width = 5, height = 5, unit = "in")

ggplot(dq_df1, aes(STEM, fill = STEM)) +
  geom_bar(show.legend = F) +
  xlab("STEM") +
  ylab("Frequency") +
  ggtitle("STEM Distribution of Survey Partcipants (DQ)")
ggsave("DQ_STEM.png", width = 5, height = 5, unit = "in")

ordered_soc.class <- c("Working Class","Lower Middle Class","Middle Class","Above Middle Class")
ggplot(dq_df1, aes(x = factor(social_class,ordered_soc.class), fill = social_class)) +
  geom_bar(show.legend = F) +
  xlab("Social Class") +
  ylab("Frequency") +
  ggtitle("Social Class Distribution of Survey Participants (DQ)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75))
ggsave("DQ_SocialClass.png", width = 5, height = 5, unit = "in")

## Distribution statistics

gender_stats <- dq_df %>% group_by(gender) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
gender_stats

first_gen_stats <- dq_df %>% group_by(first_gen) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
first_gen_stats

hs_gpa_stats <- dq_df %>% group_by(HS_GPA) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
hs_gpa_stats

class_stats <- dq_df %>% group_by(classification) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
class_stats

stem_stats <- dq_df %>% group_by(STEM) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
stem_stats

social_class_stats <- dq_df %>% group_by(social_class) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
social_class_stats

study_hrs_stats <- dq_df %>% summarise(mean = mean(study_hrs, na.rm = TRUE), sd = sd(study_hrs, na.rm = TRUE))
study_hrs_stats

sleep_hrs_stats <- dq_df %>% summarise(mean = mean(sleep_hrs, na.rm = TRUE), sd = sd(sleep_hrs, na.rm = TRUE))
sleep_hrs_stats

work_hrs_stats <- dq_df %>% summarise(mean = mean(work_hrs, na.rm = TRUE), sd = sd(work_hrs, na.rm = TRUE))
work_hrs_stats

extracurr_hrs_stats <- dq_df %>% summarise(mean = mean(extracurr_hrs, na.rm = TRUE), sd = sd(extracurr_hrs, na.rm = TRUE))
extracurr_hrs_stats

tm_stats <- dq_df %>% summarise(mean = mean(time_management, na.rm = TRUE), sd = sd(time_management, na.rm = TRUE))
tm_stats

resources_stats <- dq_df %>% group_by(resources) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
resources_stats

compl_time_stats <- dq_df %>% group_by(compl_time) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(dq_df)*100)
compl_time_stats

sn_ai_stats <- dq_df %>% summarise(mean = mean(sn_ai, na.rm = TRUE), sd = sd(sn_ai, na.rm = TRUE))
sn_ai_stats

sn_paid_stats <- dq_df %>% summarise(mean = mean(sn_paid, na.rm = TRUE), sd = sd(sn_paid, na.rm = TRUE))
sn_paid_stats

sn_online_stats <- dq_df %>% summarise(mean = mean(sn_online, na.rm = TRUE), sd = sd(sn_online, na.rm = TRUE))
sn_online_stats

sn_gc_stats <- dq_df %>% summarise(mean = mean(sn_gc, na.rm = TRUE), sd = sd(sn_gc, na.rm = TRUE))
sn_gc_stats

# Analysis

## Estimates

dq_df_ai <- dq_df$ai[!is.na(dq_df$ai)]
ai_prev <- sum(dq_df_ai == 1)/length(dq_df_ai)
ai_se <- sqrt((ai_prev*(1-ai_prev))/length(dq_df_ai))
ai.ci.low <- ai_prev - qnorm(0.975)*ai_se
ai.ci.high <- ai_prev + qnorm(0.975)*ai_se
ai.dq.results <- list()
ai.dq.results[[1]] <- rbind(cbind(ai_prev,ai_se,ai.ci.low,ai.ci.high))
ai.dq.results[[2]] <- cbind(length(dq_df_ai))
colnames(ai.dq.results[[1]]) <- c("Estimate", "Std. Error", "95%CI(Low)", "95%CI(High)")
colnames(ai.dq.results[[2]]) <- "Sample Size"
ai.dq.results


dq_df_paid <- dq_df$paid[!is.na(dq_df$paid)]
paid_prev <- sum(dq_df_paid == 1)/length(dq_df_paid)
paid_se <- sqrt((paid_prev*(1-paid_prev))/length(dq_df_paid))
paid.ci.low <- paid_prev - qnorm(0.975)*paid_se
paid.ci.high <- paid_prev + qnorm(0.975)*paid_se
paid.dq.results <- list()
paid.dq.results[[1]] <- rbind(cbind(paid_prev,paid_se,paid.ci.low,paid.ci.high))
paid.dq.results[[2]] <- cbind(length(dq_df_paid))
colnames(paid.dq.results[[1]]) <- c("Estimate", "Std. Error", "95%CI(Low)", "95%CI(High)")
colnames(paid.dq.results[[2]]) <- "Sample Size"
paid.dq.results


dq_df_online <- dq_df$online[!is.na(dq_df$online)]
online_prev <- sum(dq_df_online == 1)/length(dq_df_online)
online_se <- sqrt((online_prev*(1-online_prev))/length(dq_df_online))
online.ci.low <- online_prev - qnorm(0.975)*online_se
online.ci.high <- online_prev + qnorm(0.975)*online_se
online.dq.results <- list()
online.dq.results[[1]] <- rbind(cbind(online_prev,online_se,online.ci.low,online.ci.high))
online.dq.results[[2]] <- cbind(length(dq_df_online))
colnames(online.dq.results[[1]]) <- c("Estimate", "Std. Error", "95%CI(Low)", "95%CI(High)")
colnames(online.dq.results[[2]]) <- "Sample Size"
online.dq.results

dq_df_gc <- dq_df$gc[!is.na(dq_df$gc)]
gc_prev <- sum(dq_df_gc == 1)/length(dq_df_gc)
gc_se <- sqrt((gc_prev*(1-gc_prev))/length(dq_df_gc))
gc.ci.low <- gc_prev - qnorm(0.975)*gc_se
gc.ci.high <- gc_prev + qnorm(0.975)*gc_se
gc.dq.results <- list()
gc.dq.results[[1]] <- rbind(cbind(gc_prev,gc_se,gc.ci.low,gc.ci.high))
gc.dq.results[[2]] <- cbind(length(dq_df_gc))
colnames(gc.dq.results[[1]]) <- c("Estimate", "Std. Error", "95%CI(Low)", "95%CI(High)")
colnames(gc.dq.results[[2]]) <- "Sample Size"
gc.dq.results

## Logistic Regression
dq_df <- dq_df %>% na.omit(c("study_hrs","sleep_hrs","work_hrs","extracurr_hrs","time_management","resources","compl_time")) %>% mutate(tmr_score = (study_hrs+sleep_hrs+work_hrs+extracurr_hrs+time_management+resources+compl_time)/7)

dq_df <- dq_df %>% na.omit(c("sn_ai","sn_paid","sn_online","sn_gc")) %>% mutate(sn_score = (sn_ai+sn_paid+sn_online+sn_gc)/4)

ai_mod <- glm(ai~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = dq_df, family = "binomial")
summary(ai_mod)

paid_mod <- glm(paid~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = dq_df, family = "binomial")
summary(paid_mod)

online_mod <- glm(online~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = dq_df, family = "binomial")
summary(online_mod)

gc_mod <- glm(gc~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = dq_df, family = "binomial")
summary(gc_mod)
