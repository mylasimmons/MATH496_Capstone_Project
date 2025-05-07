library(tidyverse)
library(forcats)
library(cWise)
library(RRreg)
source("C:/Users/mysty/OneDrive/MylaSimmons/MylaSimmons/capstone-project/code/.Rproj.user/recode.fun.R")

cm_df <- read_csv("C:/Users/mysty/OneDrive/MylaSimmons/MylaSimmons/capstone-project/code/Misuse of AI-RR-Student-Survey (Crosswise Model)_last.csv")

# Cleaning/Wrangling
cm_df <- cm_df[-(which(cm_df$DistributionChannel == "preview")),]
cm_df <- cm_df[-(which(cm_df$Q2 == "I don't consent to participate in this study")),]
cm_df <- cm_df[-c(1:2),] 
cm_df <- cm_df %>% mutate(Q3 = ifelse(Q3 == "Prefer not to say",NA,Q3))
cm_df <- cm_df %>% mutate(Q5 = ifelse(Q5 == "Don’t know",NA,Q5))
cm_df <- cm_df %>% mutate(Q5 = ifelse(Q5 == "Prefer not to say",NA,Q5))
cm_df <- cm_df %>% mutate(Q8 = ifelse(Q8 == "Wealthy Class",NA,Q8))
cm_df <- cm_df %>% rename(gender = Q3, HS_GPA = Q5, classification = Q6, social_class = Q8)
dem_cols <- c("gender","HS_GPA","classification","social_class")
cm_df[,dem_cols] <- lapply(cm_df[,dem_cols], factor)
cm_df$HS_GPA <- fct_collapse(cm_df$HS_GPA, "<3.00" = c("1.00 – 1.99","2.00 – 2.99"))
cm_df$social_class <- fct_collapse(cm_df$social_class, "Above Middle Class" = c("Upper Middle Class","Wealthy Class"))

cm_df <- cm_df %>% mutate(first_gen = recode.fun(Q4, scale = 15),
                          STEM = recode.fun(Q7, scale = 16))
cm_df <- cm_df %>% mutate(study_hrs = recode.fun(Q9, scale = 1),
                          sleep_hrs = recode.fun(Q10, scale = 2),
                          work_hrs = recode.fun(Q11, scale = 3),
                          extracurr_hrs = recode.fun(Q12, scale = 4),
                          time_management = recode.fun(Q13, scale = 5),
                          resources = recode.fun(Q14, scale = 6),
                          compl_time = recode.fun(Q15, scale = 6))
cm_df <- cm_df %>% mutate(ai = recode.fun(Q18, scale = 7),
                          paid = recode.fun(Q20, scale = 7),
                          online = recode.fun(Q22, scale = 7),
                          gc = recode.fun(Q24, scale = 7),
                          anchor = recode.fun(Q26, scale = 7))
cm_df <- cm_df %>% mutate(sn_ai = recode.fun(Q27, scale = 8),
                          sn_paid = recode.fun(Q28, scale = 8),
                          sn_online = recode.fun(Q29, scale = 8),
                          sn_gc = recode.fun(Q30, scale = 8))


dem_cols2 <- c("first_gen","STEM")
cm_df[,dem_cols2] <- lapply(cm_df[,dem_cols2], factor)

# Visualizations
cm_df1 <- na.omit(cm_df)

ggplot(cm_df1, aes(gender, fill = gender)) +
  geom_bar(show.legend = F) +
  xlab("Gender") +
  ylab("Frequency") +
  ggtitle("Gender Distribution of Survey Participants (CM)")
ggsave("CM_Gender.png", width = 1, height = 1, unit = "in")

ggplot(cm_df1, aes(first_gen, fill = first_gen)) +
  geom_bar(show.legend = F) +
  xlab("First Genderation") +
  ylab("Frequency") +
  ggtitle("First Generation College Student Distribution of Survey Partcipants (CM)")
ggsave("CM_FirstGen.png", width = 6.5, height = 6.5, unit = "in")

ggplot(cm_df1, aes(HS_GPA, fill = HS_GPA)) +
  geom_bar(show.legend = F) +
  xlab("High School GPA") +
  ylab("Frequency") +
  ggtitle("High School GPA Distribution of Survey Participants (CM)")
ggsave("CM_HSGPA.png", width = 5.5, height = 5, unit = "in")

ordered_class <- c("Freshman","Sophomore","Junior","Senior")
ggplot(cm_df1, aes(x = factor(classification,ordered_class), fill = classification)) +
  geom_bar(show.legend = F) +
  xlab("Classification") +
  ylab("Frequency") +
  ggtitle("Classification Distribution of Survey Participants (CM)")
ggsave("CM_Class.png", width = 5, height = 5, unit = "in")

ggplot(cm_df1, aes(STEM, fill = STEM)) +
  geom_bar(show.legend = F) +
  xlab("STEM") +
  ylab("Frequency") +
  ggtitle("STEM Dsitribution of Survey Participants (CM)")
ggsave("CM_STEM.png", width = 5, height = 5, unit = "in")

ordered_soc.class <- c("Working Class","Lower Middle Class","Middle Class","Above Middle Class")
ggplot(cm_df1, aes(x = factor(social_class,ordered_soc.class), fill = social_class)) +
  geom_bar(show.legend = F) +
  xlab("Social Class") +
  ylab("Frequency") +
  ggtitle("Social Class Distribution of Survey participants (CM)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75))
ggsave("CM_SocialClass.png", width = 5, height = 5, unit = "in")

## Distribution Statistics
gender_stats <- cm_df %>% group_by(gender) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
gender_stats

first_gen_stats <- cm_df %>% group_by(first_gen) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
first_gen_stats

hs_gpa_stats <- cm_df %>% group_by(HS_GPA) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
hs_gpa_stats

class_stats <- cm_df %>% group_by(classification) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
class_stats

stem_stats <- cm_df %>% group_by(STEM) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
stem_stats

social_class_stats <- cm_df %>% group_by(social_class) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
social_class_stats

study_hrs_stats <- cm_df %>% summarise(mean = mean(study_hrs, na.rm = TRUE), sd = sd(study_hrs, na.rm = TRUE))
study_hrs_stats

sleep_hrs_stats <- cm_df %>% summarise(mean = mean(sleep_hrs, na.rm = TRUE), sd = sd(sleep_hrs, na.rm = TRUE))
sleep_hrs_stats

work_hrs_stats <- cm_df %>% summarise(mean = mean(work_hrs, na.rm = TRUE), sd = sd(work_hrs, na.rm = TRUE))
work_hrs_stats

extracurr_hrs_stats <- cm_df %>% summarise(mean = mean(extracurr_hrs, na.rm = TRUE), sd = sd(extracurr_hrs, na.rm = TRUE))
extracurr_hrs_stats

tm_stats <- cm_df %>% summarise(mean = mean(time_management, na.rm = TRUE), sd = sd(time_management, na.rm = TRUE))
tm_stats

resources_stats <- cm_df %>% group_by(resources) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
resources_stats

compl_time_stats <- cm_df %>% group_by(compl_time) %>% na.omit() %>% summarise(count = n(), percentage = n()/nrow(cm_df)*100)
compl_time_stats

sn_ai_stats <- cm_df %>% summarise(mean = mean(sn_ai, na.rm = TRUE), sd = sd(sn_ai, na.rm = TRUE))
sn_ai_stats

sn_paid_stats <- cm_df %>% summarise(mean = mean(sn_paid, na.rm = TRUE), sd = sd(sn_paid, na.rm = TRUE))
sn_paid_stats

sn_online_stats <- cm_df %>% summarise(mean = mean(sn_online, na.rm = TRUE), sd = sd(sn_online, na.rm = TRUE))
sn_online_stats

sn_gc_stats <- cm_df %>% summarise(mean = mean(sn_gc, na.rm = TRUE), sd = sd(sn_gc, na.rm = TRUE))
sn_gc_stats

# Analysis

## Estimates
ai_estim <- bc.est(Y=ai, A=anchor, p=0.2, p.prime=0.2, data=cm_df %>% select(ai,anchor))
ai_estim

paid_estim <- bc.est(Y=paid, A=anchor, p=0.2, p.prime=0.2, data=cm_df%>% select(paid,anchor))
paid_estim

online_estim <- bc.est(Y=online, A=anchor, p=0.2, p.prime=0.2, data=cm_df%>% select(online,anchor))
online_estim

gc_estim <- bc.est(Y=gc, A=anchor, p=0.2, p.prime=0.2, data=cm_df%>% select(gc,anchor))
gc_estim

## Logistic Regression
cm_df <- cm_df %>% na.omit(c("study_hrs","sleep_hrs","work_hrs","extracurr_hrs","time_management","resources","compl_time")) %>% mutate(tmr_score = (study_hrs+sleep_hrs+work_hrs+extracurr_hrs+time_management+resources+compl_time)/7)
cm_df <- cm_df %>% na.omit(c("sn_ai","sn_paid","sn_online","sn_gc")) %>% mutate(sn_score = (sn_ai+sn_paid+sn_online+sn_gc)/4)

ai_mod <- RRlog(ai~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = cm_df, model = "Crosswise", p = 0.2)
summary(ai_mod)

paid_mod <- RRlog(paid~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = cm_df, model = "Crosswise", p = 0.2)
summary(paid_mod)

online_mod <- RRlog(online~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = cm_df, model = "Crosswise", p = 0.2)
summary(online_mod)

gc_mod <- RRlog(gc~gender+first_gen+HS_GPA+classification+STEM+social_class+tmr_score+sn_score, data = cm_df, model = "Crosswise", p = 0.2)
summary(gc_mod)
