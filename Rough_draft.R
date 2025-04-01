#Haenyeo KNHANES Analysis

library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(patchwork)
library(effsize)
library(nnet)
library(ggpubr)

setwd("/Users/u1100249/Desktop/Haenyeo_Data/")

data07 <- read_sav("HN07_select.sav")
data08 <- read_sav("HN08_select.sav")
data09 <- read_sav("HN09_select.sav")
data10 <- read_sav("HN10_select.sav")
data11 <- read_sav("HN11_select.sav")
data12 <- read_sav("HN12_select.sav")
data13 <- read_sav("HN12_select.sav")
data14 <- read_sav("HN14_select.sav")
data15 <- read_sav("HN15_select.sav")
data16 <- read_sav("HN16_select.sav")
data17 <- read_sav("HN17_select.sav")
data18 <- read_sav("HN18_select.sav")
data19 <- read_sav("HN19_select.sav")
data20 <- read_sav("HN20_select.sav")
data21 <- read_sav("HN21_select.sav")
data22 <- read_sav("HN22_select.sav")




#LOOK AT DATA
#Seoul is region 1, Jeju is region 17. Check to make sure both regions are in every year.
datasets <- list("2007" = data07,
                 "2008" = data08, 
                 "2009" = data09, 
                 "2010" = data10,
                 "2011" = data11,
                 "2012" = data12,
                 "2013" = data13,
                 "2014" = data14,
                 "2015" = data15,
                 "2016" = data16,
                 "2017" = data17,
                 "2018" = data18,
                 "2019" = data19,
                 "2020" = data20,
                 "2021" = data21,
                 "2022" = data22)
check_datasets<- names(Filter(function(df) all(c(1, 17) %in% df$region), datasets))
print(check_datasets)
#only the years 2016-2022 have both regions so we will exclude all other years from analysis.
##check to make sure the column variables are the same across the 7 years.
colnames(data16)
colnames(data17)
colnames(data18)
colnames(data19)
colnames(data20)
colnames(data21)
colnames(data22)
unique_cols <- unique(unlist(lapply(limited_datasets, names)))
print(unique_cols)
##HE_PLS HE_PLS_30 are only featured in some years but they are not currently relevant.




#SUB-DATASETS
# 2016-2022 subset
data<- bind_rows(data07, data08, data09, data10, data11, data12, 
                 data13, data14, data15, data16, data17, data18, 
                 data19, data20, data21, data22) #combine 2016-2022
df <- data %>%
  filter(region %in% c(1, 17)) #limit to only Jeju and Seoul
df <- df %>%
  mutate(region = recode(region, `1` = "Seoul", `17` = "Jeju")) #rename regions
unique(df$region) #confirm
# Jeju subset
df_jeju <- df %>%
  filter(region == "Jeju") #create a jeju only dataset
# Seoul subset
df_seoul <- df %>%
  filter(region == "Seoul") #create a seoul only dataset




###DESCRIPTIVE STATS

#COUNTS
#number of individuals per region
jeju_count <- df %>%
  filter(region == "Jeju") %>%
  count()
print(jeju_count) #1106 individuals across all years
seoul_count <- df %>%
  filter(region == "Seoul") %>%
  count()
print(seoul_count) #23,748 individuals
#gender split
sex_count <- df %>%
  group_by(region) %>%
  count(sex)
print(sex_count) #pretty even
#age range
summary_age <- df %>% 
  group_by(region) %>% 
  summarize(median = median(age),
            mean = mean(age),
            std_dev = sd(age),
            max_age = max(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE))
summary_age #Seoul and Jeju are very similar in age range with jeju having a slightly higer median age.
ggplot(data = df, aes(x = age, y = factor(region))) +
  geom_boxplot(fill = "white", color = "black") +
  theme_classic() +
  labs(x = "Region", y = "Age", title = "Age Comparison Between Regions")


#BP COMPARISON
#plot systolic
threshold_data <- data.frame(
  yintercept = c(120, 130),
  Threshold = c("Normal BP (120 mmHg)", "Hypertension BP (130 mmHg)")
)

ggplot(data = df, aes(x = factor(region), y = HE_sbp)) +
  geom_boxplot(fill = "white", color = "black") +
  geom_hline(data = threshold_data, aes(yintercept = yintercept, color = Threshold), 
             linetype = "dashed", size = 1) +  # Horizontal lines at 120 and 130 mmHg
  scale_color_manual(name = "BP Thresholds", 
                     values = c("Normal BP (120 mmHg)" = "lightblue", 
                                "Hypertension BP (130 mmHg)" = "coral")) +
  theme_classic() +
  labs(x = "Region", y = "Systolic BP", title = "Systolic BP Comparison") #systolic BP is slightly higher in Jeju than in Seoul
#plot diastolic
ggplot(data = df, aes(x = factor(region), y = HE_dbp)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_classic() +
  labs(x = "Region", y = "Diastolic BP", title = "Diastolic BP Comparison") #virtually no difference
#plot them together
bp_long <- df %>%
  select(region, HE_sbp, HE_dbp) %>%
  pivot_longer(cols = c(HE_sbp, HE_dbp),
               names_to = "BP_Type",
               values_to = "BP_Value") %>%
  mutate(BP_Type = recode(BP_Type,
                          "HE_sbp" = "Systolic BP",
                          "HE_dbp" = "Diastolic BP"))
threshold_data <- data.frame(
  yintercept = c(120, 130),
  Threshold = c("Normal BP (120 mmHg)", "Hypertension BP (130 mmHg)"),
  BP_Type = "Systolic BP"  # Only apply to systolic
)
ggplot(bp_long, aes(x = factor(region), y = BP_Value)) +
  geom_boxplot(fill = "white", color = "black") +
  geom_hline(data = threshold_data, aes(yintercept = yintercept, color = Threshold),
             linetype = "dashed", size = 1) +
  scale_color_manual(name = "BP Thresholds",
                     values = c("Normal BP (120 mmHg)" = "lightblue",
                                "Hypertension BP (130 mmHg)" = "coral")) +
  facet_wrap(~ BP_Type, scales = "free_y") +
  theme_classic() +
  labs(x = "Region", y = "Blood Pressure (mmHg)", title = "Blood Pressure Comparison by Region")
#plot them together with a different variable
bp_readings_jeju <- data.frame(
  Reading = c("1st", "2nd", "3rd"),
  Systolic_BP = c(mean(df_jeju$HE_sbp1, na.rm = TRUE), 
                  mean(df_jeju$HE_sbp2, na.rm = TRUE), 
                  mean(df_jeju$HE_sbp3, na.rm = TRUE)),
  Diastolic_BP = c(mean(df_jeju$HE_dbp1, na.rm = TRUE), 
                   mean(df_jeju$HE_dbp2, na.rm = TRUE), 
                   mean(df_jeju$HE_dbp3, na.rm = TRUE))
)
bp_readings_seoul <- data.frame(
  Reading = c("1st", "2nd", "3rd"),
  Systolic_BP = c(mean(df_seoul$HE_sbp1, na.rm = TRUE), 
                  mean(df_seoul$HE_sbp2, na.rm = TRUE), 
                  mean(df_seoul$HE_sbp3, na.rm = TRUE)),
  Diastolic_BP = c(mean(df_seoul$HE_dbp1, na.rm = TRUE), 
                   mean(df_seoul$HE_dbp2, na.rm = TRUE), 
                   mean(df_seoul$HE_dbp3, na.rm = TRUE))
)
bp_jeju_systolic <- bp_readings_jeju %>% #reshape systolic
  select(Reading, Systolic_BP) %>%
  mutate(Region = "Jeju")
bp_seoul_systolic <- bp_readings_seoul %>%
  select(Reading, Systolic_BP) %>%
  mutate(Region = "Seoul")
bp_systolic_long <- bind_rows(bp_jeju_systolic, bp_seoul_systolic) %>% #make long
  rename(BP_Type = Systolic_BP)

bp_jeju_diastolic <- bp_readings_jeju %>% #reshape diastolic
  select(Reading, Diastolic_BP) %>%
  mutate(Region = "Jeju")
bp_seoul_diastolic <- bp_readings_seoul %>%
  select(Reading, Diastolic_BP) %>%
  mutate(Region = "Seoul")
bp_diastolic_long <- bind_rows(bp_jeju_diastolic, bp_seoul_diastolic) %>% #make long
  rename(BP_Type = Diastolic_BP)

plot_systolic <- ggplot(bp_systolic_long, aes(x = Reading, y = BP_Type, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Systolic BP Readings",
       x = "Reading Number",
       y = "Mean Systolic BP (mmHg)") +
  scale_color_manual(values = c("lightblue", "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

plot_diastolic <- ggplot(bp_diastolic_long, aes(x = Reading, y = BP_Type, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Diastolic BP Readings",
       x = "Reading Number",
       y = "Mean Diastolic BP (mmHg)") +
  scale_color_manual(values = c("lightblue", "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

(plot_systolic + plot_diastolic) + plot_annotation(title = "Mean BP Readings by Region")


#HYPERTENSION COMPARISON
#simple variable
plot_hypertension_diagnosis <- function(df_jeju, df_seoul, column_jeju, column_seoul, label) {
  
  jeju_table <- table(df_jeju[[column_jeju]])
  jeju_table <- jeju_table[names(jeju_table) %in% c("0", "1")]  # Keep only "0" and "1"
  
  seoul_table <- table(df_seoul[[column_seoul]])
  seoul_table <- seoul_table[names(seoul_table) %in% c("0", "1")]  # Keep only "0" and "1"
  
  jeju_prop <- prop.table(jeju_table) * 100
  seoul_prop <- prop.table(seoul_table) * 100
  
  names(jeju_prop) <- ifelse(names(jeju_prop) == "1", "Yes", "No")
  names(seoul_prop) <- ifelse(names(seoul_prop) == "1", "Yes", "No")
  
  data <- data.frame(
    Region = rep(c("Jeju", "Seoul"), each = length(jeju_prop)),
    Family = rep(label, times = length(jeju_prop)),
    Response = rep(names(jeju_prop), 2),
    Proportion = c(jeju_prop, seoul_prop)
  )
  
  ggplot(data, aes(x = Response, y = Proportion, fill = Region)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = label,
         x = "Response",
         y = "Proportion (%)") +
    scale_fill_manual(values = c("lightblue", "coral")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels = c("No", "Yes"))  
}
plot_hypertension_diagnosis(df_jeju, df_seoul, "DI1_dg", "DI1_dg", "Hypertension Diagnosis")
#complex variable
plot_hypertension_status <- function(df_jeju, df_seoul, column_jeju, column_seoul, label) {
  levels_to_keep <- c("1", "2", "3", "4")
  level_labels <- c("Normal", "Prehypertension", "Early Hypertension", "Hypertension")
  
  jeju_table <- table(df_jeju[[column_jeju]])
  jeju_table <- jeju_table[names(jeju_table) %in% levels_to_keep]
  jeju_prop <- prop.table(jeju_table) * 100
  names(jeju_prop) <- level_labels[match(names(jeju_prop), levels_to_keep)]
  
  seoul_table <- table(df_seoul[[column_seoul]])
  seoul_table <- seoul_table[names(seoul_table) %in% levels_to_keep]
  seoul_prop <- prop.table(seoul_table) * 100
  names(seoul_prop) <- level_labels[match(names(seoul_prop), levels_to_keep)]
  
  data <- data.frame(
    Region = rep(c("Jeju", "Seoul"), each = length(jeju_prop)),
    Hypertension = rep(names(jeju_prop), 2),
    Proportion = c(jeju_prop, seoul_prop)
  )
  
  ggplot(data, aes(x = Hypertension, y = Proportion, fill = Region)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = label,
         x = "Hypertension Status",
         y = "Proportion (%)") +
    scale_fill_manual(values = c("lightblue", "coral")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_hypertension_status(df_jeju, df_seoul, "HE_HP", "HE_HP", "Hypertension Status")


#STROKE COMPARISON
df$parental_stroke <- ifelse(df$HE_STRfh2 == 1 | df$HE_STRfh1 == 1, 1, 0)
parental_strokes <- df[df$HE_STRfh1 != 9 & df$HE_STRfh2 != 9, ]
parental_strokes <- df[df$parental_stroke %in% c(0, 1), ]
parental_strokes$parental_stroke_label <- factor(
  parental_strokes$parental_stroke,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
ggplot(parental_strokes, aes(x = region, fill = parental_stroke_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("Yes" = "coral", "No" = "lightblue")
  ) +
  labs(
    y = "proportion",
    fill = "Parental Stroke",
    title = "Parental Stroke History"
  ) +
  theme_minimal()




###TESTS

#COHENS D
jeju <- df[df$region == "Jeju" & df$parental_stroke %in% c(0, 1), ]
seoul <- df[df$region == "Seoul" & df$parental_stroke %in% c(0, 1), ]
#systolic
d_jeju <- cohen.d(jeju$HE_sbp ~ jeju$parental_stroke)
d_seoul <- cohen.d(seoul$HE_sbp ~ seoul$parental_stroke)
print(d_jeju)
print(d_seoul)

#diastolic
d2_jeju <- cohen.d(jeju$HE_dbp ~ jeju$parental_stroke)
d2_seoul <- cohen.d(seoul$HE_dbp ~ seoul$parental_stroke)
print(d2_jeju)
print(d2_seoul)

#make a table of these results
effect_sizes <- data.frame(
  Region = c("Jeju", "Seoul"),
  Systolic_d = c(d_jeju_est, d_seoul_est),
  Diastolic_d = c(d2_jeju_est, d2_seoul_est)
)
print(effect_sizes)
effect_sizes <- effect_sizes %>%
  mutate(across(where(is.numeric), round, 2))
ggtexttable(effect_sizes, rows = NULL, theme = ttheme("light"))


#Z TEST
compare_cohens_d <- function(d1, n1_1, n1_2, d2, n2_1, n2_2) {
  se1 <- sqrt((n1_1 + n1_2) / (n1_1 * n1_2) + (d1^2 / (2 * (n1_1 + n1_2))))
  se2 <- sqrt((n2_1 + n2_2) / (n2_1 * n2_2) + (d2^2 / (2 * (n2_1 + n2_2))))
  se_diff <- sqrt(se1^2 + se2^2)
  
  z <- (d1 - d2) / se_diff
  p <- 2 * (1 - pnorm(abs(z)))
  
  return(list(z = z, p = p))
}
n_jeju_0 <- sum(jeju$parental_stroke == 0)
n_jeju_1 <- sum(jeju$parental_stroke == 1)
n_seoul_0 <- sum(seoul$parental_stroke == 0)
n_seoul_1 <- sum(seoul$parental_stroke == 1)

#systolic
d_jeju_est <- -0.24
d_seoul_est <- -0.30
z_test_result <- compare_cohens_d(
  d1 = d_jeju_est,
  n1_1 = n_jeju_0,
  n1_2 = n_jeju_1,
  d2 = d_seoul_est,
  n2_1 = n_seoul_0,
  n2_2 = n_seoul_1
)
print(z_test_result)

#diastolic
d2_jeju_est <- -0.32
d2_seoul_est <- -0.25
z_test_result2 <- compare_cohens_d(
  d1 = d2_jeju_est,
  n1_1 = n_jeju_0,
  n1_2 = n_jeju_1,
  d2 = d2_seoul_est,
  n2_1 = n_seoul_0,
  n2_2 = n_seoul_1
)
print(z_test_result2)

#plot results
z_test_result <- list(z = 0.44, p = 0.66)    
z_test_result2 <- list(z = -0.51, p = 0.61) 
z_table <- data.frame(
  Comparison = c("Systolic BP (d)", "Diastolic BP (d)"),
  Z_score = c(round(z_test_result$z, 2), round(z_test_result2$z, 2)),
  P_value = c(round(z_test_result$p, 4), round(z_test_result2$p, 4))
)
ggtexttable(z_table, rows = NULL, theme = ttheme("light")) +
  ggtitle("Z-Test Comparing Effect Size")


#REGRESSIONS
#BP predicting stroke/no stroke outcome
df_clean <- df[df$parental_stroke %in% c(0, 1), ]
log_model <- glm(
  parental_stroke ~ HE_sbp * region,
  data = df_clean,
  family = binomial
)
summary(log_model) #not significant

#maybe with covariates?
log_model_with_sodium <- glm(
  parental_stroke ~ HE_sbp * region + N_NA + age + sex,
  data = df_clean,
  family = binomial
)
summary(log_model_with_sodium) #still not significant

#Hypertension predicting stroke/no stroke outcome
model_htn <- glm(
  parental_stroke ~ factor(HE_HP) * region + N_NA + age + sex,
  data = df_clean,
  family = binomial
)
summary(model_htn)

#BP predicting hypertension states
df_clean$HE_HP <- factor(df_clean$HE_HP)
multi_model <- multinom(HE_HP ~ HE_sbp * region, data = df_clean)
summary(multi_model)
z_scores <- summary(multi_model)$coefficients / summary(multi_model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))
round(p_values, 4)
print(p_values) #literally all 0?
format.pval(p_values, digits = 10, eps = .Machine$double.eps) #not 0 but very small

#plotting to see whats going on with regional differences in th last regression
bp_seq <- seq(
  from = min(df_clean$HE_sbp, na.rm = TRUE),
  to = max(df_clean$HE_sbp, na.rm = TRUE),
  length.out = 100
)
new_data <- expand.grid(
  HE_sbp = bp_seq,
  region = c("Jeju", "Seoul")
)
pred_probs <- predict(multi_model, newdata = new_data, type = "probs")
pred_probs_df <- as.data.frame(pred_probs)
pred_probs_df$`1` <- 1 - rowSums(pred_probs_df)
pred_probs_df <- pred_probs_df[, c("1", "2", "3", "4")]
pred_data <- cbind(new_data, pred_probs_df)
pred_long <- pivot_longer(
  pred_data,
  cols = c("1", "2", "3", "4"),
  names_to = "HTN_Category",
  values_to = "Probability"
)
pred_long$HTN_Category <- factor(
  pred_long$HTN_Category,
  levels = c("1", "2", "3", "4"),
  labels = c("Normal", "Pre-HTN", "Early HTN", "HTN")
)
ggplot(pred_long, aes(x = HE_sbp, y = Probability, color = HTN_Category)) +
  geom_line(size = 1.2) +
  facet_wrap(~region) +
  labs(
    title = "SBP and Region predict Hypertension Status outcomes",
    x = "Systolic BP (mmHg)",
    y = "Probability",
    color = "Hypertension Status"
  ) +
  theme_minimal()

