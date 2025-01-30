#1: R Practice
setwd("/Users/u1100249/Desktop/Statistics/")
list.files()
KG_Samples = read.table("igsr_samples.tsv",sep = "\t", header = T)
head(KG_Samples)
library(tidyverse)
#seperate function
head(KG_Samples$Data.collections,2)
KG_Samples = KG_Samples %>%
  separate(Data.collections,c("A","B","C","D","E"),",")
#number of samples
nrow(KG_Samples)
#gender split
table(KG_Samples$Sex)
KG_Samples %>% group_by(Sex) %>% tally()
#complex query
table(KG_Samples$Population.code,KG_Samples$Sex)
KG_Samples %>% filter(Population.code =="GBR") %>% group_by(Sex) %>% tally()
#41
#sample function
sample(c(0:100),1)
#43
KG_Samples$Age = sample(c(0:100),size = nrow(KG_Samples), replace = TRUE)
head(KG_Samples,2)
#reorganize columns
KG_Samples = KG_Samples[,c(1,2,14,3:8)]
head(KG_Samples,2)
colnames(KG_Samples)
KG_Samples = subset(KG_Samples, select=-c(Biosample.ID))
colnames(KG_Samples)
#summary statistics
summary_Age <- KG_Samples %>% 
  summarize(mean = mean(Age), std_dev = sd(Age))
summary_Age
summary_sex_Age <- KG_Samples %>% 
  group_by(Sex) %>% 
  summarize(mean = mean(Age, na.rm = TRUE), 
            std_dev = sd(Age, na.rm = TRUE))
summary_sex_Age
KG_Samples %>% 
  group_by(Population.name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
KG_Samples <- KG_Samples %>% 
  mutate(Age_Months = Age * 52)
#plotting
ggplot(data = KG_Samples, mapping = aes(x = Age)) +
  geom_histogram(color = "white", bins = 25)+
  theme_classic()
KG_Samples = KG_Samples %>% 
  mutate(Age_EUR = if_else(condition = Age <50 & Superpopulation.code=="EUR",true = 1,false = 0))
head(KG_Samples,2)

#2: Problem Set R

#1. Load the dataset and show the first 5 lines
KG_Samples = read.csv("diabetes.csv", header = T)
head(KG_Samples,5)
#2. How many patients have diabetes?
sum(KG_Samples$Outcome == 1)
##ANSWER: 268
#3. How many patients have diabetes that are older than 45?
sum(KG_Samples$Outcome == 1 & KG_Samples$Age > 45)
##ANSWER: 58
#4. What is the mean and variance of glucose levels for individuals without diabetes
## I first removed individuals with 0 as Glucose lelvel
KG_Samples$Glucose[KG_Samples$Glucose == 0] <- NA
mean(KG_Samples$Glucose[KG_Samples$Outcome == 0], na.rm = TRUE)
var(KG_Samples$Glucose[KG_Samples$Outcome == 0], na.rm = TRUE)
##MEAN: 110.6439
##VARAINCE: 613.8951
#5. Create a new discrete variable that has 1 if the individual has diabetes and high blood pressure (above 100), 2 if an indivual has diabetes and low blood pressure and 3 if the individual does not have diabetes.
KG_Samples <- KG_Samples %>% 
  mutate(Diabetes_HighBP = if_else(Outcome == 1 & BloodPressure > 100, 
                                   1, 
                                   if_else(Outcome == 1 & BloodPressure <= 100, 
                                           2, 
                                           3)))
head(KG_Samples,5)
#6. Construct two plots of the distribution of BMI for individuals with diabetes and without diabetes
## I first removed the individuals with a BMI of 0.
KG_Samples$BMI[KG_Samples$BMI == 0] <- NA
## plot for no diabetes
ggplot(data = KG_Samples %>% na.omit() %>% filter(Outcome == 0), mapping = aes(x = BMI)) + 
  geom_histogram(color = "white", bins = 15) + 
  theme_classic() + 
  labs(title = "No Diabetes", x = "BMI", y = "Frequency")
#plot for diabetes
ggplot(data = KG_Samples %>% na.omit() %>% filter(Outcome == 1), mapping = aes(x = BMI)) + 
  geom_histogram(color = "white", bins = 15) + 
  theme_classic() + 
  labs(title = "Diabetes", x = "BMI", y = "Frequency")
