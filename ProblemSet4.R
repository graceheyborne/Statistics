#Problem Set 4
#Grace Heyborne

#set up
setwd("/Users/u1100249/Desktop/PS4/") #working directory
expectancy <- read.csv("Life_Expectancy_Data.csv") #load data
for(i in 1:ncol(expectancy)) {
  expectancy[ , i][is.na(expectancy[ , i])] <- median(expectancy[ , i], na.rm=TRUE) #imputation
}

#libraries
library(tidyverse)
library(corrplot)
library(ggpubr)




#Part 1

#summary statistics
str(expectancy)
summary(expectancy) 
#just glancing at the summary statistics and visually comparing the Min/Med/Max, it looks like very few of these variables will be normally distributed...
#but lets check...

#use boxplot/histogram/QQ plots to get a sense of outliers, skew, and normality.
#convert the data to long format
data_long <- expectancy %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
#boxplot
ggplot(data_long, aes(x = "", y = Value)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) + 
  labs(title = "Boxplots") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
#histogram
ggplot(data_long, aes(x = Value)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(title = "Histograms")
#QQ plot
qq_data <- data_long %>%
  group_by(Variable) %>%
  arrange(Value) %>%
  mutate(
    n = n(),
    p = (row_number() - 0.5) / n,
    theoretical_quantile = qnorm(p)
  )
ggplot(qq_data, aes(x = theoretical_quantile, y = Value)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(title = "QQ Plots", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#as expected, there is a lot of skew and not very many normally distributed variables in this dataset.
#looking at the boxplots: 
    #Diphtheria, Hepatitis.B, and Polio have a substantial amount of low value outliers.
#looking at the histograms:
    #GDP, HIV.AIDS, infant.deaths, measles, percentage.expenditure, population, and under.five.deaths are all positively skewed.
#looking at the QQ plots confirms which variables are closest to a normal distribution:
    #adult.mortality, alcohol, BMI, income.composition, schooling, and total.expenditure
    #the outcome variable, life.expectancy, is also normally distributed (yay)
#TAKEAWAY: the variables with low value outliers and positive skew may introduce bias to our analysis and not be the best predictors.

#use a correlation matrix to look at collinearity 
cor_matrix <- cor(expectancy, use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "color", tl.cex = 0.7, type = "lower", diag = FALSE)
#variables that have high collinearity (and should not be used together as predictors in the same LM) are: 
    #infant.deaths and under.five.deaths
    #percent.expenditure and GDP
    #Schooling and income.composition
cor(expectancy$Polio, expectancy$Diphtheria, use = "complete.obs", method = "pearson")
    #Polio and Diphtheria have a collinearity of 0.67, so they are on the cusp of the threshold...
    
#use scatterplots to look at correlations with life expectancy
ggscatter(expectancy, x = "Adult.Mortality", y = "Life.expectancy", add = "reg.line", conf.int = TRUE)
ggscatter(expectancy, x = "Schooling", y = "Life.expectancy", add = "reg.line", conf.int = TRUE)
#these two variables are the most correlated with Life.expectancy (negatively and then positively, respectively)




#Part 2

#I decided to test models in three steps: 
    #(1) based on predictor groups (lifestyle/illness/mortality/expenditure) without factoring in collinearity
    #(2) in predictor groups, removing some predictors based on collinearities
    #(3) without groups, keeping only the best preforming predictors across all models

#lifestyle model: alcohol, BMI
#illness model: HepB, Measles, Polio, Diphtheria, HIV.AIDS
#mortality model: adult mortality, infant.deaths, under.five.deaths
#expenditures: percent.expenditure, total.expenditure, GDP, income.composition.of.resources, and schooling 

#Step 1
mod_lifestyle <- lm(Life.expectancy ~  Alcohol + BMI, data = expectancy)
coef(mod_lifestyle)
summary(mod_lifestyle)
#R^2 is 0.36 and RSE 7.60
#coef of 0.576 (Alcohol) and 0.230 (BMI) suggest that life expectancy is predicted to increases with increased alcohol consumption and BMI (the reverse is likely true, and Life.expecntancy is predicting alcohol/BMI here accidentally)
#REMOVE BOTH PREDICTORS due to unexpected/illogical effects

mod_illness <- lm(Life.expectancy ~ Hepatitis.B + Measles + Polio + Diphtheria + HIV.AIDS, data = expectancy)
coef(mod_illness)
summary(mod_illness)
#R^2 is slightly higher at 0.49 and RSE slightly lower at 6.77 (better model than lifestyle)
#coef of -0.032 (HepB), -0.0000667 (Measles), 0.088 (Polio), 0.109 (Diphtheria), -0.902 (HIV.AIDS)
#Polio and Diphtheria positively predicted life expectancy, while HIV.AIDS negatively predicted it.
#REMOVE PREDICTORS HepB and Measles due to small effects.

mod_mortality <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths + under.five.deaths, data = expectancy)
coef(mod_mortality)
summary(mod_mortality)
#R^2 is higher at 0.54 and RSE lower at 6.42 (better model than illness)
#Adult.mortality (-0.049) and under.five.deaths (-0.14) negatively predicted life expectancy
#surprisingly, infant.death is positively effecting life expectancy (0.185) (likely a collinearity effect)
#REMOVE PREDICTOR infant.death due to unexpected/illogical effect.

mod_expenditure <- lm(Life.expectancy ~ percentage.expenditure + Total.expenditure + GDP + Income.composition.of.resources + Schooling, data = expectancy)
coef(mod_expenditure)
summary(mod_expenditure)
#R^2 high at 0.56 and RSE low at 6.29 (better model than mortality)
#percentage.expenditure is not significant (p = 0.436)
#total expenditure (2.4), income composition (14.12), and schooling (1.19) all positvely predict life expectancy
#REMOVE non-significant precentage.expendditure

#Step 2
#removing infant.deaths and percent expenditure solved 2/4 collinearity issues
#it is difficult to decide which to remove out of the other collinearity issue variables (schooling/income.composition and Polio/Diphtheria) so I run models that include both and then only 1 each

mod_illness2 <- lm(Life.expectancy ~ Polio + Diphtheria + HIV.AIDS, data = expectancy) #model Polio and Diphtheria but without HepB and Measles 
coef(mod_illness2)
summary(mod_illness2)
#R^2 is unaffected (0.48 now vs 0.49 before), RSE is only slightly higher at 6.83 (vs 6.77) showing that their effect was minimal
mod_illness3 <- lm(Life.expectancy ~ Diphtheria + HIV.AIDS, data = expectancy) #Diphtheria only
coef(mod_illness3)
summary(mod_illness3)
#R^2 of 0.45 and RSE of 6.99
mod_illness4 <- lm(Life.expectancy ~ Polio  + HIV.AIDS, data = expectancy) #Polio only
coef(mod_illness4)
summary(mod_illness4)
#R^2 of 0.44 and RSE 7.05
#Polio and Diphtheria are so similar in the models, but one has to be cut to prevent collinearity issues so...
#REMOVE PREDICTOR Polio

mod_mortality2 <- lm(Life.expectancy ~ Adult.Mortality + under.five.deaths, data = expectancy) #after removing infants.deaths
coef(mod_mortality2)
summary(mod_mortality2)
#R^2 is slightly lower at 0.50 (vs 0.54), RSE is slightly higher at 6.66 (vs 6.42)
#removing infant.deaths was a good choice that didn't drastically effect the model
#under.five.deaths has a small effect of only -0.009 so...
#REMOVE under.five.deaths

mod_expenditure2 <- lm(Life.expectancy ~  Total.expenditure + GDP + Income.composition.of.resources, data = expectancy) #Income.composition only 
coef(mod_expenditure2)
summary(mod_expenditure2)
# R^2 of 0.50 and RSE of 6.697
mod_expenditure3 <- lm(Life.expectancy ~  Total.expenditure + GDP + Schooling, data = expectancy) #Schooling only
coef(mod_expenditure3)
summary(mod_expenditure3)
#R^2 of 0.52 and RSE of 6.527
#REMOVE PREDICTOR Income.composition and keep Schooling.

#Step 3
#HIV.AIDS (coef -0.9) AND Diphtheria (coef 0.15) are the best preforming predictors from the illness model
#Adult.Mortality (coef -0.052) is the best preforming predictor from the mortality model
#Total.expenditure (coef 2.05), GDP (coef 1.07), and Schooling (coef 1.85) are the best predictors from the expenditure model

#combining these best preforming predictors we get:
mod_best <- lm(Life.expectancy ~  Total.expenditure + GDP + Schooling + Adult.Mortality + HIV.AIDS + Diphtheria, data = expectancy)
coef(mod_best)
summary(mod_best)
#best model so far with R^2 of 0.78 and RSE of only 4.45.

#visualize the models (the best model and the previous top preforming model)
par(mfrow = c(2, 2)) 
plot(mod_best)
par(mfrow = c(2, 2))
plot(mod_expenditure3)
#funny enough the plots from the top preforming expenditure grouped model look slightly better (less variable on the Residuals vs Leverage plot)
#the best model still visualizes well with better R^2 and RSE




#Part 3
'''
The best linear model for predicting the outcome Life.expectancy from the WHO dataset included the following predictors: HIV.AIDS (coef -5.08), 
Diphtheria (coef 6.72), Adult.Mortality (coef -2.29), Total.expenditure (coef 1.99), GDP (coef 7.27), and Schooling (coef 1.18). HIV.AIDS and
Adult.mortality predicted negative effects on life expectancy, while Diphtheria, Total.expenditure, GDP, and School predicted positive effects on
Life.expectancy. I choose these predictors because they were the best predictors from the grouped linear models (illness, mortality, and expenditure).
Additionally, Diptheria and Schooling were the better preforming predictors relative to the predictors they had high collinearity with (Polio and 
Income.composition) (since I had to remove one of the predictors from each high collinearity pair, they were the obvious choice). Aside from those 2
high collinearity varaibles I did not include percentage.expenditure (because it was not significant in the model), HepB/Measles/under.five.deaths
(because their effect sizes were small), and infant.deaths (because its effect was strangely positive). Lastly, I did not include either lifestyle
predictor, alcohol and BMI, because their effect was also strangely positive. I figured that this was likely the effect that the outcome (life 
expectancy)had on the predictors (alcohol and BMI) because people tend to gain weight and drink more as they age. This left me with my "best model" 
predictors that scored significantly better than the next best model (R^2 of 0.78:0.52 and RSE of 4.45:6.53).

The next best model included expenditure predictors: Total.expenditure (coef 2.05), GDP (coef 1.07), and Schooling (coef 1.86). These predictors made
up 3/6 of the the best model predictors in the end. My inturpretation of this is that life expectency is heavily influenced by variables that relate to 
economic status (schooling isnt strictly an economic status variable, but I would suspect that theres overlap between economic status and years of 
schooling). HIV.AIDS likely had a strong negative effect due to the high mortality rate associated with the condition. Dipthermia was the best predictor
amoungst the vaccine group, and supports the notion that vaccinations improve life expectancy. Finally, adult mortality has a strong effect on life 
expectancy but this really doesnt tell us much, since we would assume that adult mortality would negativly effect life expectancy like we see in the 
model. It boosts model preformance but is not very meaningful for making life expectancy inferences. 
'''
