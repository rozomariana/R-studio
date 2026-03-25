#Intro to Frequentist (Multilevel) Generalised Linear Models (GLM) in R with glm and lme4
library(lme4) # for multilevel models
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(effects) #for plotting parameter effects
library(jtools) #for transformaing model summaries
library(ROCR) #for calculating area under the curve (AUC) statistics
#he data stems from a national survey of primary education in Thailand (Raudenbush & Bhumirat, 1992). Each row in the data refers to a pupil. The outcome variable REPEAT is a dichotomous variable indicating whether a pupil has repeated a grade during primary education. The SCHOOLID variable indicates the school of a pupil. The person-level predictors include: SEX (0 = female, 1 = male) and PPED (having had preschool education, 0 = no, 1 = yes). The school-level is MSESC, representing school mean SES (socio-economic status) scores.
#The main research questions that this tutorial seeks to answer using the Thai Educational Data are:

#Ignoring the clustering structure of the data, what are the effects of gender and preschool education on whether a pupil repeats a grade?
#Ignoring the clustering structure of the data, what is the effect of school mean SES on the proportion of pupil repeating a grade?
#Considering the clustering structure of the data, what are the effects of gender, preschool education and school mean SES on whether a pupil repeats a grade?
#These three questions are answered by using these following models, respectively: binary logistic regressioin; binomial logistic regression; multilevel binary logistic regression.
#ThaiEdu_Raw <- read_sav("https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%206/Thaieduc/thaieduc.sav?raw=true") esta no funciona.
ThaiEdu_Raw <- read_sav("https://github.com/MirjamMoerbeek/Data_Sets_ML_book/blob/main/thaieduc.sav?raw=true")
ThaiEdu_New <- ThaiEdu_Raw %>%
  mutate(SCHOOLID = factor(SCHOOLID),
         SEX = if_else(SEX == 0, "girl", "boy"),
         SEX = factor(SEX, levels = c("girl", "boy")),
         PPED = if_else(PPED == 0, "no", "yes"),
         PPED = factor(PPED, levels = c("no", "yes")))
#funcion deprecated...usar across si se puede pero toca ver la sintaxis
ThaiEdu_New %>%
  summarise_each(list(~sum(is.na(.)))) %>%
  gather()

ThaiEdu_New <- ThaiEdu_New %>%
  filter(!is.na(MSESC))
#Binary Logistic Regression
#explorando los datos
ThaiEdu_New %>%
  group_by(SEX) %>%
  summarise(REPEAT = sum(REPEAT))

ThaiEdu_New %>%
  group_by(PPED) %>%
  summarise(REPEAT = sum(REPEAT))
 
 #It seems that the number of pupils who repeated a grade differs quite a bit between the two genders, with more male pupils having to repeat a grade. More pupils who did not have preschool education repeated a grade. This observation suggests that SEX and PPED might be predictive of REPEAT.
#Fit a Binary Logistic Regression Model
#R has the base package installed by default, which includes the glm function that runs GLM. The arguments for glm are similar to those for lm: formula and data. However, glm requires an additional argument: family, which specifies the assumed distribution of the outcome variable; within family we also need to specify the link function. The default of family is gaussian(link = "identity"), which leads to a linear model that is equivalent to a model specified by lm. In the case of binary logistic regression, glm requires that we specify a binomial distribution with the logit link, namely family = binomial(link = "logit").

Model_Binary <- glm(formula = REPEAT ~ SEX + PPED,
                    family = binomial(link = "logit"),
                    data = ThaiEdu_New)
summary(Model_Binary)

#Interpretation
#From the summary output above, we can see that SEX positively and significantly predicts a pupil’s probability of repeating a grade, while PPED negatively and significantly so. Specifically, in comparison to being a girl, being a boy is more likely to repeat a grade. Having previous schooling is less likely to result in repeating a grade.
#To interpret the value of the parameter estimates, we need to exponentiate the estimates. The summ function from the jtools packages provides an easy to do so for any model fitted by glm. See below.
summ(Model_Binary, exp = T) # set "exp = T" to show esponentiated estimates; if you need standardised estimaets, set "scale = T"
#Note that the interpretation of the parameter estimates is linked to the odds rather than probabilities. The definition of odds is: P(event occurring)/P(event not occurring). In this analysis, assuming everything else stays the same, being a boy increases the odds of repeating a grade by 54%, in comparison to being a girl; having preschool education lowers the odds of repeating a grade by (1 – 0.54)% = 46%, in comparison to not having preschool education, assuming everything else stays constant.

#Visualisation of Parameter Effects
#To make the interpretation of the parameter effects even easier, we can use the allEffects function from the effects package to visualise the parameter effects. See below.
plot(allEffects(Model_Binary))
#Note that in both plots, the y scale refers to the probability of repeating a grade rather than the odds. Probabilities are more interpretable than odds. The probability scores for each variable are calculated by assuming that the other variables in the model are constant and take on their average values. As we can see, assuming that a pupil has an average preschool education, being a boy has a higher probability (~0.16) of repeating a grade than being a girl ~0.11). Likewise, assuming that a pupil has an average gender, having preschool education has a lower probability (~0.11) of repeating a grade than not having preschool education (~0.18). Note that in both plots the confidence intervals for the estimates are also included to give us some idea of the uncertainties of the estimates.
#Note that the notion of average preschool education and gender may sound strange, given they are categorical variables (i.e. factors). If you are not comfortable with the idea of assuming an average factor, you can specify your intended factor level as the reference point, by using the fixed.predictors = list(given.values = ...) argument in the allEffects function. See below:
plot(allEffects(Model_Binary, fixed.predictors = list(given.values=c(SEXboy=0, PPEDyes = 0))))
#Setting SEXboy = 0 means that for the PPED effect plot, the reference level of the SEX variable is set to 0; PPEDyes = 0 results in the 0 being the reference level of the PPED variable in the SEX effect plot.
#Therefore, as the two plots above show, assuming that a pupil has no preschool education, being a boy has a higher probability (~0.20) of repeating a grade than being a girl ~0.14); assuming that a pupil is female, having preschool education has a lower probability (~0.09) of repeating a grade than not having preschool education (~0.15).

#Model Evaluation: Goodness of Fit

#Likelihood ratio test
#A logistic regression model has a better fit to the data if the model, compared with a model with fewer predictors, demonstrates an improvement in the fit. This is performed using the likelihood ratio test, which compares the likelihood of the data under the full model against the likelihood of the data under a model with fewer predictors. Removing predictor variables from a model will almost always make the model fit less well (i.e. a model will have a lower log likelihood), but it is useful to test whether the observed difference in model fit is statistically significant.
#specify a model with only the `SEX` variable
Model_Binary_Test <- glm(formula = REPEAT ~ SEX,
                         family = binomial(link = "logit"),
                         data = ThaiEdu_New)

#use the `anova()` function to run the likelihood ratio test
anova(Model_Binary_Test, Model_Binary, test ="Chisq")
#As we can see, the model with both SEX and PPED predictors provide a significantly better fit to the data than does the model with only the SEX variable. Note that this method can also be used to determine whether it is necessary to include one or a group of variables.

#AIC
Model_Binary_Test$aic
Model_Binary$aic
#Correct Classification Rate
#The percentage of correct classification is another useful measure to see how well the model fits the data
#use the `predict()` function to calculate the predicted probabilities of pupils in the original data from the fitted model
Pred <- predict(Model_Binary, type = "response")
Pred <- if_else(Pred > 0.5, 1, 0)
ConfusionMatrix <- table(Pred, pull(ThaiEdu_New, REPEAT)) #`pull` results in a vector
#correct classification rate
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)
#We can see that the model correctly classifies 85.8% of all the observations. However, a closer look reveals that the model predicts all of the observations to belong to class “0”, meaning that all pupils are predicted not to repeat a grade. Given that the majority category of the REPEAT variable is 0 (No), the model does not perform better in classification than simply assigning all observations to the majority class 0 (No).
#AUC (area under the curve).
#An alternative to using correct classification rate is the Area under the Curve (AUC) measure. The AUC measures discrimination, that is, the ability of the test to correctly classify those with and without the target response. In the current data, the target response is repeating a grade. We randomly pick one pupil from the “repeating a grade” group and one from the “not repeating a grade” group. The pupil with the higher predicted probability should be the one from the “repeating a grade” group. The AUC is the percentage of randomly drawn pairs for which this is true. This procedure sets AUC apart from the correct classification rate because the AUC is not dependent on the imblance of the proportions of classes in the outcome variable. A value of 0.50 means that the model does not classify better than chance. A good model should have an AUC score much higher than 0.50 (preferably higher than 0.80).
#Compute AUC for predicting Class with the model
Prob <- predict(Model_Binary, type="response")
Pred <- prediction(Prob, as.vector(pull(ThaiEdu_New, REPEAT)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values

#Binomial Logistic Regression
#As mentioned in the beginning, logistic regression can also be used to model count or proportion data. Binary logistic regression assumes that the outcome variable comes from a bernoulli distribution (which is a special case of binomial distributions) where the number of trial 𝑛
# is 1 and thus the outcome variable can only be 1 or 0. In contrast, binomial logistic regression assumes that the number of the target events follows a binomial distribution with 𝑛
#trials and probability 𝑞. In this way, binomial logistic regression allows the outcome variable to take any non-negative integer value and thus is capable of handling count data.
#The Thai Educational Data records information about individual pupils that are clustered within schools. By aggregating the number of pupils who repeated a grade by school, we obtain a new data set where each row represents a school, with information about the proportion of pupils repeating a grade in that school. The MSESC (mean SES score) is also on the school level; therefore, it can be used to predict proportion or count of pupils who repeat a grade in a particular school. See below.

#Tranform Data
ThaiEdu_Prop <- ThaiEdu_New %>%
  group_by(SCHOOLID, MSESC) %>%
  summarise(REPEAT = sum(REPEAT),
            TOTAL = n()) %>%
  ungroup()

head(ThaiEdu_Prop)
#In this new data set, REPEAT refers to the number of pupils who repeated a grade; TOTAL refers to the total number of students in a particular school.
#Explore data
ThaiEdu_Prop %>%
  ggplot(aes(x = exp(MSESC)/(1+exp(MSESC)), y = REPEAT/TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm")

#We can see that the proportion of students who repeated a grade is negatively related to the inverse-logit of MSESC. Note that we model the variable MSESC as its inverse-logit because in a binomial regression model, we assume a linear relationship between the inverse-logit of the linear predictor and the outcome (i.e. proportion of events), not linearity between the predictor itself and the outcome.
#Fit a Binomial Logistic Regression Model
#To fit a binomial logistic regression model, we also use the glm function. The only difference is in the specification of the outcome variable in the formula. We need to specify both the number of target events (REPEAT) and the number of non-events (TOTAL-REPEAT) and wrap them in cbind().
Model_Prop <- glm(formula = cbind(REPEAT, TOTAL-REPEAT) ~ MSESC,
                  family = binomial(logit),
                  data = ThaiEdu_Prop)

summary(Model_Prop)
# Interpretation
#The parameter interpretation in a binomial regression model is the same as that in a binary logistic regression model. We know from the model summary above that the mean SES score of a school is negatively related to the odds of students repeating a grade in that school. To enhance interpretability, we use the summ() function again to calculate the exponentiated coefficient estimate of MSESC. Since MSESC is a continous variable, we can standardise the exponentiated MSESC estimate (by multiplying the original estimate with the SD of the variable, and then then exponentiating the resulting number).
#Note that to use the summ() function for a binomial regression model, we need to make the outcome variable explicit objects:
REPEAT <- pull(filter(ThaiEdu_Prop, !is.na(MSESC)), REPEAT)
TOTAL <- pull(filter(ThaiEdu_Prop, !is.na(MSESC)), TOTAL)
summ(Model_Prop, exp = T, scale = T)

#pull(.data, var = -1, name = NULL, ...)
#Extract a single column
#pull() is similar to $. It's mostly useful because it looks a little nicer in pipes, it also works with remote data frames, and it can optionally name the output.
# exp = TRUE → aplica la función exponencial a los coeficientes.
#Esto se usa sobre todo en modelos logísticos (glm(family = binomial)), porque los coeficientes están en log-odds, y al hacer exp() los conviertes en odds ratios.
#scale = TRUE → estandariza las variables predictoras antes de mostrar los resultados.
#Esto permite comparar la magnitud de los efectos entre variables medidas en distintas escalas.

#We can see that with a SD increase in MSESC, the odds of students repeating a grade is lowered by 1 – 85% = 15%.
#recordando que: The definition of odds is: P(event occurring)/P(event not occurring)
#We can visualise the effect of MSESC.
plot(allEffects(Model_Prop))
#he plot above shows the expected influence of MSESC on the probability of a pupil repeating a grade. Holding everything else constant, as MSESC increases, the probability of a pupil repeating a grade lowers (from 0.19 to 0.10). The blue shaded areas indicate the 95% confidence intervals of the predicted values at each value of MSESC.

#Multilevel Binary Logistic Regression
#The binary logistic regression model introduced earlier is limited to modelling the effects of pupil-level predictors; the binomial logistic regression is limited to modelling the effects of school-level predictors. To incorporate both pupil-level and school-level predictors, we can use multilevel models, specifically, multilevel binary logistic regression. If you are unfamiliar with multilevel models, you can use Multilevel analysis: Techniques and applications for reference and this tutorial for a good introduction to multilevel models with the lme4 package in R.
#In addition to the motivation above, there are more reasons to use multilevel models. For instance, as the data are clustered within schools, it is likely that pupils from the same school are more similar to each other than those from other schools. Because of this, in one school, the probability of a pupil repeating a grade may be high, while in another school, low. Furthermore, even the relationship between the outcome (i.e. repeating a grade) and the predictor variabales (e.g. gender, preschool education, SES) may be different across schools. Also note that there are missing values in the MSESC variable. Using multilevel models can appropriately address these issues.
#See the following plot as an example. The plot shows the proportions of students repeating a grade across schools. We can see vast differences across schools. Therefore, we may need multilevel models.
ThaiEdu_New %>%
  group_by(SCHOOLID) %>%
  summarise(PROP = sum(REPEAT)/n()) %>%
  plot()

 #We can also plot the relationship between SEX and REPEAT by SCHOOLID, to see whether the relationship between gender and repeating a grade differs by school.
 ThaiEdu_New %>%
  mutate(SEX = if_else(SEX == "boy", 1, 0)) %>%
  ggplot(aes(x = SEX, y = REPEAT, color = as.factor(SCHOOLID))) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))
  #In the plot above, different colors represent different schools. We can see that the relationship between SEX and REPEAT appears to be quite different across schools.

#We can make the same plot for PPED and REPEAT.
ThaiEdu_New %>%
  mutate(PPED = if_else(PPED == "yes", 1, 0)) %>%
  ggplot(aes(x = PPED, y = REPEAT, color = as.factor(SCHOOLID))) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))

#The relationship between PPED and REPEAT also appears to be quite different across schools. However, we can also see that most of the relationships follow a downward trend, going from 0 (no previous schooling) to 1 (with previous schooling), indicating a negative relationship between PPED and REPEAT.
# Because of the observations above, we can conclude that there is a need for multilevel modelling in the current data, with not only a random intercept (SCHOOLID) but potentially also random slopes of the SEX and PPED

#Center Variables 
#Prior to fitting a multilevel model, it is necessary to center the predictors by using an appropriately chosen centering method (i.e. grand-mean centering or within-cluster centering), because the centering approach matters for the interpretation of the model estimates. Following the advice of Enders and Tofighi (2007), we should use within-cluster centering for the first-level predictors SEX and PPED, and grand-mean centering for the second-level predictor MSESC.
ThaiEdu_Center <- ThaiEdu_New %>%
  mutate(SEX = if_else(SEX == "girl", 0, 1),
         PPED = if_else(PPED == "yes", 1, 0)) %>%
  group_by(SCHOOLID) %>%
  mutate(SEX = SEX - mean(SEX),
         PPED = PPED - mean(PPED)) %>%
  ungroup() %>%
  mutate(MSESC = MSESC - mean(MSESC, na.rm = T))
#Intercept Only Model
#To specify a multilevel model, we use the glmer function from the lme4 package. Note that the random effect term should be included in parentheses. In addition, within the parentheses, the random slope term(s) and the cluster terms should be separated by |. Note that we use an additional argument control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)) in the glmer function to specify a higher number of maximum iterations than default (10000). This might be necessary because a multilevel model may require a large number of iterations to converge.
#We start by specifying an intercept-only model, in order to assess the impact of the clustering structure of the data.
Model_Multi_Intercept <- glmer(formula = REPEAT ~ 1 + (1|SCHOOLID),
                               family = binomial("logit"),
                               data = ThaiEdu_Center,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

                       summary(Model_Multi_Intercept)
                       #Below we calculate the ICC (intra-class correlation) of the intercept-only model.
                       #performance::icc(Model_Multi_Intercept)
                       #An ICC of 0.33 means that 33% of the variation in the outcome variable can be accounted for by the clustering stucture of the data. This provides evidence that a multilevel model may make a difference to the model estimates, in comparison with a non-multilevel model. Therefore, the use of multilevel models is necessary and warrantied.

#Full Model
#It is good practice to build a multilevel model step by step. However, as this tutorial’s focus is not on muitilevel modelling, we go directly from the intercept-only model to the full-model that we are ultimately interested in. In the full model, we include not only fixed effect terms of SEX, PPED and MSESC and a random intercept term, but also random slope terms for SEX and PPED. Note that we specify family = binomial(link = "logit"), as this model is essentially a binary logistic regression model.
Model_Multi_Full <- glmer(REPEAT ~ SEX + PPED + MSESC + (1 + SEX + PPED|SCHOOLID),
                          family = binomial("logit"),
                          data = ThaiEdu_Center,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
summary(Model_Multi_Full)
#The results (pertaining to the fixed effects) are similar to the results of the previous binary logistic regression and binomial logistic regression models. On the pupil-level, SEX has a significant and positive influence on the odds of a pupil repeating a grade, while PPED has a significant and negative influence. On the school-level, MSESC has a significant and negative effect on the outcome variable. Let’s also look at the variance of the random effect terms.
#Again, we can use the summ() function to retrieve the exponentiated coefficient estimates for easier interpretation.
summ(Model_Multi_Full, exp = T)
#We can also use the allEffects function to visualise the effects of the parameter estimates. Note that because the first-level categorical variables (SEX and PPED) are centered, they are treated as continuous variables in the model and as well in the following effect plots.
plot(allEffects(Model_Multi_Full))

#In addition to the fixed-effect terms, let’s also look at the random effect terms. From the ICC value before, we know that it’s necessary to include a random intercept. However, the necessity of including random slopes for SEX and PPED is less clear. To find this out, we can use the likelihood ratio test and AIC to judge whether the inclusion of the random slope(s) improves model fit.
#let's fit a less-than-full model that leaves out the random slope term of `SEX`
Model_Multi_Full_No_SEX <- glmer(REPEAT ~ SEX + PPED + MSESC + (1 + PPED|SCHOOLID),
                                 family = binomial("logit"),
                                 data = ThaiEdu_Center,
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

#let's fit a less-than-full model that leaves out the random slope term of `PPED`
Model_Multi_Full_No_PPED <- glmer(REPEAT ~ SEX + PPED + MSESC + (1 + SEX|SCHOOLID),
                                  family = binomial("logit"),
                                  data = ThaiEdu_Center,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
#let's fit a less-than-full model that leaves out the random slope terms of both `SEX` and `PPED`
Model_Multi_Full_No_Random_Slope <- glmer(REPEAT ~ SEX + PPED + MSESC + (1|SCHOOLID),
                                      family = binomial("logit"),
                                      data = ThaiEdu_Center,
                                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
#Likelihood ratio test:
#compare the full model with that model that excludes `SEX`
anova(Model_Multi_Full_No_SEX, Model_Multi_Full, test="Chisq")

#compare the full model with that model that excludes `PPED`
anova(Model_Multi_Full_No_PPED, Model_Multi_Full, test="Chisq")

anova(Model_Multi_Full_No_Random_Slope, Model_Multi_Full, test="Chisq")
#From the all insignificant likelihood ratio test results (Pr(>Chisq) > 0.05), we can conclude that there is no significant improvement in model fit by adding any random slope terms.
#AIC:
AIC(logLik(Model_Multi_Full)) #full model
AIC(logLik(Model_Multi_Full_No_SEX)) #model without SEX
AIC(logLik(Model_Multi_Full_No_PPED)) #model without PPED
AIC(logLik(Model_Multi_Full_No_Random_Slope)) #model without random slopes
#From the AIC results, we see that including random slope terms either does not substantially improve AIC (indicated by lower AIC value) or leads to worse AIC (i.e. higher). Therefore, we also conclude there is no need to include the random effect term(s).

#Other Family (Distribution) and Link Functions
#So far, we have introduced binary and binomial logistic regression, both of which come from the binomial family with the logit link. However, there are many more distribution families and link functions that we can use in glm analysis. For instance, to model binary outcomes, we can also use the probit link or the complementary log-log (cloglog) instead of the logit link. To model count data, we can also use Poisson regression, which assumes that the outcome variable comes from a Poisson distribution and uses the logarithm as the link function. For an overview of possible glm models, see the Wikipedia page for GLM.




