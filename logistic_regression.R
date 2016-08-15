## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

setwd("C:/Git/logistic_regression")

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


#   Use the NH11 data set that we loaded earlier.

# study the data 
library(dplyr)
library(tidyr)

dim(NH11)
str(NH11$everwrk)
str(NH11$age_p)
str(NH11$r_maritl)

summary(NH11$everwrk)
summary(NH11$age_p)
summary(NH11$r_maritl)

#Summary above indicates that there are 18,949 NAs for everworked out of 33014 which is 57% (more)
#than half of data is missing. With so much missing values , Not sure if the exercise in itself is 
# of any value

everwrk_null <- NH11 %>%  group_by(r_maritl) %>% filter(is.na(everwrk)) %>% 
  select(age_p,r_maritl,everwrk)

nrow(everwrk_null %>% filter(r_maritl == "8 Living with partner"))

# All the status levels have significant NAs for ever worked. 

lapply(NH11$r_maritl, levels)

everwrk.mod <- glm(everwrk~age_p+r_maritl,data=NH11, family="binomial")

coef(summary(everwrk.mod))

##   since there are some neative values and also not very helpful
##   transform the coefficients to make them easier to interpret

everwrk.mod.tab <- coef(summary(everwrk.mod))
everwrk.mod.tab[, "Estimate"] <- exp(coef(everwrk.mod))
everwrk.mod.tab

# ALong with Transformed co-effecients, we can used predict() function to predict .For Eg what 
# is the probability of person age 20 and person age with Unknown marital status to have ever worked
#

# # Create a dataset with predictors set at desired levels
predeverwrk <- with(NH11,
                expand.grid(age_p = c(20, 40),
                r_maritl = "5 Divorced"))

# predict everwrk at those levels
cbind(predeverwrk, predict(everwrk.mod, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predeverwrk))

##   This tells us that a 20 year old person with Divorced marital status has a 14% probability of
##   having everworked, while and 40 year old person with Divorced marital status
##   has a 8% probability of having ever worked.

predeverwrk <- with(NH11,
                    expand.grid(age_p = c(30, 40),
                                r_maritl = "5 Divorced"))

cbind(predeverwrk, predict(everwrk.mod, type = "response",
                           se.fit = TRUE, interval="confidence",
                           newdata = predeverwrk))

# Interestingly when we check people with ages 30 and 40 with status divorced, probability of having
# ever worked is reduced to 10% and .8% .


plot(allEffects(everwrk.mod))

## based on the plots, it can be understood that probabilty of working is highest at age of 20 
## independent of marital state, and decreases with increase in age. 

# Marital status however has some unusual observation like - "5 Divorced" 
# have least probability for having everworked followed by "8 Living with Partner" 
# "4 Widowed" have the highest probability to have ever worked
# Status 0 and 3 have zero records and hence have been left out. Coeff table however indicates
# that status 3 has the least probability (lesser than 5 divorced) of having worked although it has 
# not listed status 0