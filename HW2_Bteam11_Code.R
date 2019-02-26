#setwd("~/IAA/Logistic/HW")
library(haven)
library(tidyverse)
library(MASS)
library(mgcv)
library(visreg)
library(car)
library(ROCR)
library(DescTools)
library(Hmisc)

# Read data
df <- haven::read_sas("C:/Users/Jerry/Documents/MSA18/Logistic_Regression/data/insurance_t.sas7bdat")

# Helper function, counts unique values in the column
count_uniques <- function(df1, column) {
  nrow(unique(df1[column]))
}

# Convert columns with more than 10 values to continuous, otherwise make them numeric
for (name in names(df)){
  ifelse (count_uniques(df, name) > 10,
          df[[name]] <- as.numeric(df[[name]]),
          df[[name]] <- as.factor(df[[name]])
  )
}

# The bank branch is an exception to this rule
# Convert into factor
df$BRANCH <- as.factor(df$BRANCH)

# Cleaning
library(dplyr)

for (column in names(df)) {
  # Remove columns with more than 1000 missing values
  if( sum(is.na(df[[column]])) > 1000){
    df[[column]] <- NULL
  }
  
  # For each column, check whether the 75th percentile is 0. If so, remove it
  else if( class(df[[column]]) != "factor"){
    if (quantile(df[[column]], 0.75, na.rm=T) == 0) {
      df[[column]] <- NULL  # Remove column
    }
  }
}

# Impute missing values with the mean of the column
# ACCTAGE/CRSCORE
#df$ACCTAGE[is.na(df$ACCTAGE)] <- mean(df$ACCTAGE, na.rm=T)
#df$CRSCORE[is.na(df$CRSCORE)] <- mean(df$CRSCORE, na.rm=T)

# Identify the numeric columns, and factor columns, make a temp df for each
num_cols <- unlist(lapply(df, is.numeric))
numeric_df = df[,num_cols]

factor_cols <- unlist(lapply(df, is.factor))
factor_df = df[, factor_cols]

# Correlation matrix of numeric variables
cor(numeric_df)

# TELLER, DEP, and CHECKS are highly correlated, and CHECKS has highest variation, so drop the other two
df$TELLER <- NULL
df$DEP <- NULL

# Run logistic regression
log_model <- glm(INS ~ ., data=df, family=binomial(link="logit"))
summary(log_model)

# In order to pare down model, remove the worst predictor pairwise above 0.3 p-value (We know this isn't a great way to do it)
df$MMCRED <- NULL
df$CASHBK <- NULL
df$CRSCORE <- NULL
df$RES <- NULL
df$INAREA <- NULL
df$NSF <- NULL
df$SDB <- NULL

# The final model
log_model <- glm(INS ~ ., data=df, family=binomial(link="logit"))
summary(log_model)

# Compare two individuals and their odds ratio
# The first individual is a "Risky" inidivudal, i.e., they don't have a Money Market,
# CD account, IRA account, or a Mortgage. The Second individual is a risk adverse person.
newdata <- data.frame(ACCTAGE=c(mean(df$ACCTAGE), mean(df$ACCTAGE)),
                      DDA = c('1' , '1'),
                      DDABAL = c(mean(df$DDABAL), mean(df$DDABAL)),
                      DEPAMT = c(mean(df$DEPAMT), mean(df$DEPAMT)),
                      CHECKS = c(mean(df$CHECKS), mean(df$CHECKS)),
                      DIRDEP = c('0', '0'),
                      SAV = c('1', '1'),
                      SAVBAL = c(mean(df$SAVBAL), mean(df$SAVBAL)),
                      ATM = c('1', '1'),
                      ATMAMT = c(mean(df$ATMAMT), mean(df$ATMAMT)),
                      CD = c('0', '1'),  # Individual
                      IRA = c('0', '1'),
                      LOC = c('1', '1'),
                      ILS = c('0', '0'),
                      MM = c('0', '1'),
                      MTG = c('1', '0'),  # 1st ind has mortgage
                      MOVED = c('0', '0')
)

# See the two subjects' difference in odds
exp(diff(
  predict(log_model,
          newdata = newdata)
))

summary(log_model)

# Transform Vars ----------------------------------------------------------

# take out more unsignificant variables
df_transformed <- df
df_transformed$LOC <- NULL
df_transformed$MOVED <- NULL
df_transformed$DEPAMT <- NULL
df_transformed$ATM <- NULL
df_transformed$ATMAMT <- NULL

# Log transform our continuous variables that are very right skewed
# ACCTAGE shift up by 1 and take log
df_transformed$ACCTAGE <- log(df$ACCTAGE + 1)

# DDABAL Shift up by 400 and take log
df_transformed$DDABAL <- log(df$DDABAL + 400)

# SAVBAL shift up by 1 and take log
df_transformed$SAVBAL <- log(df$SAVBAL + 1)

# create a new variable save propensity, which is SAVBAL / (DDABAL + SAVBAL + 400)
df_transformed$sav_prop <- (df$SAVBAL) / (df$SAVBAL + df$DDABAL + 400)

# Fit new model -----------------------------------------------------------
# Current AIC: 8381
transformed_model <- glm(INS ~ ., data=df_transformed, family=binomial(link="logit"))
summary(transformed_model)

# Model with Interactions ----------------------------------------------------
# Current AIC: 8352
interaction_model <- glm(INS ~ . + MM*SAV + SAVBAL*MM,  data=df_transformed, family=binomial(link="logit"))
summary(interaction_model)

# check influential points ----------------------------------------------
# check overall influential points
plot(interaction_model, 4)
# 3821, 4108, 7488

# check influential points for each variable
for (i in colnames(df_transformed)) {
  dfbetasPlots(interaction_model, terms = i, id.n = 5)
}

dfbetasPlots(interaction_model, terms = "MM*SAV", id.n = 5)
dfbetasPlots(interaction_model, terms = "SAVBAL*MM", id.n = 5)

# ACCTAGE: 1980, 5632, 6223
# DDA: 766, 2987, 6964, 4848, 6373
# DDABAL: 140, 1722, 7382
# CHECKS: 171, 721, 2302, 2721, 3554
# DIRDEP: 171, 2302
# SAV: 1321, 2987, 3395, 5720, 6964
# SAVBAL: 1321, 2987, 3395, 5720, 6964

df_int[3821,]

df_int <- data.frame(interaction_model$data)
int_mean <- sapply(df_int, mean, na.rm=T)

interaction_model$coefficients
int_mean

# calibration curve ---------------------------------------------------
obs.phat <- data.frame(y = interaction_model$y, phat = fitted(interaction_model))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "Calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# Brier score function---------------------------------
brier_score <- function(obj, new_x = NULL, new_y = NULL){
  # computes [scaled] brier score
  #
  # inputs:
  # 1. obj: either a model from glm() or a data frame.
  #         the data frame must have a vector responses "y" and a vector of
  #         either probabilities "p" or linear predictor "lp"
  #         (NOTE: for now only )
  # 2. new_x: specify new dataset to get predicted probabilities for new obs.
  #             if NULL, the estimated probabilities from original obs will
  #             be used.
  # 3. new_y: use new responses. if NULL, original ones will be used.
  #
  # output:
  #   brier score, max brier score, scaled brier score
  
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  
  p_obs <- mean(y, na.rm = TRUE)
  
  p <- predict(obj, newdata = new_x, type = "response")
  
  # brier score
  brier_score <- mean((y - p)^2, na.rm = TRUE)
  
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  
  res <- data.frame(brier_score = brier_score,
                    brier_max = brier_max,
                    brier_scaled = brier_scaled)
  res
}

brier_score(interaction_model)

# discrimination slope = mean(p1) - mean(p0) -------------------
D <- mean(fitted(interaction_model)[interaction_model$y == 1]) - mean(fitted(interaction_model)[interaction_model$y == 0])

# create data frame of outcome and predicted probabilities
df_phat <- data.frame(y = interaction_model$y,
                      phat = fitted(interaction_model))

ggplot(df_phat, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

# c-statistic and Somers' D -----------------------------------
# predicted prob goes first, outcome second
rcorr.cens(fitted(interaction_model), interaction_model$y)[-c(5, 6, 9)] # ignoring output i don't need

# ROC curves ------------------------------------------------
# the predicted probabilities go first, the actual outcomes (as a factor) second
pred <- prediction(fitted(interaction_model), factor(interaction_model$y))

# then in performance, "measure" is the y-axis, and "x.measure" is the x-axis
# for a roc curve, we want tpr vs. fpr. "sens" and "spec" also work
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# then we can plot
plot(perf, colorize = TRUE)

# add 45-degree line (random guessing)
abline(a = 0, b = 1, lty = 2)

# AUC
auc <- performance(pred, measure = "auc")@y.values


# classification table-----------------------------------
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table$youdenJ <- with(classif_table, tpr + tnr - 1)

# find row with max
classif_table[which.max(classif_table$youdenJ),]







# validation data -------------------------------------------------


df_val <- haven::read_sas("C:/Users/Jerry/Documents/MSA18/Logistic_Regression/data/insurance_v.sas7bdat")

# ACCTAGE shift up by 1 and take log
df_val$ACCTAGE <- log(df_val$ACCTAGE + 1)

# DDABAL Shift up by 400 and take log
df_val$DDABAL <- log(df_val$DDABAL + 400)

# SAVBAL shift up by 1 and take log
df_val$SAVBAL <- log(df_val$SAVBAL + 1)

# create a new variable save propensity, which is SAVBAL / (DDABAL + SAVBAL + 400)
df_val$sav_prop <- (df_val$SAVBAL) / (df_val$SAVBAL + df_val$DDABAL + 400)


# Convert columns with more than 10 values to continuous, otherwise make them numeric
for (name in names(df_val)){
  ifelse (count_uniques(df_val, name) > 10,
          df_val[[name]] <- as.numeric(df_val[[name]]),
          df_val[[name]] <- as.factor(df_val[[name]])
  )
}

# The bank branch is an exception to this rule
# Convert into factor
df_val$BRANCH <- as.factor(df_val$BRANCH)

# convert the target into numeric
df_val$INS <- as.numeric(df_val$INS) - 1

pred_val <- predict(interaction_model, df_val)
prob_val <- exp(pred_val) / (1 + exp(pred_val))


prediction_val <- prediction(prob_val, factor(df_val$INS))
perf_val <- performance(prediction_val, measure = "tpr", x.measure = "fpr")



# calibration curve ----------------------------------------------------
obs.phat <- data.frame(y = df_val$INS, phat = prob_val)
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "Calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# brier score---------------------------------------------------
brier_score(interaction_model, df_val, df_val$INS)

# discrimination slope = mean(p1) - mean(p0) -------------------
D <- mean((prob_val)[df_val$INS == 1], na.rm = TRUE) - mean((prob_val)[df_val$INS], na.rm = TRUE)

# create data frame of outcome and predicted probabilities
df_phat <- data.frame(y = df_val$INS,
                      phat = prob_val)

ggplot(df_phat, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

# c-statistic and Somers' D -----------------------------------
# predicted prob goes first, outcome second
rcorr.cens(prob_val, df_val$INS)[-c(5, 6, 9)] # ignoring output i don't need

# then we can plot
plot(perf_val, colorize = TRUE)
title("ROC curve")

# add 45-degree line (random guessing)
abline(a = 0, b = 1, lty = 2)

# AUC
auc <- performance(prediction_val, measure = "auc")@y.values


# classification table-----------------------------------
classif_table <- data.frame(threshold = perf_val@alpha.values[[1]],
                            tpr = perf_val@y.values[[1]],
                            tnr = 1 - perf_val@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table$youdenJ <- with(classif_table, tpr + tnr - 1)

# find row with max
classif_table[which.max(classif_table$youdenJ),]

# tpr
tpr = as.numeric(classif_table[which.max(classif_table$youdenJ),][2])

# tnr
tnr = as.numeric(classif_table[which.max(classif_table$youdenJ),][3])

# false negative
length(df_val$INS[df_val$INS == 1])*(1 - tpr)

# false positive
length(df_val$INS[df_val$INS == 0])*(1 - tnr)

