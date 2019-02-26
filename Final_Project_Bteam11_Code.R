library(haven)
library(ggplot2)
library(tidyverse)
library(MASS)
library(mgcv)
library(visreg)
library(car)
library(ROCR)
library(DescTools)
library(Hmisc)

construction <- data.frame(read_sas("C:/Users/Jerry/Documents/MSA18/Logistic_Regression/data/construction.sas7bdat", NULL))

# Data type correction --------------------------------------------------------------


construction$Sector <- as.factor(construction$Sector)

# make column factor and give friendly name
construction$region <- as.factor(construction$Region_of_Country)
construction$Region_of_Country <- NULL

# Make the competitor binary variables factors, and whether we won bid
for (i in 6:16) {
  construction[, i] <- as.factor(construction[,i])
}


for (i in c(seq(1,3), 5, 17, 18)) {
  construction[, i] <- as.numeric(construction[,i])
}



# Training / Test Split ---------------------------------------------------

# Get the proportion of events where we won a bid
win_proportion <- mean(as.numeric(construction$Win_Bid)) - 1

# Size of training set
train_proportion = 0.7

# Train test split
set.seed(1)
train_index <- sample(seq(1, nrow(construction)), size = train_proportion * nrow(construction))

train <- construction[train_index,]
test <- construction[-train_index,]

mean(as.numeric(test$Win_Bid)) - 1 # Checking to make sure the class balance is similar in the test set


# EDA ---------------------------------------------------------------------


# use training data for EDA to limit bias
hist(train$Estimated_Cost__Millions_)
# Slightly skewed right, not too badly though

hist(train$Estimated_Years_to_Complete)
# Skew right

hist(train$Bid_Price__Millions_)
# Skewed farther right

hist(train$Number_of_Competitor_Bids)
# Fairly normal

hist(train$Winning_Bid_Price__Millions_)
# Simiar to Bid_Price__Millions... Have to be careful about collinearity here

hist(train$Cost_After_Engineering_Estimate_)
# Highly right skew, definite outlier


# Competitor Bids #### TODO Check if no one in data won
hist(as.numeric(train$Competitor_A))
hist(as.numeric(train$Competitor_B))
hist(as.numeric(train$Competitor_C))
hist(as.numeric(train$Competitor_D))
hist(as.numeric(train$Competitor_E))
hist(as.numeric(train$Competitor_F))
hist(as.numeric(train$Competitor_G))
hist(as.numeric(train$Competitor_H))
hist(as.numeric(train$Competitor_I))
hist(as.numeric(train$Competitor_J))


# How many jobs for each Sector
ggplot(train, aes(x=Sector)) +
  geom_bar()


# How many jobs in each region
ggplot(train, aes(x=region)) +
  geom_bar()

# How many jobs for each sector in each region
ggplot(train, aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(region))


# How many bid wins per secotor
ggplot(train, aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))


# How many bid wins per secotor when competitor A is involved
train %>%
  filter(Competitor_A == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_B == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_C == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_D == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_E == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_F == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_G == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_H == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_I == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))
train %>%
  filter(Competitor_J == "1") %>%
  ggplot(aes(x=Sector)) + 
  geom_bar() + facet_wrap(vars(Win_Bid))

# How many wins in each region compared to number of bids on each project
ggplot(train, aes(x=Win_Bid)) +
  geom_bar(aes(x=Win_Bid, alpha = 0.5, color="red")) +
  geom_bar(aes(x=Number_of_Competitor_Bids)) +
  facet_wrap(vars(region))

# How many bids in each region per project
ggplot(train, aes(x=Number_of_Competitor_Bids)) +
  geom_bar() + facet_wrap(vars(region))


# How many number of bids when we won
ggplot(train, aes(x=Number_of_Competitor_Bids)) +
  geom_bar() + facet_wrap(~Win_Bid)


# Check Variable Relationships -----------------------------------------------------


# Check for highly correlated columns
numeric_columns <- c("Estimated_Cost__Millions_",
                     "Estimated_Years_to_Complete",
                     "Bid_Price__Millions_",
                     "Number_of_Competitor_Bids",
                     "Winning_Bid_Price__Millions_",
                     "Cost_After_Engineering_Estimate_"
                     )

cor(train[numeric_columns])

# The Esmiated Cost/Bid_Price/Winning_Bid Price/Cost_After are highly correlated,
# let's extract more information out of them with some feature engineering

# Calculate estimated profit of each job - our Bid proposal compared to the estimated cost
train$est_profit <- train$Bid_Price__Millions_ - train$Estimated_Cost__Millions_

# Remove highly correlated Variables
train$Estimated_Cost__Millions_ <- NULL
train$Winning_Bid_Price__Millions_ <- NULL
train$Cost_After_Engineering_Estimate_ <- NULL

# Scale the estimated profit with the bid revenue - how much are we getting out
# of this project if we win the bid?
train$profit_rate <- train$est_profit / train$Bid_Price__Millions_

# Number of major competitors also bidding
train$num_major_competitors <- apply(apply(train[, 5:14], FUN = as.numeric,MARGIN = 2), FUN = sum, MARGIN = 1)

# From examining our past wins, we identified that we consistently fail to win bids
# in sectors 4, 5, and 6
train$sectors_we_lose <- train$Sector %in% c("4","5", "6")

# These are the sectors we are particularly good at winning
train$sectors_we_rock <- train$Sector %in% c("1","2", "10")

# Split the Sector multinomial variable into binary variable
train$sector_1 <- train$Sector == "1"
train$sector_2 <- train$Sector == "2"
train$sector_4 <- train$Sector == "4"
train$sector_5 <- train$Sector == "5"
train$sector_6 <- train$Sector == "6"
train$sector_8 <- train$Sector == "8"
train$sector_9 <- train$Sector == "9"
train$sector_10 <- train$Sector == "10"



# Modeling ---------------------------------------------------------------

# AIC: 242.2 
# BIC: 305.2
simple_model <- glm(Win_Bid ~ profit_rate + num_major_competitors + Sector + region,
                    data=train, family=binomial(link="logit"))
summary(simple_model)
BIC(simple_model)

# AIC: 162.36
# BIC: 193.9
complex_model <- glm(Win_Bid ~ profit_rate + Number_of_Competitor_Bids + region +
                       sectors_we_lose,
                     data=train, family=binomial(link="logit"))
summary(complex_model)
BIC(complex_model)

# AIC: 131.1
# BIC: 186.2
more_complex_model <- glm(Win_Bid ~ profit_rate + Number_of_Competitor_Bids +
                            region + sector_4 + sector_5 + Competitor_F +
                            Competitor_J + Competitor_H+sector_6 + sector_9,
                          data=train, family=binomial(link="logit"))
summary(more_complex_model)
BIC(more_complex_model)

final_model <- glm(Win_Bid ~ profit_rate +Number_of_Competitor_Bids + region + sector_4 + Competitor_H+ sector_5+ Competitor_J + sector_6 + Competitor_F + sector_9, data=train, family=binomial(link="logit"))
summary(final_model)

# Partial Residuals -------------------------------------------------------


# Complex model
visreg(complex_model, "profit_rate", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for pforfit_rate",
       x = "profit_rate", y = "log(odds)")

visreg(complex_model, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for num_major_competitors",
       x = "num_major_competitors", y = "log(odds)")


# More Complex model
visreg(more_complex_model, "profit_rate", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for pforfit_rate",
       x = "profit_rate", y = "log(odds)")

visreg(more_complex_model, "Number_of_Competitor_Bids", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for num_major_competitors",
       x = "num_major_competitors", y = "log(odds)")


# Diagnostics -------------------------------------------------------------

# There are a few Influential Observations
plot(more_complex_model, 4)


# Plot a calibration curve for the more complex model
obs.phat <- data.frame(y = more_complex_model$y, phat = fitted(more_complex_model))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()


# Plot a discrimination plot - it appears we can accurately predict which jobs we are
# not going to win, the ones we will win are a bit tougher to predict apparently
df_phat <- data.frame(y = complex_model$y,
                      phat = fitted(complex_model))

ggplot(df_phat, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")


# Fitstat function provided by Matt
fitstat <- function(obj, new_x = NULL, new_y = NULL){
  # this function computes a bunch of fitstats.
  #
  # inputs:
  # 1. obj: a data frame or a model object from glm() or gam()
  #         the data frame must have a vector of responses "y" AND a vector of
  #         either probabilities "p" or linear predictor "lp"
  #         (linear predictors are obtained with predict(..., type = "link"))
  # 2. new_x: specify new dataset to get predictions for new obs. if NULL,
  #           results will be computed using original obj data
  # 3. new_y: use new responses. if NULL, results will be computed using 
  #           original obj data
  #
  # output: a list of two elements
  #   1. fitstats = coefficient of discrimination, raw/max/scaled brier scores,
  #      c-statistic, Somers' D, stdev of c/Somers, Nagelkerke R^2
  #   2. est = a data frame with y (new_y), p (predicted probabilities for
  #      new_y), and lp (predicted linear predictor for new_y). these will be
  #      same as those in obj if no new data is given
  
  # check structure
  if(is.null(obj$y)){
    stop("obj must be a model object or a data frame with arguments y and either
         p or lp")
  }
  
  # if no new data given, use old response
  if(is.null(new_y)){
    new_y <- obj$y
  }
  
  # get predicted probabilities and linear predictors for data
  if(any(class(obj) == "glm")){
    # predict from model object
    p_hat <- predict(obj, newdata = new_x, type = "response")
    lp <- predict(obj, newdata = new_x, type = "link")
  } else if(is.null(obj$lp)){
    # predict from data frame
    p_hat <- obj$p
    lp <- qlogis(p_hat)
  } else {
    lp <- obj$lp
    p_hat <- plogis(lp)
  }
  
  # remove missing values
  new_y <- new_y[!is.na(p_hat)]
  p_hat <- p_hat[!is.na(p_hat)]
  lp <- lp[!is.na(p_hat)]
  
  ### compute coefficient of discrimination ###
  # D = 0.5(r2res + r2mod), where r2res = 1 - (brier/brier_max), and
  # r2mod = sum(((p_hat - p_obs)^2))/sum(((new_y - p_obs)^2))
  p1 <- p_hat[new_y == 1]
  p0 <- p_hat[new_y == 0]
  coef_discrim <- mean(p1) - mean(p0)
  
  ### compute [scaled] brier score ###
  # raw brier score
  brier_score <- mean((new_y - p_hat)^2)
  
  # max brier score is just the observed proportion
  # brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  brier_max <- mean((new_y - mean(new_y))^2)
  
  # scaled brier score
  # this is basically the %improvement over null/intercept-only model
  brier_scaled <- 1 - (brier_score/brier_max)
  
  ### compute c-statistic/Somers' Dxy ###
  rank_stats <- as.numeric(Hmisc::rcorr.cens(p_hat, new_y))
  somers <- rank_stats[2] # Somers' D
  c_stat <- rank_stats[1] # c-statistic
  rankSD <- rank_stats[3] # standard deviation for c & somers'
  
  ### compute nagelkerke r2 ###
  # like scaled brier score, indicates %improvement over null model
  p_obs <- mean(new_y) # mean/observed proportion of data
  mod_dev <- -2*sum(new_y*lp - log(1 + exp(lp))) # model deviance
  nullmod_int <- qlogis(p_obs) # intercept for null model
  null_dev <- -2*sum(new_y*nullmod_int - log(1 + exp(nullmod_int))) # null dev
  LRstat <- null_dev - mod_dev # likelihood ratio statistic
  n_obs <- length(new_y) # sample size
  R2nagelkerke <- (1 - exp(-LRstat/n_obs))/(1 - exp(-null_dev/n_obs))
  
  fitstats <- data.frame(coef_discrim = coef_discrim,
                         brier_score = brier_score,
                         brier_max = brier_max,
                         brier_scaled = brier_scaled,
                         somers = somers,
                         c_stat = c_stat,
                         rankSD = rankSD,
                         R2nagelkerke = R2nagelkerke)
  
  predictions <- data.frame(y = new_y,
                            p = p_hat,
                            lp = lp)
  
  res <- list("fitstats" = fitstats,
              "est" = predictions)
  return(res)
  }

# Get diagnostic statistics for our model, it appears that the more_complex_model has
# Superior statistics
fitstat(complex_model)$fitstats
fitstat(more_complex_model)$fitstats


# Model Assessment with Simulation ----------------------------------------

library(mgcv)
fit.glm <- glm(Win_Bid ~ profit_rate + Number_of_Competitor_Bids +
                 region + sector_4 + sector_5 + Competitor_F +
                 Competitor_J + Competitor_H+sector_6 + sector_9,
               data=train, family=binomial(link="logit"))

fit.gam <- gam(Win_Bid ~ profit_rate + Number_of_Competitor_Bids +
                 region + sector_4 + sector_5 + Competitor_F +
                 Competitor_J + Competitor_H+sector_6 + sector_9,
               data = train, family = binomial, method = "REML")

# Observed difference 
d_obs <- fit.glm$deviance - fit.gam$deviance


# Generate simulated data
sim_models <- function(glm_model, gam_model){
  # function to simulate difference in deviance between two models
  
  # inputs:
  # glm_model = fitted glm() model
  # gam_model = fitted mgcv::gam() model
  
  # outputs:
  # d_sim = the difference in deviance between the two models on simulated data
  
  # check that the first model is actually glm()
  if(any(class(glm_model) == "gam"))
    stop("Error: glm_model must be fit by glm()")
  
  n_obs <- nrow(glm_model$data) # sample size
  y_name <- all.vars(glm_model$formula)[1] # get name of response
  y_index <- match(y_name, colnames(glm_model$data)) # get index of y
  
  phat <- predict(glm_model, type = "response") # predicted probabilities
  sim_data <- glm_model$data
  # generate new response
  sim_data[,y_index] <- rbinom(n = n_obs, size = 1, prob = phat)
  
  # fit glm to fake data and get deviance
  sim_glm_dev <- glm(glm_model$formula, family = binomial(link = "logit"),
                     data = sim_data)$deviance
  # fit gam to fake data and get deviance
  sim_gam_dev <- gam(gam_model$formula, family = binomial(link = "logit"), 
                     data = sim_data, method = "REML")$deviance
  # take the difference in deviance
  d_sim <- sim_glm_dev - sim_gam_dev
  return(d_sim)
}

# Simulate
set.seed(9418)
d_sim <- replicate(200, sim_models(fit.glm, fit.gam)) # do this 200 times
paste("p-value = ", mean(d_sim >= d_obs))

# plot results
hist(d_sim, breaks = 40, main = "distribution of D_sim", xlab = "D_sim")
abline(v = d_obs, col = "red")




# Validate model ----------------------------------------------------------


#  Apply the same transformations to the test set that we did to the train set
test$est_profit <- test$Bid_Price__Millions_ - test$Estimated_Cost__Millions_
test$profit_rate <- test$est_profit / test$Bid_Price__Millions_
test$sector_4 <- test$Sector == "4"
test$sector_5 <- test$Sector == "5"
test$sector_6 <- test$Sector == "6"
test$sector_9 <- test$Sector == "9"


# Get diagnostic statistics for the test data
fitstat(final_model, test, as.numeric(as.factor(test$Win_Bid)) - 1)$fitstats

# ROC curve --------------------------------------------------------
pred <- prediction(fitted(final_model), factor(final_model$y))

# then in performance, "measure" is the y-axis, and "x.measure" is the x-axis
# for a roc curve, we want tpr vs. fpr. "sens" and "spec" also work
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# then we can plot
plot(perf, colorize = TRUE)

# add 45-degree line (random guessing)
abline(a = 0, b = 1, lty = 2)
title("ROC curve")
# AUC
auc <- performance(pred, measure = "auc")@y.values
# classification table-----------------------------------
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table$youdenJ <- with(classif_table, tpr + tnr- 1)

# find row with max
classif_table[which.max(classif_table$youdenJ),]

# tpr
tpr = as.numeric(classif_table[which.max(classif_table$youdenJ),][2])

# tnr
tnr = as.numeric(classif_table[which.max(classif_table$youdenJ),][3])


test$Win_Bid <- as.numeric(test$Win_Bid) - 1
# false negative
length(test$Win_Bid[test$Win_Bid == 1])*(1 - tpr)

# false positive
length(test$Win_Bid[test$Win_Bid == 0])*(1 - tnr)





