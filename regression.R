rm(list=ls())
library(tidyverse)
library(lme4)
library(MuMIn)

ofc_df<-read.csv('/Users/christinamaher/Desktop/ieeg_data/ofc_df.csv', header = TRUE) # data frame containing one HG power value per trial per OFC electrode and associated model-based/model-agnostic regressors of interest (expected value, reward prediction error, reward, and relevant dimension)

### START WITH GROUP-LEVEL (i.e., all electrodes in a given region) regression using linear mixed effects model. This allows us to determine the behavioral variable that is most predictive of HG activity in a given region of interest.
# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet - syntax for mixed effects models
# current syntax computes a random intercept and slope for each electrode and does not assume correlation between them.
df$r=as.factor(ofc$r) # reward - 0 (loss) vs 1 (gain)
df$rd=as.factor(lpfc$rd) # relevant dimension - 0 (color) vs 1 (shape)

model1 <- lmer(power ~  (1|elec_id) + ev + (0 + ev |elec_id), REML=FALSE, data=df) # power ~ expected value (model-based regressor)
model1_bic <- BIC(model1)
model1_r2 <- r.squaredGLMM(model1)

model2 <- lmer(power ~  (1|elec_id) + r + (0 + r |elec_id), REML=FALSE, data=df) # power ~ reward (model-agnostic regressor)
model2_bic <- BIC(model2)
model2_r2 <- r.squaredGLMM(model2)

model3 <- lmer(power ~  (1|elec_id) + rd + (0 + rd |elec_id), REML=FALSE, data=df) # power ~ relevant dimension (model-agnostic regressor)
model3_bic <- BIC(model3)
model3_r2 <- r.squaredGLMM(model3)

model4 <- lmer(power ~  (1|elec_id) + rpe + (0 + rpe |elec_id), REML=FALSE, data=df) # power ~ reward prediction error (model-based regressor)
model4_bic <- BIC(model4)
model4_r2 <- r.squaredGLMM(model4)

best_model <- which.min(c(model1_bic,model2_bic,model3_bic,model4_bic)) # lowest BIC = best fitting model
cat("The best fitting model is:", best_model)
cat("The R2 of the best fitting model is:", c(model1_r2,model2_r2,model3_r2,model4_r2)[best_model])

