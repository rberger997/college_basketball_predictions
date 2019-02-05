# Make predictive models for CBB games
# Use linear regression model to predict final margin of victory

# Load model data
library(here)
library(magrittr)
model_data <- read.csv(here('data/past_seasons/model_data_2014_2018.csv'))


str(model_data)
colnames(model_data)

# Columns we want to predict
pred_cols <- c('Road','Home','Road_score','Home_score',
               'Home_w','margin', 'Total')
library(Hmisc)
predictors <- Cs(AdjE_off_road, AdjE_off_home,
                 AdjE_def_road, AdjE_def_home,
                 BARTHAG_road, BARTHAG_home,
                 EFF_FG_off_road, EFF_FG_off_home,
                 EFF_FG_def_road, EFF_FG_def_home,
                 TO_off_road, TO_off_home,
                 TO_def_road, TO_def_home,
                 Reb_off_road, Reb_off_home,
                 Reb_def_road, Reb_def_home,
                 FTrate_off_road, FTrate_off_home,
                 FTrate_def_road, FTrate_def_home,
                 X2pt_off_road, X2pt_off_home,
                 X2pt_def_road, X2pt_def_home,
                 X3pt_off_road, X3pt_off_home,
                 X3pt_def_road, X3pt_def_home,
                 AdjTempo_road, AdjTempo_home )

model_data1 <- model_data[,c(pred_cols, predictors)]


# Check for multicollinearity
x <- model_data[,13:28]

library(GGally)
#ggpairs(x)


# There is correlation between the aggregated statistics AdjEfficiency and BARTHAG but very little for the raw stats. Test model on both raw and aggregated stats

data_barthag <- model_data1[,c(1:7,12,13)]
data_AdjE <- model_data1[,1:11]
data_raw <- model_data1[,c(1:7, 14:39)]



model_accuracy <- data.frame()
model_sd <- data.frame()

library(magrittr)
# Logistic regression model on Home_w
for(df in list(data_raw, data_AdjE, data_barthag)){
  
  model_perf <- data.frame()
  
  for(i in 1:10){
    
    subset1 <- sample(nrow(df), size = 0.8*nrow(df))
    train <- df[subset1, ]
    test <- df[-subset1, ]
    
    # Set up model
    fit1 <- lm(margin ~ ., 
                train[,c(6, 8:ncol(df))])
    
    summary(fit1)
    
    test$predicted_margin <- predict(fit1, newdata = test) %>% 
      round()
    test$predicted_Home_w <- ifelse(test$predicted_margin <= 0, 1,0)
    
    
    tab <- prop.table(table(test$predicted_Home_w == test$Home_w))
    model_perf <- rbind(model_perf, c(i, tab[1], tab[2]))
  }
  
  
  model_accuracy <- rbind(model_accuracy,
                          mean(model_perf[,3]))
  model_sd <- rbind(model_sd,
                    sd(model_perf[,3]))
  
}
model_accuracy <- cbind(model_accuracy, model_sd, 
                        c('raw', 'AdjE', 'Barthag'))
colnames(model_accuracy) <- c('Accuracy','StDev','Data_subset')

model_accuracy


# No real difference in data subset chosen. Use Adjusted efficiency to match with logistic regression model




# Make and save linear regression model for margin prediction
df <- data_AdjE
subset1 <- sample(nrow(df), size = 1*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

# Set up model
fit2 <- lm(margin ~ ., 
           train[,c(6, 8:ncol(df))])

summary(fit2)

# Save model
saveRDS(fit2, file = here('models/lin_regression_adje_model_margin.rds'))




# Make and save model for predicting total score of games
df <- data_AdjE
subset1 <- sample(nrow(df), size = .8*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

# Set up model
fit3 <- lm(Total ~ ., 
           train[,c(7:ncol(df))])

summary(fit3)


# Test model 
test$pred_total <- predict(fit3, newdata = test) %>% 
  round()

# check error in predictions
test$pred_total_error <- abs(test$pred_total - test$Total)
mean(test$pred_total_error)
mean(test$pred_total_error)/mean(test$Total)

# Prediction is typically within 15 points of actual. Absolute error of ~11%

hist(test$pred_total_error, breaks = 20)


# Save model
saveRDS(fit3, file = here('models/lin_regression_adje_model_total.rds'))


