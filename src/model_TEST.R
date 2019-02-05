# Make predictive models for CBB games

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

model_data1 <- model_data[,c(pred_cols, predictors)] #%>% 
  filter(Home_score != 'x')


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
set.seed(i)
subset1 <- sample(nrow(df), size = 0.8*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

# Set up model
fit1 <- glm(Home_w ~ ., 
            train[,c(5, 8:ncol(df))], family = 'binomial')

summary(fit1)

test$predicted_Home_w <- predict(fit1, newdata = test, type = 'response') %>% 
  round()


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

# Using 3 different sets of predictors gives almost same results
# The highest accuracy comes from using just home and road AdjE





# Try predicting using a neural network
library(neuralnet)

df <- data_AdjE


model_perf <- data.frame()

#for(i in 1:10){
i <- 1
  set.seed(i)
subset1 <- sample(nrow(df), size = 0.8*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

net_model <- neuralnet(Home_w~AdjE_off_road+AdjE_off_home+
                         AdjE_def_road+AdjE_def_home, 
                       data = train)

pred <- compute(net_model, test[,c(8:11)])
test$predicted_Home_w <- pred$net.result %>% round()


tab <- prop.table(table(test$predicted_Home_w == test$Home_w))
model_perf <- rbind(model_perf, c(i, tab[1], tab[2]))

print(paste(i, 'iterations done'))
#}


# Best model is the AdjE data using logistic regression
# Make and save this model:
df <- data_AdjE
subset1 <- sample(nrow(df), size = 0.8*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

# Set up model
fit1 <- glm(Home_w ~ ., 
            train[,c(5, 8:ncol(df))], family = 'binomial')

summary(fit1)

test$predicted_Home_w <- predict(fit1, newdata = test, 
                                 type = 'response') %>% 
  round()

prop.table(table(test$predicted_Home_w == test$Home_w))

# Save model
dir.create(here('models/'))
saveRDS(fit1, here('models/log_regression_adje_model.rds'))



# Test model on 2019 season games
season <- read.csv(here('data/model_data.csv'))
season <- filter(season, as.Date(Date) > as.Date('2018-11-20'))

head(season)
season$pred <- predict(fit1, newdata = season, 
                                 type = 'response') #%>% 
season$pred_Home_w <- round(season$pred,0)
season$pred_correct <- ifelse(season$pred_Home_w == season$Home_w, 1, 0)

# Check accuracy for 2019 season
table(season$pred_correct)
prop.table(table(season$pred_correct))

