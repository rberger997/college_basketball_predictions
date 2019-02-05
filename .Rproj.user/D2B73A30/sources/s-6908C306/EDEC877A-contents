

# Try predicting using a neural network
library(neuralnet)
library(here)

# Load model data
model_data1 <- read.csv(here('data/model_data1.csv'))


df <- model_data1

# Train/test split data
subset1 <- sample(nrow(df), size = 0.8*nrow(df))
train <- df[subset1, ]
test <- df[-subset1, ]

# Make ANN model
net_model <- neuralnet(Home_w~AdjE_off_road+AdjE_off_home+
                         AdjE_def_road+AdjE_def_home, 
                       data = train)

# Make predictions on test set
pred <- compute(net_model, test[,c(8:11)])
test$predicted_Home_w <- pred$net.result %>% round()

# Check accuracy
prop.table(table(test$predicted_Home_w == test$Home_w))


# The neural network model is only predicting about 63% accuracy. This is lower than the logistic regression model (72%). Try to add some hidden layers to increase performance (but will slow the training time down a lot)


# Make ANN model
net_model <- neuralnet(Home_w~AdjE_off_road+AdjE_off_home+
                         AdjE_def_road+AdjE_def_home, 
                       data = train,
                       hidden = 3)

# Make predictions on test set
pred <- compute(net_model, test[,c(8:11)])
test$predicted_Home_w <- pred$net.result %>% round()

# Check accuracy
prop.table(table(test$predicted_Home_w == test$Home_w))
