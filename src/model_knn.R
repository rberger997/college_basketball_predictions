# Make predictive models for CBB games
# Use k nearest neighbors model to predict if the home team will win (binary classification)

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


# Split data into three subsets of predictor variables
data_barthag <- model_data1[,c(5,12,13)]
data_AdjE <- model_data1[,c(5,8:11)]
data_raw <- model_data1[,c(5, 14:39)]


df <- data_AdjE

# Normalize data before building model
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm <- lapply(df[,-1], normalize) %>% 
  as.data.frame()

# Convert Home_w into a factor
df$Home_w <- factor(df$Home_w, levels = c(0,1), labels = c('Home_loss', 'Home_win'))
head(df)

# Split into train/test data
subset1 <- sample(nrow(df_norm), size = 0.8*nrow(df))
train <- df_norm[subset1, ]
test <- df_norm[-subset1, ]

# Set up labels for train/test sets
train_labels <- df[subset1, 1]
test_labels <- df[-subset1, 1]

# Define k for model
k <- round(sqrt(nrow(df)), 0)

library(class)
cbb_knn <- knn(train = train,
               test = test,
               cl = train_labels,
               k = k)

library(caret)
confusionMatrix(test_labels,
                cbb_knn)

# 69% accuracy with the model. Try to improve by varying k
library(e1071)

k <- seq(10, 200, by = 10)

str(train)
knntuning <- tune.knn(x = train,
                      y = train_labels,
                      k = k)

knntuning
summary(knntuning)

# Really not much difference in accuracy by tuning value of k

# Build new model with optimal value of k (170)
cbb_knn <- knn(train = train,
               test = test,
               cl = train_labels,
               k = 160)


# Check accuracy using a confusion matrix
confusionMatrix(test_labels,
                cbb_knn)





# Test model on 2019 season games
season <- read.csv(here('data/season_data2019.csv'))

season$Home_w_factor <- factor(season$Home_w, 
                               levels = c(0,1), 
                               labels = c('Home_loss', 'Home_win'))

# Select AdjE columns
season1 <- select(season, Home_w_factor, AdjE_off_road, AdjE_off_home,
                  AdjE_def_road, AdjE_def_home) %>%
  .[complete.cases(.), ]

# Normalize AdjE columns
season1[,2:5] <- lapply(season1[,2:5], normalize) %>% 
  as.data.frame()


# Use all previous games as training set
train <- df_norm
train_labels <- df[, 1]



# Check accuracy for 2019 season
season1$pred_Home_w <- knn(train = train,
                          test = season1[,2:5],
                          cl = train_labels,
                          k = 180)

prop.table(table(season1$pred_Home_w == season1$Home_w_factor))

# Only 65% accuracy on 2019 season using kNN model


