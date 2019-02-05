# Run model on upcoming games and check season accuracy
library(here)
library(dplyr)

# Update games and torvik data (only use if updating last 2 days)
source(here('src/update_data_master.R'))

fit1 <- readRDS(here('models/log_regression_adje_model.rds'))


# Test model on 2019 season games
season <- read.csv(here('data/season_data2019.csv'))


season$pred <- predict(fit1, newdata = season, 
                       type = 'response') #%>% 
season$pred_Home_w <- round(season$pred,0)
season$pred_correct <- ifelse(season$pred_Home_w == season$Home_w, 1, 0)



# Check accuracy for yesterday, last week, last month
check_accuracy <- function(n_days){
  
x <- filter(season, as.Date(Date) < Sys.Date()) %>% 
  filter(as.Date(Date) >= Sys.Date()-n_days) %>%
  select(pred_correct) %>% 
  table()
names(x) <- c('Incorrect', 'Correct')


print(paste('Last',n_days,'days:', 
            100*round(prop.table(x)[2], 3),
      'percent accuracy'))
x
}

# Check accuracy for 2019 season
check_accuracy(300)

check_accuracy(1)
check_accuracy(7)
check_accuracy(30)
check_accuracy(60)


# Chart of running model accuracy
season1 <- season[complete.cases(season), ] %>% 
  filter(as.Date(Date) < Sys.Date())


season1$games <- 1:nrow(season1)
season1$running_accuracy <- cumsum(season1$pred_correct)/season1$games

library(ggplot2)
ggplot(aes(y = running_accuracy, x = as.Date(Date)),
       data = season1)+
  geom_line(lwd = .8, col = 'red')+
  ylab('Running Model Accuracy')+
  xlab('Date')+
  ggtitle('2019 NCAA basketball predictive model accuracy')




# Predictions for todays games
# Predict winner using logistic regression model
todays_games <- filter(season, as.Date(Date) == Sys.Date())
todays_games$pred <- predict(fit1, 
                                    newdata = todays_games, 
                                    type = 'response') %>% 
  round(., 4)


# Predict margin and total using linear regression models
fit_margin <- readRDS(here('models/lin_regression_adje_model_margin.rds'))

fit_total <- readRDS(here('models/lin_regression_adje_model_total.rds'))


todays_games$pred_margin <- predict(fit_margin, 
                                    newdata = todays_games,
                                    type = 'response') %>% 
  round()

todays_games$pred_total <- predict(fit_total, 
                                    newdata = todays_games,
                                    type = 'response') %>% 
  round()

# Convert margin and total into predicted Home/Road scores
todays_games$pred_Home <- round(todays_games$pred_total/2 - todays_games$pred_margin/2, 0)

todays_games$pred_Road <- round(todays_games$pred_total/2 + todays_games$pred_margin/2, 0)




todays_games <- todays_games[,c(7,1,2,49:55)] %>% 
  mutate(pred_winner = ifelse(pred >= 0.5, 
                              as.character(Home), 
                              as.character(Road)),
         confidence = abs(pred - 0.5) + 0.5) %>% 
  select(c('Date', 'Road', 'pred_Road', 'Home', 'pred_Home',
           'pred_winner', 'confidence', 'pred_margin', 
           'pred_total'))

dir.create(here('results/'))
write.csv(todays_games, file = here('results/todays_games_predictions.csv'), row.names = F)



# Upload as a google sheet to google drive
library(googlesheets)

gs_upload(file = here('results/todays_games_predictions.csv'), overwrite = T)


# Save and upload season stats to google drive
write.csv(season1, 
          file = here('results/2019_season_completed.csv'), 
          row.names = F)
gs_upload(file = here('results/2019_season_completed.csv'), overwrite = T)
