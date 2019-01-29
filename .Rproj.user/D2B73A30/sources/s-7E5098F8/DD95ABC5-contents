# Run model on upcoming games and check season accuracy

library(here)
library(dplyr)

fit1 <- readRDS(here('models/log_regression_adje_model.rds'))


# Test model on 2019 season games
season <- read.csv(here('data/season_data2019.csv'))


season$pred <- predict(fit1, newdata = season, 
                       type = 'response') #%>% 
season$pred_Home_w <- round(season$pred,0)
season$pred_correct <- ifelse(season$pred_Home_w == season$Home_w, 1, 0)

# Check accuracy for 2019 season
table(season$pred_correct)
prop.table(table(season$pred_correct))


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


check_accuracy(1)
check_accuracy(7)
check_accuracy(30)
check_accuracy(60)

# Predict todays games
todays_games <- filter(season, as.Date(Date) == Sys.Date())
todays_games$pred <- predict(fit1, 
                                    newdata = todays_games, 
                                    type = 'response') %>% 
  round(., 4)

todays_games <- todays_games[,c(7,1,2,49)] %>% 
  mutate(pred_winner = ifelse(pred >= 0.5, 
                              as.character(Home), 
                              as.character(Road)))




# Chart of running model accuracy
season1 <- season[complete.cases(season), ]
season1$running_accuracy <- 0
season1$games <- 0
for(i in 1:nrow(season1)){
  x <- season1[1:i, ]
  
  season1[i, 52] <- sum(x[,51])/nrow(x)
  season1[i, 53] <- i
}

library(ggplot2)
ggplot(aes(y = running_accuracy, x = as.Date(Date)),
       data = season1)+
  geom_line(lwd = .8, col = 'red')+
  ylab('Running Model Accuracy')+
  xlab('Date')+
  ggtitle('2019 NCAA basketball predictive model accuracy')
