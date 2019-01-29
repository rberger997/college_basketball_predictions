# Update data
library(here)
library(magrittr)
source(here('src/FUNCTION_update_games_and_torvik.R'))

read.csv(here('data/all_torvik2019.csv')) %>% tail(., 5)

update_games_and_torvik(season = 2019, last_n_days = 2)



# Merge data together 
library(dplyr)
library(magrittr)
games <- read.csv(here('data/all_games2019.csv')) %>% 
  mutate(Total = Home_score + Road_score)
torvik <- read.csv(here('data/all_torvik2019.csv'))

# Fill NAs in scores with x in games
games[is.na(games)] <- 'x'

# Convert date to Date format
games$Date <- as.Date(games$Date)
torvik$Date <- as.Date(torvik$Date)
torvik$Date <- torvik$Date + 1

# Remove duplicated torvik day ratings
torvik <- torvik[!duplicated(torvik[c(2,22)]), ]


# Add road team stats
newdf <- left_join(games, torvik, by = c('Road' = 'Team', 'Date'))
colnames(newdf)[9:28] <- paste0(colnames(newdf[9:28]), '_road')

# Add home team stats
newdf <- left_join(newdf, torvik, by = c('Home' = 'Team', 'Date'))
colnames(newdf)
colnames(newdf)[29:48] <- paste0(colnames(newdf[29:48]), '_home')


# Get tidy data for modeling
model_data <- newdf[complete.cases(newdf), ] 

table(is.na(model_data))

# Save model data
write.csv(model_data, file = here('data/model_data.csv'), 
          row.names = F)


write.csv(newdf, file = here('data/season_data2019.csv'), 
          row.names = F)
