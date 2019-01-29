# Prep 2014-2018 data for model

# Load datasets
library(here)
library(readr)

games_all <- data.frame()
for(i in 14:18){
  temp <- read_csv(here(paste0('data/past_seasons/all_games20',i,'.csv')))
  games_all <- rbind(games_all, temp)

  print(paste0('20',i,' done'))  
}

games_all$Total <- games_all$Home_score + games_all$Road_score

torvik_all <- data.frame()
for(i in 14:18){
  temp <- read_csv(here(paste0('data/past_seasons/all_torvik20',i,'.csv')))
  torvik_all <- rbind(torvik_all, temp)
  
  print(paste0('20',i,' done'))  
}



# Convert date to Date format
games_all$Date <- as.Date(games_all$Date)
torvik_all$Date <- as.Date(torvik_all$Date)
torvik_all$Date <- torvik_all$Date + 1

# Remove duplicated torvik day ratings
torvik_all <- torvik_all[!duplicated(torvik_all[c(2,22)]), ]


# Add road team stats
newdf <- left_join(games_all, torvik_all, 
                   by = c('Road' = 'Team', 'Date'))
colnames(newdf)[9:28] <- paste0(colnames(newdf[9:28]), '_road')

# Add home team stats
newdf <- left_join(newdf, torvik_all, by = c('Home' = 'Team', 'Date'))
colnames(newdf)
colnames(newdf)[29:48] <- paste0(colnames(newdf[29:48]), '_home')


# Get tidy data for modeling
model_data <- newdf[complete.cases(newdf), ] 

table(is.na(model_data))

# Save model data
write.csv(model_data, 
          file = here('data/past_seasons/model_data_2014_2018.csv'), 
          row.names = F)

