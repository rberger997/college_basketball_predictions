# Scrape college basketball games for entire 2018-2019 season using scrape_games function

source('~/Desktop/data_projects/college_basketball_predictions/src/FUNCTION_scrape_cbb_games.R')

all_games <- data.frame()

for(i in c(11,12,1,2,3)){
  year <- 2018
  if(i == 11){
    days <- 6:30
  }
  if(i == 12){
    days <- c(1:25, 27:31)
  }
  if(i == 1){
    days <- 1:31
    year <- 2019
  }
  if(i == 2){
    days <- 1:28
    year <- 2019
  }
  if(i == 3){
    days <- 1:10
    year <- 2019
  }
  
  for(j in days){
    test1 <- scrape_games(day = j, month = i, year = year)
    all_games <- rbind(all_games, test1)
    
    print(paste0(i,'-', j, ' done'))
  }
}

head(all_games)

all_games$Date <- as.Date(all_games$Date)

# Fill NAs with x
all_games[is.na(all_games)] <- 'x'

# Save games as csv file
write.csv(all_games, file = '~/Desktop/data_projects/college_basketball_predictions/data/all_games2019.csv', row.names = F)
