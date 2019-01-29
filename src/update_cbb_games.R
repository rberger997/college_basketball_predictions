# Need to update the college basketball games with new results

update_game_results <- function(last_n_days){

# Load the function to scrape game info from cbb reference
source('~/Desktop/data_projects/college_basketball_predictions/src/FUNCTION_scrape_cbb_games.R')

# Update info from last n number of days

# Load games csv file
all_games <- read.csv('~/Desktop/data_projects/college_basketball_predictions/data/all_games2019.csv')

all_games$Date <- as.Date(all_games$Date)


n <- last_n_days

# Get dates needed for scraping
dates <- seq(Sys.Date()-n, by = 'day', length.out = n)
new_games <- data.frame()

# Iterate through dates
for(i in 1:n){
year <- substr(as.character(dates[i]), 1,4)
month <- substr(as.character(dates[i]), 6,7)
day <- substr(as.character(dates[i]), 9,10)

# Scrape games from day i
temp <- scrape_games(day, month, year)

# Remove the empty games from the all_games object, add in new games, re-sort
library(dplyr)
library(magrittr)
all_games <- dplyr::filter(all_games, !Date == dates[i]) %>% 
  rbind(., temp) %>% 
  arrange(Date)

# Progress update
print(paste0(month,'-',day,'-',year,' updated'))

}
# Save the updated file
write.csv(all_games, file = '~/Desktop/data_projects/college_basketball_predictions/data/all_games2019.csv', row.names = F)

print('File saved')
}


update_game_results(last_n_days = 4)

