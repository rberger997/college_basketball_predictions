# Need to update the college basketball games and torvik ratings with new results

# This function will scrape and update season data given last n number of days to update

update_games_and_torvik <- function(season, last_n_days){


  
# Load the function to scrape game info from cbb reference
  library(here)
  source(here('src/FUNCTION_scrape_torvik1.R'))
  source(here('src/FUNCTION_scrape_cbb_games.R'))  
  
# Update info from last n number of days

# Load games csv file
all_games <- read.csv(here('data/all_games2019.csv'))
all_torvik <- read.csv(here('data/all_torvik2019.csv'))

all_games$Date <- as.Date(all_games$Date)
all_torvik$Date <- as.Date(all_torvik$Date)

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
temp_games <- scrape_games(day, month, year)
temp_torvik <- scrape_torvik(season, day, month)

# Remove the empty games from the all_games object, add in new games, re-sort
library(dplyr)
library(magrittr)
all_games <- dplyr::filter(all_games, !Date == dates[i]) %>% 
  rbind(., temp_games) %>% 
  arrange(Date)

all_torvik <- rbind(all_torvik, temp_torvik)
all_torvik <- all_torvik[!duplicated(all_torvik), ]

# Progress update
print(paste0(month,'-',day,'-',year,' updated'))

}
# Save the updated files
write.csv(all_games, file = here('data/all_games2019.csv'), row.names = F)
write.csv(all_torvik, file = here('data/all_torvik2019.csv'), row.names = F)


print('File saved')
}

