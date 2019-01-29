# Scrape cbb games from previous seasons
scrape_season_games <- function(season){


library(here)
source(here('src/FUNCTION_scrape_cbb_games.R'))

cbb18 <- seq(as.Date(paste0(season-1,'-11-01')), 
             as.Date(paste0(season,'-04-10')), by = 'days')


games_full <- data.frame()

for(i in seq_along(cbb18)){
  tryCatch({
  date1 <- cbb18[i]
  
  year <- substr(as.character(date1), 1,4)
  month <- substr(as.character(date1), 6,7)
  day <- substr(as.character(date1), 9,10)
 
  temp <- scrape_games(day = day, month = month, year = year)
  
  games_full <- rbind(games_full, temp)
  
  print(paste(date1, 'done'))
  }, error=function(e){print('No games')})
}

return(games_full)
}

# Create folder for seasons data
dir.create(here('data/past_seasons/'))

# Scrape last 5 seasons
library(magrittr)
library(readr)

games2014 <- scrape_season_games(season = 2014)
write_csv(games2014, path = here('data/past_seasons/all_games2014.csv'))

games2015 <- scrape_season_games(season = 2015) 
write_csv(games2015, 
          path = here('data/past_seasons/all_games2015.csv'))

games2016 <- scrape_season_games(season = 2016)
write_csv(games2016, 
          path = here('data/past_seasons/all_games2016.csv'))

games2017 <- scrape_season_games(season = 2017) 
write_csv(games2017, 
          path = here('data/past_seasons/all_games2017.csv'))

games2018 <- scrape_season_games(season = 2018) 
write_csv(games2018, 
          path = here('data/past_seasons/all_games2018.csv'))