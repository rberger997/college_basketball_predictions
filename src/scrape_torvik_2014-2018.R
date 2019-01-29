# Scrape Torvik data from previous seasons
scrape_season_torvik <- function(season){
  
  library(here)
  source(here('src/FUNCTION_scrape_torvik.R'))
  
  cbb18 <- seq(as.Date(paste0(season-1,'-11-01')), 
               as.Date(paste0(season,'-04-10')), by = 'days')
  
  
  torvik_full <- data.frame()
  
  for(i in seq_along(cbb18)){
    tryCatch({
      date1 <- cbb18[i]
      
      month <- substr(as.character(date1), 6,7)
      day <- substr(as.character(date1), 9,10)
      
      temp <- scrape_torvik(day = day, 
                            month = month, 
                            season = season)
      
      torvik_full <- rbind(torvik_full, temp)
      
      print(paste(date1, 'done'))
    }, error=function(e){print('No ratings')})
  }
  
  return(torvik_full)
}

# Create folder for seasons data
dir.create(here('data/past_seasons/'))

# Scrape last 5 seasons
library(magrittr)
library(readr)

torvik2014 <- scrape_season_torvik(season = 2014)
write_csv(torvik2014, 
          path = here('data/past_seasons/all_torvik2014.csv'))

torvik2015 <- scrape_season_torvik(season = 2015) 
write_csv(torvik2015, 
          path = here('data/past_seasons/all_torvik2015.csv'))

torvik2016 <- scrape_season_torvik(season = 2016)
write_csv(torvik2016, 
          path = here('data/past_seasons/all_torvik2016.csv'))

torvik2017 <- scrape_season_torvik(season = 2017) 
write_csv(torvik2017, 
          path = here('data/past_seasons/all_torvik2017.csv'))

torvik2018 <- scrape_season_torvik(season = 2018) 
write_csv(torvik2018, 
          path = here('data/past_seasons/all_torvik2018.csv'))

torvik2014 <- read.csv(here('data/past_seasons/all_torvik2014.csv'))
torvik2014$Date <- as.Date(torvik2014$Date)

# Combine together and save
t1 <- torvik2014
t1 <- rbind(t1, torvik2015)
t1 <- rbind(t1, torvik2016)
t1 <- rbind(t1, torvik2017)
t1 <- rbind(t1, torvik2018)

t1 <- t1[complete.cases(t1), ]

table(is.na(t1))
