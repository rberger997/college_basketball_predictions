
# Function for scraping Torvik data for a single date

scrape_torvik <- function(season, day, month){

  if(as.numeric(month) > 6){
    year <- as.numeric(season) - 1
  }else{
    year <- as.numeric(season)
  }
  

library(rvest)
url <- read_html(paste0('http://www.barttorvik.com/?year=',season,'&sort=&lastx=0&hteam=&conlimit=All&state=All&begin=',as.numeric(season) - 1,'1101&end=',year,month,day,'&top=0&quad=4&venue=All&type=All&mingames=0#'))
torvik <- html_table(url)[[1]]

  
# Clean the data
  colnames(torvik) <- c('Rk','Team','Conf','G','Rec','AdjE_off','AdjE_def',
                        'BARTHAG','EFF_FG_off','EFF_FG_def','TO_off','TO_def',
                        'Reb_off','Reb_def','FTrate_off','FTrate_def',
                        'X2pt_off','X2pt_def','X3pt_off','X3pt_def','AdjTempo',
                        'WAB')
  
  torvik <- torvik[2:nrow(torvik), ]
  
  library(dplyr)
  torvik <- filter(torvik, Rk != 'Rk')
  
  # Remove tournament info from team name (seed, round made)
  torvik$Team <- gsub(pattern = '\\s[[:digit:]].*', 
                      replacement = '',
                      x = torvik$Team)
  
  # non numeric cols 2,3,5
  torvik[,c(1,4,6:ncol(torvik))] <- lapply(torvik[,c(1,4,6:ncol(torvik))], as.numeric)
  
  # Round off percentages to one decimal
  torvik[,c(1,4,6,7,9:ncol(torvik))] <- lapply(torvik[,c(1,4,6,7,9:ncol(torvik))], round, digits=1)
  
  torvik[,8] <- round(torvik[,8], 4)
  
  
  
  
  
  
  # Issue with Torvik formatting: ranks are included in all entries. They are added as negligible decimals to values that have decimal places (50.5 rank 250 becomes 50.5250). For integer values they skew values (50 rank 250 becomes 50250). Need to create a function that will find and fix these values.
  
  fix_values <- function(df_col){
    for(i in 1:length(df_col)) {
      if(df_col[i] > 1e5) {
        df[i, colnum] <- df_col[i]/1e4
      }
      if(df_col[i] > 1e4) {
        df_col[i] <- df_col[i]/1e3
      }
      if(df_col[i] > 1e3) {
        df_col[i] <- df_col[i]/1e2
      }
      if(df_col[i] > 1e2) {
        df_col[i] <- df_col[i]/1e1
      }
      df_col <- round(df_col, 1)
    }
    return(df_col)
  }
  
  # Fix percentage values using function
  torvik[,9:21] <- lapply(torvik[,9:21], fix_values)
  
  # Drop wins above bubble, add date column
  torvik <- torvik[,-22]
  
  # Convert abbreviation of St. to State (matches better with games data)
  torvik$Team <- gsub(pattern = 'St\\.', replacement = 'State', x = torvik$Team)
  
  # Convert team names to match with games data
  name_dict <- readr::read_csv('~/Desktop/data_projects/college_basketball_predictions/data/cbb_name_dictionary.csv')
  
  torvik <- dplyr::left_join(torvik, name_dict, by = c('Team' = 'torvik_names')) %>% 
    select(-Team) %>% 
    select(c(1,21,2:20)) %>% 
    rename(Team = games_names) %>% 
    mutate(Date = as.Date(paste(year, month, day, sep = '-')))
  
  print(paste0(month,'-',day,'-',year, ' scraped from Torvik'))
  
return(torvik)
}

