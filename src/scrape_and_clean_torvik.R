
library(rvest)
url <- read_html('http://www.barttorvik.com/?year=2019&sort=&lastx=0&hteam=&conlimit=All&state=All&begin=20181101&end=20190117&top=0&quad=4&venue=All&type=All&mingames=0#')
torvik <- html_table(url)[[1]]

head(torvik)
str(torvik)


# Create name conversions with games data
library(readr)

# **Do this part once and save dictionary file **
# all_games <- read_csv(file = '~/Desktop/data_projects/college_basketball_predictions/data/all_games2019.csv')
# head(all_games)
# 
# # Check number of matching names
# table(unique(all_games$Home) %in% unique(torvik$Team))
# 
# # 52 team names have to be changed to match games data. Let's get that list
# torvik_names <- sort(torvik[,2][!torvik[,2] %in% all_games$Home])
# games_names <- sort(unique(all_games$Home[!all_games$Home %in% torvik[,2]]))
# match_names <- sort(torvik$Team[torvik$Team %in% all_games$Home])
# 
# 
# write_csv(as.data.frame(torvik_names), path = '~/Desktop/data_projects/college_basketball_predictions/data/torvik_names.csv')
# write_csv(as.data.frame(games_names), path = '~/Desktop/data_projects/college_basketball_predictions/data/games_names.csv')
# write_csv(as.data.frame(match_names), path = '~/Desktop/data_projects/college_basketball_predictions/data/match_names.csv')

# Manually match up team names and make a dictionary in excel 
# *******


clean_torvik <- function(torvik){
  
  colnames(torvik) <- c('Rk','Team','Conf','G','Rec','AdjE_off','AdjE_def',
                       'BARTHAG','EFF_FG_off','EFF_FG_def','TO_off','TO_def',
                       'Reb_off','Reb_def','FTrate_off','FTrate_def',
                       '2pt_off','2pt_def','3pt_off','3pt_def','AdjTempo',
                       'WAB')
    
    paste(colnames(torvik), torvik[1,])
torvik <- torvik[2:nrow(torvik), ]

library(dplyr)
torvik <- filter(torvik, Rk != 'Rk')

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
  rename(Team = games_names)



return(torvik)
}

torvik <- clean_torvik(torvik)

torvik$date <- Sys.Date()
head(torvik)


str(torvik)


library(magrittr)


# Scrape and combine torvik ratings from multiple days

test_multiple <- data.frame()

for(month in c('11','12','01')){
  if(month == '11'){
    days <- 10:30
    year <- 2018
  }
  if(month == '12'){
    days <- 1:31
    year <- 2018
  }
  if(month == '01'){
    days <- 1:17
    year <- 2019
  }

for(day in days){
  if(day < 10){
    url <- read_html(paste0('http://www.barttorvik.com/?year=2019&sort=&lastx=0&hteam=&conlimit=All&state=All&begin=20181101&end=',year,month,'0',day,'&top=0&quad=4&venue=All&type=All&mingames=0#'))
  } else{
    url <- read_html(paste0('http://www.barttorvik.com/?year=2019&sort=&lastx=0&hteam=&conlimit=All&state=All&begin=20181101&end=',year,month,day,'&top=0&quad=4&venue=All&type=All&mingames=0#'))
  }
  
  
  torvik <- html_table(url)[[1]] %>% 
    clean_torvik()
  
  torvik$Date <- paste(year, month, day, sep = '-')
  
  test_multiple <- rbind(test_multiple, torvik)
  print(paste0(month,'-' ,day, ' done'))
}
}


# Save torvik data
write_csv(test_multiple, path = '~/Desktop/data_projects/college_basketball_predictions/data/torvik_all_2019.csv')

mich <- filter(test_multiple, Team == 'Villanova')
plot(mich$BARTHAG)

