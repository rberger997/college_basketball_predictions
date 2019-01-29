
# Make function to scrape games info from college basketball reference
library(rvest)
scrape_games <- function(day, month, year){

  
  url <- read_html(paste0('https://www.sports-reference.com/cbb/boxscores/index.cgi?month=',month,'&day=',day,'&year=',year))
games <- html_table(url)

# Games come out as a list. Need to convert to tidy dataframe
length(games)

new_df <- data.frame()

for(i in 1:length(games)){

game1 <- data.frame(Road = games[[i]][1,1], 
           Home = games[[i]][2,1],
           Road_score = games[[i]][1,2],
           Home_score = games[[i]][2,2])

game1$Home_w <- ifelse(game1$Home_score > game1$Road_score, 1, 0)

game1$margin <- game1$Road_score - game1$Home_score

# Remove AP rank from team names
game1$Road <- gsub(pattern = '\\s\\([0-9]{,2}\\)$', 
                   replacement = '',
                   x = game1$Road)

game1$Home <- gsub(pattern = '\\s\\([0-9]{,2}\\)$', 
                   replacement = '',
                   x = game1$Home)

game1$Date <- paste(year, month, day, sep = '-')

new_df <- rbind(new_df, game1)


}

return(new_df)
}




as.numeric(Sys.Date())
as.POSIXct(Sys.Date())

date1 <- as.character(Sys.Date())
class(date1)
substr(date1, 6,7)
