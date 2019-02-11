
# Create SQLite database for data
library(here)
newdf <- read.csv(here('data/season_data2019.csv'))

library(dbplyr)
cbb_db <- src_sqlite(here('data/cbb_2019_data.sqlite'), create = TRUE)

# Add data to SQLite database
newdf$Date <- as.character(newdf$Date)
dbWriteTable(cbb_db1, 'data_2019_season', newdf, overwrite = TRUE)

cbb_db1 <- DBI::dbConnect(RSQLite::SQLite(), here('data/cbb_2019_data.sqlite'))

src_dbi(cbb_db1)
x <- tbl(cbb_db1, sql('SELECT *
                     FROM data_2019_season
                     WHERE Home_score > 100'))

x <- dbReadTable(cbb_db1, 'data_2019_season')

x1 <- dbSendQuery(cbb_db1, 
                  'SELECT Home, Home_score, Road, Road_score, Date
 FROM data_2019_season
 WHERE Home_score > 50 
AND Home_score != "x"
AND Home == "Michigan"
AND Date > "2019-01-01"
 ORDER BY Home_score DESC') %>% 
  dbFetch()

str(newdf)
