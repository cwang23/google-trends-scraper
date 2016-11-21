# Author Clara Wang
# Sources: http://dirk.eddelbuettel.com/blog/2015/11/29/
#          https://cran.r-project.org/web/packages/gtrendsR/index.html
# Program to download data from Google Trends
# January 2016

## --------------------------------------< PREPARE WORKSPACE >-----------------------------------------------

rm(list = ls())

wd <- "WORKING DIRECTORY"
setwd(wd)

# install packages first using install.packages("package name")
library(gtrendsR)
library(dplyr)
library(reshape2)asdf
library(zoo)


## --------------------------------------< SEARCH INFO >--------------------------------------

# login to Google account 
# must be logged in to this account in browser for code to work
GOOGLE_USER = "GOOGLE USERNAME (e.g. google@google.com")
GOOGLE_PASS = "GOOGLE PASSWORD"
account <- gconnect(usr = GOOGLE_USER, psw = GOOGLE_PASS)

# search queries
# each query can include up to 5 search terms (separate using commas)
query1=c("noodle")
query2=c("mmr")
query3=c("noodle", "mmr")


# dates
# these dates and years can be used to download daily data for each year listed
dates <- c("01-01^03-31", "04-01^06-30", "07-01^09-30", "10-01^12-31")  # the ^ is used to separate MM-DD
year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015") 


## --------------------------------------< DOWNLOAD NATIONAL DAILY DATA >--------------------------------------

# search query1
for(i in year){
  account <- gconnect(usr = GOOGLE_USER, psw = GOOGLE_PASS) # included in loop because sometimes ran into error where needed to reconnect to Google account
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query1,
                     geo = "US",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
                     )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query1_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}
 

# search query2
for(i in year){
  account <- gconnect(usr = GOOGLE_USER, psw = GOOGLE_PASS)
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query2,
                     geo = "US",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
      )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query2_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}


# search query3
for(i in year){
  account <- gconnect(usr = GOOGLE_USER, psw = GOOGLE_PASS)
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query3,
                     geo = "US",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
      )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query3_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}



## -------------------------------------< DOWNLOAD CALIFORNIA DAILY DATA >-----------------------------------------

# can download daily data for a different area by changing the "geo" parameter (e.g. can download data for MA by inputting "US-MA")

# search query1
for(i in year){
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query1,
                     geo = "US-CA",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
      )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query1CA_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}


# search query2
for(i in year){
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query2,
                     geo = "US-CA",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
      )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query2CA_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}


# search query3
for(i in year){
  for(j in dates){
    date <- paste(i, j, sep = "-")
    print(date)
    # download daily data, skip to next download if run into error
    tryCatch({
      info = gtrends(query = query3,
                     geo = "US-CA",
                     start_date = as.Date(paste(i, substr(j, 1, 5), sep = "-")),
                     end_date = as.Date(paste(i, substr(j, 7, 11), sep = "-"))
      )
      # subset for daily data and make into data frame
      dataframe <- as.data.frame(info$trend)
      # save data frame as CSV file
      write.csv(dataframe, file = paste0("query3CA_", date, ".csv"))
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")}
    )
  }
}




## ---------------------------------< DOWNLOAD NATIONAL WEEKLY DATA >----------------------------------

# search query1
info = gtrends(query = query1,
               geo = "US",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
               )
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query1weekly.csv")


# search query2
info = gtrends(query = query2,
               geo = "US",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
               )
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query2weekly.csv")


# search query3
info = gtrends(query = query3,
               geo = "US",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
               )
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query3weekly.csv")




## -----------------------------------< DOWNLOAD STATE WEEKLY DATA >-----------------------------------

# search query1
info = gtrends(query = query1,
               geo = "US-CA",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
)
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query1CAweekly.csv")


# search query2
info = gtrends(query = query2,
               geo = "US-CA",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
)
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query2CAweekly.csv")


# search query3
info = gtrends(query = query3,
               geo = "US-CA",
               start_date = as.Date("2006-01-01"),
               end_date = as.Date("2015-12-31")
)
dataframe <- as.data.frame(info$trend)
write.csv(dataframe, file = "query3CAweekly.csv")




## -------------------------------------< ADJUST NATIONAL DATA >-----------------------------------------

# merge query1 files and calculate adjusted values
query1_files <- dir(pattern = "query1_")    # query1 daily data files in wd
query1_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query1_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query1_df <- bind_rows(query1_df, thisFile)  # stack CSVs
}
query1_df[2] <- NULL         # delete unecessary column
query1_df <- query1_df[-1,]  # delete unecessary row

# download weekly data and merge
query1weekly <- read.csv("query1weekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("noodle.US", "X"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly" = noodle.US)
query1_df <- left_join(query1_df, query1weekly, by = "start")

# calculate adjusted values
query1_df$X <- na.locf(query1_df$X)
query1_df$weekly <- na.locf(query1_df$weekly)
query1_df <- query1_df %>%
  group_by(X) %>%
  mutate(weekly_avg = mean(as.numeric(noodle.US))) %>%
  mutate(adjust_fact = as.numeric(weekly)/weekly_avg) %>%
  mutate(adjust_value = as.numeric(noodle.US)*adjust_fact)


# merge query2 files and calculate adjusted values
query2_files <- dir(pattern = "query2_")    # query2 daily data files in wd
query2_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query2_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query2_df <- bind_rows(query2_df, thisFile)  # stack CSVs
}
query2_df[2] <- NULL         # delete unecessary column
query2_df <- query2_df[-1,]  # delete unecessary row

# download weekly data and merge
query2weekly <- read.csv("query2weekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("mmr.US", "X"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly" = mmr.US)
query2_df <- left_join(query2_df, query2weekly, by = "start")

# calculate adjusted values
query2_df$X <- na.locf(query2_df$X)
query2_df$weekly <- na.locf(query2_df$weekly)
query2_df <- query2_df %>%
  group_by(X) %>%
  mutate(weekly_avg = mean(as.numeric(mmr.US))) %>%
  mutate(adjust_fact = as.numeric(weekly)/weekly_avg) %>%
  mutate(adjust_value = as.numeric(mmr.US)*adjust_fact)


# merge query3 files and calculate adjusted values
query3_files <- dir(pattern = "query3_")    # query3 daily data files in wd
query3_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query3_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query3_df <- bind_rows(query3_df, thisFile)  # stack CSVs
}
query3_df[2] <- NULL         # delete unecessary column
query3_df <- query3_df[-1,]  # delete unecessary row

# download weekly data and merge
query3weekly <- read.csv("query3weekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("X", "mmr.US", "noodle.US"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly_noodle" = noodle.US, "weekly_mmr" = mmr.US)
query3_df <- left_join(query3_df, query3weekly, by = "start")

# calculate adjusted values
query3_df$X <- na.locf(query3_df$X)
query3_df$weekly_noodle <- na.locf(query3_df$weekly_noodle)
query3_df$weekly_mmr <- na.locf(query3_df$weekly_mmr)
query3_df <- query3_df %>%
  group_by(X) %>%
  mutate(weekly_noodle_avg = mean(as.numeric(noodle.US))) %>%
  mutate(weekly_mmr_avg = mean(as.numeric(mmr.US))) %>%
  mutate(noodle_adjust_fact = as.numeric(weekly_noodle)/weekly_noodle_avg) %>%
  mutate(mmr_adjust_fact = as.numeric(weekly_mmr)/weekly_mmr_avg) %>%
  mutate(noodle_adjust_value = as.numeric(noodle.US)*noodle_adjust_fact) %>%
  mutate(mmr_adjust_value = as.numeric(mmr.US)*mmr_adjust_fact)




## -------------------------------------< ADJUST STATE DATA >-----------------------------------------

# merge query1 files and calculate adjusted values
query1CA_files <- dir(pattern = "query1CA_")    # query1 daily data files in wd
query1CA_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query1CA_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query1CA_df <- bind_rows(query1CA_df, thisFile)  # stack CSVs
}
query1CA_df[2] <- NULL         # delete unecessary column
query1CA_df <- query1CA_df[-1,]  # delete unecessary row

# download weekly data and merge
query1CAweekly <- read.csv("query1CAweekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("noodle.US.CA", "X"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly" = noodle.US.CA)
query1CA_df <- left_join(query1CA_df, query1CAweekly, by = "start")

# calculate adjusted values
query1CA_df$X <- na.locf(query1CA_df$X)
query1CA_df$weekly <- na.locf(query1CA_df$weekly)
query1CA_df <- query1CA_df %>%
  group_by(X) %>%
  mutate(weekly_avg = mean(as.numeric(noodle.US.CA))) %>%
  mutate(adjust_fact = as.numeric(weekly)/weekly_avg) %>%
  mutate(adjust_value = as.numeric(noodle.US.CA)*adjust_fact)


# merge query2 files and calculate adjusted values
query2CA_files <- dir(pattern = "query2CA_")    # query2 daily data files in wd
query2CA_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query2CA_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query2CA_df <- bind_rows(query2CA_df, thisFile)  # stack CSVs
}
query2CA_df[2] <- NULL         # delete unecessary column
query2CA_df <- query2CA_df[-1,]  # delete unecessary row

# download weekly data and merge
query2CAweekly <- read.csv("query2CAweekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("mmr.US.CA", "X"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly" = mmr.US.CA)
query2CA_df <- left_join(query2CA_df, query2CAweekly, by = "start")

# calculate adjusted values
query2CA_df$X <- na.locf(query2CA_df$X)
query2CA_df$weekly <- na.locf(query2CA_df$weekly)
query2CA_df <- query2CA_df %>%
  group_by(X) %>%
  mutate(weekly_avg = mean(as.numeric(mmr.US.CA))) %>%
  mutate(adjust_fact = as.numeric(weekly)/weekly_avg) %>%
  mutate(adjust_value = as.numeric(mmr.US.CA)*adjust_fact)


# merge query3 files and calculate adjusted values
query3CA_files <- dir(pattern = "query3CA_")    # query3 daily data files in wd
query3CA_df <- data.frame(start = c("none"))  # create data frame for initial merge

for (i in query3CA_files){
  thisFile <- read.csv(i, stringsAsFactors = FALSE, check.names = FALSE) 
  print(i)  # to check that the code is running
  query3CA_df <- bind_rows(query3CA_df, thisFile)  # stack CSVs
}
query3CA_df[2] <- NULL         # delete unecessary column
query3CA_df <- query3CA_df[-1,]  # delete unecessary row

# download weekly data and merge
query3CAweekly <- read.csv("query3CAweekly.csv", stringsAsFactors = FALSE) %>%
  melt(id.vars = c("X", "mmr.US.CA", "noodle.US.CA"), 
       measure.vars = c("start", "end"), 
       value.name = "start",
       variable.name = "null") %>%
  select(X, start, "weekly_noodle" = noodle.US.CA, "weekly_mmr" = mmr.US.CA)
query3CA_df <- left_join(query3CA_df, query3CAweekly, by = "start")

# calculate adjusted values
query3CA_df$X <- na.locf(query3CA_df$X)
query3CA_df$weekly_noodle <- na.locf(query3CA_df$weekly_noodle)
query3CA_df$weekly_mmr <- na.locf(query3CA_df$weekly_mmr)
query3CA_df <- query3CA_df %>%
  group_by(X) %>%
  mutate(weekly_noodle_avg = mean(as.numeric(noodle.US.CA))) %>%
  mutate(weekly_mmr_avg = mean(as.numeric(mmr.US.CA))) %>%
  mutate(noodle_adjust_fact = as.numeric(weekly_noodle)/weekly_noodle_avg) %>%
  mutate(mmr_adjust_fact = as.numeric(weekly_mmr)/weekly_mmr_avg) %>%
  mutate(noodle_adjust_value = as.numeric(noodle.US.CA)*noodle_adjust_fact) %>%
  mutate(mmr_adjust_value = as.numeric(mmr.US.CA)*mmr_adjust_fact)



## --------------------------------< MERGE ALL NATIONAL DATA >--------------------------------------

# clean and merge all query data frames into one master data frame
query1_df <- query1_df %>%
  select("Date" = start,
         "Q1DailySVI noodle" = noodle.US,
         "Q1WeeklySVI noodle" = weekly,
         "Q1CalcWeeklyAvg noodle" = weekly_avg,
         "Q1AdjFact noodle" = adjust_fact,
         "Q1AdjValue noodle" = adjust_value
  )
query1_df <- query1_df[,-1]

query2_df <- query2_df %>%
  select("Date" = start,
         "Q2DailySVI mmr" = mmr.US,
         "Q2WeeklySVI mmr" = weekly,
         "Q2CalcWeeklyAvg mmr" = weekly_avg,
         "Q2AdjFact mmr" = adjust_fact,
         "Q2AdjValue mmr" = adjust_value
  )
query2_df <- query2_df[,-1]

query3_df <- query3_df %>%
  select("Date" = start,
         "Q3DailySVI noodle" = noodle.US,
         "Q3DailySVI mmr" = mmr.US,
         "Q3WeeklySVI noodle" = weekly_noodle,
         "Q3WeeklySVI mmr" = weekly_mmr,
         "Q3CalcWeeklyAvg noodle" = weekly_noodle_avg,
         "Q3CalcWeeklyAvg mmr" = weekly_mmr_avg,
         "Q3AdjFact noodle" = noodle_adjust_fact,
         "Q3AdjFact mmr" = mmr_adjust_fact,
         "Q3AdjValue noodle" = noodle_adjust_value,
         "Q3AdjValue mmr" = mmr_adjust_value
  )
query3_df <- query3_df[,-1]


# merge all queries into one master matrix
masterMatrix <- full_join(query1_df, query2_df, by = "Date")
masterMatrix <- full_join(masterMatrix, query3_df, by = "Date")


write.csv(masterMatrix, "nationalMasterMatrix.csv")


## --------------------------------< MERGE ALL STATE DATA >--------------------------------------

# clean and merge all query data frames into one master data frame
query1CA_df <- query1CA_df %>%
  select("Date" = start,
         "Q1DailySVI noodle" = noodle.US.CA,
         "Q1WeeklySVI noodle" = weekly,
         "Q1CalcWeeklyAvg noodle" = weekly_avg,
         "Q1AdjFact noodle" = adjust_fact,
         "Q1AdjValue noodle" = adjust_value
  )
query1CA_df <- query1CA_df[,-1]

query2CA_df <- query2CA_df %>%
  select("Date" = start,
         "Q2DailySVI mmr" = mmr.US.CA,
         "Q2WeeklySVI mmr" = weekly,
         "Q2CalcWeeklyAvg mmr" = weekly_avg,
         "Q2AdjFact mmr" = adjust_fact,
         "Q2AdjValue mmr" = adjust_value
  )
query2CA_df <- query2CA_df[,-1]

query3CA_df <- query3CA_df %>%
  select("Date" = start,
         "Q3DailySVI noodle" = noodle.US.CA,
         "Q3DailySVI mmr" = mmr.US.CA,
         "Q3WeeklySVI noodle" = weekly_noodle,
         "Q3WeeklySVI mmr" = weekly_mmr,
         "Q3CalcWeeklyAvg noodle" = weekly_noodle_avg,
         "Q3CalcWeeklyAvg mmr" = weekly_mmr_avg,
         "Q3AdjFact noodle" = noodle_adjust_fact,
         "Q3AdjFact mmr" = mmr_adjust_fact,
         "Q3AdjValue noodle" = noodle_adjust_value,
         "Q3AdjValue mmr" = mmr_adjust_value
  )
query3CA_df <- query3CA_df[,-1]


# merge all queries into one master matrix
stateMasterMatrix <- full_join(query1CA_df, query2CA_df, by = "Date")
stateMasterMatrix <- full_join(stateMasterMatrix, query3CA_df, by = "Date")


write.csv(stateMasterMatrix, "stateMasterMatrix.csv")

