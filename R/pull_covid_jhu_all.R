library(httr)
library(readr)
library(dplyr)
library(tidyr)

request <- GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
stop_for_status(request)
files <- unlist(lapply(content(request)$tree, "[", "path"))
drs <- grepl(files, pattern = "csse_covid_19_data/csse_covid_19_daily_reports/")
daily_reports <- files[drs]

x <- length(daily_reports) -1

for(i in seq(from = 2, x, 1)){
  url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
                daily_reports[i])
  daily <- read.csv(url)
  
  if(i >= 41 & i < 62){
    daily <- daily %>% select(-Latitude, -Longitude)
  }
  
  #if(i < 13 | (i >= 54 & i < 63)){
  if(i < 62){
    colnames(daily) <- c("Province_State", "Country_Region", "Last_Update",
                         "Confirmed", "Deaths", "Recovered")  
  }
  
  if(i < 62){
    daily <- daily %>% 
      replace_na(list(Confirmed = 0, Deaths = 0, Recovered = 0)) %>% 
      mutate(Active = Confirmed - Deaths - Recovered)
  }
  
  if(i >= 62){
    daily <- daily[-1]
    daily <- daily %>% select(-Admin2, -Lat, -Long_,
                              -Combined_Key) %>% 
    replace_na(list(Confirmed = 0, Deaths = 0, Recovered = 0, Active = 0))
      
  }
  
  daily$date <- as.Date(substr(daily_reports[i], 48, 57),
                        format = "%m-%d-%Y", tz = "UTC") 
  daily <- daily %>% select(-Last_Update)
  
  if(exists("all_reports")){
    all_reports <- rbind(all_reports, daily)
  } else {
    all_reports <- daily
  }
}
