
library(tidyverse)
library(rvest)
library(rdrop2)
library(reshape2)

# read it back with readRDS
token <- readRDS("droptoken.rds")


# Updated the database ----------------------------------------------------

daily_counts <- 
  drop_read_csv("work/covid19nz/data/days.csv", stringsAsFactors = FALSE,
                dtoken = token) %>% 
  mutate(Date = as.Date(date, "%d/%m/%Y"))

current_Cases_summary <- 
  read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases")

date_updated <- 
  current_Cases_summary %>% 
  html_nodes(".georgia-italic") %>% 
  html_text(TRUE) 

updated_date <- 
  as.Date(str_split(date_updated, "\\, |\\.")[[1]][2], format = "%d %B %y")


last_date <- 
daily_counts %>% 
  pull(Date) %>% 
  last()


daily_counts_num <- 
  daily_counts %>% 
  select(confirmed : established)


current_Cases_summary_tabs <- 
  current_Cases_summary %>% 
  html_table() %>% 
  "[["(1) %>% 
  rename(
    "x" = "",
    "total_to_date" = "Total to date",
    "new_in_last_24_hours" = "New in last 24 hours")  %>% 
  mutate(
    total_to_date = 
           as.numeric(str_replace(total_to_date, ",", "")))


Transmission_type <- 
  current_Cases_summary %>% 
  html_table() %>% 
  "[["(4) %>% 
  rename(
    "Transmission_type" = "Transmission type",
    "percent_of_cases" = "% of cases")  %>% 
  mutate(percent_of_cases = percent_of_cases %>%
           str_replace("%", "") %>% 
           as.numeric() %>% 
           "/"(100))

if(last_date != updated_date) {
  
  temp_updated <- 
    current_Cases_summary_tabs %>% 
   melt() %>% 
    pull(value) %>% 
    "["(c(7,1,8,2, 9, 3, 11, 5, 12, 6))
  
  daily_counts <- 
    daily_counts_num %>% 
    rbind(
      c(temp_updated,
        round(temp_updated[6] * Transmission_type$percent_of_cases)[c(1,2,4,3)], 
        temp_updated[6])) %>% 
    mutate(Date = c(daily_counts$Date, updated_date)) %>% 
    mutate(date = format(Date,  "%d/%m/%Y"))

  write.csv(daily_counts, file = "days.csv", row.names = FALSE)
  drop_upload("days.csv", path = "work/covid19nz/data/",
              dtoken = token)
    
  file.remove("days.csv")
  }



current_Cases_detail <- 
  read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details")


covid_19_confirmed_cases <- 
  current_Cases_detail %>% 
  html_table() %>% 
  "[["(1)%>% 
  rename(Date = `Date of report`,
         Age = `Age group`) %>% 
  mutate(Confirmed = TRUE,
         Age = str_replace(Age, "\\s+to\\s+", "-")) 
                                  
covid_19_probable_cases <- 
  current_Cases_detail %>% 
  html_table() %>% 
  "[["(2) %>% 
  rename(Date = `Date of report`,
         Age = `Age group`) %>% 
  mutate(Confirmed = FALSE,
         Age = str_replace(Age, "\\s+to\\s+", "-"))

covid_19_cases <- 
  covid_19_confirmed_cases %>% 
  bind_rows(covid_19_probable_cases) %>% 
  mutate(Age = ifelse(Age =="" | is.na(Age), "Unknown", Age),
         Sex = ifelse(Sex =="", "Unknown", Sex)) %>%
  mutate(
    Date = as.Date(Date,  "%d/%m/%Y"), 
    Age = factor(Age, levels = c("Unknown", "<1", "1-4",
                                 "5-9", "10-14", "15-19",
                                 "20-29", "30-39", "40-49",
                                 "50-59", "60-69", "70+" ))) 

# daily_counts <-
#   read_csv("https://raw.githubusercontent.com/nzherald/nz-covid19-data/master/data/days.csv") %>%
#   mutate(Date = as.Date(date))

# daily_counts <- 
#   read_csv("data/days.csv") %>% 
#   mutate(Date = as.Date(date))


global_cases <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

global_deaths <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
global_recovered <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# library(leaflet)
# library(rgdal)
# library(RColorBrewer)
# library(rgeos)

# nzDHB <- readOGR("data", 'district-health-board-2015') %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# nzDHB <- subset(nzDHB,
#                !as.character(nzDHB@data$DHB2015_Na)  %in%
#                  "Area outside District Health Board")
# 
# nzDHB_simp <- gSimplify(nzDHB, tol = 0.001, topologyPreserve = FALSE)
# 
# 
# names(nzDHB@data)[2] <- "DHB"
# 
# nzDHB@data$DHB <- as.character(nzDHB@data$DHB)
# 
# nzDHB_simp <- SpatialPolygonsDataFrame(nzDHB_simp,
#                                        data = nzDHB@data)
# 
# 
# nzDHB_simp %>%
#   leaflet() %>%
#   addTiles() %>%
#   setView(lng=171, lat= -40.90056 , zoom = 6) %>%
#   addPolygons( popup = nz_dhb@data$DHB2015_Na)

# saveRDS(nzDHB_simp, "data/nzDHB_simp.rds")

# nzDHB_simp <- readRDS("data/nzDHB_simp.rds")
# 
# nzDHB_simp %>%
#   leaflet() %>%
#   addTiles() %>%
#   setView(lng=171, lat= -40.90056 , zoom = 6) %>%
#   addPolygons( popup = nzDHB_simp@data$DHB2015_Na) %>% 
#   addMiniMap(centerFixed = TRUE)











