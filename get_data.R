
library(tidyverse)
library(readxl)
library(httr)


GET("https://www.health.govt.nz/system/files/documents/pages/covid-cases-30_mar_2020.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

covid_19_confirmed_cases <- 
  read_excel(tf, sheet = 1, skip = 3) %>% 
  mutate(Confirmed = TRUE)

covid_19_probable_cases <- 
  read_excel(tf, sheet = 2, skip = 3)%>% 
  mutate(Confirmed = FALSE)

covid_19_cases <- 
  covid_19_confirmed_cases %>% 
  rename(ReportDate = `Report Date`) %>%
  bind_rows(covid_19_probable_cases) %>% 
  rename(Date = `ReportDate`,
         Age = `Age Group`) %>%
  mutate(Age = ifelse(is.na(Age), "Missing", Age),
         Sex = ifelse(is.na(Sex), "Unknown", Sex)) %>%
  mutate(Age = factor(Age, 
                      levels = c("Unknown", "<1", "1 to 4",
                                 "5 to 9", "10 to 14", "15 to 19",
                                 "20 to 29", "30 to 39", "40 to 49",
                                 "50 to 59", "60 to 69", "70+" )))


  
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











