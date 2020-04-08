

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(DT)
library(highcharter)

source("get_data.R")
nzDHB_simp <- readRDS("data/nzDHB_simp.rds")

date_range <- range(covid_19_cases$Date, na.rm = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 in NZ"),
  dashboardSidebar(
    h4("Ministry of Health website", date_updated[1]),
    # selectInput("DHB", "Select a DHB:",
    #             choices = c(nzDHB_simp$DHB, "New Zealand"),
    #             selected = "New Zealand", width = "200%"),
    # sliderInput("date_range",
    #             "Dates:",
    #             min = date_range[1],
    #             max = date_range[2],
    #             value = date_range[2],
    #             timeFormat="%Y-%m-%d", animate = TRUE),
    # checkboxInput("confirmed", "Click for confirmed cases",
    #               value = FALSE, width = "200%"),
    # checkboxInput("auck", "Click to zoom into Auckland",
    #               value = FALSE, width = "200%"),
    h4("Note:"),
    p(
      "Plots are based from the data collected by Chris Knox of New Zealand Herald data journalism team in ",
      a("https://github.com/nzherald/nz-covid19-data",
        href = "https://github.com/nzherald/nz-covid19-data"),
      " and Ministry of Health current cases website in ",
      a(
        "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details",
        href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details"
      ),
      ", which is under the Ministry of Health's creative commons license:",
      a("https://www.health.govt.nz/about-site/copyright",
        href = "https://www.health.govt.nz/about-site/copyright"),
      ". The gobal data came from Johns Hopkins CSSE in",
      a("https://github.com/CSSEGISandData/COVID-19",
        href = "https://github.com/CSSEGISandData/COVID-19"),
      "."
    ),
    
    box(
      h4("Contact:"),
      h5(a("Kevin Chang", href = "mailto:kevin.ct.chang@gmail.com")),
      p(
        "Source code can be founded in ",
        a("https://github.com/kcha193/covid19nz", href = "https://github.com/kcha193/covid19nz")
      ),
      width = 12,
      background = "black"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(includeScript("google-analytics.js"),
              tags$style(
                HTML(".leaflet-container { background: #ADD8E6; }")
              )),
    
    column(
      width = 7,
      box(
        title = "Daily data from NZ Herald and Johns Hopkins CSSE",
        valueBoxOutput("nz_Total", 4),
        valueBoxOutput("nz_Death", 4),
        valueBoxOutput("nz_Recovered", 4),
        valueBoxOutput("global_Total", 4),
        valueBoxOutput("global_Death", 4),
        valueBoxOutput("global_Recovered", 4),
        width = 12
      ),
      tabBox(
        title = "Daily data from NZ Herald and Johns Hopkins CSSE",
        tabPanel("Total",
                 highchartOutput("line_plot",  height = "480px")),
        tabPanel("Daily",
                 highchartOutput("bar_plot",  height = "480px")),
        tabPanel(
          "Global",
          highchartOutput("line_plot_global",  height = "480px")
        ),
        tabPanel(
          "Transmission",
          highchartOutput("line_plot_cause",  height = "480px")
        ),
        width = 12,
        height = 500
      )
    ),
    
    column(
      width = 5,
      
      tabBox(
        title = "NZ cases of COVID-19",
        tabPanel(
          "Map",
          leafletOutput("map",  height = "730px"),
          
          p(
            "This shapefiles of DHB are based on/includes Statistics New Zealand's data which are licensed by",
            a("Statistics New Zealand", href = "http://www.stats.govt.nz/", shape =
                "rect"),
            "for re-use under the",
            a(
              "Creative Commons Attribution 4.0 International",
              href = "https://creativecommons.org/licenses/by/4.0/",
              shape = "rect"
            ),
            "licence."
          )
        ),
        tabPanel("DHB",
                 highchartOutput("bar_dhb",  height = "730px")),
        tabPanel("Gender",
                 highchartOutput("bar_gender",  height = "730px")),
        tabPanel("Age",
                 highchartOutput("bar_age",  height = "730px")),
        tabPanel("International Travel",
                 highchartOutput("bar_travel",  height = "730px")),
        tabPanel(
          "Last country before return (Top 20)",
          highchartOutput("bar_last_country",  height = "730px")
        ),
        
        
        tabPanel("Data",
                 DTOutput("data_raw",  height = "730px")),
        width = 12,
        height = 750
      )
    )
  )
)




server <- function(input, output, session) {
  
  
  
  covid_19_date <- reactive({
    
    covid_19_final <- 
      covid_19_cases %>% 
       mutate(Date = as.character(Date))
    
    
    # %>% 
    #   filter(Date <= input$date_range) %>% 
    #   mutate(Date = as.character(Date))
    
    # if(input$confirmed){
    #   covid_19_final <- 
    #     covid_19_final %>% 
    #     filter(Confirmed)
    # }
    
    
    return(covid_19_final)
  }) 
  
  
  covid_19_dhb <- 
    reactive({
  
      # if(input$DHB != "New Zealand"){
      #   covid_19_final <- 
      #     covid_19_date()  %>% 
      #     filter(DHB == input$DHB)
      # } else {
      #   covid_19_final <- 
      #     covid_19_date()
      #   
      # }
      
      return( covid_19_date())
    })
  
  
  output$data_raw <- 
    renderDT({
      covid_19_date() %>% 
        select(Date:DHB, Confirmed)
      
    })
  
  
  output$nz_Total <- renderValueBox({
    
    latest <- 
      daily_counts$totalCases %>% 
      last()
    
    yesterday <- 
    daily_counts$totalCases %>% 
      nth(-2)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") cases in NZ"),
      
      color = "red"
    )
    
    
  })
  
  output$nz_Death <- renderValueBox({
    
    
    latest <- 
      daily_counts$totalDeaths %>% 
      last()
    
    yesterday <- 
      daily_counts$totalDeaths %>% 
      nth(-2)
    
    change <- latest - yesterday
    
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") deaths in NZ"),
      color = "blue"
    )

  })
  output$nz_Recovered <- renderValueBox({
    
    latest <- 
      daily_counts$totalRecovered %>% 
      last()
    
    yesterday <- 
      daily_counts$totalRecovered %>% 
      nth(-2)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") recovered NZ"),
      color = "green"
    )

  })
  
  output$global_Total <- renderValueBox({
   
    latest <- 
    global_cases%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") cases globally"),
      
      color = "red"
    )
 
  })
  
  output$global_Death <- renderValueBox({
    
    
    latest <- 
      global_deaths%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_deaths%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") deaths globally"),
      
      color = "blue"
    ) })
  
  
  output$global_Recovered <- renderValueBox({
    
    latest <- 
      global_recovered%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_recovered%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") recovered globally"),
      
      color = "green"
    )
    
  })
  
  
  
  output$line_plot <- 
    renderHighchart({
      
      # dat <- 
      #   covid_19_dhb() %>% 
      #   count(Date) %>% 
      #   mutate(Cases = cumsum(n))
      
      daily_counts %>% 
        mutate(active = totalConfirmed - totalRecovered) %>% 
        select(Date, totalCases, totalConfirmed, totalDeaths, active) %>% 
        rename('Total cases' = totalCases, 
               'Total confirmed Cases' = totalConfirmed,
               'Total death' = totalDeaths,
               'Total active confirmed cases' = active)  %>% 
        gather("Type", "Count", -Date) %>% 
        hchart("line", hcaes(x = Date, y = Count, group = Type)) 
      
    })
  
  output$bar_plot <- 
    renderHighchart({
    
      hchart(daily_counts, "column", hcaes(x = Date, y = cases),
             name = "Total cases") 
    })
  
  
  output$line_plot_cause <- 
    renderHighchart({
      
      daily_counts %>% 
        select(Date, overseas, contact, investigating, community) %>% 
        rename('Recent overseas travel' = overseas, 
               'Contact with known case	' = contact,
               'Community transmission' = community,
               'Source under investigation' = investigating)  %>% 
        na.omit() %>% 
        gather("Type", "Count", -Date) %>% 
        hchart("line", hcaes(x = Date, y = Count, group = Type))
    })
  
  output$line_plot_global <- 
    renderHighchart({
      
      global_cases %>% 
        rename(State =  'Province/State', 
               Country = 'Country/Region') %>%
        gather( "Date",  "Count",
                -State,  -Country, -Lat, -Long) %>% 
        mutate(Date = as.Date(Date, "%m/%d/%y")) %>% 
        group_by(Country, Date) %>% 
        summarise(Count = sum(Count)) %>% 
        filter(Country %in% 
                 c("New Zealand", "China", "Japan", "Korea, South", "Spain",
                   "Taiwan*", "Ireland", "US", "Australia", 
                   "United Kingdom", "Italy")) %>% 
        hchart("line", hcaes(x = Date, y = Count, group = Country))  %>% 
        hc_yAxis(type = 'logarithmic',
                 title = list(text = "Total case counts in logarithmic scale"))
    })
  
  output$bar_dhb <-
    renderHighchart({
    
      
      dat <-
        covid_19_date() %>% 
        count(DHB) %>% 
        rename(Cases = n) %>% 
        mutate(DHB = forcats::fct_reorder(DHB, Cases)) %>% 
        arrange(desc(DHB))
      
      highchart() %>% 
        hc_xAxis(categories = dat$DHB) %>% 
        hc_add_series(data = dat, type = "bar", hcaes(x = DHB, y = Cases),
                      showInLegend = FALSE,
                      name = "Total cases")
      
    
      
    })

  output$bar_gender <-
    renderHighchart({

      dat <-
        covid_19_dhb() %>% 
        count(Sex) %>% 
        rename(Cases = n) %>% 
        mutate(Sex = forcats::fct_reorder(Sex, Cases)) %>% 
        arrange(desc(Sex))
      
      highchart() %>% 
        hc_xAxis(categories = dat$Sex) %>% 
        hc_add_series(data = dat, type = "bar", hcaes(x = Sex, y = Cases),
                      showInLegend = FALSE,
                      name = "Total cases")
      
    })
  
  
  output$bar_age <-
    renderHighchart({
      
      dat <-
        covid_19_dhb() %>% 
        count(Age) %>% 
        rename(Cases = n) %>% 
        arrange(desc(Age))
      
      highchart() %>% 
        hc_xAxis(categories = dat$Age) %>% 
        hc_add_series(data = dat, type = "bar", hcaes(x = Age, y = Cases),
                      showInLegend = FALSE,
                      name = "Total cases")
    })
  
  
  output$bar_travel <-
    renderHighchart({
      
      dat <-
        covid_19_dhb() %>% 
        count(`International travel`) %>% 
        rename(Cases = n) %>% 
        mutate(`International travel` = factor(`International travel`,
                                               levels = c("Yes", "No", ""),
                                               labels = c("Yes", "No", "Unknown")))%>% 
        arrange(`International travel`) 
      
      highchart() %>% 
        hc_xAxis(categories = dat$`International travel`) %>% 
        hc_add_series(data = dat, type = "bar", 
                      hcaes(x = `International travel`, y = Cases),
                      showInLegend = FALSE,
                      name = "Total cases")
    })

  output$bar_last_country <-
    renderHighchart({
      
      dat <-
        covid_19_dhb() %>% 
        filter(`International travel` == "Yes") %>% 
        mutate(`Last country before return` = 
                 ifelse(`Last country before return` == "", "Unknown", 
                        `Last country before return`)) %>% 
        count(`Last country before return`) %>% 
        rename(Cases = n) %>% 
        mutate(`Last country before return` = 
                 forcats::fct_reorder(`Last country before return`, Cases)) %>% 
        arrange(desc(`Last country before return`)) %>% 
        slice(1:20)
      
      
      
      highchart() %>% 
        hc_xAxis(categories = dat$`Last country before return`) %>% 
        hc_add_series(data = dat, type = "bar", 
                      hcaes(x = `Last country before return`, y = Cases),
                      showInLegend = FALSE,
                      name = "Total cases")
    })
  data_map_final <- reactive({
    
    dat_final <-
      covid_19_date() %>%
        count(DHB)
    
    nzDHB_simp@data <- 
      nzDHB_simp@data %>%
      left_join(dat_final)
    
    return(nzDHB_simp)
  })
  
  
  # Mapping  -----------------------------------------------------
  
  
  
  output$map <- renderLeaflet({
    
    data_map_final <- data_map_final()
    
    pal <- colorNumeric(palette = brewer.pal(11,"Blues"), 
                        domain = data_map_final@data$n)
    
    m <- 
      leaflet(data_map_final) %>%
      addTiles(options = tileOptions(minZoom = 5, maxZoom = 10)) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal(n), 
                  label = ~paste0(DHB, " DHB: ", n),
                  layerId = data_map_final@data$DHB, 
                  labelOptions = labelOptions(textsize = "20px")) %>%
      addLegend(pal = pal, values = ~n, opacity = 1.0,
                title = "Number", 
                position = "bottomright",
                na.label = "Missing")  %>%
      setView(lng=172, lat= -40.90056 , zoom = 5.5)  

    
    # if(input$auck)
    #   m %>% setView(lng=174.75, lat= -36.85, zoom = 10)
    # else 
    #   m %>% setView(lng=172, lat= -40.90056 , zoom = 5.5)
    
  })
  
  
  # End of Mapping  -----------------------------------------------------
  
  
  # observe({
  #   
  #   event <- input$map_shape_click
  #   DHB <-  c(nzDHB_simp$DHB, "New Zealand")
  #   
  #   
  #   
  #   if(is.null(event)){
  #     updateSelectInput(session, "DHB", "Select a DHB:",
  #                       choices = DHB, 
  #                       selected = "New Zealand")
  #   } else {
  #     
  #     
  #     updateSelectInput(session, "DHB", "Select a DHB:",
  #                       choices = DHB, selected = event$id)
  #   }
  # })
  
  
  
  
}


shinyApp(ui = ui, server = server)

