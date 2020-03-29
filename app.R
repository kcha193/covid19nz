

library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(readxl)
library(tidyverse)
library(DT)
library(highcharter)

source("get_data.R")
nzDHB_simp <- readRDS("data/nzDHB_simp.rds")

date_range <- range(covid_19_cases$Date, na.rm = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 in NZ"),
  dashboardSidebar(
    
    h3("Note:"), 
    p("Plots are based from the case data in ", 
      a("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details",
        href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details"),
      ", which is under the Ministry of Health's creative commons license:",
      a("https://www.health.govt.nz/about-site/copyright",
        href = "https://www.health.govt.nz/about-site/copyright"), "."),
    
    sliderInput("date_range",
                "Dates:",
                min = as.Date(date_range[1],"%Y-%m-%d"),
                max = as.Date(date_range[2],"%Y-%m-%d"),
                value = as.Date(date_range[2],"%Y-%m-%d"),
                timeFormat="%Y-%m-%d", animate = TRUE),
    
    selectInput("DHB", "Select a DHB:",
                choices = c(nzDHB_simp$DHB, "New Zealand"), 
                selected = "New Zealand"),
    checkboxInput("confirmed", "Click for confirmed cases",
                  value = FALSE, width = "200%"),
    checkboxInput("auck", "Click to zoom into Auckland",
                  value = FALSE, width = "200%"),
    box(
      h4("Latest Update:"),
      h4("2019-03-29"),
      h4("Contact email:"),
      h5(a("Kevin Chang", href = "mailto:kevin.ct.chang@gmail.com")),
      width = 12,
      background = "black"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(
      tags$style(HTML(".leaflet-container { background: #ADD8E6; }"))
    ),
    
    column(width = 6,
    box(
      valueBoxOutput("case_Total", 6), 
      valueBoxOutput("confirmed_Total", 6),
      width = 12, height = 130),
      
    tabBox(  
      tabPanel("Total",
               highchartOutput("line_plot",  height = "250px")),
      tabPanel("Daily",
               highchartOutput("bar_plot",  height = "250px")),
      width = 12, height = 290
    ),
    
    tabBox(
      tabPanel("DHB",
               highchartOutput("bar_dhb")),
      tabPanel("Gender", 
               highchartOutput("bar_gender")),
      tabPanel("Age", 
               highchartOutput("bar_age")),
      width = 12,
      height = 350)
    ),
    
    
    tabBox(title = "NZ cases of COVID-19", 
           tabPanel("Map",
                    leafletOutput("map",  height = "750px"), 
        
        p("This shapefiles of DHB are based on/includes Statistics New Zealand's data which are licensed by",
          a("Statistics New Zealand", href="http://www.stats.govt.nz/", shape="rect"),
          "for re-use under the",
          a("Creative Commons Attribution 4.0 International", 
            href="https://creativecommons.org/licenses/by/4.0/", shape="rect"),
          "licence.")), 
        tabPanel("Data",
                 DTOutput("data_raw")), 
        width = 6,
        height = 920)
  )
)




server <- function(input, output, session) {
  
  
  
  covid_19_date <- reactive({
    
    covid_19_final <- 
      covid_19_cases %>% 
      filter(Date <= input$date_range) %>% 
      mutate(Date = as.character(Date))
    
    if(input$confirmed){
      covid_19_final <- 
        covid_19_final %>% 
        filter(Confirmed)
    }
    
    
    return(covid_19_final)
  }) 
  
  
  covid_19_dhb <- 
    reactive({
  
      if(input$DHB != "New Zealand"){
        covid_19_final <- 
          covid_19_date()  %>% 
          filter(DHB == input$DHB)
      } else {
        covid_19_final <- 
          covid_19_date()
        
      }
      
      return(covid_19_final)
    })
  
  
  output$data_raw <- 
    renderDT({
      covid_19_date() %>% 
        select(Date:DHB, Confirmed)
      
    })
  
  
  output$case_Total <- renderValueBox({
    valueBox(
      covid_19_dhb() %>%
        count() %>% pull(n), "Cases overall", 
      icon = icon("ambulance", lib = "font-awesome"),
      color = "blue"
    )
  })
  output$confirmed_Total <- renderValueBox({
    valueBox(
      covid_19_dhb() %>% 
        filter(Confirmed) %>% 
        count() %>% 
        pull(n), "Case confirmed", 
      icon = icon("first-aid", lib = "font-awesome"),
      color = "red"
    )
  })
  
  
  output$line_plot <- 
    renderHighchart({
      
      dat <- 
        covid_19_dhb() %>% 
        count(Date) %>% 
        mutate(Cases = cumsum(n))
      
      
      hchart(dat, "line", hcaes(x = Date, y = Cases))
    })
  
  output$bar_plot <- 
    renderHighchart({
      
      dat <-
      covid_19_dhb() %>% 
        count(Date) %>% 
        rename(Cases = n)
      
      hchart(dat, "column", hcaes(x = Date, y = Cases))
      
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
                      showInLegend = FALSE)
      
    
      
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
                      showInLegend = FALSE)
      
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
                      showInLegend = FALSE)
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
    
    pal <- colorNumeric(palette = rev(brewer.pal(11,"RdYlGn")), 
                        domain = data_map_final@data$n)
    
    m <- 
      leaflet(data_map_final) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal(n), 
                  label = ~paste0(DHB, " DHB: ", n),
                  layerId = data_map_final@data$DHB, 
                  labelOptions = labelOptions(textsize = "20px")) %>%
      addLegend(pal = pal, values = ~n, opacity = 1.0,
                title = "Number", 
                position = "bottomright",
                na.label = "Missing")
    
    
    if(input$auck)
      m %>% setView(lng=174.75, lat= -36.85, zoom = 10)
    else 
      m %>% setView(lng=172, lat= -40.90056 , zoom = 5.5)
    
  })
  
  
  # End of Mapping  -----------------------------------------------------
  
  
  observe({
    
    event <- input$map_shape_click
    DHB <-  c(nzDHB_simp$DHB, "New Zealand")
    
    
    
    if(is.null(event)){
      updateSelectInput(session, "DHB", "Select a DHB:",
                        choices = DHB, 
                        selected = "New Zealand")
    } else {
      
      
      updateSelectInput(session, "DHB", "Select a DHB:",
                        choices = DHB, selected = event$id)
    }
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)

