library(tidyverse)
library(sf)
library(shiny)
library(forecast)
library(scales)
library(leaflet)
library(leaflet.extras)

africa_output <- function(input_country){
  filtered <- africa_covid %>% 
    dplyr::filter(Country_Region == input_country) %>% 
    gather(key = Type, value = Number, Confirmed, Deaths, Recovered, Active)
  
  disp_date <- dplyr::filter(filtered, Type == 'Active', Number >= 1)
  disp_date <- disp_date[1, 2]
  
  filtered_gg <- ggplot(data = filtered) + xlab("Date")  
  
  if (max(filtered$Number) > 20000){
    filtered_gg <- filtered_gg + 
      geom_smooth(aes(x = date, y = Number, color = Type),
                  se = F, size = 0.75) +
      geom_forecast(data = dplyr::filter(filtered, Type == "Active"), 
                    aes(x = date, y = Number),
                    color = "red", showgap = F, size = 0.75) +
      scale_y_continuous(trans = 'log10', labels = comma) +
      ylab("Number - Logistic Scale")
  } else {
    filtered_gg <- filtered_gg + 
      geom_line(aes(x = date, y = Number, color = Type), size = 0.75) +
      geom_forecast(data = dplyr::filter(filtered, Type == "Active"),
                    aes(x = date, y = Number),
                    color = "red", showgap = F, size = 0.75)
  }
  
  filtered_gg +
    ggtitle(paste0(input_country, " Covid Cases and Projections")) +
    theme(legend.title.align=0.5) +
    scale_x_date(date_breaks = "1 week",
                 labels = date_format("%d-%b"),
                 limits = (c(disp_date$date - 5, Sys.Date() + 10)))
}

africa_covid <- read_csv("https://raw.githubusercontent.com/Slushmier/covid-africa/master/Data/africa_covid.csv")
africa_polys <- sf::st_read("https://raw.githubusercontent.com/Slushmier/covid-africa/master/Data/africaPolys.geojson")
  
countries <- africa_covid %>% dplyr::distinct(Country_Region) %>% arrange()

country_popup <- paste0("<strong>Covid-19 Data by Country</strong>",
                        "<br><br><strong>Country: </strong>", 
                        africa_polys$name_long, 
                        "<br><strong>Date of Data: </strong>", 
                        africa_polys$date,
                        "<br><strong>Confirmed cases (JHU): </strong>",
                        africa_polys$Confirmed,
                        "<br><strong>Deaths (JHU): </strong>",
                        africa_polys$Deaths,
                        "<br><strong>Recovered cases (JHU): </strong>",
                        africa_polys$Recovered,
                        "<br><strong>Active cases (JHU): </strong>",
                        africa_polys$Active,
                        "<br><strong>Confirmed Cases per 1 million people: </strong>",
                        round(africa_polys$case_rate, 5),
                        "<br><strong>Population Estimate: </strong>",
                        africa_polys$pop_est,
                        "<br><strong>Population Source: </strong>",
                        africa_polys$source,
                        "<br><strong>Year of Population Estimate: </strong>",
                        africa_polys$sourceyear)

ui <- fluidPage(title = "Covid-19 Cases in Africa",

  titlePanel("Covid-19 Cases in Africa"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("countryinput",
                  label = "Country for Projections:",
                  selected = "Ethiopia",
                  choices = countries),
      p("Case data comes from",
        tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
               "the Johns Hopkins University COVID-19 Github page."), 
        " Case numbers are certainly lower than actual case numbers."),
      br(),
      p("Projections are done with the forecast package in R. The red bands 
          are ten day projections for numbers of Covid-19 cases. The narrow, 
          dark red bands are 95% confidence intervals, the wider, light red 
          bands are 80% confidence intervals."),
      br(),
      p(tags$a(href = "https://github.com/Slushmier/covid-africa", 
               "Here is the GitHub repository for this page."))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Africa Case Map", leafletOutput("africamap", height = "500px")),
                  tabPanel("Country Case Projections", plotOutput("plot")),
                  tabPanel("Country Case Data", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output){
  
  output$plot <- renderPlot({
    africa_output(input$countryinput)
  })
  
  output$table <- renderTable({
    dataout <- dplyr::filter(africa_covid,
                            Country_Region == input$countryinput) %>% 
      arrange(desc(date))
    dataout$date <- as.character(dataout$date)
    dataout
  }, digits = 0)
  
  output$africamap <- renderLeaflet({
    pal_map <- colorQuantile("Reds", domain = africa_polys$case_rate,
                             n = 5)
    
    leaflet(africa_polys) %>% 
      addTiles() %>% 
      addPolygons(color = "gray", weight = 1, smoothFactor = 0.5,
                  opacity = 0.5, fillOpacity = 0.2, 
                  fillColor = ~colorQuantile("Reds", case_rate, n = 5)
                  (case_rate),
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = T),
                  popup = country_popup,
                  label = ~paste0(name_long, ": ", Confirmed, " confirmed cases."),
                  labelOptions = labelOptions(direction = "auto")) %>% 
      addLegend("topright", pal = pal_map, values = ~case_rate,
                title = "Confirmed Cases Per <br>1 Million People",
                opacity = 0.5,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  p = paste0(round(p * 100), '%')
                  cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))
                  # mouse over the legend labels to see the percentile ranges
                  paste0(
                    '<span title="', p[-n], " - ", p[-1], '">', cuts,
                    '</span>')
                }
      ) %>%
      addFullscreenControl() %>% 
      setView(lng = 16.942, lat = 1.261, zoom = 3)
  })
}

shinyApp(ui, server)