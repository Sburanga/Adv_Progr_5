library(shiny)
library(dplyr)
library(ggplot2)
#' Title my_sunshine
#'
#' @field server_components list. It contains ui and server for the Shiny application.
#' @description This class uses another class in order to have access in an API.
#' It is also create a Shiny application for air pollution. 
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @export My_shiny
#' @exportClass My_shiny
My_shiny<-
  setRefClass(
  "My_shiny",
  fields = list(
    server_components="list"
  ),
  methods=list(
    initialize=function(){
      server_components<<-list()
      server_components$ui <<- 
        navbarPage("My Application",
           tabPanel("Component 1",
                    plotOutput("plot_1")),
           
           tabPanel("Component 2",
                    fluidPage(
                      
                      titlePanel("Concentration of PM25"),
                      
                      fluidRow(
                        column(2,
                               radioButtons(inputId = "radio", 
                                            label = "Select the country you wish to visualize:", 
                                            choices = c("Turkey", "Italy", "Greece", "Sweden"), 
                                            inline = FALSE,
                                            width = NULL)
                        ),
                        hr(),
                        column(10, 
                               fluidRow(plotlyOutput("plot_2", height = "500px")))))))   
      
      server_components$server<<- function(input, output){
        
        Sys.setenv(
          'MAPBOX_TOKEN' = 
            'pk.eyJ1Ijoic3RldG84MjAiLCJhIjoiY2ptYm1hNGoxMDVzODNxcDh5YWYwdWIyeiJ9.vqmnBQELpRxT2klgrWJvuQ')
        
        countries = list(
          "Turkey",
          "Italy",
          "Greece",
          "Sweden"
        )
        
        api <- MyShiny::Worldwide_Pollution$new(countries)
        
        output$plot_1 = renderPlot({
          facets = c(
            "country",
            "value_pm5"
          )
          plot_pm25_means(api$get_facets_all_responses(facets)) 
        })
        
        output$plot_2 = renderPlotly({
          facet_vector<-c(
            "country",
            "filename",
            "value_pm5",
            "Category PM25",
            "data_location_latitude",
            "data_location_longitude")
          
          df<-api$get_only_faced_data(api$responses[[input$radio]], facet_vector)
          if(input$radio=="Italy") zoom <- 4.1
          else if(input$radio=="Sweden") zoom <- 3
          else zoom <- 5
          p<-plot_mapbox(mode = "scattermapbox") %>%
            add_markers(
              data = df, y = ~data_location_latitude, x = ~data_location_longitude,
              color=~as.factor(`Category PM25`), text = ~filename, hoverinfo = "text",
              hovertext = paste('</br>Category: ', df$`Category PM25`, "</br>Region: ", df$filename,
                                "</br>Value: ", df$value_pm5),
              marker=list(size=10), alpha = 0.5,
              colors = rev(RColorBrewer::brewer.pal(length(unique(df$`Category PM25`)),"PiYG"))) %>%
            layout(
              plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
              mapbox = list(style = 'dark',
                            scope = "europe",
                            zoom = zoom,
                            center = list(lat = mean(as.numeric(df$data_location_latitude)),
                                          lon = mean(as.numeric(df$data_location_longitude)))),
              legend = list(orientation = 'h',
                            font = list(size = 8)),
              margin = list(l = 0, r = 0,
                            b = 0, t = 0,
                            pad = 0)
            )
          p
          
        })
      }
    },
  run=function(){
    shinyApp(ui = server_components$ui, server = server_components$server)
  },
  plot_pm25_means=function(all_data){
    mean_table = all_data %>%
      group_by(country) %>%
      summarise(mean=mean(value_pm5))
    g = ggplot(mean_table, aes(x=country, y=mean)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title = "Means of P5 in Countries", x="Countries", y="Mean") +
      scale_y_continuous(breaks=seq(0,70,by=5))
    return(g)
  }
      
  )
)

    