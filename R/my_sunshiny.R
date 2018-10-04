#' Title my_sunshine
#'
#' @field server_components list. It contains ui and server for the Shiny application.
#' @description This class uses another class in order to have access in an API.
#' It is also create a Shiny application for air pollution. 
#' @import shiny
#' @import dplyr
#' @import ggplot2
My_shiny<-
   setRefClass(
  "My_shiny",
   fields = list(
   server_components="list"
   ),
  methods=list(
    initialize=function(){
      server_components<<-list()
      server_components$ui <<- navbarPage("My Application",
                       tabPanel("Component 1",
                                # tableOutput("df"),
                                plotOutput("plot_1")),
                       tabPanel("Component 2",
                                textOutput("function_2")),
                       tabPanel("Component 3",
                                textOutput("function_3"))
      )    
      server_components$server<<- function(input, output){
        facets = c(
          "country",
          "value_pm5"
        ) 
        countries = list(
          "Turkey",
          "Italy",
          "Greece",
          "Sweden"
        )
        api=MyShiny::Worldwide_Polution$new(countries)
        
        output$plot_1 = renderPlot({
          plot_pm25_means(api$get_facets_all_responses(facets)) 
        })
        output$function_2 = renderText("This is 2st!")
        output$function_3 = renderText("This is 3st!")
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

    