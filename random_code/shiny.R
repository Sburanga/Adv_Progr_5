library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)



facets = list(
  "Category PM25",
  "value_pm5"
)



ui <- navbarPage("My Application",
                 tabPanel("Component 1",
                          # tableOutput("df"),
                          plotOutput("plot_1")),
                 tabPanel("Component 2",
                          textOutput("function_2")),
                 tabPanel("Component 3",
                          textOutput("function_3"))
)

server <- function(input, output){
  facets = list(
    "Category PM25",
    "value_pm5"
  )
  
  all_data = get_all_country_data(facets)
  
  mean_table = all_data %>%
    group_by(country) %>%
    summarise(mean=mean(value_pm5))
  
  # output$df = renderTable(mean_table)
  
  output$plot_1 = renderPlot({
    g = ggplot(mean_table, aes(x=country, y=mean)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title = "Means of P5 in Countries", x="Countries", y="Mean") +
      scale_y_continuous(breaks=seq(0,70,by=5))
    g
  })
  output$function_2 = renderText("This is 2st!")
  output$function_3 = renderText("This is 3st!")
}

shinyApp(ui = ui, server = server)


# takes parameter:
# country: string
# facets: list that contains facets that you want
get_country_data <- function(country, facets){
  response = fromJSON(get_req_url(get_req_part(facets,"facet"), get_req_query("refine.country", country), get_req_query("rows", "10000")))
  return(response)
}

# returns observations of countries:
# Turkey,Greece,Italy,Sweden
# as a dataframe
get_all_country_data = function(facets){
  countries = list(
    "Turkey",
    "Italy",
    "Greece",
    "Sweden"
  )
  d = NA
  counter = 1
  for (country in countries) {
    cat(country, "request sent..." , sep = " ", "\n")
    res = fromJSON(get_req_url(get_req_part(facets,"facet"), get_req_query("refine.country", country), get_req_query("rows", "10000")))
    if(counter==1)
      d = get_only_faced_data(res,facets) 
    else
      d = rbind(d, get_only_faced_data(res,facets))
    counter = counter + 1
    cat(country, "responded!" , sep = " ", "\n")
  }
  return(d)
}

# returns &key=value
get_req_query <- function(key,val){
  return(paste(list("&",key,"=",gsub(" ", "%20", val)), collapse = ""))
}

# if you have a list for request parameters.
# This function will return repeatly get_req_query
# for facet_list and key=facet
# it returns &facet=facet_list[1]&facet=facet_list[2]
get_req_part = function(facet_list, key){
  return(paste(lapply(facet_list, FUN=get_req_query, key=key), collapse=""))
}

# takes elements as parameter
# returns the whole url for request
get_req_url = function(...){
  elements = list(...)
  root_url = "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldwide-pollution"
  return(paste(c(root_url, elements), collapse=""))
}


# returns faced columns from the data
get_only_faced_data <- function(response,facet_vector){
  return(response$records$fields[facet_vector])
}

facet_vector<-c(
  "country",
  "filename",
  "value_pm5",
  "Category PM25",
  "data_location_latitude",
  "data_location_longitude")







