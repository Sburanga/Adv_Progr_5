
library(jsonlite)
r <- fromJSON("https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldwide-pollution&facet=country&facet=value_pm5&refine.country=Armenia")

acceptable_fields = list(
  ""
)

root_url = "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldwide-pollution"

facets = list(
  "Category PM25",
  "value_pm5"
)

get_country_data <- function(country_name){
  response = fromJSON(paste(list(root_url,paste(lapply(facets, FUN=get_facet), collapse=""), "&refine.country=", country_name, "&rows=9000"), collapse = ""))
  return(response)
}

get_facet <- function(str){
  return(paste(list("&facet=",gsub(" ", "%20", str)), collapse = ""))
}

get_only_faced_data <- function(data){
  return(data$records$fields[data$facet_groups$name])
}



