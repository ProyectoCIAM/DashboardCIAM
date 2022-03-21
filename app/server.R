library(shiny)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {
    base_url <- "http://127.0.0.1:8080/api/"
    infor_url <- "folio"
    full_url <- base::paste0(base_url, infor_url)
    api_call <- httr::GET(full_url) 
  
    #retrieving json files
    folios_json <- jsonlite::fromJSON(full_url)

    #personas_json <- jsonlite::fromJSON("http://http://127.0.0.1:8080/api/persona")


    #retrieving api's response leaving the status out
    folios <- folios_json$response

    folios

    #personas <- personas_json$response




}
