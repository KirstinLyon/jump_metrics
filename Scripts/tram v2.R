library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(here)

#TODO add error messages for stability + document
#TODO how to get the country and host? does it even matter? 
#TODO fix so relative path
#TODO create link to csvs for all competitions, but split by year. No need to download everything each time.
#TODO download and bind per year and save in a folder
#TODO turn "pulling data" in to a function 


api_url <- "http://sporttech.io/api/events/"

my_raw_result <- httr::GET(api_url)

my_content <- httr::content(my_raw_result, as = "text")

my_content_from_json <- jsonlite::fromJSON(my_content)

all_events <- my_content_from_json$event

event_data_unflatten <- all_events %>%
    map(~.[c("event_id","en_name", "sport", "rules", "begin_date", "end_date")]) %>%
    bind_rows() %>% 
    mutate(begin_date = ymd_hms(begin_date),
           end_date = ymd_hms(end_date)) %>% 
    filter(!is.na(begin_date) & !is.na(end_date)) %>% 
    filter(begin_date < Sys.Date())



# Specify the full path to the output file
output_file <- "C:/Users/kirst/Sync/Projects/trampoline/Dataout/list_of_competitions.csv"

# Write to CSV file
write_csv(event_data_unflatten, output_file)

   





