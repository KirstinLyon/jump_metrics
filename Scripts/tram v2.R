library(tidyverse)
library(httr)
library(jsonlite)


api_url <- "http://sporttech.io/api/events/"

my_raw_result <- httr::GET(api_url)

str(my_raw_result)
str(my_raw_result$content)


my_content <- httr::content(my_raw_result, as = "text")
str(my_content)

my_content_from_json <- jsonlite::fromJSON(my_content)
glimpse(my_content_from_json)

event_df <- my_content_from_json$event_id

test <- my_content_from_json[["event"]][["00276cc1-8288-429f-45ae-5d8bb2582c28"]]$event_id

test <- my_content_from_json$event

glimpse(test)


#next:  I want to keep only events that have happened - and have a start_date and end_date within a range

event_ids <- lapply(test, function(event) event[["event_id"]])
event_ids_k <- unlist(event_ids) %>% 
    as_tibble()

print(event_ids_k)
