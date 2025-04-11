library(tidyverse)
library(httr)
library(jsonlite)


# API URL
api_url <- "http://sporttech.io/api/events/"

# Make GET request to API
# Make GET request to API
response <- GET(api_url)

# Check if the request was successful
if (http_type(response) == "application/json") {
    # Parse JSON content
    json_content <- content(response, as = "text", encoding = "UTF-8")
    
    # Convert JSON to list
    events_list <- jsonlite::fromJSON(json_content)
    
    # Create a tibble with nested lists
    events_tbl <- tibble(event = events_list)
    
    # Unnest the nested list column
    event_id_tbl <- unnest(events_tbl, cols = c(event))
    
    # Select only the event_id column
 #   event_id_tbl <- event_id_tbl %>%
 #       select(event_id)
    
    # Print the tibble
    print(event_id_tbl)
} else {
    # If request fails, print error message
    print("Failed to retrieve data from the API.")
}
