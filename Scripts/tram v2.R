required_packages <- c("tidyverse", "httr", "janitor", "jsonlite", "lubridate")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
    
if(length(missing_packages>0)){
    install.packages(missing_packages, repos = c("https://usaid-oha-si.r-universe.dev",
                                                 "https://cloud.r-project.org"))
}



#library(tidyverse)
#library(httr)
#library(janitor)
#library(jsonlite)
library(lubridate)
library(kickout)
library(dplyr)
library(tidyverse)





#TODO issues in data in years 2018, 2019, 2020, 2021
#TODO add "rules"
#TODO process scores based on rules
#TODO handle SYN and TRA
#TODO DECISION how much data to include? Only KTK jumpers
#TODO Create a list of KTK jumpers based on who jumping under København and copenhagen, then look for names in remaining dataset


#API_URL <- "http://sporttech.io/api/events/"
#LINK_START <- "https://sporttech.io/events/"
#LINK_END <- "/ovs/api/event/export"
#FOLDER_PATH <- "Data/"

#THIS_YEAR <- year(Sys.Date())
#THIS_MONTH <- month(Sys.Date())


#  https://sporttech.io/events/050b2ceb-f9c9-49a7-7b31-1fbdf6478027


source("Scripts/utilities.R")

  

    
    all_events_overview <- kickout::fetch_past_event_list() |> 
      kickout::process_event_list() 
    




#all events
#event_id_list <- create_list_events("Trampoline", 2022, 1)

#create a list of relevant IDs
#event_id_list_ids <- create_list_events("Trampoline", THIS_YEAR, THIS_MONTH) %>% 
#    select(event_id) %>% 
#    mutate(event_url = paste0(LINK_START, event_id, LINK_END)) %>% 
#    select(-event_id) %>% 
#    pull()



#all_data <- map(event_id_list_ids, read_competition) %>% 
#    bind_rows() 

#write_csv(all_data, "Data/all_data_2018_Q4.csv")


#READ DATA FROM FILES---------------------------------------------

#list of files
#all institution files
#input_file <- dir(FOLDER_PATH, full.name = TRUE, pattern = "*.csv")


#all_data <- map(input_file, read_competition_file) %>% 
#    bind_rows()


#unique(all_data$representing)

#write_csv(all_data, "Dataout/all_data_final.csv")


    

#TODO NEXT steps:  reading in without errors.  How to handle when there is an error? 
# Initialize an empty list to store the results
# Initialize an empty list
all_data <- list()

# Iterate over each URL and read the competition data
map(event_id_list_ids, ~{
    tryCatch({
        # Attempt to read the competition data
        competition_data <- read_competition(.x)
        
        # Add the competition data to the list
        all_data[[.x]] <- competition_data
    }, error = function(e) {
        # Print information about the error
        cat("Error occurred for URL:", .x, ":\n")
        cat("Error message:", conditionMessage(e), "\n")
        # Print the traceback
        print(traceback())
    })
})

# Combine the results into a single data frame
all_data <- bind_rows(all_data)
    


#--------------------------------------------

#process an individual event from a local file

event <- "Data/DanCup 1 2025_old.csv"

event_data_local <- kickout::fetch_event_local(event) |> 
  kickout::process_event()
  
#process an individual event from a url

id <- "5ff135d3-57b7-4421-60f8-dab9141fce48" #TRA FIG 2022

id <- "00f0f366-8746-48e1-4ef0-c222aea3c8b2" #TRA FIG

event_data_url <- kickout::fetch_event_url(id) |> 
  kickout::process_event_test()


event_id_rule <-  all_events_overview |> 
  select(event_id, rules)

event_data_url_rules <- event_data_url |> 
  left_join(event_id_rule, by = c("event_uuid" = "event_id")) 



unique(event_id_rule$rules)
