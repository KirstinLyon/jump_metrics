
library(dplyr)
library(readr)

events <-  read_csv("https://raw.githubusercontent.com/KirstinLyon/jump_metrics/refs/heads/main/Dataout/all_events.csv")

# Check for DK clubs - look at UNKNOWN
club <- events |> 
    select(Club, Country) |> 
    distinct() |> 
    arrange(Club)  

den_club <- club |> 
    filter(Country == "DEN") |> 
    select(Club) 

unknown_country <- club |> 
    filter(Country == "unknown") 

unknown_club <- club |> 
    filter(Club == "unknown") |> 
    arrange(Country) |> 
    distinct()

# Check for DK clubs - look for unknown countries and names
DK_name_club <- events |> 
    select(Competitor, Club, Country) |> 
    distinct() |> 
    filter(Country == "DEN", Club != "unknown") |> 
    arrange(Competitor)

#club <- kickout::data_representing_map