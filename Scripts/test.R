
library(dplyr)
library(readr)

events <-  read_csv("https://raw.githubusercontent.com/KirstinLyon/jump_metrics/refs/heads/main/Dataout/all_events.csv")



# CHECKS
# 1.  look at DEN - any missing clubs?  or inconcistent spelling?
# 2.  look at UNKNOWN country  - any that should be DEN?


# Check for DK clubs - look at UNKNOWN
club <- events |> 
    select(Club, Country) |> 
    distinct() |> 
    arrange(Club)  



# CHECKS
# 1. Check for DK clubs - look at UNKNOWN country and check clubs
# 2. Check for DEN clubs - look at UNKNOWN club
# 3. check for DEN and KTK - name consistency


# Check for DK and unknown countries
name_club_country <- events |> 
    select(Competitor, Club, Country) |> 
    distinct() 



# Update any inconsistencies for clubs, countries and names in trampoline stored data
# rerun fetch_data script and push data to GitHub

