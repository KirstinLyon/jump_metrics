#PROJECT:  Kickout - individual competitions
#AUTHOR:  Kirstin Lyon
#DESCRIPTION: Fetch and process data from sporttech.io using kickout package
#LICENSE: MIT
#Date: 2025-04-11

# LIBRARIES ---------------------------------------------------------
library(kickout)



# GLOBAL VARIABLES -------------------------------------------------
TEST_FILE <- "Data/DanCup 1 2025_old.csv"
TEST_ID <- ""

# FETCH DATA --------------------------------------------------------

# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    kickout::process_event_list()

# Clean local event
event_local <- kickout::fetch_event_local(TEST_FILE) |> 
    kickout::process_event()

# Fetch and clean event from URL
event_url <- kickout::fetch_event_url() |> 
    kickout::process_event()

