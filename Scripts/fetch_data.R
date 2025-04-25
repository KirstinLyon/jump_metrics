#PROJECT:  Kickout - individual competitions
#AUTHOR:  Kirstin Lyon
#DESCRIPTION: Fetch and process data from sporttech.io using kickout package
#LICENSE: MIT
#Date: 2025-04-11

# LIBRARIES ---------------------------------------------------------
library(kickout)
library(dplyr)
library(stringr)
library(readr)
library(flexdashboard)


# GLOBAL VARIABLES -------------------------------------------------

START_YEAR <- 2023

# Define each pattern on a new line for readability
EVENTS <- c("DanCup", "Scandinavian Open", "Forbunds", "DM Senior",
            "Nordic", "Frivolten", "FIG", "DM u12")

# Combine patterns into a single string with | as the separator
KEEP_EVENTS <- paste(EVENTS, collapse = "|")


# FETCH DATA --------------------------------------------------------


# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    kickout::process_event_list() |> 
    
    #Use base R to filter dates - avoid calling the lubridate library
    filter(as.integer(format(begin_date, "%Y")) >= START_YEAR,
           str_detect(en_name, KEEP_EVENTS)
    ) 



# CREATE DATASETS ------------------------------------------------------------------

# create dataset for given IDs and rename columns

events <- purrr::map(event_list$event_id, ~ kickout::fetch_event_url(.x, event_list)) |>
    bind_rows()|> 
    dplyr::rename(Event = title,
                  Competitor = name,
                  Club = club,
                  Country = country,
                  Date = date,
                  Execution = execution,
                  Mark = mark,
                  Total = mark_total,
                  Stage = stage,
                  Competition = competition,
                  Discipline = discipline,
                  Elements = number_elements,
                  Rank = rank
    ) |> 
    mutate(Date = as.Date(Date),
           Stage = case_when(str_detect(Stage,"inal") ~ Stage,
                             TRUE ~ paste(Stage, routine_number, sep ="_")
                             ),
           is_complete = case_when(Elements == 10 ~ "Complete",
                                   TRUE ~ "Incomplete"
                                   )
           )|> 
    select(event_uuid, Date, Event,   Discipline, Competition, Competitor, Club, Country, Stage,
           Rank, Total, Mark, Elements, Execution, T, H, D, everything()) |> 
    arrange(desc(Date), Discipline, Competition, group_number, performance_number, Competitor) |> 
    select(-c(group_number, performance_number))

write_csv(events,"Dataout/all_events.csv")



