#PROJECT:  Kickout - individual competitions
#AUTHOR:  Kirstin Lyon
#DESCRIPTION: Fetch and process data from sporttech.io using kickout package
#LICENSE: MIT
#Date: 2025-04-11

# LIBRARIES ---------------------------------------------------------
library(kickout)
library(lubridate)
library(dplyr)

library(stringr)
library(readr)


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
    filter(year(begin_date) >= START_YEAR) |> 
    filter(str_detect(en_name, KEEP_EVENTS)) 


# CREATE DATASETS ------------------------------------------------------------------

event_rules <- event_list |> 
    select(event_id, rules, begin_date)

# create dataset for given IDs
events <- purrr::map(event_list$event_id, fetch_event_url) |>
    bind_rows() |> 
    left_join(event_rules, by = c("event_uuid" = "event_id")) |> 
    mutate(club = case_when (club == "HASLEV TT" ~ "Haslev Trampolin og Turngymnaster",
                             club == "IK Skovbakken Trampolin" ~ "IK Skovbakken - Trampolin",
                             club == "Strib Trampolin" ~ "Strib Udspring & Trampolin",
                             .default = club) 
    ) |> 
    mutate(name = case_when(title == "DM Senior 2024" ~ paste(surname, given_panel_name, sep = " "),
                            .default = paste(given_panel_name, surname, sep = " ")
    ),
    name = str_to_title(name),
    name = str_squish(name),
    name = case_when(name == "Smilla Jensen" ~ "Smilla Thea Jensen",
                     name == "Alvilde Vega Scwartz-Petersen" ~ "Alvilde Vega Schwartz-Petersen",
                     name == "Emilie Schwartz-Petersen" ~ "Emilie My Schwartz-Petersen",
                     name == "Gustav Bruun" ~ "Gustav Heebøll Bruun",
                     name == "Johannes Bovien Nörfelt" ~ "Johannes Bovien Nørfelt",
                     name == "Johannes Nørfelt" ~ "Johannes Bovien Nørfelt",
                     name == "Storm Scotte Höjlo" ~  "Storm Skotte Højlo",
                     name == "Vanilla Søkær" ~ "Vanilla Søkjær",
                     name == "Smilla Beck" ~ "Smilla Vinding Beck",
                     name == "Storm Højlo" ~ "Storm Skotte Højlo",
                     name == "Villads Nemeth" ~"Villads Hilstrup Nemeth",
                     .default = name
    ),
    club = case_when(name == "Smilla Thea Jensen" ~ "Københavns Trampolinklub",
                     name == "Teis Petersen" ~  "Haslev Trampolin og Turngymnaster",
                     name == "Valdemar Johansen" ~ "IK Skovbakken - Trampolin",
                     name == "Aksel Koldkjaer" ~ "Springteam Nordjylland",
                     .default = club),
    country = case_when(name == "Smilla Thea Jensen" ~ "DEN",
                        name == "Teis Petersen" ~  "DEN",
                        name == "Valdemar Johansen" ~ "DEN",
                        name == "Aksel Koldkjaer" ~ "DEN",
                        .default = country),
    
    ) |> 
    select(-c(surname, given_panel_name, representing)) |> 
    rename(date = begin_date)



write_csv(events, "Dataout/all_events_discipline.csv")


ktk_data <- events |> 
    filter(club == "Københavns Trampolinklub") |> 
    select(-c(event_uuid, rules, country, club)) |>
    select(date, title, name, everything() ) |> 
    
    write_excel_csv("Dataout/KTK.csv")

# TESTS

# Check for DK clubs - look at UNKNOWN
club <- events |> 
    select(club, country) |> 
    distinct()

# Check for DK clubs - look for unknown countries and names
DK_name_club <- events |> 
    select(name, club, country) |> 
    distinct()

