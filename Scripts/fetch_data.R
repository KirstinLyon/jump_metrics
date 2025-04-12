#PROJECT:  Kickout - individual competitions
#AUTHOR:  Kirstin Lyon
#DESCRIPTION: Fetch and process data from sporttech.io using kickout package
#LICENSE: MIT
#Date: 2025-04-11

# LIBRARIES ---------------------------------------------------------
library(kickout)
library(lubridate)
library(dplyr)
library(cld2)
library(stringr)


# GLOBAL VARIABLES -------------------------------------------------

START_YEAR <- 2025

REMOVE_LANGUAGE <- c("ru", "ja", "bg", "af",
                     "de","es", "et", "fi", "fr",
                     "pt", "sr", "sv", "lv",
                     "hmn", "it", "mg")

# Define each pattern on a new line for readability
EVENTS <- c(
    "QLD", "VIC", "ACT", "TAS", "STATE",
    "Midlands", "Power", "Campeonato",
    "Rocket", "Tumbling", "Territorial",
    "Island", "Qualificativa", "Barmstedt",
    "Abertura", "Portimão", "State", "Grenchner",
    "AGC", "TRP", "Wölfe", "Olympia", "Dynamic",
    "hoppet", "UVGK", "TCI", "Test", "Interclub",
    "Hoppet", "AUS", "CSG", "test", "Asian", "Balder",
    "BGS", "Demo", "KGK", "South", "USM", "TORNEIO",
    "Kids", "Reguengos", "NSW", "KTF", "HCA",
    "Soorser", "NZ", "Schools", "Dutch", "FinCup", 
    "China", "Pokal", "Loulé", "Tri Star", "bounce",
    "Peke", "WA", "AGA", "AGS", "SM", "Happy", "Basilea",
    "Bounce", "Levo", "Uppsala", "Nissen", "Oeiras", "Beyond",
     "Sacros", "COTTBUS", "DTO", "Herbstcup","Chablais",
    "CMGI","Vic","Coimbra", "Dobrovolsky", "Espoon", 
    "Latvian", "Merino", "TV", "Friendship", "HTT", "Område",
    "Geneva", "Herbst", "Apuramento", "BounCe", "High", "Peter",
    "Scalabiscup", "Summit", "Väst", "MTGA", "AL FUNraiser",
    "Teamcup", "EsTT", "Trial", "Traps", "Eesti", "závod",
    "Gefle", "CZE", "České", "Peak", "RUSSIA", "Juegos"
)

# Combine patterns into a single string with | as the separator
REMOVE_EVENTS <- paste(EVENTS, collapse = "|")


# FETCH DATA --------------------------------------------------------

# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    kickout::process_event_list() |> 
    mutate(language = cld2::detect_language(en_name)) |> 
    filter(year(begin_date) >= START_YEAR,
           !(language %in% REMOVE_LANGUAGE)
 ) |> 
    
    # Remove unnecessary data - related to language and events 
    mutate(en_name_trunc = str_sub(en_name, start = 2, end =  10)) |> 
    filter(str_detect(en_name_trunc, "^[a-zA-ZÀ-ÿ0-9\\p{P}\\sº]+$")) |> 
    filter(!str_detect(en_name, REMOVE_EVENTS)) |> 
    select(-en_name_trunc, -language) 


# CREATE DATASETS ------------------------------------------------------------------

event_rules <- event_list |> 
    select(event_id, rules)

all_id <- event_list |> 
    select(event_id) |> 
    distinct() |> 
    pull()



#TODO  move joining rules to function
all_events_discipline <- purrr::map(event_list$event_id, kickout::fetch_event_url) |>
    bind_rows() |> 
    left_join(event_rules, by = c("event_uuid" = "event_id")) 





