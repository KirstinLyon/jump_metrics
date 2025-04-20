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
library(readr)


# GLOBAL VARIABLES -------------------------------------------------

START_YEAR <- 2023


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
    "Gefle", "CZE", "České", "Peak", "RUSSIA", "Juegos", "træf",
    "lēcien"
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


