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
library(tidyr)
library(readr)
library(janitor)


# GLOBAL VARIABLES -------------------------------------------------

START_YEAR <- 2023

TEST_FILE <- "Data/DanCup 1 2025_old.csv"

TRA_FIG_2017 <- "e5ca8a4c-806a-4f97-48af-79ee40b03c51"
TRA_FIG_2022 <- "17262eeb-cc0c-42d5-54e6-65457da96611"

remove_language <- c("ru", "ja", "bg", "af",
                     "de","es", "et", "fi", "fr",
                     "pt", "sr", "sv", "lv",
                     "hmn", "it", "mg")

# Define each pattern on a new line for readability
patterns <- c(
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
    "Latvian", "Merino", "TV", "Friendship"
)

# Combine patterns into a single string with | as the separator
remove_competitions <- paste(patterns, collapse = "|")


# FETCH DATA --------------------------------------------------------

# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    kickout::process_event_list() |> 
    mutate(language = cld2::detect_language(en_name)) |> 
    filter(year(begin_date) >= START_YEAR,
           !(language %in% remove_language)
 ) |> 
    mutate(en_name_trunc = str_sub(en_name, start = 2, end =  10)) |> 
    filter(str_detect(en_name_trunc, "^[a-zA-ZÀ-ÿ0-9\\p{P}\\sº]+$")) |> 
    filter(!str_detect(en_name, remove_competitions)) |> 
    select(-en_name_trunc, -language) 

write_csv(event_list, "Dataout/event_list.csv")

# Clean local event
event_local <- kickout::fetch_event_local(TEST_FILE) |> 
    kickout::process_event()

# Fetch and clean event from URL
event_url <- kickout::fetch_event_url(TRA_FIG) |> 
    kickout::process_event()





# MISC ------------------------------------------------------------------
unique(event_list$rules)


#columns should be event ID, discipline.  If the displine exists, add a 1 in the value

all_id <- event_list |> 
    select(event_id) |> 
    distinct() |> 
    pull()




all_events_discipline <- purrr::map(event_list$event_id, read_process) |>
    bind_rows()



read_process <- function(event_id) {
    temp <- kickout::fetch_event_url(event_id) |> 
        process_event_test()
    
}

unique(all_events_discipline$competition)



process_event_test <- function(event) {
    temp <- event |>
        janitor::clean_names() |>
        dplyr::filter(discipline %in% c("TRA", "SYN"),
                      !str_detect(competition, "Test|TEST")) |>
        
        #convert Esigma to e_sigma
        dplyr::mutate(judge = dplyr::case_when(
            grepl("^E.*[^\\x00-\\x7F]$", judge) ~ "e_sigma",
            TRUE ~ judge
        )) |>
        dplyr::filter(judge %in% c("T", "D", "H", "e_sigma")) |>
        dplyr::mutate(
            name = paste(given_panel_name, surname),
            name = stringr::str_squish(name)
        ) |>
        dplyr::select(
            -c(
                subtitle,
                number,
                time,
                code,
                external_id,
                date_of_birth,
                sex,
               given_panel_name,
                surname,
               ranked,
               team,
                team_rank,
                team_mark
            )
        ) |>
        dplyr::mutate(
            unique_person = paste0(
                event_uuid,
                discipline,
                competition,
                stage,
                group_number,
                performance_number,
                routine_number,
                name
            )
        )
    
    execution_score <- temp |>
        dplyr::filter(judge == "e_sigma")
    
    other_scores <- temp |>
        dplyr::select(judge, x, unique_person) |>
        dplyr::filter(judge != "e_sigma") |>
        tidyr::pivot_wider(names_from = judge, values_from = x)
    
    
    complete_score <- execution_score |>
        dplyr::left_join(other_scores, by = "unique_person") |>
        dplyr::select(-c(unique_person, judge)) |>
        dplyr::rename(execution = x)
    
    return(complete_score)
}

