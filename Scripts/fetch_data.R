#PROJECT:  Kickout
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

load("Data/datasets.rda")  ## for standardizing names, etc

# GLOBAL VARIABLES -------------------------------------------------

START_YEAR <- 2023


# Define each pattern on a new line for readability
EVENTS <- c("DanCup", "Scandinavian Open", "Forbunds", "DM Senior",
            "Nordic", "Frivolten", "FIG", "DM u12")

# Combine patterns into a single string with | as the separator
KEEP_EVENTS <- paste(EVENTS, collapse = "|")


# FUNCTIONS-------------------------------------
#' Standardizes names using a dataset in the kickout library.
#'
#' @param event A data frame containing event data with columns: title, surname, given_panel_name.
#'
#' @return A data frame with standardized names.
#' @keywords internal
#' @examples
#' \dontrun{
#' clean_names(event)
#' }
clean_names <- function(event){
    
    temp <- event |>
        dplyr::mutate(name = dplyr::case_when(title == "DM Senior 2024" ~ paste(surname, given_panel_name, sep = " "),
                                              TRUE ~ paste(given_panel_name, surname, sep = " ")
        ),
        name = stringr::str_to_title(name),
        name = stringr::str_squish(name),
        ) |>
        dplyr::left_join(data_name_variation_map, by = "name") |>
        dplyr::mutate(name = dplyr::case_when(!is.na(name_1) ~ name_1,
                                              TRUE ~ name
        )) |>
        dplyr::select(-c(name_1, surname, given_panel_name))
    
    
    return(temp)
    
}

#' Standardizes club and country using a dataset in the kickout library
#'
#' @param event Event data.
#'
#' @return A data frame with standardized country and club names.
#' @keywords internal
#' @examples
#' \dontrun{
#' clean_representing(event)
#' }
clean_representing <- function(event){
    
    #update club and country
    
    temp <- event |>
        
        
        dplyr::left_join(data_representing_map, by = "representing") |>
        dplyr::mutate(country = dplyr::case_when(is.na(country) ~ clean_representing_col(representing, title,1),
                                                 TRUE ~ country),
                      club = dplyr::case_when(is.na(club) ~ clean_representing_col(representing, title,2),
                                              TRUE ~ club)
        ) |>
        dplyr::select(-c(representing))
    
    return(temp)
    
    
}

#' Standardizes country and clubs for interntional competitions where only country is available using a dataset in the kickout library
#'
#' @param event event data
#'
#' @returns standardized country and club names
#' @keywords internal
#'
#' @examples
#'  \dontrun{
#'    clean_international_name(event)
#' }
clean_international_name <- function(event) {
    
    temp <-  event |>
        #fix names from large international events without a club
        dplyr::left_join(data_name_club_map, by = "name") |>
        dplyr::mutate(club = dplyr::case_when(!is.na(club_1) ~ club_1,
                                              TRUE ~ club),
                      country = dplyr::case_when(!is.na(country_1) ~ country_1,
                                                 TRUE ~ country)
        ) |>
        dplyr::select(-c(club_1, country_1))
    
    return(temp)
    
}

#' Create a club and country based on representing column
#'
#' @param representing column from sporttech.  Can contain country, club or both country and club
#' @param title title of the event
#' @param col 1 or 2.  1 for country and 2 for club
#'
#' @returns dataset with representing, country and club
#' @keywords internal
#'
#' @examples
#'  \dontrun{
#'    clean_representing_col(representing, title, col)
#' }
clean_representing_col <- function(representing, title, col) {
    rep_type <- dplyr::case_when(
        stringr::str_detect(representing, ",") ~ "country_club",
        stringr::str_detect(title, "FIG") ~ "country",
        TRUE ~ "club"
    )
    
    country <- dplyr::case_when(
        rep_type == "country" ~ representing,
        rep_type == "country_club" ~ stringr::str_split_i(representing, ",", 1),
        TRUE ~ "unknown"
    )
    
    club <- dplyr::case_when(
        rep_type == "club" ~ representing,
        rep_type == "country_club" ~ stringr::str_split_i(representing, ", ", 2),
        TRUE ~ "unknown"
    )
    
    if(col == 1)
        return (country)
    else
        return(club)
    
}



# FETCH DATA --------------------------------------------------------


# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    process_event_list("Trampoline") |> 
    
    #Use base R to filter dates - avoid calling the lubridate library
    filter(as.integer(format(begin_date, "%Y")) >= START_YEAR,
           str_detect(en_name, KEEP_EVENTS)
    ) 



# CREATE DATASETS ------------------------------------------------------------------

# create dataset for given IDs and rename columns
events <- purrr::map(event_list$event_id, ~ kickout::fetch_event_url(.x, event_list, c("TRA", "SYN"))) |>
    bind_rows()|> 
    clean_names() |>
    clean_representing() |> 
    clean_international_name() |> 
    dplyr::rename(Event = title,
                  Competitor = name,
                  Year = date_of_birth,
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
                  Rank = rank, 
    ) |> 
    mutate(Date = as.Date(Date),
           Stage = case_when(str_detect(Stage,"inal") ~ Stage,
                             TRUE ~ paste(Stage, routine_number, sep ="_")
           ),
           is_complete = case_when(Elements == 10 ~ "Complete",
                                   TRUE ~ "Incomplete"
           ),
           Year = str_extract(Year, "\\d{4}")
    )|> 
    select(event_uuid, Date, Event,   Discipline, Competition, Competitor, Year, Club, Country, Stage,
           Rank, Total, Mark, Elements, Execution, T, H, D, everything()) |> 
    arrange(desc(Date), Discipline, Competition, group_number, performance_number, Competitor) |> 
    select(-c(group_number, performance_number))

write_csv(events,"Dataout/all_events.csv")



