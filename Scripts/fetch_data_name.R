library(kickout)
library(dplyr)
library(stringr)
library(readr)
library(janitor)


START_YEAR <- 2023

DISCIPLINE <- c("TRA")

#NAMES = c("Grace", "Vjana", "Lotta")
NAMES = c("Lotta", "Grace", "Uljana")

NAMES_FULL = c("Lotta Liesirova", "Grace Matembele-Lyon", "Uljana", "Lotta Liesiriva",
          "Lotta LIESIROVA",  "Uljana Tinofeičika", "Uljana Timofeichika", 
          "Uljana Timofeičika")


KEEP_NAMES <- paste(NAMES, collapse = "|")
KEEP_NAMES_FULL <- paste(NAMES_FULL, collapse = "|")


# FETCH DATA --------------------------------------------------------


# Fetch list of competitions
event_list <- kickout::fetch_past_event_list() |> 
    kickout::process_event_list("Trampoline") |> 
    
    #Use base R to filter dates - avoid calling the lubridate library
    filter(as.integer(format(begin_date, "%Y")) == START_YEAR #,
         #  str_detect(en_name, KEEP_EVENTS)
    ) 


events_2023 <- purrr::map(event_list$event_id, ~ kickout::fetch_event_url(.x, event_list, DISCIPLINE)) |>
    bind_rows()


# Save 2025 events
write_csv(events_2024, "Data/raw_events_2024.csv")


raw_events_2023 = read_csv("Data/raw_events_2023.csv")
raw_events_2024 = read_csv("Data/raw_events_2024.csv")
raw_events_2025 = read_csv("Data/raw_events_2025.csv")

total_data = bind_rows(raw_events_2024, raw_events_2025) |> 
    clean_names() 

#Find NAMES in either given_panel_name or surname
get_right_rows_first_pass <- total_data |> 
    filter(str_detect(str_to_lower(given_panel_name), 
                      str_to_lower(KEEP_NAMES)) |
               str_detect(str_to_lower(surname), 
                          str_to_lower(KEEP_NAMES))
           )




g_competition <-  get_right_rows_first_pass |> 
    mutate(name = paste(given_panel_name, surname)) |> 
    dplyr::mutate(
        
        execution = dplyr::case_when(discipline == "TRA" ~ as.numeric(execution)/10,
                                     TRUE ~ as.numeric(execution)),
        t = dplyr::case_when(discipline == "TRA" ~  as.numeric(t)/1000,
                             TRUE ~ as.numeric(t)),
        mark_total = dplyr::case_when(discipline == "TRA" ~ as.numeric(mark_total)/ 1000,
                                      TRUE ~ as.numeric(mark_total)),
        mark = dplyr::case_when(discipline == "TRA" ~ as.numeric(mark)/ 1000,
                                TRUE ~ as.numeric(mark)),
        h = dplyr::case_when(discipline == "TRA" ~ as.numeric(h)/10,
                             TRUE ~ as.numeric(h)),
        h = h / 10,
        d = dplyr::case_when(discipline  == "TRA" ~ as.numeric(d)/10,
                             TRUE ~ as.numeric(d))
    ) |> 
    dplyr::rename(Event = title,
                  Competitor = name,
                  Birth_Year = date_of_birth,
                  Club = representing,
                  
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
           Birth_Year = str_extract(Birth_Year, "\\d{4}"),
           Event_Year = str_extract(Date, "\\d{4}")
    )|> 
    select(event_uuid, Date, Event_Year, Event,   Discipline, Competition, Competitor, Birth_Year, Club,  Stage,
           Rank, Total, Mark, Elements, Execution, t, h, d, everything()) |> 
    arrange(desc(Date), Discipline, Competition, group_number, performance_number, Competitor) |> 
    select(-c(group_number, performance_number, athlete_number, surname, given_panel_name, rules, is_complete, event_uuid,
              Discipline)) |> 
    distinct() |> 
    filter(str_detect(str_to_lower(Competitor), 
                      str_to_lower(KEEP_NAMES_FULL))
    )

write_csv(g_competition, "Data/filtered_competition_data_2024_2025.csv")

unique(g_competition$Competitor)

colnames(g_competition)
    
