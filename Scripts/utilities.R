read_event_data <- function(URL){

  #  URL <- "https://gymnasticsresults.com/api/events"
    
    response <- httr::GET(URL) 
    
  #  if(http_type(response) == "application/json"){
  #      temp <- httr::content(response, as = "text") %>% 
  #          jsonlite::fromJSON()
        
 #       all_events <- temp$event

 #       return(all_events)
 #   }else{
 #       print("no such URL")
 #   }
    
}


process_past_events_overview <- function(event_overview){
    temp <- event_overview %>%
        map(~.[c("event_id","en_name", "sport", "rules", "begin_date", "end_date")]) %>%
        bind_rows() %>% 
        mutate(begin_date = ymd_hms(begin_date),
               end_date = ymd_hms(end_date),
               year = year(begin_date),
               quarter = quarter(begin_date),
               month = month(begin_date)
               ) %>% 
        filter(!is.na(begin_date) & !is.na(end_date)) %>% 
        filter(begin_date < Sys.Date())
    
    return(temp)
}


create_list_events <- function(sport_name, year_name, month_name){
    
    temp <- all_events_overview %>% 
        filter(sport == sport_name,
               year == year_name,
 #              quarter == quarter_name,
               month == month_name
               )
    return(temp)
}


#reads in the initial data based on an event ID
read_competition <- function(event_id){
    temp <- read_csv(event_id, col_types = cols(.default = "c"))
    
    return(temp)
}

#reads in the data from files
read_competition_file <- function(file){
    
#    KTK <- c("Copenhagen", "København", "DEN", "KTK", "GymDanmark")

 
    temp <- read_csv(file, col_types = cols(.default = "c")) %>% 
        clean_names() %>% 
        filter(judge %in% c("E\u03A3"),  #this is E∑
               discipline == "TRA",
   #            str_detect(representing, paste(KTK, collapse = "|"))
               ) #%>% 
#        mutate(name = paste(given_name, surname)) %>% 
#        select(-c(subtitle, number, time,
 #               code, external_id,date_of_birth, sex,
 #               given_name, surname, ranked, 
 #               team, team_rank, team_mark)) %>% 
 #       mutate(unique_person = paste0(stage, group_number, performance_number, 
 #                                   routine_number, name, discipline, event_uuid)) |> 
 #       clean_names()
    
  #  execution_score <- temp %>% 
  #      filter(judge == "E∑")
    
 #  other_scores <- temp %>% 
 #       select(judge, x,unique_person) %>% 
 #       filter(judge != "E∑") %>% 
 #       pivot_wider(names_from = judge, values_from = x)
    
    
 #   complete_score <- execution_score %>% 
 #       left_join(other_scores, by = "unique_person") %>% 
 #       select(-c(unique_person, judge)) %>% 
 #       rename(execution = x)
    
 #   return(complete_score)
}

filter(judge %in% c("T", "D", "H", "E\u03A3"))

?cols

create_clean_competition <- function(competition){
    temp <- competition #%>% 
#        clean_names() %>% 
#        filter(judge %in% c("T", "D", "H", "E∑")) %>% 
#        mutate(name = paste(given_name, surname)) %>% 
#        select(-c(subtitle, number, time,
#                  code, external_id,date_of_birth, sex,
#                  given_name, surname, ranked, 
#                  team, team_rank, team_mark)) %>% 
#        mutate(unique_person = paste0(stage, group_number, performance_number, 
#                                      routine_number, name, discipline, event_uuid))
    
#    execution_score <- temp %>% 
#        filter(judge == "E∑")
    
    #how many columns are coming here?
#    other_scores <- temp %>% 
#        select(judge, x,unique_person) %>% 
#        filter(judge != "E∑") %>% 
#        pivot_wider(names_from = judge, values_from = x)
    
#    complete_score <- execution_score %>% 
#        left_join(other_scores, by = "unique_person") %>% 
#        select(-c(unique_person, judge)) %>% 
#        rename(execution = x)
    
    
    return(temp)
}