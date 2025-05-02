library(esquisse)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)


ktk <- read_csv("Dataout/all_events.csv") |> 
    filter (Event == "Scandinavian Open 2025", 
            Competition == "Junior Boys")

data <- ktk |> 
    pivot_longer(cols = c("Execution", "D", "T", "H"), 
                 names_to = "score_type", 
                 values_to = "score") 

esquisse::esquisser(ktk)



library(ggplot2)



ggplot(ktk) +
 aes(x = Date, y = Execution, colour = Stage) +
 geom_line(linewidth = 0.95) +
 scale_color_hue(direction = 1) +
 labs(title = "Execution") +
 theme_minimal() +
 theme(legend.position = "top", legend.justification = "right")



unique(ktk$is_complete)


data <- data.frame(
    Competitor = rep(c("A", "B", "C", "D"), each = 5),
    Points = c(10, 15, 14, 10, 12, 13, 16, 15, 14, 13, 11, 17, 16, 15, 14, 12, 13, 16, 15, 14)
)

# Create the jitter plot
ggplot(data, aes(x = as.numeric(Competitor), y = Points)) +
    geom_jitter(width = 0.2, height = 0) +  # Adjust width and height for jitter effect
    theme_minimal() +
    labs(title = "Jitter Plot of Competitors' Points",
         x = "Competitor",
         y = "Points") +
    scale_x_continuous(breaks = NULL)  # Remove x-axis labels


# Example data frame
data <- data.frame(
    Competitor = rep(c("A", "B", "C", "D"), each = 5),
    Points = c(10, 15, 14, 10, 12, 13, 16, 15, 14, 13, 11, 17, 16, 15, 14, 12, 13, 16, 15, NA)
)

# Remove rows with NA values in the Points column
data_clean <- na.omit(ktk)

# Optionally, check the range of Points
summary(data_clean$Points)

# Create the jitter plot with continuous x-axis
ggplot(ktk, aes(x = as.numeric(factor(Competitor)), y = Execution)) +
    geom_jitter(width = 0.2, height = 0) +  # Adjust width and height for jitter effect
    theme_minimal() +
    labs(title = "Jitter Plot of Competitors' Points",
         x = "Competitor",
         y = "Points") +
    scale_x_continuous(breaks = NULL)  # Remove x-axis labels



# Create the bar plot
# Create the horizontal bar plot
# Create the horizontal bar plot
ggplot(ktk, aes(x = Stage, y = score, fill = score_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Event ~ Competition, scales = "free_x", space = "free_x") +
    labs(title = "Scores by Event, Competition, and Stage",
         x = "Stage",
         y = "Score",
         fill = "Score Type") +
    theme_minimal() +
    coord_flip()  # Flip coordinates to make it horizontal

# Load necessary library
library(DT)

datatable(iris) |> 


    formatStyle(
        'Petal.Length',
        background = styleColorBar(iris$Petal.Length, 'steelblue'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'left'
    ) 

    






#---------------------

library(tidyverse)
library(DT)

data <- data.frame(
    Event = c('Event1', 'Event2', 'Event3'),
    test = c("1", "2", "3"),
    Competition = c('Comp1', 'Comp2', 'Comp3'),
    Execution = c(13.0, 14.5, 12.8),
    H = c(9.5, 9.7, 9.6),
    D = c(0.0, 1.9, 3.5),
    T = c(10.8, 10.9, 12.4)
)


#TODO how to add label and left align it.  change colour to grey
# Function to create a left-aligned bar
create_bar <- function(value, max_value) {
    percent <- (value / max_value) * 100
    bar_html <- sprintf('<div style="width: %f%%; background-color: #0000FF; height: 20px;"></div>', percent)
    return(bar_html)
}


# Apply the function to the Execution column
data <- ktk %>%
        mutate(H = create_bar(H, max(H)))

#    mutate(H = create_bar(H, max(H))) |> 
#    mutate(D = create_bar(D, max(D))) |>
#    mutate(T = create_bar(T, max(T))) |>
#    mutate(Execution = create_bar(Execution, max(Execution)))

# Create the datatable with escape = FALSE to render HTML
datatable(data, escape = FALSE)


#---------------CREATE TABLE WITH DETAILS
data <- ktk |> 
    select(Competitor, H, D, T) 

datatable(data, escape = FALSE)
    

########################

library(readr)
library(dplyr)
library(reactable)
library(shiny)

events <- read_csv("Dataout/all_events.csv")


detailed_data <- events |> 
    select(Date, Event,Event_Year, Competition, Competitor, Country, Club, Stage, Total, Mark, Elements, Execution, T, D, H,
           s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, l, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, is_complete) 



renderReactable({
    # Filter the data using dplyr
    filtered_data <- detailed_data |>
        filter(
            Competitor == input$Competitor_1,
            Stage %in% input$Stage_1,
            is_complete == input$is_complete_1
        ) |>
        rename(Year = Event_Year) |>
        select(Year, Event, everything())  # Keep t1–t10 for sparklines
    
    # Color mapping function
    color_mapping <- function(value) {
        alpha <- max(0, min(1, 0.1 * (value - 1)))
        sprintf("rgba(0, 128, 128, %.2f)", alpha)
    }
    
    # Define columns to style: s1-s10 and l
    score_columns <- c(paste0("s", 1:10), "l")
    
    # Styled columns
    styled_cols <- setNames(
        lapply(score_columns, function(col) {
            colDef(
                style = function(value) {
                    list(
                        background = color_mapping(value),
                        textAlign = "center"
                    )
                }
            )
        }),
        score_columns
    )
    
    # Add sparkline column
 #   styled_cols[["Trend"]] <- colDef(
 #       cell = function(rowIndex) {
 #           values <- as.numeric(filtered_data[rowIndex, paste0("t", 1:10)])
 #           sparkline(values, lineColor = "teal", fillColor = "lightblue")
 #       },
 #       name = "Trend",
 #       html = TRUE,
 #       align = "center"
 #   )
    
    reactable(
        filtered_data,
        columns = styled_cols,
        pagination = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, nrow(filtered_data))
    )
})


unique
