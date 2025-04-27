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
