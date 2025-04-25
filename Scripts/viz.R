library(esquisse)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)


ktk <- read_csv("Dataout/all_events.csv") |> 
    filter (str_detect(Competitor, "Grace"),
            Discipline == "TRA")

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
