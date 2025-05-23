library(dplyr)
library(readr)

data <- starwars

write_csv(data, "Dataout/starwars.csv")


#data_from_github <- read_csv("https://raw.githubusercontent.com/KirstinLyon/jump_metrics/refs/heads/main/Dataout/starwars.csv")
