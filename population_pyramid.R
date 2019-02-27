library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

population_data <- t(read.xlsx("vaerak_002_201700.xlsx", sheetIndex = 1, startRow = 2, endRow = 158, header = FALSE))
colnames(population_data) <- as.character(population_data[1,])

population_data <- data.frame(population_data) %>%
  slice(2:n()) %>%
  select(-one_of(c("NA..2"))) %>%
  rename(Gender = NA., AgeGroup = NA..1) %>%
  fill(Gender) %>%
  gather("Year", "Population", -Gender, -AgeGroup) %>%
  mutate(Year = gsub("X([0-9]{4})", "\\1", Year),
         Year = as.numeric(Year),
         Population = as.numeric(Population),
         AgeGroup = relevel(relevel(AgeGroup, "5 - 9"), "0 - 4"))

animate(ggplot(population_data, aes(x = AgeGroup, fill = Gender)) +
  geom_col(data = subset(population_data, Gender == "Naiset"), aes(y=Population)) +
  geom_col(data = subset(population_data, Gender == "Miehet"), aes(y=Population*(-1))) +
  coord_flip() +
  transition_states(Year), nframes = 200)