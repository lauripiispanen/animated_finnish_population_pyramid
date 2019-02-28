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

label_format <- function(x) {
  format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

animate(ggplot(population_data, aes(x = AgeGroup, fill = Gender)) +
  geom_col(data = subset(population_data, Gender == "Naiset"), aes(y=Population)) +
  geom_col(data = subset(population_data, Gender == "Miehet"), aes(y=Population*(-1))) +
  coord_flip() +
  labs(title = 'Population distribution of Finland (year: {closest_state})') +
  guides(fill = FALSE) +
  scale_y_continuous(labels = label_format) +
  annotate("text", label="Men", x = 1, y = -250000) +
  annotate("text", label="Women", x = 1, y = 250000) +
  transition_states(Year), nframes = 2 * (max(population_data$Year) - min(population_data$Year)))