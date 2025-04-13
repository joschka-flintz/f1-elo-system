#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute Elo System
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# top ----

# load packages
groundhog::groundhog.library(c("tidyverse", "magrittr", "ggthemes"), 
                             date = "2025-02-01")

# load functions
source("code/compute_team_rating.R")
source("code/compute_drivers_elo.R")
source("code/compute_driver_elo_change_in_race.R")

# color palette
palette_11 <- c(
  "#1b9e77",  # Teal green
  "#d95f02",  # Orange
  "#7570b3",  # Purple
  "#e7298a",  # Pink
  "#66a61e",  # Olive green
  "#e6ab02",  # Mustard yellow
  "#a6761d",  # Brown
  "#666666",  # Dark grey
  "#1f78b4",  # Blue
  "#b2df8a",  # Light green
  "#fb9a99"   # Light red
)

# own ggtheme
own_theme <-
  theme_fivethirtyeight() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 9),
        axis.title.y = element_text(margin = margin(t = 0, r = 10,
                                                    b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0,
                                                    b = 0, l = 0))
  )
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# load data
data_base <- readRDS("data/processed/race_data.rds")
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

elo_output <- compute_drivers_elo(data_base,
                                  race_ID = "raceId",
                                  driver_ID = "driverRef",
                                  constructor_ID = "constructorRef",
                                  elo_start_value = 1000,
                                  elo_pot = 20,
                                  z = 500,
                                  r = 3,
                                  j = 3,
                                  only_top_2 = FALSE,
                                  avg_elo_change_of_duels = TRUE)

# final elo standing
drivers_elo <- elo_output$drivers_final_elo
# race data with elo after each race
data <- elo_output$race_data
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# find drivers elo changes in a given race
#_________________________________________
drivers_elo_change <- 
  compute_driver_elo_change_in_race(
    driver = "hamilton",
    race_data = 
      data[data$raceId == 954 & !is.na(data$position),],
    elo_data = 
      data %>% 
      filter(raceId < 954 & 
               driverRef %in%
               data[data$raceId == 954 & !is.na(data$position),]$driverRef) %>% 
      group_by(driverRef) %>% 
      filter(raceId == max(raceId)),
    z = 500,
    r = 3,
    elo_pot = 20
  )

# total elo change
sum(drivers_elo_change$driver_elo_delta)
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


# Top 20
#_______________________________________

# final elo
drivers_elo %>% 
  filter(row_number() <= 20) %>% 
  left_join(data %>% 
             # filter(!is.na(position)) %>% 
              count(driverRef) %>% 
              rename("n_races" = "n"), 
            by = "driverRef") %>% 
  left_join(data %>% 
              distinct(driverRef, .keep_all = TRUE) %>% 
              select(driverRef, name = surname),
            by = "driverRef") %>% 
  mutate(elo = round(elo)) %>% 
  select(name, elo, n_races)

# Peak Elo Ratings
data %>% 
  group_by(driverRef) %>% 
  mutate(race_number = 1:n()) %>%
  filter(elo == max(elo)) %>% 
  filter(race_number == max(race_number)) %>% 
  ungroup() %>% 
  arrange(-elo) %>% 
  #filter(row_number() <= 15) %>% 
  select(surname, max_elo = elo, race_number)

# worst elo
drivers_elo %>% 
  filter(row_number() > n() - 20) %>% 
  left_join(drivers_n_races, by = "driverRef") %>% 
  left_join(data %>% 
              distinct(driverRef, .keep_all = TRUE) %>% 
              select(driverRef, name = surname),
            by = "driverRef") %>% 
  mutate(elo = round(elo)) %>% 
  select(name, elo, n_races)
#____________________________________________________

# Focus on some of the greatest drivers, irrespective of their standing in the
# final standing (with a bit of recency bias)
drivers_of_interest <- 
  c("max_verstappen", "hamilton", "alonso", "michael_schumacher",
    "fangio", "clark", "hunt", "vettel", "lauda", "senna", "moss")
code <-
  c("VER", "HAM", "ALO", "MSC", "FAN", "CLA", "HNT", "VET", "LAU", "SEN", "MOS")

data %>% 
  filter(driverRef %in% drivers_of_interest) %>% 
  left_join(tibble(driverRef = drivers_of_interest,
                   code = code),
            by = "driverRef") %>% 
  group_by(driverRef) %>% 
  mutate(race_number = 1:n()) %>%
  ggplot() +
  geom_line(aes(x = race_number, y = elo, color = code)) +
  own_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white")
        ) +
  xlab("Race") +
  ylab("Elo Rating") +
  scale_color_manual(values = palette_11)
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# elo developments
#____________________________________

data %>% 
  filter(driverRef == "ascari") %>% 
  ggplot() +
  geom_line(aes(x = raceId, y = elo)) +
  own_theme

data %>% 
  filter(driverRef %in% c("max_verstappen", "hamilton")) %>% 
  ggplot() +
  geom_line(aes(x = raceId, y = elo, color = driverRef)) +
  own_theme


