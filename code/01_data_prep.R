#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparation of Raw Data
# -> Merging different data sets
# -> Data cleaning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages
groundhog::groundhog.library(c("tidyverse", "magrittr"), date = "2025-02-01")

# load data sets
results <- read.csv("data/raw/results.csv") # -> main data set
races <- read.csv("data/raw/races.csv")
circuits <- read.csv("data/raw/circuits.csv")
drivers <- read.csv("data/raw/drivers.csv")
teams <- read.csv("data/raw/constructors.csv")
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 


# 1. Race Data ------------------------------------------------------------
#__________________________________________________________________________

# add information about the race
#_______________________________
# add information about the circuit to data on race-level 
races %<>% 
  left_join(circuits %>% select(circuitId, circuitRef, name),
            by = "circuitId") %>%
  arrange(year, round) %>% 
  mutate(raceId_ordered = 1:n()) %>% 
  select(raceId, raceId_ordered, year, round, circuitRef, circuit_name = name.y)

# add information about races to main data
results %<>%
  left_join(races,
            by = "raceId") %>% 
  mutate(raceId = raceId_ordered) %>% 
  select(-raceId_ordered)


# add information on driver and constructor
#__________________________________________
# add information on driver
results %<>%
  left_join(drivers %>% select(driverId, driverRef, forename, surname),
            by = "driverId")

# add information on constructor
results %<>%
  left_join(teams %>% select(constructorId, constructorRef, name),
            by = "constructorId")

rm(drivers, teams, circuits, races)
#______________________________________________________________________

# reordering of main data + data manipulation
results %<>%
  # reordering
  select(raceId, year, round, circuitRef, driverRef, constructorRef, grid,
         position, positionOrder, points, laps, milliseconds,
         circuit_name, forename, surname, constructor_name = name) %>%  
  # some data manipulation
  mutate(milliseconds = ifelse(milliseconds == "\\N", NA, milliseconds),
         milliseconds = as.numeric(milliseconds),
         seconds = milliseconds / 1000,
         .after = milliseconds) %>% 
  mutate(DNF = ifelse(position == "\\N", 1, 0),
         position = ifelse(position == "\\N", NA, position),
         position = as.numeric(position)) %>% 
  relocate(DNF,
           .after = seconds) %>% 
  # sort based on season, race, and finishing order
  arrange(year, round, positionOrder)

# remove Indy 500
results %<>%
  filter(!(circuitRef == "indianapolis" & between(year, 1950, 1960))) 

#______________________________________________________________________

results %>% 
  saveRDS("data/processed/race_data.rds")

rm(results)

