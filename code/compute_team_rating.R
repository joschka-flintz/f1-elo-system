# Compute Constructors' Performances
#____________________________________
# the idea is to create a variable that indicates the performance of the 
# constructor to allow for comparisons of drivers across teams
# -> for each race, get the average finishing position of all cars of a
#    constructor (only cars that finished the race)
# -> set it relative to the number of cars that have finished the race
# -> for each race, compute constructor's average relative position for the 
#    three races before and after the race, and the race itself (7 races) to 
#    get indicator for constructor's general performance level 
#' @param data Race data. Output from 01_data_prep.R
#' @param race_ID name of the variable with unique race identifier
#' @param constructor_ID name of the variable with unique constructor identifier
#' @param j see Readme
#' @param only_top_2 in case of races in which more than two cars of a team
#'                   have finished, compute average team position only using
#'                   the two best positions

compute_team_rating <- function(data,
                                race_ID = "raceId",
                                constructor_ID = "constructorRef",
                                j = 3,
                                only_top_2 = FALSE) {

  # number of drivers that finished a race
  n_finishes <-
    data %>% 
    group_by(!!sym(race_ID)) %>% 
    summarize(n_finishes = max(position, na.rm = TRUE))
  
  # constructors' average finishing position
  if (!only_top_2) {
    
    cons_finish <-
      data %>% 
      group_by(!!sym(race_ID), year, !!sym(constructor_ID)) %>% 
      summarize(avg_position = mean(position, na.rm = TRUE))
    
  } else {
    
    cons_finish <-
      data %>% 
      arrange(!!sym(race_ID), !!sym(constructor_ID), position) %>% 
      group_by(!!sym(race_ID), year, !!sym(constructor_ID)) %>% 
      filter(row_number() %in% 1:2) %>% 
      summarize(avg_position = mean(position, na.rm = TRUE))
    
  }
  
  # add info on number of finishers
  cons_finish %<>%
    left_join(n_finishes, by = race_ID) %>% 
    mutate(rel_avg_position = avg_position / n_finishes)
  
  # compute constructor's average relative position for the 7 races
  cons_finish %<>% 
    group_by(year, !!sym(constructor_ID)) %>% 
    mutate(rolling_rel_avg_position = 
             map_dbl(row_number(), function(i) {
               # Define window: i-3 to i+3 (including current row)
               indices <- (i - j):(i + j)
               # Keep only valid indices
               valid_indices <- indices[indices >= 1 & indices <= n()]
               # Sum the values at valid indices
               mean(rel_avg_position[valid_indices], na.rm = TRUE)
             })
    ) %>% 
    ungroup() %>% 
    rename("cons_avg_position" = "avg_position",
           "cons_rel_avg_position" = "rel_avg_position",
           "cons_rolling_rel_avg_position" = "rolling_rel_avg_position")
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  
  # merge with main data
  data %<>%
    left_join(cons_finish %>% 
                select(all_of(c(race_ID, constructor_ID)), n_finishes,
                       starts_with("cons_")),
              by = c(race_ID, constructor_ID))
  
  return(data)
  
}
