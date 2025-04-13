#' @description Function that computes a driver's elo change in each duel with
#'              another driver in a given race
#' @param driver driver for which to compute elo change
#' @param race_data data with race results and further information (result from
#'                  01_data_prep.R). Has to only contain one race and no NA for
#'                  position.
#' @param elo_data data with elo values for drivers before the race of interest
#' @param race_ID name of the variable with unique race identifier
#' @param driver_ID name of the variable with unique driver identifier
#' @param elo_pot number of elo points that are played for in each duel between
#'                two drivers
#' @param z parameter for importance of current elo rating for drivers' elo 
#'          input share 
#' @param r parameter for importance of inter-team comparisons in elo rating

compute_driver_elo_change_in_race <- function(driver,
                                              race_data,
                                              elo_data,
                                              driver_ID = "driverRef",
                                              race_ID = "raceId",
                                              z = 800,
                                              r = 2,
                                              elo_pot = 10,
                                              output_df = TRUE) {
  
  
  if (length(unique(race_data[[race_ID]])) > 1) {
    
    stop("More than one race in race data!")
    
  }
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  # information on driver's finishing position, elo, and constructor rating
  #________________________________________________________________________
  # driver position 
  driver_position <- 
    race_data$position[race_data[[driver_ID]] == driver]
  # driver elo before the race 
  driver_elo <- 
    elo_data$elo[elo_data[[driver_ID]] == driver]
  # rating of team 
  driver_team_rating <- 
    race_data$cons_rolling_rel_avg_position[race_data[[driver_ID]] == driver]
  
  # elo input dependent on driver's elo
  raw_driver_elo_input <- elo_pot^(driver_elo/z)
  
  # all opponents that have finished the race
  names_of_opponents <-
    race_data %>%
    filter(!!sym(driver_ID) != driver) %>% 
    pull(!!sym(driver_ID))
  
  # iterate through all opponents to get elo changes from the duels between
  # the driver and the opponents
  #________________________________________________________________________
  driver_elo_change <-
    names_of_opponents %>% 
    map(function(opponent) {
      
      # information on opponent's finishing position, elo, and constructor rating
      #__________________________________________________________________________
      opponent_position <- 
        race_data$position[race_data[[driver_ID]] == opponent]
      opponent_elo <- elo_data$elo[elo_data[[driver_ID]] == opponent]
      opponent_team_rating <- 
        race_data$cons_rolling_rel_avg_position[race_data[[driver_ID]] == opponent]
      
      # elo input dependent on opponent's elo
      raw_opponent_elo_input <- elo_pot^(opponent_elo/z)
      
      # determine winner of duel
      #_________________________________________________
      driver_won <- driver_position < opponent_position
      
      # determine elo input shares of driver and opponent
      #__________________________________________________
      
      # sum of driver's and opponent's elo input
      combined_elo_input <- raw_driver_elo_input + raw_opponent_elo_input
      
      # elo input shares for driver and opponent
      driver_elo_input_share <- raw_driver_elo_input / combined_elo_input
      opponent_elo_input_share <- raw_opponent_elo_input / combined_elo_input
      
      # compute elo input by multiplying elo pot with elo input shares
      driver_elo_input <- elo_pot * driver_elo_input_share
      opponent_elo_input <- elo_pot * opponent_elo_input_share
      
      
      # determine weighting factor based on teams' rating
      #__________________________________________________
      # -> team rating divided by sum of both team ratings
      better_cons_K <- 
        min((driver_team_rating^r / (driver_team_rating^r + opponent_team_rating^r)),
            (opponent_team_rating^r / (driver_team_rating^r + opponent_team_rating^r)))
      
      worse_cons_K <- 
        max((driver_team_rating^r / (driver_team_rating^r + opponent_team_rating^r)),
            (opponent_team_rating^r / (driver_team_rating^r + opponent_team_rating^r)))
      
      # if driver of better team has won
      if ((driver_team_rating <= opponent_team_rating & driver_won) |
          (opponent_team_rating <= driver_team_rating & !driver_won)) {
        
        K <- better_cons_K
        
        # if driver of worse team has won  
      } else {
        
        K <- worse_cons_K
        
      }
      
      # change in elo of driver from this duel
      #_______________________________________
      driver_elo_delta <- 
        (driver_won * elo_pot - driver_elo_input) * K
      #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
      
      # Output
      #____________________________
      
      if (output_df) {
        
        # put elo change in dataframe that provides overview of the duel
        df_driver_elo_delta <- 
          tibble(
            driver = driver,
            driver_team_rating = driver_team_rating,
            driver_elo = driver_elo,
            driver_elo_input = driver_elo_input,
            opponent = opponent,
            opponent_team_rating = opponent_team_rating,
            opponent_elo = opponent_elo,
            opponent_elo_input = opponent_elo_input,
            driver_position = driver_position,
            opponent_position = opponent_position,
            driver_elo_delta = driver_elo_delta
          )
        
        return(df_driver_elo_delta)
        
      } else {
        
        return(driver_elo_delta)
        
      }
    
  
    }) 
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  if (output_df) {
    
    # bind each driver's data frame
    driver_elo_change %<>% bind_rows()
    
  } else {
    
    # sum up vector of elo changes from each duel
    driver_elo_change %<>% unlist() %>% sum()
    
  }

  return(driver_elo_change)
    
}    
    
    
    
    