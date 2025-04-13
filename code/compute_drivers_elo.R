#' @description Function that computes the drivers' elo
#' @returns Returns list with two objects: drivers_final_elo contains elo rating
#'          of all drivers after the last race, race_data is equal to the data
#'          input but with updated elo values that show a driver's elo after a
#'          given race
#' @param data Race data. Output from 01_data_prep.R
#' @param race_ID name of the variable with unique race identifier
#' @param driver_ID name of the variable with unique driver identifier
#' @param constructor_ID name of the variable with unique constructor identifier
#' @param elo_start_value initial elo value of all drivers before their first race
#' @param elo_pot number of elo points that are played for in each duel between
#'                two drivers
#' @param z parameter for importance of current elo rating for drivers' elo 
#'          input share 
#' @param r parameter for importance of inter-team comparisons in elo rating
#' @param j number of races before and after the race considered
#' @param avg_elo_change_of_duels if TRUE, computes a driver's average elo 
#'                                change across all duels in a race. if FALSE,
#'                                takes the sum of elo changes of all duels in
#'                                a race   

compute_drivers_elo <- function(data,
                                race_ID = "raceId",
                                driver_ID = "driverRef",
                                constructor_ID = "constructorRef",
                                elo_start_value = 1000,
                                elo_pot = 10,
                                z = 800,
                                r = 2,
                                avg_elo_change_of_duels = FALSE,
                                ...) {
  
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
  
  data <- compute_team_rating(data = data,
                              race_ID = race_ID,
                              constructor_ID = constructor_ID,
                              ...)
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  # create empty column for elo rating
  data %<>% mutate(elo = NA, .after = driver_ID)
  
  # create dataframe with drivers' elo to get updated after each race
  # -> initialized with elo of 1000
  drivers_elo <-
    data %>% 
    distinct(!!sym(driver_ID)) %>% 
    mutate(elo = 1000,
           recent_elo_change = NA, 
           !!sym(race_ID) := 0)
  
  # create empty list that stores results of each race with updated elo vaues
  data_list <- list()
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  # iterate through all races
  for (race in unique(data[[race_ID]])) {
    
    print(paste0("Race ", race))
    
    # filter out one race at a time
    data_race <- data %>% filter(!!sym(race_ID) == race)
    
    # keep only those drivers that have finished the race
    # -> keep only best position of drivers who have finished multiple times 
    df_race_finished <-
      data_race %>% 
      filter(!is.na(position)) %>% 
      select(all_of(c(driver_ID, constructor_ID)), 
                    cons_rolling_rel_avg_position, position) %>% 
      group_by(!!sym(driver_ID)) %>% 
      arrange(position) %>% 
      distinct(!!sym(driver_ID), .keep_all = TRUE) %>% 
      ungroup() %>% 
      mutate(drivers_elo_change = NA)
    
    # iterate through each driver who has finished the race to compute his
    # elo change
    race_elo_changes <-
      df_race_finished[[driver_ID]] %>% 
      map(function(driver) {
        
        # drivers elo change in each of his duels
        driver_elo_change <-
          compute_driver_elo_change_in_race(driver = driver,
                                            race_data = df_race_finished,
                                            elo_data = drivers_elo,
                                            driver_ID = driver_ID,
                                            race_ID = race_ID,
                                            z = z,
                                            r = r,
                                            elo_pot = elo_pot,
                                            output_df = FALSE)
        
        # set sum of gained elo points relative to the number of duels
        if (avg_elo_change_of_duels) {
          
          n_opponents <-
            df_race_finished %>%
            filter(!!sym(driver_ID) != driver) %>% 
            nrow()
          
          driver_elo_change <- driver_elo_change / n_opponents
          
        }
        #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
        
        return(driver_elo_change)
        
      }) %>% unlist()
    
    df_race_finished$drivers_elo_change <- race_elo_changes
    #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    
    # update dataframe with updated elo of drivers
    #_____________________________________________
    drivers_elo %<>% 
      left_join(df_race_finished %>% select(all_of(driver_ID), drivers_elo_change),
                by = driver_ID) %>% 
      mutate(recent_elo_change = drivers_elo_change) %>% 
      select(-drivers_elo_change) %>% 
      mutate(
        recent_elo_change = ifelse(is.na(recent_elo_change), 0, recent_elo_change),
        elo = elo + recent_elo_change,
        !!sym(race_ID) := race
      )
    
    # add updated elo to individual race data frame
    data_race %<>% 
      left_join(drivers_elo %>% select(-recent_elo_change),
                by = c(driver_ID, race_ID)) %>% 
      mutate(elo.x = elo.y) %>% 
      rename("elo" = "elo.x") %>% 
      select(-elo.y)
    
    # append individual race data frame to list that contains all individual
    # race data frames
    data_list[[race]] <- data_race
    
  }
  #__________________________________________________________________
  
  # turn list with individual race data to dataframe
  # -> basically, equal to input data but with updated elo values
  final_data <- data_list %>% bind_rows()
  
  # create output
  #_______________
  
  output <-
    list(
      drivers_final_elo = drivers_elo %>% arrange(-elo),
      race_data = final_data
    )
  
  return(output)

}
