#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Web application to run ELO system
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# top ----

library(shiny)
library(tidyverse)
library(magrittr)
library(DT)
library(ggthemes)
library(shinycssloaders)
library(quarto)

data_base <- readRDS("data/processed/race_data.rds")
source("code/compute_team_rating.R")
source("code/compute_driver_elo_change_in_race.R")
source("code/compute_drivers_elo.R")

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

#quarto::quarto_render("www/info.qmd", output_format = "html")
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



# USER INTERFACE ----------------------------------------------------------
#__________________________________________________________________________

ui <- fluidPage(
  
  titlePanel("F1 Driver ELO System"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2, 
      sliderInput("season_range", 
                  label = "Select Season Range", 
                  min = 1950, 
                  max = 2024,  # Adjust this based on available data
                  value = c(1950, 2024),  # Default range
                  step = 1,  # Steps of 1 year
                  sep = ""),
      numericInput("elo_start", "Initial ELO Value", value = 1000, min = 500, max = 2000),
      numericInput("elo_pot", "ELO Pot", value = 10, min = 1, max = 100),
      numericInput("z", "z (ELO weight)", value = 500, min = 100, max = 2000),
      numericInput("r", "r (Inter-team comparison)", value = 3, min = 1, max = 10),
      checkboxInput("avg_change", "Average ELO change across duels", value = FALSE),
      actionButton("run", "Run ELO System")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Final Rankings", 
                 fluidRow(
                   column(6,
                          h3("Final ELO Ranking"),
                          withSpinner(DTOutput("elo_table"))
                          ),  # First table
                   column(6,
                          h3("Peak ELO Ranking"),
                          withSpinner(DTOutput("elo_peak_table"))
                          )  # Second table
                   )
                 ),
        tabPanel("Race Details",
                 fluidRow(
                   column(3,
                          selectInput("detail_season", "Select Season", choices = NULL),
                          selectInput("detail_round", "Select Round", choices = NULL),
                          selectInput("detail_driver", "Select Driver", choices = NULL),
                          actionButton("generate_detail", "Generate Table")
                   ),
                   column(9,
                          h3("Driver Performance in Selected Race"),
                          withSpinner(DTOutput("race_detail_table"))
                          )
                   )
                 ),
        tabPanel("Driver Comparison Over Time",
                 fluidRow(
                   column(3,
                          sliderInput("plot_season_range",
                                      label = "Select Season Range",
                                      min = 1950,
                                      max = 2024,
                                      value = c(2000, 2024),
                                      step = 1,
                                      sep = "")
                   ),
                   column(9,
                          selectizeInput("plot_drivers",
                                         label = "Select up to 11 Drivers",
                                         choices = NULL,  # will be updated
                                         multiple = TRUE,
                                         options = list(maxItems = 11))
                   )
                 ),
                 fluidRow(
                   column(12, withSpinner(plotOutput("elo_plot",
                                                     height = "550px")))
                 )
        ),
        tabPanel("Driver Comparison Over Career",
                 fluidRow(
                   column(9,
                          selectizeInput("plot_drivers_career",
                                         label = "Select up to 11 Drivers",
                                         choices = NULL,  # will be updated
                                         multiple = TRUE,
                                         options = list(maxItems = 11))
                   )
                 ),
                 fluidRow(
                   column(12, withSpinner(plotOutput("elo_plot_career",
                                                     height = "550px")))
                 )
        ),
        tabPanel("About the ELO System",
                 fluidRow(
                   column(12,
                          tags$iframe(
                            src = "info.html",         # Shiny looks in /www/ automatically
                            width = "100%",
                            height = "2200px",          # Adjust as needed
                            style = "border: none;"
                          )
                   )
                 )
        )
      )
    )
  )
)


# SERVER ------------------------------------------------------------------
#__________________________________________________________________________


server <- function(input, output, session) {
  

  # 1. Run ELO system -------------------------------------------------------
  #__________________________________________________________________________
  
  elo_results <- eventReactive(input$run, {
    
    filtered_data <-
      data_base %>%
      filter(year >= input$season_range[1] & year <= input$season_range[2])
    
    compute_drivers_elo(
      data = filtered_data,
      race_ID = "raceId",
      driver_ID = "driverRef",
      constructor_ID = "constructorRef",
      elo_start_value = input$elo_start,
      elo_pot = input$elo_pot,
      z = input$z,
      r = input$r,
      avg_elo_change_of_duels = input$avg_change
    )
    
  })
  #_________________________________________________________________
  #_________________________________________________________________
  
  # 2. Ranking tables -------------------------------------------------------
  #__________________________________________________________________________
  
  drivers_elo <- reactive({
    
    req(elo_results())  # ensures it's available before proceeding
    
    elo_results()$drivers_final_elo %>% 
    left_join(elo_results()$race_data %>% 
                count(driverRef) %>% 
                rename("n_races" = "n"), 
              by = "driverRef") %>% 
    left_join(elo_results()$race_data %>% 
                distinct(driverRef, .keep_all = TRUE) %>% 
                mutate(name = paste0(forename, " ", surname)) %>% 
                select(driverRef, name),
              by = "driverRef") %>% 
    mutate(elo = round(elo, 2)) %>% 
    select(Driver = name, ELO = elo, Races = n_races)
    
  })
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  
  drivers_peak_elo <- reactive({
    
    req(elo_results())  # ensures it's available before proceeding
    
    elo_results()$race_data %>% 
      group_by(driverRef) %>% 
      mutate(race_number = 1:n(),
             Driver = paste0(forename, " ", surname),
             elo = round(elo, 2)) %>%
      filter(elo == max(elo)) %>% 
      filter(race_number == max(race_number)) %>% 
      ungroup() %>% 
      arrange(-elo) %>% 
      select(Driver, ELO = elo, Race = race_number)
    
  })
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  
  output$elo_table <- renderDT({
    
    datatable(drivers_elo(), 
              options = list(#pageLength = 15,
                              bPaginate = FALSE,
                              #fillContainer = TRUE,
                              info = FALSE,
                              scrollX = FALSE,
                              scrollY = '600px'))
  })
  
  output$elo_peak_table <- renderDT({
    
    datatable(drivers_peak_elo(), 
              options = list(#pageLength = 15,
                             bPaginate = FALSE,
                             info = FALSE,
                             scrollX = FALSE,
                             scrollY = '600px'))
  })
  #__________________________________________________________________
  #__________________________________________________________________
  
  # 3. Race-specific table --------------------------------------------------
  #__________________________________________________________________________
  
  # Populate season options once elo_results is available
  observeEvent(elo_results(), {
    race_data <- elo_results()$race_data
    
    updateSelectInput(session, "detail_season", 
                      choices = sort(unique(race_data$year)))

  })
  
  # Update round options based on selected season
  observeEvent(input$detail_season, {
    req(elo_results(), input$detail_season)
    race_data <- elo_results()$race_data
    
    rounds <- 
      race_data %>%
      filter(year == input$detail_season) %>%
      pull(round) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "detail_round", choices = rounds)
  })
  
  # Update driver options based on selected race
  observeEvent({input$detail_season
                input$detail_round
                }, {
    
    req(elo_results(), input$detail_season, input$detail_round)
    
    race_data <- elo_results()$race_data
    
    drivers <- 
      race_data %>%
      filter(year == input$detail_season &
             round == input$detail_round &
             !is.na(position)) %>% 
      mutate(name = paste(forename, surname)) %>%
      distinct(driverRef, name) %>%
      arrange(name)
    
    updateSelectInput(session, "detail_driver",
                      choices = setNames(drivers$driverRef, drivers$name))
    
  })
  
  # Render a table based on selected season, round, and driver
  output$race_detail_table <- renderDT({
    req(input$generate_detail)
    req(elo_results())
    
    isolate({
      
      selected_race_data <-
        elo_results()$race_data %>%
        filter(year == input$detail_season,
               round == input$detail_round,
               !is.na(position)
               )
      
      # create race 0 with starting elo for computations in first race
      elo_base <- 
        elo_results()$race_data %>% 
        distinct(driverRef, .keep_all = TRUE) %>% 
        mutate(raceId = 0,
               elo = input$elo_start)
      
      selected_elo_data <-
        elo_results()$race_data %>%
        bind_rows(elo_base) %>%
        filter(raceId < unique(selected_race_data$raceId)) %>% 
        filter(driverRef %in% selected_race_data$driverRef) %>% 
        group_by(driverRef) %>% 
        filter(raceId == max(raceId))
      
      drivers_elo_change <- 
        compute_driver_elo_change_in_race(
          driver = input$detail_driver,
          race_data = selected_race_data,
          elo_data = selected_elo_data,
          z = input$z,
          r = input$r,
          elo_pot = input$elo_pot
        )
      
      # make table nicer
      #__________________
      drivers_elo_change %<>%
        mutate(
          across(c(driver_team_rating, opponent_team_rating),
                 ~ round(.x, 3)),
          across(c(driver_elo, opponent_elo, driver_elo_input, 
                   opponent_elo_input, driver_elo_delta),
                 ~ round(.x, 2))
        ) %>% 
        left_join(selected_race_data %>% 
                    distinct(driverRef, .keep_all = TRUE) %>% 
                    mutate(driver_name = paste0(forename, " ", surname)) %>% 
                    select(driverRef, driver_name),
                  by = c("driver" = "driverRef")) %>% 
        left_join(selected_race_data %>% 
                    distinct(driverRef, .keep_all = TRUE) %>% 
                    mutate(opponent_name = paste0(forename, " ", surname)) %>% 
                    select(driverRef, opponent_name),
                  by = c("opponent" = "driverRef")) %>% 
        select(driver_name, driver_team_rating, driver_elo, driver_elo_input,
               opponent_name, opponent_team_rating, opponent_elo, opponent_elo_input,
               driver_position, opponent_position, driver_elo_delta)
      
      datatable(drivers_elo_change,
                options = list(bPaginate = FALSE,
                               searching = FALSE,
                               info = FALSE,
                               scrollY = "600px",
                               scrollX = "600px"),
                escape = FALSE,
                colnames = c("Driver", "Driver<br>Team Rating", "Driver<br>Elo",
                             "Driver<br>Elo Input", 
                             "Opponent", "Opponent<br>Team Rating", "Opponent<br>Elo",
                             "Opponent<br>Elo Input",
                             "Driver<br>Position", "Opponent<br>Position",
                             "Driver<br>Elo Change"))
    })
  })
  #____________________________________________________________________
  #____________________________________________________________________
  
  # 4. ELO over time --------------------------------------------------------
  #__________________________________________________________________________
  
  # Keep plot season range in sync with global one
  observeEvent(input$season_range, {
    updateSliderInput(session, "plot_season_range",
                      min = input$season_range[1],
                      max = input$season_range[2],
                      value = input$season_range)
  })
  
  # Update driver options based on selected plot seasons
  observeEvent({
    input$plot_season_range
    elo_results()
  }, {
    req(elo_results())
    
    race_data <- elo_results()$race_data
    
    drivers_available <- 
      race_data %>%
      filter(year >= input$plot_season_range[1],
             year <= input$plot_season_range[2],
             !is.na(position)) %>%
      distinct(driverRef, forename, surname) %>%
      mutate(name = paste(forename, surname)) %>%
      arrange(name)
    
    choices_named <- setNames(drivers_available$driverRef, drivers_available$name)
    
    updateSelectizeInput(session, "plot_drivers", choices = choices_named, server = TRUE)
  })
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  output$elo_plot <- renderPlot({
    req(elo_results(), input$plot_drivers)
    
    race_data <- elo_results()$race_data
    
    plot_data <- 
      race_data %>%
      filter(driverRef %in% input$plot_drivers,
             year >= input$plot_season_range[1],
             year <= input$plot_season_range[2]) %>%
      mutate(name = paste(forename, surname),
             year_round = paste0(year, ", Round ", round)) %>% 
      arrange(raceId)
    
    
    # Get the unique year-round combinations
    year_round_levels <- unique(plot_data$year_round)
    
    # Display only every n-th tick on the x-axis
    if (input$plot_season_range[1] == input$plot_season_range[2]) {
      
      n_ticks <- 1
      
    } else if ((input$plot_season_range[2] - input$plot_season_range[1]) == 1) {
      
      n_ticks <- 2
      
    } else if ((input$plot_season_range[2] - input$plot_season_range[1]) == 2) {
      
      n_ticks <- 4
      
    } else {
      
      n_ticks <- 10
      
    }
    
    # year-rounds that are displayed on x-axis
    x_ticks <- year_round_levels[seq(1, length(year_round_levels), n_ticks)]
    
    ggplot(plot_data, 
           aes(x = factor(year_round, levels = unique(year_round)), 
               y = elo, color = name, group = name)) +
      geom_line() +
      labs(title = "Driver ELO Over Time",
           y = "ELO") +
      theme_fivethirtyeight() +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.background = element_blank(),
            legend.key = element_rect(fill = "white"),
            axis.title = element_text(size = 16),
            axis.title.y = element_text(margin = margin(t = 0, r = 10,
                                                        b = 0, l = 0)),
            #axis.title.x = element_text(margin = margin(t = 10, r = 0,
            #                                            b = 0, l = 0)),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 14),      # Axis tick labels
            plot.title = element_text(size = 18),     # Plot title
            legend.text = element_text(size = 16)  
            ) +

      scale_color_manual(values = palette_11) +
      scale_x_discrete(breaks = x_ticks)
    
  })
  #____________________________________________________________________
  #____________________________________________________________________
  
  # 5. ELO over career ------------------------------------------------------
  #__________________________________________________________________________
  
  # Update driver options based on selected plot seasons
  observeEvent({
    elo_results()
  }, {
    req(elo_results())
    
    race_data <- elo_results()$race_data
    
    drivers_available <- 
      race_data %>%
      filter(!is.na(position)) %>%
      distinct(driverRef, forename, surname) %>%
      mutate(name = paste(forename, surname)) %>%
      arrange(name)
    
    choices_named <- setNames(drivers_available$driverRef, drivers_available$name)
    
    updateSelectizeInput(session, "plot_drivers_career",
                         choices = choices_named, server = TRUE)
  })
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  output$elo_plot_career <- renderPlot({
    req(elo_results(), input$plot_drivers_career)
    
    race_data <- elo_results()$race_data
    
    plot_data <- 
      race_data %>%
      filter(driverRef %in% input$plot_drivers_career) %>%
      mutate(name = paste(forename, surname)) %>% 
      group_by(name) %>% 
      mutate(race_number = 1:n()) %>% 
      ungroup()
    
    elo_race_zero <-
      plot_data %>% 
      distinct(driverRef, .keep_all = TRUE) %>% 
      mutate(race_number = 0,
             elo = input$elo_start)
    
    plot_data %<>%
      bind_rows(elo_race_zero) %>% 
      arrange(name, race_number)
    
    
    ggplot(plot_data, 
           aes(x = race_number, 
               y = elo, color = name)) +
      geom_line() +
      labs(title = "Driver ELO Over Career",
           y = "ELO",
           x = "Race Number") +
      theme_fivethirtyeight() +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.background = element_blank(),
            legend.key = element_rect(fill = "white"),
            axis.title = element_text(size = 16),
            axis.title.y = element_text(margin = margin(t = 0, r = 10,
                                                        b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0,
                                                        b = 0, l = 0)),
            axis.text = element_text(size = 14),      # Axis tick labels
            plot.title = element_text(size = 18),     # Plot title
            legend.text = element_text(size = 16)  
      ) +
      scale_color_manual(values = palette_11)
    
  })
  
}

shinyApp(ui = ui, server = server)
