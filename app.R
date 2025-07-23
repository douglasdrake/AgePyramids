#
#
# Comparing Age Pyramids --------------------------------------------------
#
library(shiny)
library(bslib)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(duckdb))
suppressPackageStartupMessages(library(duckplyr))

options(dplyr.summarise.inform = FALSE)

# Convert table reference to tibble ---------------------------------------

age_query_to_tibble <- function(.query) {
  age_levels <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
  )
  result <- .query %>%
    as_tibble() %>%
    mutate(
      age = ordered(age,
        levels = age_levels,
        labels = age_levels
      ),
      sex = factor(
        sex,
        levels = c("Female", "Male")
      )
    )
  result
}

# Load US data ------------------------------------------------------------

read_us_data <- function() {
  .conn <- dbConnect(drv = duckdb::duckdb())

  duckdb_read_csv(.conn,
    "us_data",
    files = "data/us_age_sex.csv.gz",
    col.types = c(
      year = "INTEGER",
      age = "VARCHAR",
      sex = "VARCHAR",
      popn = "INTEGER"
    )
  )

  query <- dbGetQuery(.conn, "SELECT * FROM us_data") %>%
    age_query_to_tibble()

  dbDisconnect(.conn)

  query
}

.us_age_sex <- read_us_data()

# Load states data --------------------------------------------------------

read_states_data <- function() {
  .conn <- dbConnect(drv = duckdb::duckdb())

  duckdb_read_csv(.conn,
    "states_data",
    files = "data/states_age_sex.csv.gz",
    col.types = c(
      year = "INTEGER",
      state_fips = "VARCHAR",
      state_name = "VARCHAR",
      age = "VARCHAR",
      sex = "VARCHAR",
      popn = "INTEGER"
    )
  )

  query <- dbGetQuery(.conn, "SELECT * FROM states_data") %>%
    age_query_to_tibble()

  dbDisconnect(.conn)

  query
}

.states_age_sex <- read_states_data()

# Helper function ---------------------------------------------------------

# Ref: https://5harad.com/mse125/r/visualization_code.html
millions_units <- function(x) {
  x <- abs(x)
  labels <- ifelse(x < 1000, x, # less than thousands
    ifelse(x < 1e6, paste0(round(x / 1e3, 1), "K"), # in thousands
      ifelse(x < 1e9, paste0(round(x / 1e6, 1), "M"), # in millions
        ifelse(x < 1e12, paste0(round(x / 1e9, 1), "B"), # in billions
          ifelse(x < 1e15, paste0(round(x / 1e12, 1), "T"), # in trillions
            "Big"
          )
        )
      )
    )
  )
  return(labels)
}

# Faceted age pyramids ----------------------------------------------------

plot_faceted_age_pyramids <- function(.data, plot_labs) {
  p <- .data %>%
    mutate(
      signed_sex = if_else((sex == "Male"), -popn, popn)
    ) %>%
    group_by(year, age, sex) %>%
    summarize(total = sum(signed_sex)) %>%
    ggplot(aes(x = age, y = total, fill = sex)) +
    scale_y_continuous(labels = millions_units) +
    scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#D55E00")) +
    facet_wrap("year") +
    theme_minimal(base_size = 14) +
    geom_col() +
    coord_flip() +
    plot_labs
  p
}

# Comparing Select States By Years ----------------------------------------

compare_states_by_year <- function(.data,
                                   states,
                                   years,
                                   scales_vary = "free_x",
                                   plot_labs) {
  # print(length({{ years }}))
  p <- .data %>%
    mutate(signed_sex = if_else((sex == "Male"), -popn, popn)) %>%
    group_by(year, state_name, age, sex) %>%
    summarize(total = sum(signed_sex)) %>%
    ggplot(aes(x = age, y = total, fill = sex)) +
    scale_y_continuous(labels = millions_units) +
    scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#D55E00")) +
    facet_wrap(c("state_name", "year"),
      scales = scales_vary,
      nrow = length({{ states }}), ncol = length({{ years }}),
      labeller = (\(x) (label_value(x, multi_line = TRUE)))
    ) +
    geom_col() +
    coord_flip() +
    theme(plot.title = ggtext::element_textbox_simple()) +
    plot_labs
  p
}


# Define the UI -----------------------------------------------------------

ui <- page_sidebar(

  # Application title
  title = div(
    h1("AgePyramids", style = "font-weight: bold; color: #2c3e50"),
    h4("Exploring Change Across States and Time", style = "color: #7f8c8d")
  ),

  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    helpText(
      "Select a Summay Level to Display."
    ),
    selectInput("summarylevel", "Summary Level", choices = c("US", "State")),
    selectizeInput("years", "Years (Choose up to 3)",
      choices = c("", 2023:1970),
      options = list(maxItems = 3)
    ),
    uiOutput("states_menu"),
    checkboxInput(
      "vary_scale",
      "Vary Scales With State",
      value = FALSE
    ),
    br(),
    actionButton("goplot", "Update Plot"),
  ),
  # Show a plot of the generated distribution
  card(
    plotOutput("age_pyramids")
  )
)


# Server for plotting age pyramids ----------------------------------------

# Define server logic required to draw the age pyramids
server <- function(input, output, session) {
  summarylevel <- reactive({
    summarylevel <- str_to_lower(input$summarylevel)
  })

  # Note that going from State to US does not flush the
  # values last read into input$states but the menu is torn down.
  output$states_menu <- renderUI({
    if (summarylevel() == "us") {
      NULL
    } else {
      selectizeInput(
        "states", "States (Choose up to 3)",
        choices = c("", state.name, "District of Columbia"),
        options = list(maxItems = 3)
      )
    }
  })

  years <- reactive({
    req(input$years)
    years <- as.integer(input$years)
  })

  statesInput <- reactive({
    req(input$summarylevel)
    if (summarylevel() == "state") {
      req(input$states)
      statesInput <- input$states
    } else {
      # this is here to prevent old state names from persisting
      # after the menu for summary level switches back to US.
      # our app tears down the menu but the old state names persist
      # in the app.
      statesInput <- NULL
    }
    statesInput
  })

  vary_scales_states <- reactive({
    # req(input$statesInput)
    # req(input$vary_scales)
    vary_scales_states <- ifelse(input$vary_scales, "free_x", "fixed")
  })

  query <- eventReactive(input$goplot, {
    req(input$years)
    if (summarylevel() == "state") {
      req(statesInput())
      query <- .states_age_sex %>%
        filter((year %in% years()) & (state_name %in% statesInput()))
    } else {
      query <- .us_age_sex %>%
        filter(year %in% years())
    }
    # print(query %>% head(n = 1))
    # print(query %>% distinct(year) %>% count())

    if (summarylevel() == "us") {
      pyr_title <- paste0("United States: ", str_flatten(years(), collapse = ", "))
      age_pyramids <- query %>%
        plot_faceted_age_pyramids(
          plot_labs = labs(
            x = "Age",
            y = "Population (in Millions)",
            fill = "Gender",
            title = pyr_title,
            caption = "Source: U.S. Census Bureau Population Estimates Program"
          )
        )
    } else {
      pyr_subtitle <- ifelse(input$vary_scale, "Scales Vary By State", "")
      pyr_title <- paste0(
        str_flatten(statesInput(), collapse = ", "),
        ": ",
        str_flatten(years(), collapse = ", ")
      )
      age_pyramids <- query %>%
        compare_states_by_year(
          states = statesInput(),
          years = years(),
          scales_vary = ifelse(input$vary_scale, "free_x", "fixed"),
          plot_labs = labs(
            x = "Age",
            y = "Population (K = Thousands, M = Millions)",
            fill = "Gender",
            title = pyr_title,
            # subtitle = pyr_subtitle,
            caption = "Source: U.S. Census Bureau Population Estimates Program"
          )
        )
    }
    age_pyramids
  })

  output$age_pyramids <- renderPlot({
    query()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
