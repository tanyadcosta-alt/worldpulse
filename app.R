# Start by selecting all the relevant packages in the files pane > packages
# Then load all the relevant libraries below
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(leaflet)
library(lubridate)
library(sf) # For handling spatial data
library(jsonlite)
library(tidyr)
library(stringr)
library(rnaturalearth) # For world country data
library(rnaturalearthdata) # For higher resolution world country data
library(scales) # For pretty_breaks and comma formatting in ggplot
library(shinythemes)
library(RColorBrewer)
library(htmltools)


# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
# Read your data files
pov <- read_csv("povclean.csv") #read file on % of population under poverty line
gdp <- read_csv("gdpclean.csv") #read file on GDP

# // pivot longer the gdp file so that each data point is delineated by the year
gdp_long <- pivot_longer(
  gdp,
  cols = '1961':'2024', # Explicitly define columns to pivot
  cols_vary = "fastest",
  names_to = "Year", # New column for year
  values_to = "value", # New column for GDP value
  names_repair = "check_unique" # Ensure unique column names after pivoting
)

# // clean the gdp long file: remove rows where GDP value is NA
gdp_clean <- gdp_long %>%
  filter(!is.na(value))

# // rename the "value" column to "gross_domestic_product" for clarity
names(gdp_clean)[names(gdp_clean) == "value"] <- "gross_domestic_product"

# // change Year to numeric type (important for plotting)
gdp_clean$Year <- as.numeric(gdp_clean$Year)

# // full join gdp_clean with pov
# Now joining only by "Country" and "Year"
gdp_pov2 <- full_join(
  gdp_clean,
  pov,
  by = c("Country", "Year"), # Using "Country" and "Year" columns for join
  suffix = c(".x", ".y")
)

# // read parties.csv
parties <- read_csv("partiesclean.csv")

# // final join of combined GDP/Poverty data with parties data
# Now joining only by "Country" and "Year"
final_join <- inner_join(
  gdp_pov2,
  parties,
  by = c("Year"), # "Year" columns for join
  suffix = c(".x", ".y")
)

# // Clean final_join: drop NAs in 'Code', ensure 'Year' is integer,
# // and clean 'percent_on_4.20' column
df <- final_join %>%
  drop_na(Code) %>% # Remove rows where 'Code' is NA
  mutate(
    Year = as.integer(Year), # Ensures 'Year' is an integer
    # Clean 'percent_on_4.20': remove '%' symbol and convert to numeric
    percent_on_4.20 = str_remove(as.character(percent_on_4.20), "%"),
    percent_on_4.20 = as.numeric(percent_on_4.20)
  )

saveRDS(df, "df.rds")
df <- readRDS("df.rds")

# --- Step 1: Data Loading ---
country_data <- readRDS("df.rds")
world_map_geo <- st_read("custom.geo.json")

# --- Step 2: User Interface (UI) ---
ui <- fluidPage(
  theme = shinytheme("superhero"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  titlePanel("Why Your Vote Matters"),
  
  # --- SUB-HEADER ADDED HERE ---
  # You can change the text and style of the sub-header below.
  h4("An Interactive GDP and Poverty Map that provides a visual exploration of the effects of Politics on Global Economics", 
     style = "font-size: 16px; color: #B0C4DE; margin-top: -10px; margin-bottom: 20px;"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_slider_ui"),
      tags$hr(),
      tags$div(
        style = "background-color: #2c3e50; padding: 15px; border-radius: 5px; margin-top: 10px;",
        uiOutput("static_country_info")
      )
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

# --- Step 3: Server (Backend Logic) ---
server <- function(input, output, session) {

  # --- UI Rendering ---
  output$year_slider_ui <- renderUI({
    req(country_data)
    sliderInput("year", "Select Year:",
                min = min(country_data$Year, na.rm = TRUE),
                max = max(country_data$Year, na.rm = TRUE),
                value = min(country_data$Year, na.rm = TRUE),
                step = 1,
                animate = animationOptions(interval = 1500, loop = TRUE),
                sep = "")
  })

  # --- Data Filtering ---
  filtered_data_by_year <- reactive({
    req(input$year)
    country_data %>% filter(Year == input$year)
  })

  # --- Map Rendering ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 30, zoom = 2)
  })

  observe({
    data_for_map <- filtered_data_by_year()
    map_data <- merge(world_map_geo, data_for_map, by.x = "adm0_a3", by.y = "Code", all.x = TRUE)

    pal <- colorNumeric(
      palette = brewer.pal(11, "RdYlGn"),
      domain = map_data$gross_domestic_product,
      na.color = "#808080"
    )

    map_data$poverty_opacity <- ifelse(is.na(map_data$percent_on_4.20), 0.5,
                                       0.3 + (map_data$percent_on_4.20 / 100) * 0.7)

    labels <- lapply(1:nrow(map_data), function(i) {
      label_parts <- c(paste0("<strong>", map_data$name[i], "</strong>"))
      if (!is.na(map_data$gross_domestic_product[i])) {
        label_parts <- c(label_parts, paste0("GDP: ", format(map_data$gross_domestic_product[i], big.mark = ",")))
      }
      if (!is.na(map_data$percent_on_4.20[i])) {
        label_parts <- c(label_parts, paste0("% of population living on under $4.20: ", map_data$percent_on_4.20[i], "%"))
      }
      HTML(paste(label_parts, collapse = "<br>"))
    })

    gdp_range <- range(map_data$gross_domestic_product, na.rm = TRUE)
    if(all(is.finite(gdp_range))) {
      legend_colors <- pal(seq(gdp_range[1], gdp_range[2], length.out = 100))
      legend_labels <- pretty(gdp_range, n = 3)

      color_bar <- paste0(
        "<div style='display: flex; flex-direction: row; height: 10px; width: 100%; border-radius: 5px; overflow: hidden;'>",
        paste0("<div style='flex: 1; background-color: ", legend_colors, ";'></div>", collapse = ""),
        "</div>"
      )
      
      label_bar <- paste0(
        "<div style='display: flex; justify-content: space-between; font-size: 12px; color: white; margin-top: 4px;'>",
        paste0("<span>", format(legend_labels, big.mark = ","), "</span>", collapse = ""),
        "</div>"
      )

      legend_html <- paste0(
        "<div style='background-color: rgba(44, 62, 80, 0.85); padding: 8px; border-radius: 5px; width: 220px;'>",
        "<div style='color: white; font-weight: bold; font-size: 14px; margin-bottom: 5px;'>GDP</div>",
        color_bar,
        label_bar,
        "</div>"
      )
    } else {
      legend_html <- NULL
    }


    leafletProxy("map", data = map_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(gross_domestic_product),
        weight = 1, opacity = 1, color = "white", dashArray = "3",
        fillOpacity = ~poverty_opacity,
        highlightOptions = highlightOptions(weight = 3, color = "#F39C12", fillOpacity = 0.9, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      clearControls() %>%
      {if(!is.null(legend_html)) addControl(., html = legend_html, position = "topright") else .}
  })

  # --- Static Information Panel Rendering ---
  output$static_country_info <- renderUI({
    req(input$year)
    default_country_iso <- "USA"
    details <- filtered_data_by_year() %>% filter(Code == default_country_iso)

    if (nrow(details) == 0) {
      return(HTML(paste("No data available for", default_country_iso, "in", input$year)))
    }

    bullets <- strsplit(details$Events, "\\*")[[1]]
    bullets <- bullets[bullets != ""]

    processed_bullets <- lapply(bullets, function(bullet) {
      bullet <- gsub("\n", "<br>", bullet)
      bullet <- gsub("([^<br>]*?):(.*)", "<span style='font-size: 90%;'><b>\\1</b></span>:<span style='font-size: 80%;'>\\2</span>", bullet)
      bullet <- gsub("( startling fact:|interesting fact:)", "<br><div style='margin-top: 0.8em;'><b>\\1</b></div>", bullet, ignore.case = TRUE)
      return(bullet)
    })

    event_html_items <- paste0("<li style='margin-bottom: 1em;'>", processed_bullets, "</li>", collapse = "")
    formatted_events <- paste0("<ul style='padding-left: 20px; list-style-type: square;'>", event_html_items, "</ul>")

    HTML(paste(
      "<b>President:</b>", details$President, "<br>",
      "<b>Party:</b>", details$Party, "<br>",
      "<hr>",
      "<b>Events:</b>",
      formatted_events
    ))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)