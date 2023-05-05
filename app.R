#install.packages("shiny")
library(shiny)
library(tidyverse)

# Load data from saved RDS file
prevalence_clean <- readRDS("prevalence.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Prevalence of HIV among adults aged 15 to 49"),
  sidebarLayout(
    sidebarPanel(
      helpText("Data source: Global Health Observatory - WHO"),
      selectInput(inputId = "country",
                  label = "Choose a country:",
                  choices = c("All", sort(unique(prevalence_clean$country))))
    ),
    mainPanel(
      plotOutput(outputId = "prevalence_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on country input
  filtered_data <- reactive({
    if (input$country == "All") {
      prevalence_clean
    } else {
      prevalence_clean %>% filter(country == input$country)
    }
  })
  
  # Create plot
  output$prevalence_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = year, y = value)) +
      geom_line() +
      xlab("Year") +
      ylab("Prevalence (%)") +
      ggtitle("Prevalence of HIV among adults aged 15 to 49")
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
