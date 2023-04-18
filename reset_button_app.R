library(shiny)
library(plotly)
library(DT)
library(dplyr)
# Load the iris dataset
data(iris)

# Define the UI
ui <- fluidPage(
  titlePanel("Iris Mean Histogram"),

  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select Variable:",
                  choices = c("Sepal Length" = "Sepal.Length",
                              "Sepal Width" = "Sepal.Width",
                              "Petal Length" = "Petal.Length",
                              "Petal Width" = "Petal.Width")),
      actionButton('reset', 'Reset')
    ),

    mainPanel(
      plotlyOutput("hist"),
      DTOutput("table")
    )
  )
)

# Define the server
server <- function(input, output) {

  # Calculate the mean of the selected variable
  mean_var <- reactive({
    iris %>%
      group_by(Species) %>%
      summarize(mean_var = mean(.data[[input$var]]))
  })

  # Create the histogram plot
  output$hist <- renderPlotly({
    plot_ly(mean_var(), x = ~Species, y = ~mean_var, type = 'bar', source ='hist') %>%
      layout(title = paste("Mean", input$var, "by Species"))
  })

  # Create the table
  output$table <- renderDT({
    mean_var()
  })

  # Connect the table to the plotly histogram using event_data
  observeEvent(event_data("plotly_click", source = "hist"), {
    selected_species <- event_data("plotly_click", source = "hist")$x

    selected_row <- mean_var() %>%
      filter(Species == selected_species)
    output$table <- renderDT(selected_row)
  })
  observeEvent(input$reset,{
    output$table <- renderDT({
    mean_var()
  })
  })

}

# Run the Shiny app
shinyApp(ui = ui, server = server)
