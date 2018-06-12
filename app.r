# TAC App

library(shiny)
library(dplyr)
library(haven)
# library(tidyr)
# library(ggplot2)

load("source.Rdata")

ui <- pageWithSidebar(
  headerPanel("TAC Demo App"),
  # headerPanel("Download Terrorism in Armed Conflict (TAC) Data"),
  sidebarPanel(
    checkboxGroupInput('match', 'UCDP-GTD Matching', choices_gname_match),
    checkboxGroupInput("crit", "Terrorism Definition:", choices_crit,
                       selected = c(1,2,3)),
    checkboxGroupInput("attacktype", "Attack Types:", choices_attacktype, 
                       inline = TRUE, selected = c(2,3,4,5,7)),
    checkboxGroupInput("attacktype", "Target Types:", choices_targtype, inline = TRUE,
                       selected = c(1, 6, 8, 9, 11, 13, 14, 15, 16, 18, 19, 21))
  ),
  mainPanel(
    tableOutput("data"),
    downloadButton("downloadCSV", "Download as CSV")
  )
)

server <- function(input, output) {
  
  output$data <- renderTable({
      head(link)
  }, rownames = TRUE)

  output$downloadCSV <- downloadHandler(
    content = function(file) {
      write.csv(link, file)
    },
    filename = "TAC.csv",
    contentType = "text/csv"
  )
}

shinyApp(ui, server)