# TAC App

library(shiny)
library(shinyjs)
library(dplyr)

source("merge.r")

choices_unit <- list("Group-year" = "group", "Dyad-year" = "dyad")
choices_crit <- list("Political, economic, religious, or social goal" = 1,
                     "Intention to coerce, intimidate or publicize to larger audience(s)" = 2, 
                     "Outside international humanitarian law" = 3)
choices_targtype <- list("Business" = 1,
                         "Government (general)" = 2,
                         "Police" = 3,
                         "Military" = 4,
                         "Abortion related" = 5,
                         "Airports & aircraft" = 6,
                         "Government (diplomatic)" = 7,
                         "Educational institution" = 8,
                         "Food or water supply" = 9,
                         "Journalists & media" = 10,
                         "Maritime" = 11,
                         "NGO" = 12,
                         "Other" = 13,
                         "Private citizens & property" = 14,
                         "Religious figures/institutions" = 15,
                         "Telecommunication" = 16,
                         "Terrorists/non-state militias" = 17,
                         "Tourists" = 18,
                         "Transportation (non-aviation)" = 19,
                         "Unknown" = 20,
                         "Utilities" = 21,
                         "Violent political parties" = 22)

choices_attacktype <- list("Assassination" = 1,
                           "Armed assault" = 2,
                           "Bombing/explosion" = 3,
                           "Hijacking" = 4,
                           "Hostage taking (barricade)" = 5,
                           "Hostage taking (kidnapping)" = 6,
                           "Facility/infrastructure" = 7,
                           "Unarmed assault" = 8,
                           "Unknown" = 9)

ui <- pageWithSidebar(
  # headerPanel("TAC Data App"),
  headerPanel("Generate TAC Count Data"),
  sidebarPanel(
    downloadButton("downloadCSV", "Download as CSV"),
    useShinyjs(), # Set up shinyjs
    br(), br(),
    actionButton("reset", "Reset to Defaults")),
  mainPanel(
    useShinyjs(), # Set up shinyjs
    div(id = "form",
        radioButtons("unit", 'Unit of observation', choices_unit),
        checkboxGroupInput("crit", "Required Criteria", choices_crit,
                           selected = c(1,2,3)),
        checkboxGroupInput("attacktype", "Included Attack Types", choices_attacktype,
                           inline = TRUE, selected = c(2,3,4,5,7)),
        checkboxGroupInput("targtype", "Included Target Types", choices_targtype, inline = FALSE,
                           selected = c(1, 6, 8, 9, 11, 13, 14, 15, 16, 18, 19, 21))
    )
  )
)

server <- function(input, output) {
  
  # output$data <- renderTable({
  #     head(link)TRUE %in% TRUE
  # }, rownames = TRUE)

  output$downloadCSV <- downloadHandler(
    content = function(file) {
      showNotification("Generating data")
      write.csv(tac.generate(input$unit, input$crit, input$attacktype, input$targtype), file)
    },
    filename = "TAC_custom.csv",
    contentType = "text/csv"
  )
  
  observeEvent(input$reset, {
    reset("form")
  })
  
}

shinyApp(ui, server)