library(shiny)

ui <- fluidPage(
  titlePanel("Mini app Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("budget", "Budget média (€)", min = 0, max = 100000, value = 25000, step = 5000),
      numericInput("conv", "Taux de conversion (%)", value = 2.5, min = 0.1, max = 50, step = 0.1),
      actionButton("refresh", "Recalculer")
    ),
    mainPanel(
      verbatimTextOutput("kpi"),
      textOutput("message")
    )
  )
)

server <- function(input, output, session) {
  goal <- reactiveVal(500)

  observeEvent(input$refresh, {
    leads <- (input$budget * input$conv) / 100
    goal(round(leads / 5))
  })

  output$kpi <- renderPrint({
    list(
      budget = paste0(format(input$budget, big.mark = " ", scientific = FALSE), " €"),
      conversion = paste0(input$conv, " %")
    )
  })

  output$message <- renderText({
    paste("Objectif estimé :", goal(), "ventes / semaine")
  })
}

shinyApp(ui, server)
