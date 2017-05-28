library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Movimento Geometrico Browniano"),
    sidebarLayout(
      sidebarPanel(
        numericInput("tempo", "tempo:", value = 10),
        numericInput("espaco", "espaco:", value = 1001),
        numericInput("media", "media:", value = 0.1),
        numericInput("desvio", "desvio:", value = 0.2),
        submitButton("aplicar alteracoes")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
)