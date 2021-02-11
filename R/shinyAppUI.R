
#' @import shiny
#' @import reactable
#' @import wordcloud2

library(shiny)


ui <- fluidPage(

  # Application title
  titlePanel("Explorador de tweets"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      numericInput("num_tweets_to_download",
                   "Cantidad de tweets a descargar:",
                   min = 100,
                   max = 18000,
                   value = 200,
                   step = 100),
      textInput("hashtag_to_search",
                "Palabras o hashtag:",
                value = "#rstudio"),
      dateRangeInput("date_picker", label = "Seleccionar fechas:", start = as.character(Sys.Date()-10), end = as.character(Sys.Date())),
      checkboxInput("rt", label = "Incluir retweets", value = TRUE),
      hr(),
      actionButton("get_data", "Obtener Datos", class = "btn-primary"),
      br(),br(),
      downloadButton("download_data", "Descargar Datos")
    ),

    # Show results
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tabla", reactable::reactableOutput("tweet_table")),
                  tabPanel("Tweets/tiempo",
                           sidebarPanel(selectInput("time_unit", label = h3("Unidad de Tiempo"),
                                                    choices = list("Segundos" = "seconds", "Minutos" = "minutes", "Horas" = "hours", "Dias" = "days"),
                                                    selected = "minutes"),

                                        hr(),
                                        fluidRow(column(3, verbatimTextOutput("value")))

                           ),
                           plotOutput("plot")),
                  tabPanel("Nube de Palabras",
                           wordcloud2::wordcloud2Output("cloud")),
                  tabPanel("Sentimientos",
                           plotOutput("sentiment"))
      )
    )
  )
)
