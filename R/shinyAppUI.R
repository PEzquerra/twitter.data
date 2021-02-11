
#' @import shiny


library(shiny)

#### function to make URL clickable ####
make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

#for dplyr strategy
column_sum <- function(x) {
  colSums(x[,])
}

cbind_dplyr <- function(x) {
  cbind(sentiment = row.names(x),
        x, row.names=NULL)
}

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
                  tabPanel("Tabla", reactableOutput("tweet_table")),
                  tabPanel("Tweets/tiempo",
                           sidebarPanel(selectInput("time_unit", label = h3("Unidad de Tiempo"),
                                                    choices = list("Segundos" = "seconds", "Minutos" = "minutes", "Horas" = "hours", "Dias" = "days"),
                                                    selected = "minutes"),

                                        hr(),
                                        fluidRow(column(3, verbatimTextOutput("value")))

                           ),
                           plotOutput("plot")),
                  tabPanel("Nube de Palabras",
                           wordcloud2Output("cloud")),
                  tabPanel("Sentimientos",
                           plotOutput("sentiment"))
      )
    )
  )
)
