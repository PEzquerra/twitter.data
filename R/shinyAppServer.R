# Define server logic
#' @import reactable
#' @import rtweet
#' @import dplyr
#' @import glue
#' @import purrr 
#' @import wordcloud2 
#' @import tm
#' @import qdap

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

shinyAppServer <- function(input, output) {

  tweet_df <- eventReactive(input$get_data, {
    search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = input$rt, lang = "es")
  })

  output$plot <- renderPlot({
    req(tweet_df)
    ts_plot(tweet_df(), by = input$time_unit)

  })

  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      filter(between(as.Date(created_at), input$date_picker[1], input$date_picker[2]) ) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })

  output$tweet_table <- reactable::renderReactable({
    reactable::reactable(tweet_table_data(),
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200),
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweet_table_data(), file, row.names = FALSE)
    }
  )


  output$cloud <- wordcloud2::renderWordcloud2({
    req(tweet_df())
    tweet_df()$text %>%
      rm_twitter_url() %>%
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
      VectorSource() %>%
      Corpus() %>%
      tm_map(tolower) %>%
      tm_map(removeWords, stopwords("spanish")) %>%
      tm_map(removeWords, "na") %>%
      freq_terms(60) %>%
      wordcloud2()
  })


  output$sentiment <- renderPlot({
    req(tweet_df())
    tweet_df()$text %>%
      get_nrc_sentiment(language = "spanish") %>%
      column_sum() %>%
      data.frame() %>%
      cbind_dplyr() %>%
      ggplot(aes(x = sentiment, y = .,
                 fill = sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

}
