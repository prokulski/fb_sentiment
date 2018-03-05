# setwd("~/RProjects/FB_sentiment/shiny")
# rm(list = ls())

library(Rfacebook)
library(tidyverse)
library(lubridate)
library(forcats)
library(tidytext)
library(wordcloud)
library(knitr)
library(kableExtra)
library(formattable)

use_polimorph <- TRUE


# slowniki i tokeny ----
load("fb_oauth.rda")


# steming słownik
if(use_polimorph) {
  polimorph <- readRDS("polimorfologik.RDS")
} else {
  polimorph <- tibble(stem = "", word = "")
}


# slownik sentymentu
senti <<- read_csv("nawl-analysis.csv")
senti_meanings <- tribble(~category, ~meaning,
                          "A", "Złość",
                          "H", "Szczęście",
                          "S", "Smutek",
                          "F", "Strach",
                          "D", "Obrzydzenie",
                          "N", "Neutralność",
                          "U", "Nieznany")

# stop words
pl_stop_words <- read_lines("polish_stopwords.txt")


# wyglad wykresow ----
theme_set(theme_minimal())




# funkcje ----

getFPPosts <- function(page_name) {
  fb_page <- getPage(page = page_name, token = fb_oauth, n=50)
  
  # tylko potrzebne informacje
  fb_page <- fb_page %>%
    select(id, created_time, message, likes_count, comments_count) %>%
    mutate(created_time = ymd_hms(created_time))
  
  return(fb_page)
}

getPostComments <- function(post_id, fb_page) {
  
  # liczba komentarzy do tego postu
  post_coments <- fb_page[fb_page$id == post_id, "comments_count"]
  
  # pobieramy komentarze z wybranego postu, max 100 sztuk
  fb_post <- getPost(post_id, token = fb_oauth, n = max(post_coments, 1000), comments = TRUE)
  
  # tabela z komciami - tylko potrzebne dane
  comments <- fb_post$comments %>%
    select(created_time, message, likes_count) %>%
    mutate(created_time = ymd_hms(created_time)) %>%
    arrange(created_time)
  
  return(comments)
}

getMostCommentedPost <- function(fb_page) {
  # post z najwiekszą liczbą komentarzy
  n_post <- which(fb_page$comments_count == max(fb_page$comments_count))
  
  # id wybranego postu
  post_id <- fb_page[n_post, "id"]
  
  return(post_id)
}

plotCommentsTime <- function(comments) {
  # liczba komentarzy w poszczególnych minutach
  comments %>%
    # czas publikacji komentarza z dokladnoscia do 5 minut
    mutate(created_time = make_datetime(year(created_time),
                                        month(created_time),
                                        day(created_time),
                                        hour(created_time),
                                        5 * (minute(created_time) %/% 5), 0)) %>%
    count(created_time) %>%
    ggplot() +
    geom_col(aes(created_time, n), fill = "lightgreen", color = "gray10") +
    labs(title = "Liczba komentarzy według czasu publikacji",
         x = "", y ="")
}

getCommentsWords <- function(comments) {
  comments_words <- comments %>%
    select(created_time, message) %>%
    mutate(created_time = ymd_hms(created_time)) %>%
    # czas publikacji komentarza z dokladnoscia do 5 minut
    mutate(created_time = make_datetime(year(created_time),
                                        month(created_time), 
                                        day(created_time), 
                                        hour(created_time), 
                                        5 * (minute(created_time) %/% 5), 0)) %>%
    # znajki przestankowe na spacje
    mutate(message = gsub("[[:punct:]]", " ", message)) %>%
    # steming po czasie
    unnest_tokens(words, message) %>%
    filter(nchar(words) > 3) %>%
    filter(!words %in% stop_words)
  
  if(use_polimorph) {
    comments_words <- comments_words %>%
      # steming
      left_join(polimorph, by = c("words" = "word")) %>%
      filter(!is.na(stem))
  } else {
    comments_words <- comments_words %>% rename(stem = words)
  }
  
  return(comments_words)
}

plotCommentsWordCloud <- function(comments_words) {
  # chmurka słów z komentarzy
  cloud_words <- comments_words %>%
    count(stem) %>%
    ungroup()
  
  plot <- wordcloud(cloud_words$stem, cloud_words$n,
                    max.words = 100,
                    random.order = TRUE, random.color = TRUE,
                    colors = rainbow(10),
                    scale = c(0.7, 2.2))
  
  return(plot)
}

plotCommentsMostPopular <- function(comments_words) {
  # najpopularniejsze słowa w czasie
  comments_words %>%
    count(created_time, stem) %>%
    ungroup() %>%
    group_by(created_time) %>%
    filter(n == max(n), n != 1) %>%
    ungroup() %>% arrange(desc(created_time), n) %>%
    mutate(stem = fct_inorder(stem)) %>%
    ggplot() +
    geom_jitter(aes(created_time, stem, size = n, color = stem),
                show.legend = FALSE, alpha = 0.7, height = 0.2) +
    scale_size_continuous(range = c(5, 10)) +
    labs(title = "Najpopularniejsze słowa w komentarzach wg czasu publikacji",
         x = "", y = "")
}

plotCommentsSenti <- function(comments_words) {
  # sentiment słów po czasie
  comments_words_senti <- comments_words %>%
    select(created_time, stem) %>%
    left_join(senti %>% select(word, category), by = c("stem" = "word")) %>%
    filter(!is.na(category))
  
  # sentiment po czasie
  comments_words_senti %>%
    count(created_time, category) %>%
    left_join(senti_meanings, by = "category") %>%
    ggplot() +
    geom_jitter(aes(created_time, meaning, size = n, color = meaning),
                show.legend = FALSE, alpha = 0.7, height = 0.25) +
    scale_size_continuous(range = c(5, 10)) +
    labs(title = "Nacechowanie emocjonalne słów w komentarzach",
         x = "", y = "")
}

tableCommentsSenti <- function(comments_words) {
  # sentiment słów po czasie
  comments_words_senti <- comments_words %>%
    select(created_time, stem) %>%
    left_join(senti %>% select(word, category), by = c("stem" = "word")) %>%
    filter(!is.na(category))
  
  # najpopularniejsze słowa w danej kategorii
  comments_words_senti %>%
    count(stem, category)
}

plotCommentsBiWordCloud <- function(comments) {
  # chmurka bi-gramów z komentarzy
  biwords <- comments %>%
    select(message) %>%
    unnest_tokens(words, message, token = "ngrams", n = 2) %>%
    separate(words, c("word1", "word2")) %>%
    filter(!word1 %in% pl_stop_words) %>%
    filter(!word2 %in% pl_stop_words) %>%
    unite(words, word1, word2, sep = " ") %>%
    count(words) %>%
    ungroup()
  
  plot <- wordcloud(biwords$words, biwords$n,
                    max.words = 100,
                    random.order = TRUE, random.color = TRUE,
                    colors = rainbow(10),
                    scale = c(0.8, 1.4))
  
  return(plot)
}

getPostDetails <- function(post_id, fb_page) {
  
  # liczba komentarzy do tego postu
  post_details <- fb_page[fb_page$id == post_id, ]
  
  czas <- format(post_details$created_time, "%Y-%m-%d %H:%M")
  message <- post_details$message
  likes <- post_details$likes_count
  comments <- post_details$comments_count
  
  return(list(time = czas, message = message, likes = likes, comments = comments))
}



# aplikacja ----

cat("\n\nMożemy zaczynać!\n\n")



shinyServer(function(input, output, session) {
  
  # ubij sesję przy zamknięciu karty przeglądarki
  session$onSessionEnded(stopApp)
  
  # chowamy warstwę "loading" i pokazujemy apkę
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  
  
  # dodac obsługę inputów
  # jeśli zmieni się coś w wybieraku - wypełnij pole z urlem strony
  observe({
    page_name <- input$fanpage_select
    
    updateTextInput(session, "fanpage_name", value = page_name)
  })
  
  # po guziku załaduj dane o fp
  observeEvent(input$go_button, {
    page <- input$fanpage_name
    
    showModal(modalDialog(title = "Poczekaj...",
                          span("Trwa wczytywanie postów z fanpage"),
                          tags$b(page),
                          tags$br(),
                          span("Poczekaj deczko :)"),
                          footer = NULL)
    )
    
    # pobranie postów
    fb_page_table <<- getFPPosts(page)
    
    
    # update zawartosci
    
    # tabela z postami i pozwolić na wybor odpowiedniego
    output$fanpage_posts <- renderDataTable({
      fb_page_table %>%
        select(created_time, message, comments_count, likes_count) %>%
        arrange(desc(created_time)) %>%
        mutate(created_time = format(created_time, "%Y-%m-%d %H:%M")) %>%
        DT::datatable(rownames = FALSE, selection = "single",
                      colnames = c("Czas", "Treść posta", "Komentarze", "Like"),
                      escape = c(-1),
                      options = list(dom = 'ftp'))
    }, server = TRUE)
    
    removeModal()
  })
  
  # po wybraniu wiersza z postem - załaduj komcie i przelicz całość
  observeEvent(input$fanpage_posts_rows_selected, {
    
    # wybrany wiersz tabeli
    selectedRow <- input$fanpage_posts_rows_selected
    post_id <<- fb_page_table[selectedRow, "id"]
    
    # najbardziej komentowany post
    # post_id <- getMostCommentedPost(fb_page_table)
    # cat("= najbardziej komentowany komć znaleziony\n")
    
    
    # detale postu
    post_details <<- getPostDetails(post_id, fb_page_table)
    
    
    showModal(modalDialog(title = "Poczekaj...",
                          span("Trwa wczytywanie "),
                          span(post_details$comments),
                          span(" komentarzy do postu:"),
                          tags$br(), tags$br(),
                          tags$b(post_details$message),
                          tags$br(), tags$br(),
                          span("Poczekaj deczko :)"),
                          footer = NULL)
    )
    
    
    
    # pobranie komenatarzy
    comments_table <<- getPostComments(post_id, fb_page_table)
    
    # rozbicie komciow na slowa
    comments_words_table <<- getCommentsWords(comments_table)
    
    # tabela sentymentów
    senti_table <<- tableCommentsSenti(comments_words_table)
    
    
    # render wyników
    
    # detale postu
    output$post_details <- renderText({
      paste0("<br/>Post:<br /><br /><strong>", post_details$message, "</strong><br /><br />",
             "Post opublikowany:<strong> ", post_details$time, "</strong><br />",
             "<strong>", post_details$likes, "</strong> like'ów, <strong>",
             post_details$comments, "</strong> komentarzy.<br /><br />")
    })
    
    
    # komentarze w czasie
    output$comments_time <- renderPlot({
      plotCommentsTime(comments_table)
    })
    
    # najpopularniejsze słowa (stemy)
    output$comments_words <- renderPlot({
      plotCommentsMostPopular(comments_words_table)
    })
    
    
    # word cloud ze stemów
    output$word_cloud <- renderPlot({
      plotCommentsWordCloud(comments_words_table)
    })
    
    
    # chmurka bigramów
    output$bigram_cloud <- renderPlot({
      plotCommentsBiWordCloud(comments_table)
    })
    
    
    # sentiment w czasie
    output$senti_time <- renderPlot({
      plotCommentsSenti(comments_words_table)
    })
    
    
    # sentiment - najpopularniejsze w kategoriach
    output$senti_table <- function() {
      senti_table %>%
        # filter(!category %in% c("U", "N")) %>%
        ungroup() %>%
        group_by(category) %>%
        top_n(5, n) %>%
        ungroup() %>%
        left_join(senti_meanings, by = "category") %>%
        arrange(category, desc(n)) %>%
        select(meaning, stem, n) %>%
        rename(Emocja=meaning, `Słowo`=stem, `Liczba wystąpień`=n) %>%
        knitr::kable("html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 11) %>%
        collapse_rows(columns = 1) %>%
        scroll_box(height = "450px")
    }
    
    
    # liczba słów danego sentymentu
    output$senti_table_sum <- function() {
      senti_table %>%
        group_by(category) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        left_join(senti_meanings, by = "category") %>%
        select(meaning, n) %>%
        #            mutate(n = color_bar("lightgreen")(n)) %>%
        rename(Emocja=meaning, `Liczba słów` = n) %>%
        knitr::kable("html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 11) %>%
        scroll_box(height = "450px")
    }
    
    
    # tabela z komentarzami
    output$comments <- renderDataTable({
      comments_table %>%
        mutate(created_time = format(created_time, "%Y-%m-%d %H:%M")) %>%
        DT::datatable(rownames = FALSE,
                      colnames = c("Czas", "Komantarz", "Like'ów"),
                      escape = c(-2), selection = "none",
                      options = list(dom = 'ftp'))
    })
    
    removeModal()
  })
  
})
