library(shiny)
library(shinyjs)
library(DT)

# lista fanpages
fanpages_list <- list("Antyweb" = "Antyweb",
                      "Bankier.pl" = "Bankierpl",
                      "FAKT" = "fakt24pl",
                      "Gazeta Wyborcza" = "wyborcza",
                      "Gazeta.pl" = "gazetapl",
                      "Interia.pl" = "FaktyINTERIA",
                      "NaTemat.pl" = "natematpl",
                      "Newsweek Polska" = "NewsweekPolska",
                      "Niezależna.pl" = "NiezaleznaPL",
                      "OKO.press" = "oko.press",
                      "Onet Wiadomości" = "OnetWiadomosci",
                      "Polityka" = "TygodnikPolityka",
                      "Polsat News" = "polsatnewspl",
                      "Radio RMF RM" = "rmffm",
                      "Radio TOK FM" = "tokfm",
                      "Radio ZET" = "radiozet",
                      "Spider's Web" = "SpidersWebPL",
                      "TV Republika" = "RepublikaTV",
                      "TVN Fakty" = "Fakty.TVN",
                      "TVN24" = "tvn24pl",
                      "TVP Info" = "tvp.info",
                      "TVP Panorama" = "panoramatvp",
                      "TVP Wiadomości" = "wiadomoscitvp",
                      "WP.pl" = "WirtualnaPolska",
                      "Wprost" = "tygodnikwprost")


# css dla warstwy przykrywającej przy ładowaniu
appCSS <- "
#loading-content {
margin: auto;
position: absolute;
top: 0;
left: 0;
width: 100%;
height: 100%;
background: #333;
oppacity: 0.7;
color: white;
z-index: 100;
text-align: center;
vertical-align: middle;
padding: 50px;
}
"


fluidPage(
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h2("Przygotowanie aplikacji - poczekaj chwilę...")
  ),
  
  
  headerPanel("Facebook page's comments"),
  
  sidebarPanel(
    selectInput("fanpage_select", label = "Wybierz fan page:",
                choices = fanpages_list,
                selected = "tvn24pl"),
    
    textInput("fanpage_name", label = "lub wpisz nazwę innego:", value = "tvn24pl"),
    
    actionButton("go_button", "Pobierz posty"),
    
    HTML("<p>Więcej fajnych rzeczy na <a href=\"http://prokulski.net?utm_source=shiny&utm_medium=fbsenti\">blogu</a></p>"),
    
    width = 4
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Posty", dataTableOutput("fanpage_posts")),
      tabPanel("Komentarze",
               htmlOutput("post_details"),
               plotOutput("comments_time", height = "200px"),
               plotOutput("comments_words", height = "400px"),
               fixedRow(
                 column(6, plotOutput("word_cloud", height = "400px")),
                 column(6, plotOutput("bigram_cloud", height = "400px"))
               ),
               fixedRow(
                 column(3, tableOutput("senti_table")),
                 column(3, tableOutput("senti_table_sum")),
                 column(6, plotOutput("senti_time", height = "500px"))
               ),
               dataTableOutput("comments")
      ),
      # kod GA
      tags$head(includeScript("ga_tracking.js"))
    ),
    width = 8
  )
)
