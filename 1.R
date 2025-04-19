library(shiny)
library(shinythemes)
library(caret)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(lsa)

load_and_process_data <- function(file_path) {
  data <- read.csv(file_path)
  data <- na.omit(data)
  df <- data[, c("Rank", "Title", "Genre", "Director", "Actors", "Year", "Rating", "Votes", "Metascore")]
  df$Genre <- strsplit(as.character(df$Genre), ", ")
  unique_genres <- unique(unlist(df$Genre))
  genre_matrix <- as.data.frame(do.call(rbind, lapply(df$Genre, function(x) {
    table(factor(x, levels = unique_genres))
  })))
  df <- cbind(df, genre_matrix)
  return(df)
}

normalize_features <- function(df, genre_matrix_columns) {
  features <- df[, c("Rating", "Votes", "Metascore", genre_matrix_columns)]
  return(scale(features))
}

recommend_movie <- function(movie_title, similarity_matrix, df, top_n = 5) {
  movie_idx <- which(df$Title == movie_title)
  if(length(movie_idx) == 0) return(NULL)
  sim_scores <- similarity_matrix[movie_idx, ]
  sorted_idx <- order(sim_scores, decreasing = TRUE)
  top_indices <- sorted_idx[2:(top_n + 1)]
  return(df[top_indices, c("Title", "Genre", "Director", "Year", "Rating", "Metascore")])
}

df <- load_and_process_data("E:/Btech/6thsem/BDA/pbl/IMDB2.csv")
genre_matrix_columns <- colnames(df)[10:ncol(df)]
normalized_features <- normalize_features(df, genre_matrix_columns)
similarity_matrix <- lsa::cosine(t(normalized_features))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Movie Recommendation System"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("movie", "Select a Movie:", choices = sort(df$Title),
                     options = list(placeholder = 'Type to search...', maxOptions = 1000)),
      numericInput("num_rec", "Number of Recommendations:", value = 5, min = 1, max = 10),
      actionButton("recommend", "Get Recommendations", class = "btn-primary"),
      br(), br(),
      h4("How it works:"),
      tags$ul(
        tags$li("Genre similarity"),
        tags$li("IMDB Rating"),
        tags$li("Metascore"),
        tags$li("Number of votes")
      )
    ),
    mainPanel(
      h3(textOutput("rec_title")),
      br(),
      tableOutput("recommendations"),
      plotOutput("rating_plot")
    )
  )
)

server <- function(input, output) {
  recommendations <- eventReactive(input$recommend, {
    req(input$movie)
    recommend_movie(input$movie, similarity_matrix, df, input$num_rec)
  })
  
  output$rec_title <- renderText({
    recs <- recommendations()
    if(is.null(recs)) {
      "Movie not found in database. Please try another title."
    } else {
      paste("Movies similar to:", input$movie)
    }
  })
  
  output$recommendations <- renderTable({
    recs <- recommendations()
    req(recs)
    recs
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$rating_plot <- renderPlot({
    recs <- recommendations()
    req(recs)
    selected_movie <- df[df$Title == input$movie, c("Title", "Rating", "Metascore")]
    comparison <- rbind(
      data.frame(Movie = selected_movie$Title, Rating = selected_movie$Rating, 
                 Metascore = selected_movie$Metascore, Type = "Selected"),
      data.frame(Movie = recs$Title, Rating = recs$Rating, 
                 Metascore = recs$Metascore, Type = "Recommended")
    )
    ggplot(comparison, aes(x = Movie, y = Rating, fill = Type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Rating), vjust = -0.3) +
      labs(title = "Rating Comparison", y = "IMDB Rating (1-10)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
