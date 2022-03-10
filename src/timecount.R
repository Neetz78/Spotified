library(tidyverse)
library(ggplot2)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(plotly)

data <- read.csv('https://github.com/ubco-mds-2021-labs/dashboard1-group-g/raw/main/data/clean_spotify.csv', sep = '\t')
year_list <- as.list(as.character(seq(1957,2020, by = 3)))
names(year_list) <- as.character(seq(1957,2020, by = 3))

count_vs_year <- function(data){
  plot <- ggplot(data, aes(x = Year, color = Playlist.Genre)) + 
    geom_line(stat = 'count') + 
    theme_classic() + 
    labs(x = "Album Release Year", y = "Number of Songs Released", color = "Genre")
}

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id = "timecountplot"),
      dccDropdown(
        id = "genre-widget",
        options = list(list(label = "Pop", value = "Pop"),
                       list(label = "Rap", value = "Rap"),
                       list(label = "Rock", value = "Rock"),
                       list(label = "Latin", value = "Latin"),
                       list(label = "R&B", value = "R&B"),
                       list(label = "Edm", value = "Edm")
                       ),
        value = unique(data$Playlist.Genre), 
        multi = TRUE
      ),
      htmlDiv(
        list(
          htmlLabel('Album Release Year'),
          dccRangeSlider(
            id = 'year-widget',
            min = 1957,
            max = 2020,
            marks = year_list,
            value = list(1957,2020)
          )
          
        )
      )
    )
  )
)

#Callback for the time count plot.
app |> add_callback(
  output('timecountplot', 'figure'),
  list(input('genre-widget', 'value'), 
       input('year-widget', 'value')),
  function(genres, years) {
    newData <- data |> filter(Playlist.Genre %in% genres, 
                              Year >= as.integer(years[[1]]), 
                              Year <= as.integer(years[[2]]))
    p <- count_vs_year(newData)
    ggplotly(p)
  }
)


app$run_server(debug = T)