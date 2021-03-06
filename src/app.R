library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

data <- read.csv("https://github.com/ubco-mds-2021-labs/dashboard1-group-g/raw/main/data/clean_spotify.csv", sep = "\t") # nolint
year_list <- as.list(as.character(seq(1957, 2020, by = 3)))
names(year_list) <- as.character(seq(1957, 2020, by = 3))

count_vs_year <- function(data) {
  plot <- ggplot(data, aes(x = Year, color = Playlist.Genre)) +
    geom_line(stat = "count") +
    theme_classic() +
    labs(x = "Album Release Year", y = "Number of Songs Released", color = "Genre") # nolint
}

pop_vs_year <- function(data) {
  plot <- ggplot(data, aes(x = Year, y = Popularity, color = Playlist.Genre)) +
    geom_line(stat = "summary", fun = mean) +
    theme_classic() +
    labs(x = "Album Release Year", y = "Mean of Popularity", color = "Genre") # nolint
}

top_n_by_popularity <- function(data, ycol="Name") {
  data_filtered <- data %>%
  select(ycol, "Popularity") %>%
  group_by(data[, ycol]) %>%
  summarize ( # nolint
    Popularity = mean(Popularity)) %>%
    arrange(desc(Popularity))
  data_filtered_top10 <- data_filtered[1:10, ]
  colnames(data_filtered_top10) <- c("field", "Popularity")
  chart <- ggplot(data_filtered_top10, aes(x = reorder(field, Popularity) , y = Popularity, color = field)) + # nolint
    geom_col() +
    theme(axis.title = element_text(face = "bold")) + labs(y = "Popularity", x = ycol) # nolint
    chart + coord_flip() # nolint
}

sub_genre_plot <- function(data) {
  newdata <- data %>%
  group_by(Playlist.Subgenre) %>%
  count(Playlist.Subgenre) %>%
  setNames(c("Playlist.Subgenre", "Count")) %>%
  ggplot() +
  aes(
      x = Count,
      y = Playlist.Subgenre,
      color = Playlist.Subgenre,
      size = Count) +
  ggplot2::geom_point(alpha = 0.7) +
  #ggplot2::geom_col(alpha = 0.7) +
  #ggplot2::coord_polar("y", start = 0) +
  labs(x = "Record Count", y = "Subgenre", legend = "Count") +
  theme_classic() +
  theme(axis.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
        axis.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
        legend.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
        legend.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black") # nolint
  )
}


tophead <- div(
    dbcRow(
        list(
            dbcCol(
                div("Spotified"), # nolint
                width = 8,
                style = list("color" = "green", "textAlign" = "center",  "font-size" = 40, "margin-top" = 10), # nolint
                md = 10 # nolint
            ),
            dbcCol(
                img(
                    src = "assets/logo1.png",
                    style = list("height" = 50, "margin-top" = 15)# nolint
                )
            )
        ),
        style = list("background-color" = "black", "height" = 70)
    )
)

dropdown <- div(
    style = list(
      borderBottom = "thin lightgrey solid",
      backgroundColor = "rgb(250, 250, 250)",
      padding = "10px 5px"
    ),
    div(
      html$label("Genre"),
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
        htmlDiv(list(
          htmlLabel("Album Release Year"),
          dccRangeSlider(
            id = "year-widget",
            min = 1957,
            max = 2020,
            marks = year_list,
            value = list(1957, 2020)
          )
        )))
)

row1 <- div(
  style = list(
      borderBottom = "thin lightgrey solid",
      backgroundColor = "#d8f1c0",
      padding = "10px 5px"
    ),
          dbcRow(
              list(
                dbcCol(
                    div(
                      dccRadioItems(
                        id = "top_n_type",
                        options = list(list(label = "Name", value = "Name"),list(label = "Artist", value = "Artist")), # nolint
                        value = "Name",
                        labelStyle = list(display = "inline-block")
                      ),
                    dccGraph(id = "top10plot"),
                    style = list(width = "80%", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
                  ),
                    md = 6
                ),
                dbcCol(
                    div(
                      dccGraph(id = "countvsyear"),
                      style = list(width = "80%", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
                    ),
                    md = 6
                )
              )

          )
)

row2 <- div(
  style = list(
      borderBottom = "thin lightgrey solid",
      backgroundColor = "#d8f1c0",
      padding = "10px 5px"
    ),
          dbcRow(
                list(
                  dbcCol(
                    div(
                      dccGraph(id = "subgenre"),
                      style = list(width = "80 %", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
                    ),
                    md = 6
                  ),
                  dbcCol(
                    div(
                      dccGraph(id = "popvsyear"),
                      style = list(width = "80%", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
                    ),
                    md = 6
                  )
                )
          )
)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app %>% set_layout(tophead, dropdown, row1, row2)



app |> add_callback(
  output("countvsyear", "figure"),
  list(input("genre-widget", "value"),
       input("year-widget", "value")),
  function(genres, years) {
    new_data <- data |> filter(Playlist.Genre %in% genres,
                              Year >= as.integer(years[[1]]),
                              Year <= as.integer(years[[2]]))
    p <- count_vs_year(new_data)
    ggplotly(p)
  }
)

app |> add_callback(
  output("popvsyear", "figure"),
  list(input("genre-widget", "value"),
       input("year-widget", "value")),
  function(genres, years) {
    new_data <- data |> filter(Playlist.Genre %in% genres,
                               Year >= as.integer(years[[1]]),
                               Year <= as.integer(years[[2]]))
    p <- pop_vs_year(new_data)
    ggplotly(p)
  }
)

app |> add_callback(
  output("top10plot", "figure"),
  list(input("genre-widget", "value"),
       input("year-widget", "value"),
       input("top_n_type", "value")),
  function(genres, years, yaxis) {
    new_data <- data |> filter(Playlist.Genre %in% genres,
                              Year >= as.integer(years[[1]]),
                              Year <= as.integer(years[[2]]))
    p <- top_n_by_popularity(new_data, yaxis)
    ggplotly(p)
  }
)

app |> add_callback(
  output("subgenre", "figure"),
  list(input("genre-widget", "value"),
  input("year-widget", "value")),
  function(genres, years) {
    new_data <- data |> filter(Playlist.Genre %in% genres,
                              Year >= as.integer(years[[1]]),
                              Year <= as.integer(years[[2]]))
    p <- sub_genre_plot(new_data)
    ggplotly(p)

  }
)

app %>% run_app()