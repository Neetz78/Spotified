## -----------------Load all of the dependencies-----------------#
library(dash)
library(tidyverse)
library(plotly)
library(dashHtmlComponents)

## -----------------Read in the global data-----------------#
top_songs <- read.csv("https://github.com/ubco-mds-2021-labs/dashboard2-group-g/raw/main/data/top_songs.csv", sep = "\t") # nolint
top_artists <- read.csv("https://github.com/ubco-mds-2021-labs/dashboard2-group-g/raw/main/data/top_artists.csv", sep = "\t") # nolint
by_genre <- read.csv("https://github.com/ubco-mds-2021-labs/dashboard2-group-g/raw/main/data/by_genres.csv", sep = "\t") # nolint
top_data <- list("Name" = top_songs, "Artist" = top_artists) # nolint

# Get lists of the years so that axes look nice.
year_list <- as.list(as.character(seq(1957, 2020, by = 3)))
names(year_list) <- as.character(seq(1957, 2020, by = 3))

## -----------------Functions to make plots-----------------#

#' count_vs_year
#'
#' @param df the filtered data frame you with information you want plotted.
#'
#' @return a line chart showing the count of records released in each genre over time.
#' @export
#'
#' @examples count_vs_year(df)
count_vs_year <- function(df) {
  plot <- ggplot(df, aes(x = Year, y = Number.of.Songs, color = Playlist.Genre)) +
    geom_line(stat = "summary", fun = sum) +
    theme_classic() +
    labs(x = "Album Release Year", y = "Number of Songs Released", color = "Genre") # nolint
}

#' pop_vs_year()
#'
#' @param df the filtered data frame you with information you want plotted.
#'
#' @return a line chart showing the average popularity of records released in each genre over time.
#' @export
#'
#' @examples popularity_vs_year(df)
pop_vs_year <- function(df) {
  plot <- ggplot(df, aes(x = Year, y = Mean.Popularity, color = Playlist.Genre)) +
    geom_line(stat = "summary", fun = mean) +
    theme_classic() +
    labs(x = "Album Release Year", y = "Mean of Popularity", color = "Genre") # nolint
}

#' top_n_by_popularity()
#'
#' @param df
#' @param ycol either "Name" or "Artist", depending on which to show.
#'
#' @return a bar chart showing the top 10 songs or artists from the provided data frame.
#' @export
#'
#' @examples
top_n_by_popularity <- function(df, ycol = "Name") {
  df <- arrange(df, desc(Popularity)) |>
    select(ycol, "Artist", "Popularity")

  if (ycol == "Name") {
    colnames(df) <- c("field", "Artist", "Popularity")
    df <- df |>
      group_by(field, Artist) |>
      summarize( # no lint
        Popularity = mean(Popularity)
      ) |>
      arrange(desc(Popularity))
  } else {
    colnames(df) <- c("field", "Popularity")
    df <- df |>
      group_by(field) |>
      summarize( # no lint
        Popularity = mean(Popularity)
      ) |>
      arrange(desc(Popularity))
  }


  df <- df[1:10, ]

  chart <- ggplot(df, aes(x = reorder(field, Popularity), y = Popularity, color = field)) + # nolint
    geom_col() +
    theme(axis.title = element_text(face = "bold")) +
    labs(y = "Popularity", x = ycol) # nolint
  chart + coord_flip() # nolint
}

#' count_vs_subgenre()
#'
#' @param df the filtered data frame you with information you want plotted.
#'
#' @return a bubble chart showing the number of records released in each genre in the provided data frame.
#' @export
#'
#' @examples count_vs_subgenre(df)
count_vs_subgenre <- function(df) {
  newdata <- df |>
    group_by(Playlist.Subgenre) |>
    summarise(Number.of.Songs = sum(Number.of.Songs)) |>
    setNames(c("Playlist.Subgenre", "Count")) |>
    ggplot() +
    aes(
      x = Count,
      y = Playlist.Subgenre,
      color = Playlist.Subgenre,
      size = Count
    ) +
    geom_point(alpha = 0.7) +
    labs(x = "Record Count", y = "Subgenre", legend = "Count") +
    theme_classic() +
    theme(
      axis.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      axis.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      legend.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      legend.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black") # nolint
    )
}

## -----------------Design the app layout.-----------------#

# Make a header to display at the top of the app.
tophead <- div(
  dbcRow(
    list(
      dbcCol(
        div("Spotified"), # nolint
        width = 8,
        style = list("color" = "#595959", "textAlign" = "center", "font-size" = 40, "margin-top" = 10), # nolint
        md = 10 # nolint
      ),
      dbcCol(
        img(
          src = "assets/logo1.png",
          style = list("height" = 50, "margin-top" = 15) # nolint
        )
      )
    ),
    style = list("background-color" = "#d8f1c0", "height" = 70)
  )
)

# Make the widgets that control all visualizations (year slider and genre dropdown)
dropdown <- div(
  style = list(
    borderBottom = "thin lightgrey solid",
    backgroundColor = "rgb(250, 250, 250)",
    padding = "10px 5px"
  ),
  # Make the genre widget and set the default to all.
  div(
    html$label("Genre"),
    dccDropdown(
      id = "genre-widget",
      options = list(
        list(label = "Pop", value = "Pop"),
        list(label = "Rap", value = "Rap"),
        list(label = "Rock", value = "Rock"),
        list(label = "Latin", value = "Latin"),
        list(label = "R&B", value = "R&B"),
        list(label = "Edm", value = "Edm")
      ),
      value = unique(data$Playlist.Genre),
      multi = TRUE
    ),
    # Make the year slider, set the deafult value to the entire year range.
    htmlDiv(list(
      htmlLabel("Album Release Year"),
      dccRangeSlider(
        id = "year-widget",
        min = 1957,
        max = 2020,
        marks = year_list,
        value = list(1957, 2020)
      )
    ))
  )
)

# Make a row with the top songs and counts of time plots.
row1 <- div(
  style = list(
    borderBottom = "thin lightgrey solid",
    backgroundColor = "#d8f1c0",
    padding = "10px 5px"
  ),
  dbcRow(
    list(
      # First column has the top songs/artists plot.
      dbcCol(
        div(
          dccRadioItems(
            id = "top_n_type",
            options = list(list(label = "Name", value = "Name"), list(label = "Artist", value = "Artist")), # nolint
            value = "Name",
            labelStyle = list(display = "inline-block")
          ),
          dccGraph(id = "top10plot"),
          style = list(width = "80%", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
        ),
        md = 6
      ),
      # Second column has the count of records released over time plot.
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

# Make a row with the count of songs in each subgenre plot and the change in popularity over time plot.
row2 <- div(
  style = list(
    borderBottom = "thin lightgrey solid",
    backgroundColor = "#d8f1c0",
    padding = "10px 5px"
  ),
  dbcRow(
    list(
      # First column has the count of songs in each subgenre plot.
      dbcCol(
        div(
          dccGraph(id = "subgenre"),
          style = list(width = "80 %", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
        ),
        md = 6
      ),
      # Second column has the change in popularity over time plot.
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

# Use the bootstrap theme so that the layout works.
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Add all the components intot the final layout.
app |> set_layout(tophead, dropdown, row1, row2)


## -----------------Add the Callbacks-----------------##

# Callback to filter the data using the year slider and
# genre dropdown and update count of songs per year plot.
app |> add_callback(
  output("countvsyear", "figure"),
  list(
    input("genre-widget", "value"),
    input("year-widget", "value")
  ),
  function(genres, years) {
    new_data <- by_genre |> filter(
      Playlist.Genre %in% genres,
      Year >= as.integer(years[[1]]),
      Year <= as.integer(years[[2]])
    )
    p <- count_vs_year(new_data)
    ggplotly(p)
  }
)


# Callback to filter the data using the year slider and
# genre dropdown and popularity over time plot.
app |> add_callback(
  output("popvsyear", "figure"),
  list(
    input("genre-widget", "value"),
    input("year-widget", "value")
  ),
  function(genres, years) {
    new_data <- by_genre |> filter(
      Playlist.Genre %in% genres,
      Year >= as.integer(years[[1]]),
      Year <= as.integer(years[[2]])
    )
    p <- pop_vs_year(new_data)
    ggplotly(p)
  }
)

# Callback to filter the data using the year slider, genre
# dropdown, and radio button to update the top 10 plot.
app |> add_callback(
  output("top10plot", "figure"),
  list(
    input("genre-widget", "value"),
    input("year-widget", "value"),
    input("top_n_type", "value")
  ),
  function(genres, years, yaxis) {
    new_data <- top_data[[yaxis]] |> filter(
      Playlist.Genre %in% genres,
      Year >= as.integer(years[[1]]),
      Year <= as.integer(years[[2]])
    )
    p <- top_n_by_popularity(new_data, yaxis)
    ggplotly(p) |>
      layout(showlegend = FALSE)
  }
)

# Callback to filter the data using the year slider and
# genre dropdown and update count of songs subgenre plot.
app |> add_callback(
  output("subgenre", "figure"),
  list(
    input("genre-widget", "value"),
    input("year-widget", "value")
  ),
  function(genres, years) {
    new_data <- by_genre |> filter(
      Playlist.Genre %in% genres,
      Year >= as.integer(years[[1]]),
      Year <= as.integer(years[[2]])
    )
    p <- count_vs_subgenre(new_data)
    ggplotly(p)
  }
)

## -----------------Run the App-----------------##
# app$run_server(host = '0.0.0.0')
app$run_server(debug = TRUE)
