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
#' @return a line chart showing the count of records released in each genre over time. # nolint
#' @export
#'
#' @examples count_vs_year(df)
count_vs_year <- function(df) {
  plot <- ggplot(df, aes(x = Year, y = Number.of.Songs, color = Playlist.Genre)) + # nolint
    geom_line(stat = "summary", fun = sum) +
    theme_classic() +
    labs(x = "Album Release Year", y = "Number of Songs Released", color = "Genre") + # nolint
    ggtitle("Count of Songs Released by Year") +
    theme(plot.title = element_text(face = "bold"),axis.title = element_text(face = "bold")) # nolint
}

#' pop_vs_year()
#'
#' @param df the filtered data frame you with information you want plotted.
#'
#' @return a line chart showing the average popularity of records released in each genre over time. # nolint
#' @export
#'
#' @examples popularity_vs_year(df)
pop_vs_year <- function(df) {
  plot <- ggplot(df, aes(x = Year, y = Mean.Popularity, color = Playlist.Genre)) + # nolint
    geom_line(stat = "summary", fun = mean) +
    theme_classic() +
    labs(x = "Album Release Year", y = "Mean of Popularity", color = "Genre")
  # nolint
}


subgenre_pop <- function(df) {
  data_filtered <- df %>%
                select(Playlist.Subgenre, Mean.Popularity) %>%
                group_by(Playlist.Subgenre) %>%
                summarize(Popularity = mean(Mean.Popularity)) %>%
                arrange(desc(Popularity))
  data_filtered_top10 <- data_filtered[1:10, ] # nolint
  chart <- ggplot(data_filtered_top10, aes(x = reorder(Playlist.Subgenre, Popularity) , y = Popularity, color = Playlist.Subgenre)) +  # nolint
    geom_col() +
    labs(y = "Popularity", x = "Subgenres") +
    ggtitle("Top 10 Subgenres by Popularity") +
    theme(plot.title = element_text(face = "bold"),axis.title = element_text(face = "bold")) # nolint
  chart + coord_flip()
}

#' top_n_by_popularity()
#'
#' @param df
#' @param ycol either "Name" or "Artist", depending on which to show.
#'
#' @return a bar chart showing the top 10 songs or artists from the provided data frame. # nolint
#' @export
#'
#' @examples
top_n_by_popularity <- function(df, ycol = "Name") {
  df <- arrange(df, desc(Popularity)) |>
    select(ycol, "Artist", "Popularity")

  if (ycol == "Name") {
    colnames(df) <- c("field", "Artist", "Popularity")
    title_topn <- paste("Top 10 Songs by Popularity")
    df <- df |>
      group_by(field, Artist) |>
      summarize(Popularity = mean(Popularity)) |>
      arrange(desc(Popularity))
    df <- df[1:10, ]
    chart <- ggplot(df, aes(x = reorder(field, Popularity), y = Popularity, color = field,text=paste("Name:",field,"\n","Artist:",Artist,"\n Popularity:",Popularity))) # nolint
  } else {
    title_topn <- paste("Top 10 Artists by Popularity")
    colnames(df) <- c("field", "Popularity")
    df <- df |>
      group_by(field) |>
      summarize(Popularity = mean(Popularity)) |>
      arrange(desc(Popularity))
    df <- df[1:10, ]
    chart <- ggplot(df, aes(x = reorder(field, Popularity), y = Popularity, color = field,text=paste("Artist:",field,"\n Popularity:",Popularity))) # nolint
  }
    chart <- chart + geom_col() +
    labs(y = "Popularity", x = ycol) +
    ggtitle(paste(title_topn)) +
    theme(plot.title = element_text(face = "bold"), # nolint
          axis.title = element_text(face = "bold"))
  chart + coord_flip() # nolint
}

#' count_vs_subgenre()
#'
#' @param df the filtered data frame you with information you want plotted.
#'
#' @return a bubble chart showing the number of records released in each genre in the provided data frame. # nolint
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
      plot.title = element_text(family = "Helvetica", face = "bold",  colour = "black"), # nolint
      axis.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      axis.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      legend.text = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black"), # nolint
      legend.title = element_text(family = "Helvetica", face = "bold", size = (10), colour = "black") # nolint
    ) +
    ggtitle("Record Count by Subgenres")

}

subgenre <- function(data) {
    newdata <- data %>%
  group_by(Playlist.Subgenre) %>%
  count(Playlist.Subgenre) %>%
  setNames(c("Playlist_Subgenre", "Count"))
    fig <- plot_ly(newdata,labels = ~Playlist_Subgenre, values = ~Count, marker=list(colors = c("#6867AC","#A267AC","#CE7BB0","#FFBCD1","#845460","#EAD3CB","#BDC7C9","#2B4F60","#7FC8A9","#D5EEBB","#5F7A61","#444941"))) # nolint
    fig <- fig %>% add_pie(hole = 0.3)# nolint
    fig <- fig %>% layout(title = 'Record Count by Subgenres', plot_bgcolor = "#d8f1bb") # nolint
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

# Make the widgets that control all visualizations (year slider and genre dropdown) # nolint
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
      value = unique(by_genre$Playlist.Genre),
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
          dccGraph(id = "subgenre"),
          style = list(width = "80%", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
        ),
        md = 6
      )
    )
  )
)

# Make a row with the count of songs in each subgenre plot and the change in popularity over time plot. # nolint
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
          dccGraph(id = "countvsyear"),
          style = list(width = "80 %", padding = "10px 5px", backgroundColor = "#d8f1c0") # nolint
        ),
        md = 6
      ),
      # Second column has the popularity of subgrenres
      dbcCol(
        div(
          dccGraph(id = "subgenre_popularity"),
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
  output("subgenre_popularity", "figure"),
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
    p <- subgenre_pop(new_data)
    ggplotly(p) |>
      layout(showlegend = FALSE)
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
    ggplotly(p, tooltip = "text") |>
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
    p <- subgenre(new_data)
    ggplotly(p)
  }
)

## -----------------Run the App-----------------##
# app$run_server(host = '0.0.0.0') # nolint
app$run_server(debug = TRUE)
