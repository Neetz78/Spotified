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

top_n_by_popularity <- function(data,ycol='Name'){
  data_filtered<-data%>%select(ycol,"Popularity")%>%group_by(data[,ycol])%>%summarize(
    Popularity=mean(Popularity))%>%arrange(desc(Popularity))
  data_filtered_top10<-data_filtered[1:10,]
  colnames(data_filtered_top10) <- c("field","Popularity")
  
  chart <- ggplot(data_filtered_top10, aes(x=reorder(field,Popularity) ,y=Popularity,color=field)) + 
    geom_col()+
    theme(axis.title = element_text(face="bold"))+labs(y= "Popularity", x = ycol)
  chart+coord_flip()
}


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app %>% set_layout(
  div(
    style = list(
      borderBottom = 'thin lightgrey solid',
      backgroundColor = 'rgb(250, 250, 250)',
      padding = '10px 5px'
    ),
    
    div(
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
          htmlLabel('Album Release Year'),
          dccRangeSlider(
            id = 'year-widget',
            min = 1957,
            max = 2020,
            marks = year_list,
            value = list(1957,2020)
          )
          
        ))),
    div(
      div(
        dccRadioItems(
          id = 'top_n_type',
          options = list(list(label = 'Name', value = 'Name'),
                         list(label = 'Artist', value = 'Artist')),
          value = 'Name',
          labelStyle = list(display = 'inline-block')
        ),
        dccGraph(id='top10plot'),
        
        style = list( width = '50%',padding = '10px 5px')
      ),
      div(
        dccGraph(id='countvsyear'),
        
        style = list( width = '50%',padding = '10px 5px')
      ))
      
      
    )
    

  )


app |> add_callback(
  output('countvsyear', 'figure'),
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

app |> add_callback(
  output('top10plot', 'figure'),
  list(input('genre-widget', 'value'), 
       input('year-widget', 'value'),
       input('top_n_type', 'value')),
  function(genres,years,yaxis) {
    newData <- data |> filter(Playlist.Genre %in% genres, 
                              Year >= as.integer(years[[1]]), 
                              Year <= as.integer(years[[2]]))
    p <- top_n_by_popularity(newData,yaxis)
    ggplotly(p)
  }
)


app %>% run_app()