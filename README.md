# Spotified (Group G)

[Link to Deployed App](https://spotified-r.herokuapp.com/)

## Goals

- To determine which songs and artists have been the most popular over time. 
- To determine how both the popularity and number of records released within each genre have changed over time. 
- To see the breakdown of each genre into its subgenres. 

## Team Members

- Mayukha Bheemavarapu : I love music
- Neethu Gopalakrishna : I love to read
- Sara Hall : I like to bike and run
- Xin Tian : I love sports

## Describe your topic/interest in about 150-200 words

We are all music enthusiasts and we have always been curious as to how music has evolved over time, particulary with which genres, songs and artists have been the most popular throughout recent history. 

Spotify is a very large music platform with subscribers from all around the world. If we can understand when different genres have become popular in the past, we might be able to predict future trends. With this goal in mind, we will build an interactive visualization dashboard that will show the top 10 songs or artists within the genres and timeframe selected my the user. We will also show how the number of songs released and popularity within each genre has changed, and how each genre has been divided into subgenres.

## About this Dashboard

has four plots arranged in a grid. In the top left, there is a bar chart showing either the top 10 artists by popularity, or the top 10 songs, which can be switch using the radio button above it. In the top right plot, we have the number of songs released each year in the six different genres. Then in the bottom left, we have a bubble chart showing the count of records released in each of the subgenres. Finally, in the bottom right, we have a bar chart with the top 10 subgenres by popularity. The data shown in all four of these plots is changed based on which genres are selected in the dropdown menu and which years are selected in the range slider. Overall, we have implemented everything we planned on implementing thus far. 

![](assets/Rgif.gif)

## Describe your dataset in about 150-200 words

This [dataset](https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-01-21) was provided on Github as the January 21, 2020 [Tidy Tuesday challenge](https://|github.com/rfordatascience/tidytuesday). As a result, this datset was provided with the purpose of learning how to wrangle and visualize data in R, and we are using it in a similar context to practice exploratory data analysis in Python. The data in the `csv` file were collected in January 2020 using the [`spotifyr`](https://www.rcharlie.com/spotifyr/) R package, which connects to the [Spotify Web API](https://developer.spotify.com/documentation/web-api/). The dataset contains information about around 30000 songs available on Spotify. This includes several variables identifying the song (id, name, artist, release date, album id, and album name), along with information about the playlist on which it was found (name, id, genre, and subgenre). Finally, it includes several numerical variables about the songs that we are mainly interested in analyzing (popularity, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, and duration). 

## Developers' Notes

If you are interested in contributing to our application, please let us know through emails. We would like some expert's opinions on the funcationality and UI of the dashboard. Here is a demo on how to run our application locally:

1. Clone the project from Github.
2. Install dependencies on your local computer for this project.
3. Open a terminal in the project, run `source("app.R") ', then navigate to localhost:8050 in your web browser to see the running application.
