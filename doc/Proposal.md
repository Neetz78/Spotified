**Motivation and purpose:**

Our role: Data Science Student Group

Target audience: Music enthusiasts (general population) and musicians

Spotify is a very large music platform with subscribers from all around the world. If we can understand what song qualities are associated with popularity in different genres, then this will provide valuable information to music creators. Furthermore, it may provide insights into the Spotify algorithm and how songs are ranked in terms of popularity. With this goal in mind, we will build an interactive visualization dashboard that will show how different song characteristics like energy and acousticness are related to song popularity within diffenent music genres. By using user interactions like filtering by genre, artist, or top ranking songs, we hope to elucidate some of these relationship between different song qualities. 

**Description of Data**

In our dashboard we will be visualizing around 30000 rows of data. Each of these rows corresponds to a song on a Spotify playist, and includes information including song name, artist, album, playlist genre, and various song qualities like energy, acousticness, danceability, liveness, speechiness, key, loudness, major/minor, instrumentalness, valence, tempo, and duration. The [dataset](https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-01-21) was provided on Github as the January 21, 2020 [Tidy Tuesday challenge](https://|github.com/rfordatascience/tidytuesday).

**Research Questions**

1. To determine what makes a song popular based on the attributes present in the data. For instance, track_popularity, danceability, energy, key,loudness, mode, speechiness, acousticness, instrumentalness, etc. 
2. If we look at particular genres, how to these relationships change? For instance do the songs in a particular genre have identifying characteristics?
3. While the correlations between individual metrics and song popularity are quite small, how much of the variation in song popularity can be explained by these metrics if we use all of them as predictors in a linear regression?
4. Which artists and genres are most popular? 

**Example Usage Scenario**

John is a singer and he writes most of the songs by himself. Recently, he is thinking about creating a new album. He wants to understand what song qualities are associated with popularity in different genres. He needs to explore a dataset in order to compare the effect of different variables on song popularity and the most relevant variables which will help him to create new songs. When John opens the “Spotify Song Popularity” app,  He will see plots of different variables vs song popularity. Different genres are indicated by the different color of the data points, and he can pick whichever genre that is most suitable for his songs by clicking on the name of the genre in the legend. Also, if he is only interested in the songs from recent years, he can filter the songs by selecting the range in Song Release Date. Since John’s primary concern is to find the features of the songs with the highest popularity, he can input a number in the top song section, for example 1000, and then the dashboard will only display the top 1000 songs that have the highest popularity. After filtering, if he moves the cursor to any of the data points, the name of the song and the artist will be displayed. Furthermore, on the left-hand side of the app, there are many selections John can make to filter songs by all different variables. For example, if John is writing a song with C# key, and he wants to know which variable will affect the popularity most, he can enter “C#” in the Key Selection bar. After doing those, John may notice that high “speechiness” usually leads to higher popularity. He will focus on the “speechiness” when he is writing new songs, and decides to do some follow-on studies to explore the relationship between speechiness and popularity furthermore.



