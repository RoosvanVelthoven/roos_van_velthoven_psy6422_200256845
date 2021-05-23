library(tidyverse)
library(here)

fonts <- list(
  sans = "Helvetica",
  mono = "Consolas",
  `Times New Roman` = "DejaVu Serif"
)

nba <- read.csv(here("data", "raw", "games.csv"))

nba_final_df <- read.csv(here("data", "processed", "nba_final_df.csv"))

# Code to plot our data

# Step one: we need two lists of variables to iterate over
points_home <- c("points_home_18","points_home_17","points_home_16","points_home_15")
points_away <- c("points_away_18","points_away_17","points_away_16","points_away_15") 

# Step two: we need one list of variables for our color and shape and one list of titles for our graphs
home_win <- c("home_win_18","home_win_17","home_win_16","home_win_15") 
season <- c("Home Advantage Within the NBA: Season 2018", "Home Advantage Within the NBA: Season 2017", "Home Advantage Within the NBA: Season 2016", "Home Advantage Within the NBA: Season 2015")

# Step three: we need plot parameters
xlab  <-  'Points scored by the home team'
ylab  <-  'Points scored by the away team'
pointsize  <-  2
alpha <- .5
h <- "The home team:"

min(nba4$points_home) ## [1] 64
max(nba4$points_home) ## [1] 161
# Lets use 60 - 165 for our xlim
min(nba4$points_away) ## [1] 65
max(nba4$points_away) ## [1] 168
# Lets use 60 - 170 for our ylim

# Step 4: write a loop to look at points_away and points_home across the four seasons
for (i in 1:4){
  mx<-mean(nba_final_df[,points_home[i]], na.rm = TRUE)
  my<-mean(nba_final_df[,points_away[i]], na.rm = TRUE)
  nba_plots <- ggplot(data = nba_final_df, mapping = aes_string(x = points_home[i], y = points_away[i], colour = home_win[i], shape = home_win[i]))
  # Step 5: write code to make plots of each season
  print(nba_plots + geom_point (size = pointsize, alpha = alpha) +
          labs(x = xlab, y = ylab, title = season[i], 
               subtitle = sprintf("Mean points scored by the home team = %.1f, 
mean points scored by the away team = %.1f", mx,my))+
          xlim(60, 165) +
          ylim(60, 170) +
          scale_colour_discrete(h, na.translate=FALSE) +
          scale_shape_discrete(h, na.translate=FALSE)) + 
          theme(text = element_text(size = 50), family = "Times New Roman")
    Sys.sleep(5)
}

