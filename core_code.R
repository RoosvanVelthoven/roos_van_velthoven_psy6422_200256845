# Packages
library(here)
library(tidyverse)
library(animation)
library(png)
library(grid)

# Code to read in games.csv
nba_raw <- read.csv(here("data", "raw", "games.csv"))
# nba_raw = raw data

head(nba_raw, 3) # code to call first 3 lines of data

# Code to specify which variables to keep
nba_variables <-  nba_raw %>% 
  select(SEASON, PTS_home, PTS_away, HOME_TEAM_WINS) 
# nba_variables = data of solely the variables of interest

# Code to rename columns for easy labelling
nba_relabelled <- nba_variables %>%
  rename(points_home = "PTS_home", points_away = "PTS_away", season = "SEASON",
         home_win = "HOME_TEAM_WINS")

# Code to recode home_win values 1 to win and 0 to loss
nba_relabelled[nba_relabelled == "1"] <- "Won"
nba_relabelled[nba_relabelled == "0"] <- "Lost"

# nba_relabelled = data with relabelled variables and recoded home_win values (1 = win and 0 = loss)

# Code to save the relabelled data frame
write.csv(nba_relabelled, file = here("data", "processed", "nba_relabelled.csv"))

# Code to specify which seasons to keep
nba_2018_2015 <- nba_relabelled[ !(nba_relabelled$season %in% c(2020:2019, 2014:2003)), ]
# nba_2018_2015 = data of the 2015-2018 seasons

# Code to save nba_2018_2015 as a csv file (I need this later for my min and max values)
write.csv(nba_2018_2015, file = here("data", "processed", "nba_2018_2015.csv"))

# Code to keep season 2018
nba18 <- nba_2018_2015[ !(nba_2018_2015$season %in% c(2020:2019, 2017:2003)), ]
# nba18 = data of solely the 2018 season

# Code to rename columns for easy labelling when data frames are combined
nba18 <- nba18 %>% rename(points_home_18 = "points_home", points_away_18 = "points_away", season_18 = "season", home_win_18 = "home_win")

nba17 <- nba_2018_2015[ !(nba_2018_2015$season %in% c(2020:2018, 2016:2003)), ]
# nba17 = data of solely the 2017 season

# Code to rename columns for easy labelling when data frames are combined
nba17 <- nba17 %>% rename(points_home_17 = "points_home", points_away_17 = "points_away", season_17 = "season", home_win_17 = "home_win")

nba16 <- nba_2018_2015[ !(nba_2018_2015$season %in% c(2020:2017, 2015:2003)), ]
# nba16 = data of solely the 2016 season

# Code to rename columns for easy labelling when data frames are combined
nba16 <- nba16 %>% rename(points_home_16 = "points_home", points_away_16 = "points_away", season_16 = "season", home_win_16 = "home_win")

nba15 <- nba_2018_2015[ !(nba_2018_2015$season %in% c(2020:2016, 2014:2003)), ]
# nba15 = data of solely the 2015 season

# Code to rename columns for easy labelling when data frames are combined
nba15 <- nba15 %>% rename(points_home_15 = "points_home", points_away_15 = "points_away", season_15 = "season", home_win_15 = "home_win")

# Code to add ID numbers, I need specific keys to merge my data frames
nba18 <- nba18 %>% mutate(id = row_number())
nba17 <- nba17 %>% mutate(id = row_number())
nba16 <- nba16 %>% mutate(id = row_number())
nba15 <- nba15 %>% mutate(id = row_number())

# Each data frame has a different number of games, to merge the data frames without
# losing data, I need to do a full(outer) join. 

# Code to merge nba18 and nba17, I can only merge two data frames with this code
full18_17 <- merge(nba18, nba17, by = "id", all = TRUE)

# Code to merge nba16 and nba15
full16_15 <- merge(nba16, nba15, by = "id", all = TRUE)

# Code to merge full18_17 and full16_15
full18_15 <- merge(full18_17, full16_15, by = "id", all = TRUE)

# Time to rename my data frame to reduce confusion
nba_final_df <- full18_15

# Code to remove the season columns
nba_final_df <- select(nba_final_df,-season_18, -season_17, -season_16, -season_15)

# Code to save my final data set as a csv file
write.csv(nba_final_df, file = here("data", "processed", "nba_final_df.csv"))
# nba_final_df = data set that I will use for my plots

# Some extra checks to ensure I have all my data.

# Code to determine the max value of a column
max(nba18$id) 
## [1] 1378

max(nba17$id)
## [1] 1382

max(nba16$id)
## [1] 1405

max(nba15$id)
## [1] 1416

max(nba_final_df$id)
## [1] 1416
# All the data points are there!

head(nba_final_df, 3)

# Step one: I need two lists of variables to iterate over
points_home <- c("points_home_18","points_home_17","points_home_16","points_home_15")
points_away <- c("points_away_18","points_away_17","points_away_16","points_away_15") 

# Step two: I need a list of variables for my colour and shape
home_win <- c("home_win_18","home_win_17","home_win_16","home_win_15") 

# Step three: I need a list of titles
season <- c("Home Advantage Within the NBA: Season 2018", "Home Advantage Within the NBA: Season 2017", "Home Advantage Within the NBA: Season 2016", "Home Advantage Within the NBA: Season 2015")

# Step four: I need plot parameters
xlab  <-  'Points scored by the home team'
ylab  <-  'Points scored by the away team'
pointsize  <-  2
alpha <- .5
h <- "The home team:"

# Step five: I need xlim and ylim values
min(nba_2018_2015$points_home) ## [1] 64
max(nba_2018_2015$points_home) ## [1] 161
# I will use 60 - 165 for my xlim
min(nba_2018_2015$points_away) ## [1] 65
max(nba_2018_2015$points_away) ## [1] 168
# I will use 60 - 170 for my ylim

# Step six: I need to write a loop to look at points_away and points_home across four seasons

# Code to start saving my GIF
saveGIF({
  # Code to start my loop
  for (i in 1:4){
    
    mx <- mean(nba_final_df[,points_home[i]], na.rm = TRUE)
    # This will be used for my subtitle, it determines the mean value of points scored by the home team in a certain season and then assigns it to an object.   
    # na.rm = TRUE removes missing values in the calculation
    my <- mean(nba_final_df[,points_away[i]], na.rm = TRUE)
    # This will be used for my subtitle, it determines the mean value of points scored by the away team in a certain season and then assigns it to an object.
    
    nba_plots <- 
      ggplot(data = nba_final_df,
             mapping = aes_string (x = points_home[i],
                                   # First lists of variables to iterate over
                                   y =   points_away[i], 
                                   # Second lists of variables to iterate over
                                   colour = home_win[i], 
                                   # Code to have a different colour for games won or lost by the home team
                                   shape = home_win[i])) 
    # Code to have a different shape for games won or lost by the home team
    
    # Step seven: time to add plot parameters
    p <- nba_plots +
      geom_point (size = pointsize, alpha = alpha) +
      labs(x = xlab, y = ylab, title = season[i], 
           subtitle = sprintf("Mean points scored by the home team = %.1f, 
mean points scored by the away team = %.1f", mx,my)) +
      xlim(60, 165) +
      ylim(60, 170) +
      scale_colour_discrete(h, na.translate = FALSE) + 
      # Code necessary to have different colours for a win or loss of the home team
      scale_shape_discrete(h, na.translate=FALSE) +
      # Code necessary to have different shapes for a win or loss of the home team
      theme(text = element_text(size = 12))
    # Code to set text to size 12
    
    ggsave(filename = paste("figs/nba",toString(i),".png",sep=""))
    # Code to save the plots (nba1.png, nba2.png, nba3.png and nba4.png)
    
    print(p)}
  
},interval = 8,                            # 8 seconds interval
movie.name = "visualisation.gif",        
ani.height = 500,                        
ani.width = 700, 
ani.res = 100)                           # Resolution
# My GIF was saved as visualisation.gif


# Code necessary to exhibit my gif in my index.html
knitr::include_graphics("visualisation.gif")

# Code to save a copy of my GIF in my figs folder
file.copy("visualisation.gif", here("figs"), overwrite = TRUE)