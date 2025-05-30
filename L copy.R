#### 
## Chad Mirara ####
### STA 309 Homework 1
######



# Load libraries
library(tidyverse)

viewership_dataa <- read_csv('viewership_dataa.csv')

glimpse(viewership_dataa)

unique(viewership_dataa$Event)

viewership_dataa$Viewership <- as.numeric(str_remove_all(viewership_dataa$Viewership, "[M]")) 


## Average viewership for all leagues
viewership_all <- viewership_dataa %>%
  group_by(Event) %>%
  summarize(AverageVIEWERSHIP = mean(Viewership, na.rm = TRUE))

viewership_all


viewership_dataa <- viewership_dataa %>%
  filter(Viewership != 0)
  
  group_by(Year, 'Event') %>%
  summarize(AverageVIEWERSHIP = mean('Viewership')) %>%
  
  glimpse(filtered_viewership)



# View the structure of the data
str(viewership_dataa)

# Step 3: Create the plot

final_plot <- ggplot(viewership_dataa, aes(x = Year, y = Viewership, group = Event)) +
  geom_hline(yintercept = 20, linetype = "solid", color = "grey95", size = 1) +
  geom_hline(yintercept = 40, linetype = "solid", color = "grey95", size = 1) +
  geom_hline(yintercept = 60, linetype = "solid", color = "grey95", size = 1) +
  geom_hline(yintercept = 80, linetype = "solid", color = "grey95", size = 1) +
  geom_hline(yintercept = 100, linetype = "solid", color = "grey95", size = 1) +
  geom_step(aes(color = Event), size = 1.2) +  
  scale_color_manual(values = c("Super Bowl" = "tomato1", "World Series" = "lightslateblue", "CFB National Championship Game" = "green4", "NBA Finals" = "yellow")) +  
  labs(
    title = "Nothing grabs America's attention quite like the Super Bowl",
    subtitle = "Average viewership for men's sports championships in the United States, 1967-2022",
    x = element_blank(),
    y = element_blank(),
    caption = "Viewership is defined as the average number of persons watching an event on television. Totals for the World Series and NBA Finals are averages of the viewership of each game of that year's series. 
    Viewership numbers were first made available in 1973 for MLB and in 1997 for the NBA. College football championship viewer ratings span back to the creation of the Bowl Championship Series in 1998.",
  ) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
                     label = c(" 1970 ", " '75 ", " '80 ", " '85 ", " '90 ", " '95 ", " 2000 ", " '05 ", " '10 ", " '15 ", " '20 ")) +
  
  annotate("text", x = 1979, y = 15, label = "1994-1995 Major League Baseball Strike") +
  annotate("text", x = 1966, y = 100, label = "M TV VIEWERS") +
  annotate("label", x = 2006, y = 110, label = "SUPER BOWL", color = "tomato1") +
  annotate("label", x = 1987, y = 40, label = "WORLD SERIES", color = "lightslateblue") +
  annotate("label", x = 2010, y = 40, label = "COLLEGE FOOTBALL \n CHAMPIONSHIP BOWL", color = "green4") +
  annotate("label", x = 2000, y = 12, label = "NBA FINALS", color = "yellow3") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0) +
          scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
          scale_x_continuous(breaks = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
                             label = c(" 1970 ", " '75 ", " '80 ", " '85 ", " '90 ", " '95 ", " 2000 ", " '05 ", " '10 ", " '15 ", " '20 ")))   
ggsave(plot = final_plot, file = "Mirara_viewership_plot.png", width = 16, height = 8, units = "in")
