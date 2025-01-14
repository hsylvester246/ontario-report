2+2
library(tidyverse)

sample_data <- read_csv("sample_data.csv")

# assign values to objects
name <- "agar"
name

year <- 1881
year
name <- "Fanny Hesse"
name

# bad names for object
number1 <- 3
Flower <- "marigold"
Flower
flower <- "rose"
flower



sample_data <- read_csv("sample_data.csv")

read_csv(file = 'sample_data.csv')

# lets comment note to yourself in this section what is happening
Sys.Date() # outputs current date

getwd() # outputs the current working directory

sum(5,6)

sample_data <- read_csv("sample_data.csv") # reads in csv file



# creating first plot
ggplot(data = sample_data) + 
  aes(x = temperature) +
  labs(x = "Temperature(C)") +
  aes(y = cells_per_ml/1000000) +
  labs(y= "Cells (millions/mL)") +
  geom_point() +
  labs(title = "Does temperature affect microbial abundance?") +
  aes(color = env_group) +
  aes(size = chlorophyll) +
  aes(shape = env_group) +
  labs(size = "Chlorophyll (ug/L)", 
       color = "Environmental Group",
       shape = "Environmental Group")

#combined "neater" code

ggplot(data = sample_data) +
  aes(x = temperature,
      y = cells_per_ml/1000000,
      color = env_group,
      size = chlorophyll) +
  geom_point() +
labs(x = "Temperature (C)",
     y = "Cells (millions/mL)",
     title = "Does Temperature affect microbial abundance",
     size = "Chlorophyll (ug/L)",
     color = "Environmental Group")



# importing datasets
buoy_data <- read_csv("buoy_data.csv")

dim(buoy_data)

head(buoy_data) # see beginning of data
tail(buoy_data) # see end of data

#plot some more
#introduce facets wrap
ggplot(data = buoy_data) +
  aes(x = day_of_year,
      y = temperature,
      group = sensor,
      color = depth) +
  geom_line()+
  facet_wrap(~buoy, scales = "free_y")
#structure of data object

str(buoy_data)

#facet grid (both axes shared)

ggplot(data = buoy_data) +
  aes(x = day_of_year,
      y = temperature,
      group = sensor,
      color = depth) +
  geom_line()+
  facet_grid(rows = vars(buoy))

#discrete plots
# box plot
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = c("pink","tomato","papayawhip"))
  
#scale fill brewer
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_brewer(palette = "Set1")

#custom palette time
install.packages("NatParksPalettes")
library(NatParksPalettes)
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = natparks.pals("Denali"))
#pick your own palette and download
install.packages("PNWColors")
library(PNWColors)
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = pnw_palette("Shuksan2"))

# box plot
#change transparency
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(fill = "darkblue", alpha =0.2) 

#univariate plots
ggplot(sample_data) +
  aes(x = cells_per_ml) +
  geom_histogram(bins = 10)

ggplot(sample_data) +
  aes(x = cells_per_ml) +
  geom_density(aes(fill = env_group), alpha = 0.5) +
  theme_light()

# box plot
#rotate x axis labels
box_plot <-
  ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#saving plots
ggsave("boxplot_rotate.jpg", width = 6, height = 4, dpi = 500)

box_plot

#add changes to plots for black and white
box_plot + theme_bw()

box_plot <- box_plot + theme_bw()
box_plot

ggsave("boxplot_rotated.jpg", plot = box_plot, width=6, height=4)
