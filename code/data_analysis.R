#Loading Paackages
library(tidyverse)

# Reading in data

sample_data <- read_csv("data/sample_data.csv")

summarize(sample_data, average_cells = mean(cells_per_ml))

sample_data %>% 
  summarize(average_cells = mean(cells_per_ml))

#filtering rows for certain conditions
#deep is in quotation marks because not referencing a column, if referencing a match value, use quotation marks
#double ==, values in env group is it equal to deep, true or false
# (!=) is not equal to
#> - depth > 50, depth >= 50, <=50, %in% means %n% c("Deep", "Shallow_May))- selects groups that match deep or shallow may

sample_data %>% 
  filter(env_group == "Deep") %>% 
  summarize(average_cells = mean(cells_per_ml))

sample_data %>% 
  filter(str_detect(env_group, "Shallow"))
  summarize(average_cells = mean(cells_per_ml))
  
  
#Calculate the average chlorophyll in the entire dataset

sample_data %>% 
  summarize(average_chlorophyll = mean(chlorophyll))

#Calculate the average chlorophyll just in shallow september

sample_data %>% 
  filter(env_group =="Shallow_September") %>% 
  summarize(average_chlorophyll = mean(chlorophyll))
#just look for lines with september in there
sample_data %>% 
  filter(str_detect(env_group, "September")) %>% 
  summarize(avg_chl = mean(chlorophyll))

#look for averages in all groups at once, group_by

sample_data %>% 
  group_by(env_group) %>% 
  summarize(average_cells = mean(cells_per_ml),
            min_cells = min(cells_per_ml))
#calculate the average temp per env group

sample_data %>% 
  group_by(env_group) %>% 
  summarize(average_temp = mean(temperature))

#Mutate- makes new column in data frame
#TN:TP for each observation
#can keep spaces or special characters with `backtick`

sample_data %>% 
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus)

sample_data %>% 
  mutate(temp_is_hot = temperature > 8) %>%
  group_by(env_group, temp_is_hot) %>% 
  summarize(avg_temp = mean(temperature),
            avg_cells = mean(cells_per_ml))

#select a few columns we want to keep, get rid of some
#selecting columns with select

sample_data %>% 
  select(sample_id, depth)

sample_data %>%
  select(-env_group)

sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(starts_with("total"))

#Create a data frame with only sample_id, env_group, depth, temp, cells_per_ml

sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(sample_id, env_group, depth, temperature, cells_per_ml)

sample_data %>% 
  select(1:5)

sample_data %>% 
  select(-(total_nitrogen:chlorophyll))


#cleaning data

read_csv("data/taxon_abundance.csv", skip = 2) %>% 
  select(-...10) %>% 
  rename(sequencer = ...9)

#remove lot # and sequencer columns, assign all to an object called taxon_clean

taxon_clean <- read_csv("data/taxon_abundance.csv", skip = 2) %>% 
  select(-...10) %>% 
  rename(sequencer = ...9) %>% 
  select(-sequencer, -Lot_Number)

taxon_long <- taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria,
               names_to = "Phylum", 
               values_to = "Abundance")
taxon_long %>% 
  group_by(Phylum) %>% 
  summarize(avg_abund = mean(Abundance))

taxon_long %>% 
  ggplot() +
  aes(x = sample_id,
      y = Abundance,
      fill = Phylum) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

#make long data wide

taxon_long %>% 
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance")

#Joining Data Frames

head(sample_data)

head(taxon_clean)

#inner join drops groups that dont have one or the other variables
#full join doesnt drop any columns, all data, if no data, shows up NA
#Directional Join
#left join 1,2 means keeps all ids in table 1 even if dont exist in table 2, but table 2 dont care
#right joing 1,2 means keep all ids in table 2, but get rid of table 1 values not present
#Anti-join just keeps where tables differ, keep pairs not present in either only keep C (in 1, not in 2)


#Inner Join
inner_join(sample_data, taxon_clean, by = "sample_id")

#lost alot of rows with inner join, 

anti_join(sample_data, taxon_clean, by = "sample_id")

#print values from one column

sample_data$sample_id
taxon_clean$sample_id

#make toxon clean match sep to september
#bind_rows glues two tables together
taxon_clean_goodSep <- taxon_clean %>% 
  mutate(sample_id = str_replace(sample_id, pattern = "Sep", replacement = "September"))

sample_and_taxon <-inner_join(sample_data, taxon_clean_goodSep, by = "sample_id")

write_csv(sample_and_taxon, file = "data/sample_and_taxon.csv")

#Make a plot
#Ask: Where does Chloroflexi like to live?
install.packages("ggpubr")
library(ggpubr)
sample_and_taxon %>% 
  ggplot() + 
  aes(x = depth,
      y = Chloroflexi) +
  geom_point() +
  labs(x = "Depth (m)",
       y = "Chloroflexi Relative Abundance") +
  geom_smooth(method = lm)+
  #stat_regline_equation() +
  stat_cor() +
  annotate(geom = "text",
           x = 25,
           y = 0.3,
           label = "This is a text label")
#What is the average abundance and standard deviation of chloroflexi in the three env_groups

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarize(average_chloro = mean(Chloroflexi),
            sd_chloro = sd(Chloroflexi))
