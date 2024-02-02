# SETUP ##########################################################

# set path
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))

library(tidyverse) # data toolbox
library(ggplot2) # plotting
library(cowplot)
library(egg)

# LOAD & PREPARE DATA ####################################

# load data
data_orig <- read_csv('../data/20230720.csv')

# see columns
colnames(data_orig)

# rename column and store in a new df
df <- data_orig %>%
  rename(DryDensity = `Dry Density (pcf)`, 
         InplaceMoisture = `="In-place Moisture"`,
         Coordinate = `Project Location ID`) %>%
  select(-c(1, 2, 3, 5, 7)) # drop some unrelevant cols

head(df)

# coordinate column untidy
df %>% filter(substr(df$Coordinate, 1, 1) == "N") %>% 
  select('Coordinate') %>% head()
df %>% filter(substr(df$Coordinate, 1, 1) == "E") %>% 
  select('Coordinate') %>% head()
df %>% filter(substr(df$Coordinate, 1, 1) == 2) %>%
  select('Coordinate') %>% head()

# prepare column Easting and Northing
df <- df %>% 
  # remove any unrelevant symbols and space
  mutate(Coordinate =  gsub(",", "", Coordinate)) %>%
  mutate(Coordinate =  gsub(" ", "", Coordinate)) %>%
  # mixed coordinate position Nxxxxxxx & Exxxxxxx
  mutate(Northing = ifelse(substr(Coordinate, 1, 1) == "N",
                           as.numeric(substring(Coordinate, 2, 8)),
                           as.numeric(substring(Coordinate, 10)))) %>%
  mutate(Easting = ifelse(substr(Coordinate, 1, 1) == "E",
                          as.numeric(substring(Coordinate, 2, 8)),
                          as.numeric(substring(Coordinate, 10)))) %>%
  # coordinate naming xxxxxxxN & xxxxxxE
  mutate(Northing = ifelse(substr(Coordinate, 1, 1) == 2,
                           as.numeric(substring(Coordinate, 1, 7)),
                           Northing)) %>%
  mutate(Easting = ifelse(substr(Coordinate, 1, 1) == 2,
                          as.numeric(substring(Coordinate, 9, 15)),
                          Easting)) %>%
  drop_na() 


head(df)

# EXPLORE DATA ###############################################

library(PerformanceAnalytics)
# explore correlation
chart.Correlation(df %>% select(c('DryDensity',
                                  'Percent Compaction',
                                  'InplaceMoisture')),
                  histogram=TRUE)


# explore correlation with faceting according to the locations
ggplot(df, aes(x = DryDensity, y = InplaceMoisture)) +
  geom_point() +
  facet_wrap(~ Location) +
  labs(x = "Dry Density (pcf)", y = "In-place Moisture")


# plot locations, show dry density
ggplot(df %>%
         filter(Northing >= 2107500 & Northing <= 2112500) %>%
         filter(Easting >= 6125000 & Easting <= 6135000) ) +
  geom_point(aes(x = Easting, y = Northing, color = DryDensity)) +
  theme_bw()

# Add the transparent rectangle for the selected area
area <- data.frame(
  xmin = 6130500, xmax = 6131000,
  ymin = 2109000, ymax = 2109500
)

ggplot(df %>%
         filter(Northing >= 2107500 & Northing <= 2112500) %>%
         filter(Easting >= 6125000 & Easting <= 6135000) ) +
  geom_point(aes(x = Easting, y = Northing, color = DryDensity)) +
  geom_rect(data = area, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'red', color = "red", size = 1, alpha = 0.5) +
  theme_bw()


# limit the scope (for case illustration only)
df <- df %>%
  filter(Northing >= 2109000 & Northing <= 2109500) %>%
  filter(Easting >= 6130500 & Easting <= 6131000)

ggplot(df) +
  geom_point(aes(x = Easting, y = Northing, color = DryDensity)) +
  scale_color_gradient(low = "white", high = "red") +
  theme_bw()

