library("tidyverse")
library("ggplot2")
library("sf")
library(lubridate)
library(fs)
library(ggthemes)
library(devtools)
install_github("thomasp85/transformr")
library(transformr)
# The information in the madlibs requires us to download the .csv data first.
# There is no need to download the spatial data because we are not mapping anything yet

download.file("https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ct_hartford_2019_02_25.csv.zip", "Hartford.zip")

# Now, we need to create an object that contains this data 

hartford <- read_csv("Hartford.zip")

# Given we have the data in an object we can safely delete the zip file.

file.remove("Hartford.zip")

# We use the parameter arrest_made to see how many people have been arreted
# Counting rows suffices.

total_arrests <- hartford %>% 
  filter(arrest_made == TRUE) %>%
  nrow()

# The second question requires us to set up filters by race, citations, and age

white_citations <- hartford %>% 
  filter(subject_race == 'white', 
         subject_age < 30,
         citation_issued == TRUE) %>% 
  
  # I select one column because unqiue needs to have a vector in order not to be confused
  
  select(location) %>% 
  unique() %>% 
  nrow()

# This question too asks us to set up a complex filter

first_female <- hartford %>%
  
  # by dictrict, sex, and arrest status, obviously
  
  filter(district == 'SOUTH END',
         subject_sex == 'female',
         arrest_made == TRUE) %>% 
  select(date) %>%
  
  # This is a handy way of getting rid of all the rows that are not the smallest
  
  summarize(first_date = min(date)) %>%
  
  # I couldn't find a better way to fit the format Preceptor Kane asked for
  
  mutate(first_date = str_c(month(first_date, label = TRUE, abbr = FALSE), " ", day(first_date), ", ", year(first_date)))


officer <- hartford %>% 
  
  # Because the return value of the str_detect is a boolean array, we can put it into the filter
  
  filter(str_detect(district, "SOUTH")) %>%
  
  # count is handy since ir does the group_by implicity and saves us some key strokes
  
  count(officer_id_hash) %>%
  
  # obviously we need to arrange it in descending order and 
  
  arrange(desc(n)) %>%
  slice(1:1) %>%
  
  # grab the officer name 
  
  select(officer_id_hash)


# First let's define out groups

hartford %>% 
  
  # We need to group by all three variables because we want all three to be included in our table
  
  group_by(subject_race, subject_sex, arrest_made) %>%
  
  # We also need the total count by group to calculate the arrest rate
  
  summarize(n = n()) %>% 
  
  # spread is a handy way of making sure that our data is in the fromat we want
  
  spread(arrest_made, n) %>%
  
  # During spread, we get some NA values for groups who did not have any arrests
  # We fix it by replacing the NA values with 0
  
  mutate(`FALSE` = replace_na(`FALSE`, 0),
         `TRUE` = replace_na(`TRUE`, 0), 
         
         # This is to calculate the arrest rate
         
         arrest_rate = `TRUE`/(`TRUE` + `FALSE`)) %>%
  
  # These are the only columns we need
  
  select(subject_race, subject_sex, arrest_rate) %>% 
  
  # We can't modify the race and subject sex variables until we ungroup
  # We also need ungroup for our gt() function we are about to use
  
  ungroup() %>% 
  
  # This is to make the table variables aesthetically pleasing
  
  mutate(subject_race = str_to_title(subject_race), 
         subject_sex = str_to_title(subject_sex)) %>%

# We not start plotting our table

  gt() %>% 
  
  tab_header(
    title = "Arrest Rates in the Hartford, CT by Sex and Race") %>% 
  
  cols_label(
    subject_race = "Subject Race",
    subject_sex = "Subject Sex",
    arrest_rate = "Arrest Rate"
  ) %>%
  
  fmt_percent(columns = vars(arrest_rate)) %>%
  tab_source_note("SOURCE: Stanford Open Policing Project")


# I will use the geom_freqpoly geom highlighted on the R4DS book, because I think it allways for for a 
# continuous graph

# But before we dive in, we need to calculate the number of bins needed to capture each minute (We want the difference
# between 12:30 and 12:31 to be highlighted)

ggplot(hartford) +
  
  # R4DS used freqploy, so I thought that was a good idea to replicate
  
  geom_freqpoly(aes(time), bins = 1400) +
  
  # This is to make the graph more descriptive
  
  labs(x = "Time",
       y = "Count",
       title = "Number of Stops by Time of the Day",
       subtitle = "The number of stops spikes within minutes",
       caption = "SOURCE: Stanford Open Policing Project")


# Healy recommends setting background theme to bw.

theme_set(theme_bw())

# Now, for the map, we need to download the shape files 

download.file(url = "https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ct_hartford_shapefiles_2019_02_25.tgz",
              destfile = "shapes.tgz", 
              quiet = TRUE)

# tgz files have to be untared before use

untar("shapes.tgz")

# we read shapefiles into the a dataframe

shapes_data <- read_sf("ct_hartford_shapefiles/Hartford_Neighborhoods.shp")

# we don't need the donwloaded files once we have the R objects, so I will go ahead and delete them

file_delete(c("shapes.tgz", "ct_hartford_shapefiles/"))

# I try to graph the locations of speed limit violations to see if there is some sort of pattern like some highways that are
# thought to be unpatrolled, or some other circumstances that lead people to violate the speed limit.

speed <- hartford %>% filter(reason_for_stop == "Speed Related", 
                             
                             # we cannot create a sf object if there are NA values in the coordinate columns
                             # so I decided to remove them
                             
                             !is.na(lng), 
                             !is.na(lat),
                             
                             # this is to remove the outliers that distort the graph
                             
                             lng > -72.72,
                             lng < -72.6,
                             lat < 41.82,
                             lat > 41.7)

# this is to create an sf object.

speed_locations <- st_as_sf(speed, 
                            coords = c("lng", "lat"), 
                            crs = 4326)

# This is to plot the map of Hartford from the shapes_data that we have downloaded

ggplot(data = shapes_data) +
  
  # This plots the data
  
  geom_sf() +
  
  # This plots the speed violation locations on top of the Hartford map
  
  geom_sf(data = speed_locations) +
  
  # Nice theme
  
  theme_map() +
  
  # To make the graph more descriptive
  
  labs(x = "Longitude",
       y = "Latitude",
       title = "Location of Hartford Speed Limit Violations",
       subtitle = "between years 2013 -- 2016", 
       caption = "Source: Stanford Open Policing Project" )

# As seen there is a higher density of speed limit violations on certain lines (basically, you can try to fit a line to contain
# some of the points). This points to the existence of roads where people tend to violate speed limits. Also, some borders between
# districts have a high density of speed limit violations. This finding might be interesting for the Hartford police. 


arrests <- hartford %>% 
  filter( 
    arrest_made == TRUE, 
    !is.na(lng), 
    !is.na(lat),
    lng > -72.72,
    lng < -72.64,
    lat < 41.82,
    lat > 41.7,
    district %in% c("ASYLUM HILL",
                    "DOWNTOWN",
                    "CLAY-ARSENAL",
                    "sOUTH GREEN",
                    "FROG HOLLOW",
                    "UPPER ALBANY",
                    "SHELDON-CHARTER OAK")
    ) %>%
  mutate(month = month(date),
         year = year(date)) %>% 
  select(year, month, lng, lat) %>%
  group_by(month, year) %>% 
  st_as_sf(coords = c("lng", "lat"), 
                                 crs = 4326)

ggplot(data = shapes_data) +
  
  # This plots the data
  
  geom_sf() + 
  
  geom_sf(data = arrests) +
  transition_time(year) +
  labs(title = "Stop Locations in The Central 7 Disctricts Over Three Years",
       subtitle = "Year: {frame_time}",
       caption = "Source: Stanford Open Policing Project")

download.file("https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ky_owensboro_2019_02_25.rds", 
              destfile = "Owensboro.rds",
              mode = "wb")

# Now, we need to create an object that contains this data 

Owensboro <- read_csv("Owensboro.zip")

# Given we have the data in an object we can safely delete the zip file.

file_delete("Owensboro.zip")



download.file(url = "https://stacks.stanford.edu/file/druid:tr137st9964/tr137st9964_ky_owensboro_shapefiles_2019_02_25.tgz",
              destfile = "shapes_ob.tgz", 
              quiet = TRUE)

# tgz files have to be untared before use

untar("shapes_lb.tgz")

# we read shapefiles into the a dataframe

shapes_lb_data <- read_sf("ca_long_beach_shapefiles/PoliceReportingDistricts.shp")
