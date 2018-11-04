#testing out tidycensus and tigris

api_key <- 'd0263456f8db63ed8dc1084210d76f4cef099dd3'

library(tidycensus)
library(tigris)
library(tidyverse)
library(ggplot2)
library(sf)
library(plotly)

census_api_key(api_key)
Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

wash_value <- get_acs(geography = "tract",
                      state = "MI",
                      county = "Washtenaw",
                      variables = "B25077_001",
                      geometry = TRUE)

state_pop <- get_decennial(geography = "state", variables = "P001001")

state_income <- get_acs(geography = "state", variables = "B19013_001")

head(state_income)

#median household income
mi_income <- get_acs(geography = "county",
                     variables = "B19013_001",
                     state = "MI",
                     geometry = TRUE)

head(mi_income)
#plot with ggplot
ggplot(mi_income, aes(fill = estimate, color=estimate)) + 
  geom_sf() +
  scale_fill_viridis_c() + #fill scale
  scale_color_viridis_c(guide=FALSE) + #only one legend
  #colors lines between counties
  theme_minimal(base_size = 10) + #base font size
  coord_sf(datum = NA) + #turns off lat/long grid
  labs(title = "Household Income in Michigan by County",
       subtitle = "Average value, 2016 ACS data",
       fill = 'Income in Dollars')

ggplotly(tooltip = "NAME")

wash_income <- get_acs(geography = "tract",
                       variables = c(hholdincome ="B19013_001"),
                       state = "MI",
                       county = "Washtenaw",
                       geometry = TRUE)

# Inspect the dataset
head(wash_income)

#plot with ggplot
ggplot(wash_income, aes(fill = estimate, color=estimate)) + 
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()

#wide data
# Return county data in wide format
mi_wide <- get_acs(geography = 'county',
                   state = "MI",
                   variables = c(hhincome = "B19013_001",
                                 medage = "B01002_001"),
                   output = "wide")

# Compare output to the tidy format from previous exercises
head(mi_wide)

# Create a scatterplot
plot(mi_wide$hhincomeE, mi_wide$medageE)

#finding census variable IDs
# online in Census Reporter
# load_variables in tidycensus
# load_variables(year,dataset="acs5" or "sf3", cache=TRUE)
# also dataasets "acs5/profile"
# these include name, label, concept
# B- base table, C=collapsed, DP - data profiles, S=subject
# 19001 is a table ID
# _002 - variable code in the table,
# E is estimate, M is margin of error

#load variables in dataframe
# then filter for vars
# filter(data, str_detect(name, "B19001")), or
# filter(v16p, str_detect(label, fixed("public_transportation",
# ignore_case = TRUE)))

ne_income <- get_acs(geography = "state",
                     variables = "B19013_001",
                     survey = "acs1",
                     state = c("ME", "NH", "VT", "MA",
                               "RI", "CT", "NY"))

g_scale <- ggplot(ne_income, aes(x=estimate,
                                 y=reorder(NAME, estimate))) +
  geom_point(color='navy', size=4) +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 18)

g_label <- g_scale +
  labs(x = "2016 ACS estimate",
       y = "",
       title = "Median household income by state")

g_label
