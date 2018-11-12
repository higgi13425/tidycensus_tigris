#testing out tidycensus and tigris
#census api_key
api_key <- 'd0263456f8db63ed8dc1084210d76f4cef099dd3'

library(tidycensus)
library(tigris)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggalt)
library(scales)

census_api_key(api_key)
Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

#snap
snap <- get_acs(geography = "state",
                variables = c(total_households ="B22001_001",
                              snap_received ="B22001_002",
                              not_snap = "B22001_005"),
                geometry = TRUE)

snap %>% 
  select(state=NAME, variable, estimate, geometry) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(pct_snap = round(100*snap_received/total_households,2)) %>% 
  select(state, pct_snap, geometry) %>% 
  filter(state != "Puerto Rico") ->
snap2

ggplot(snap2, aes(y=reorder(state, pct_snap), x=pct_snap)) +
  geom_lollipop(point.colour = "steelblue", point.size = 3,
                horizontal = TRUE) +
  scale_x_continuous(expand=c(0,0), 
              breaks = seq(0,40,by=5), limits = c(0,45)) +
  labs(x=NULL, y= NULL, 
       title = "States by Percent of Residents Receiving SNAP",
       subtitle = "Data from ACS 2016",
       caption = "using the get_acs function from tidycensus")+
  theme_minimal(base_family = "Arial Narrow") +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))+
  theme(axis.text.y=element_text(margin=margin(r=1, l=0)))+
  theme(plot.margin=unit(rep(30, 4), "pt"))+
  theme(plot.title=element_text(face="bold"))+
  theme(plot.subtitle=element_text(margin=margin(b=10)))+
  theme(plot.caption=element_text(size=8, margin=margin(t=10),
                  face="italic")) +
  geom_text(vjust = 0, nudge_x = 1, nudge_y=-0.35, size=3.2,
            aes(label=round(pct_snap,1)))
  
  


ggplot(snap2, aes(fill = pct_snap, color=pct_snap)) + 
  geom_sf() +
  scale_fill_viridis_c() + #fill scale
  scale_color_viridis_c(guide=FALSE) + #only one legend
  #colors lines between states
  theme_minimal(base_size = 10) + #base font size
  coord_sf(xlim = c(-60, -125), ylim = c(20, 50), datum = NA) +
  #sets limits, turns off lat/long grid
  labs(title = "Percent of Households Receiving SNAP, by State",
       subtitle = "2016 ACS data",
       fill = 'Percent')

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

#ggplotly(tooltip = "NAME")

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
