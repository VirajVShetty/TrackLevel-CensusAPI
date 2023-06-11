library(tidycensus)
library(tidyverse)
library(tmap)
library(tigris)

options(tigris_use_cache = TRUE)

# Initialize Census API Key
census_api_key("2378827784b616cfccf56ad0ba3df37dad4501de")

# Retrieving Census Data
county.working.above65 <- get_acs(geography = "county",
                                  variables = c("B23001_005E", "B23001_006E", "B23001_012E", 
                                                "B23001_013E", "B23001_019E", "B23001_020E", 
                                                "B23001_026E", "B23001_027E", "B23001_033E", 
                                                "B23001_034E", "B23001_040E", "B23001_041E", 
                                                "B23001_047E", "B23001_048E", "B23001_054E", 
                                                "B23001_055E", "B23001_061E", "B23001_062E", 
                                                "B23001_068E", "B23001_069E", "B23001_091E", 
                                                "B23001_092E", "B23001_098E", "B23001_099E", 
                                                "B23001_105E", "B23001_106E", "B23001_112E", 
                                                "B23001_113E", "B23001_119E", "B23001_120E", 
                                                "B23001_126E", "B23001_127E", "B23001_133E", 
                                                "B23001_134E", "B23001_140E", "B23001_141E", 
                                                "B23001_147E", "B23001_148E", "B23001_154E", 
                                                "B23001_155E"),
                                  year = 2017,
                                  output = "wide",
                                  geometry = T
                                  )


county.working.above65 %>%
  mutate(pop.above.65 = B23001_005E + B23001_012E + B23001_019E + B23001_026E + B23001_033E + B23001_040E + B23001_047E + B23001_054E + B23001_061E + B23001_068E + B23001_091E + B23001_098E + B23001_105E + B23001_112E + B23001_119E + B23001_126E + B23001_133E + B23001_140E + B23001_147E + B23001_154E,
         pop.above.65.labor = B23001_006E + B23001_013E + B23001_020E + B23001_027E + B23001_034E + B23001_041E + B23001_048E + B23001_055E + B23001_062E + B23001_069E + B23001_092E + B23001_099E + B23001_106E + B23001_113E + B23001_120E + B23001_127E + B23001_134E + B23001_141E + B23001_148E + B23001_155E) %>%
  select(-B23001_005E:-B23001_155E) %>%
  mutate(working.pct = 100 * (pop.above.65.labor / pop.above.65)) -> county.working.above65


us.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## State data for borders
data("state_laea", package = "tidycensus")

## Make the map
work.map <- tm_shape(county.working.above65,
                     projection = us.proj) +
  tm_fill("working.pct",
          palette = "PuBuGn",
          title = "% Working",
          style = "fisher") +
  tm_shape(state_laea,
           projection = us.proj) +
  tm_borders()+
  tm_layout(main.title = "Percentage of the Population in the Labor Force (age 65+), 2017-2021",
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("LEFT","BOTTOM"),
            main.title.size = 1,
            frame = F)

# Saving Visualization
tmap_save(work.map, "above65.png", width = 1920, height = 1080, asp = 0)

# Plotting Data
den <- ggplot(data = county.working.above65, mapping = aes(x = working.pct)) +
  geom_density() +
  ggthemes::theme_fivethirtyeight()

work.map