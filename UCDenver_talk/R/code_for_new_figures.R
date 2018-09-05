# Seasonal confounding figure
library(ggdag)
dagify(death ~ storm,
       storm ~ season,
       death ~ season,
       labels = c("storm" = "Tropical\n cyclone",
                  "death" = "Death",
                  "season" = "Time\n of year"),
       exposure = "storm",
       outcome = "death") %>% 
  tidy_dagitty() %>% 
  ggdag_adjustment_set(text = FALSE, use_labels = "label") + 
  theme(legend.position = "none")

# Pathways for storm's influence on mortality risk
library(DiagrammeR)
grViz("
      digraph causal {
      
      # Nodes
      node [shape = plaintext]
      S [label = 'Tropical\n cyclone\npath']
      M [label = 'Death']
      W [label = 'Maximum\n sustained\n wind']
      R [label = 'Rainfall']
      F [label = 'Flooding']
      T [label = 'Tornado(es)']
      D [label = 'Distance\n from the\n storm track']
      
      # Edges
      edge [color = black,
      arrowhead = vee]
      rankdir = LR
      S->M
      S->W
      S->R
      S->F
      S->T
      S->D
      W->F
      R->F
      W->M
      F->M
      R->M
      T->M
      
      # Graph
      graph [overlap = true, fontsize = 10]
      
      }")

# Associations between different hazards
library(DiagrammeR)
grViz("
      digraph neato {
      
      # Nodes
      node [shape = plaintext]
      S [label = 'Tropical\n cyclone']
      M [label = 'Death']

      node [shape = rectangle,
            color = red, 
            fontcolor = red,
            style = dashed,
            penwidth = 2]
      W [label = 'Maximum\n sustained\n wind']
      R [label = 'Rainfall']
      F [label = 'Flooding']
      T [label = 'Tornado(es)']
      D [label = 'Distance\n from the\n storm track']

      # Edges
      edge [color = black,
      arrowhead = circle]
      rankdir = LR
      S->M
      S->W
      S->R
      S->F
      S->T
      S->D
      W->F
      R->F
      W->M
      F->M
      R->M
      T->M

      edge [color = black]
      
      # Graph
      graph [overlap = true, fontsize = 10]
      
      }")

# Associations between different hazards and outcome
library(DiagrammeR)
grViz("
      digraph neato {
      
      # Nodes
      node [shape = plaintext]
      S [label = 'Tropical\n cyclone']
      
      node [fontcolor = red]
      M [label = 'Death']
      W [label = 'Maximum\n sustained\n wind']
      R [label = 'Rainfall']
      F [label = 'Flooding']
      T [label = 'Tornado(es)']
      D [label = 'Distance\n from the\n storm track']
      
      # Edges
      edge [color = black,
      arrowhead = circle]
      rankdir = LR
      S->M
      S->W
      S->R
      S->F
      S->T
      S->D
      W->F
      R->F
      
      edge [color = red,
            arrowhead = circle]
      W->M
      F->M
      R->M
      T->M

      edge [color = red,
            arrowhead = circle,
            style = dotted]
      D->M
      
      # Graph
      graph [overlap = true, fontsize = 10]
      
      }")

# Distance as metric if wind is causal path
library(DiagrammeR)
grViz("
      digraph causal {
      
      # Nodes
      node [shape = plaintext]
      S [label = 'Hurricane\n Katrina']
      M [label = 'West Nile\n neuroinvasive\n disease']
      W [label = 'Maximum\n sustained\n wind']
      X [label = 'Damage\n to homes']
      D [label = 'Distance\n from the\n storm track']
      
      # Edges
      edge [color = black,
      arrowhead = vee]
      rankdir = LR
      S->W
      S->D
      W->X
      X->M
      
      # Graph
      graph [overlap = true, fontsize = 10]
      
      }")

# Katrina--- Rain versus wind
library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
library(sp)
library(maptools)
library(rgeos)
library(dplyr)
library(viridis)
library(gridExtra)

katrina_distance <- filter_storm_data(storm = c("Katrina-2005"), include_rain = TRUE,
                              days_included = c(-2, -1, 0, 1),
                              output_vars = c("storm_id", "fips", "tot_precip", "storm_dist"))
katrina_wind <- data.table::data.table(hurricaneexposuredata::storm_winds)
katrina_wind <- katrina_wind[get("storm_id") == "Katrina-2005"]
katrina_wind <- katrina_wind %>% 
  tbl_df() %>% 
  select(fips, vmax_sust)
map_data <- left_join(katrina_distance, katrina_wind, by = "fips")

map_dim <- apply(matrix(c(-106.65037, 29.5, -67.00742, 35),
                        byrow = TRUE, ncol = 2),
                 MARGIN = 2,
                 function(x) range(x) + c(-1, 1) * 2)
tracks <- hurricaneexposuredata::hurr_tracks %>%
  dplyr::select_(~ latitude, ~ longitude, ~ storm_id,
                 ~ date) %>%
  dplyr::filter_(~ as.character(storm_id) %in% c("Katrina-2005") &
                   longitude > map_dim[1, 1] &
                   longitude < map_dim[2, 1] &
                   latitude > map_dim[1, 2] &
                   latitude < map_dim[2, 2]) %>%
  dplyr::mutate_(date = ~ lubridate::ymd_hm(date)) 
full_tracks <- hurricaneexposure:::interp_track(tracks)

out_data <- hurricaneexposure:::get_eastern_map() %>%
  dplyr::full_join(map_data, by = "fips") %>% 
  mutate(state = stringr::str_extract(polyname, "[a-z]+\\,")) %>% 
  mutate(state = stringr::str_remove(state, "\\,")) %>% 
  filter(state %in% c( "mississippi", "louisiana"))
out_wind <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ vmax_sust),
                        size = 0.2) +
  scale_fill_viridis(discrete = FALSE, option = "A",
                     name = "Maximum sustained\n winds (m/s)") +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") + 
  ggtitle("Sustained winds")

out_rain <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ tot_precip),
                        size = 0.2) +
  scale_fill_viridis(discrete = FALSE, option  = "A",
                     name = "Cumulative\n precipitation (mm)") +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") + 
  ggtitle("Rainfall")

out_distance <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ storm_dist),
                        size = 0.2) +
  scale_fill_viridis(discrete = FALSE, option = "A",
                     name = "Distance to\n storm track (km)",
                     direction = -1) +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") + 
  ggtitle("Distance from storm")

grid.arrange(out_distance, out_wind, nrow = 1) # Continuous exposure

out_data <- out_data %>% 
  mutate(dist_binary = storm_dist <= 80,
         wind_binary = vmax_sust >= 29)

out_distance_binary <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ dist_binary),
                        size = 0.2) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Exposed based\n on distance", 
                     option = "A", begin = 0.75, end = 1, 
                     direction = -1) +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") + 
  ggtitle("Distance from storm")

out_wind_binary <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ wind_binary),
                        size = 0.2) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Exposed based\n on wind", 
                     option = "A", begin = 0.75, end = 1, 
                     direction = -1) +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") + 
  ggtitle("Sustained winds")

grid.arrange(out_distance_binary, out_wind_binary, nrow = 1) # discrete exposure

out_data <- out_data %>% 
  mutate(class = case_when(
    dist_binary & wind_binary ~ "both",
    dist_binary & !wind_binary ~ "distance only",
    !dist_binary & wind_binary ~ "wind only",
    TRUE ~ "neither"
  )) %>% 
  mutate(class = ordered(class, 
                           levels = c("both", "wind only", "distance only", "neither")))

out_class <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ class),
                        size = 0.2) +
  scale_fill_manual(name = "Exposure\nassessment", 
                     values = c("darkblue", "cyan", "lightblue", "lightyellow")) +
  theme_void() +
  coord_map() +
  ggplot2::borders("state", regions = c("mississippi", "louisiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::geom_path(data = full_tracks,
                     ggplot2::aes_(x = ~ longitude,
                                   y = ~ latitude,
                                   group = ~ storm_id), color = "firebrick") +
  theme(legend.position = "bottom") 

out_class

# Katrina exposures
library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

storm <- "Katrina-2005"

distance_data <- filter_storm_data(storm = storm,
                                   output_vars = c("fips", "storm_dist")) %>%
  dplyr::mutate_(distance = ~ storm_dist <= 100) %>%
  dplyr::select(fips, distance) %>%
  dplyr::tbl_df()

rain_data <- filter_storm_data(storm = storm,
                               days_included = c(-2, -1, 0, 1),
                               include_rain = TRUE,
                               output_vars = c("fips", "tot_precip",
                                               "storm_dist")) %>%
  dplyr::mutate_(rain = ~ tot_precip >= 75 &
                   storm_dist <= 500) %>%
  dplyr::select(fips, rain) %>%
  dplyr::tbl_df()

wind_data <- filter_wind_data(storm = storm, wind_source = "modeled",
                              output_vars = c("fips", "vmax_sust")) %>%
  `colnames<-`(c("fips", "wind_value")) %>%
  dplyr::mutate_(wind = ~ wind_value >= 15) %>%
  dplyr::select(fips, wind) %>%
  dplyr::tbl_df()

storm_year <- gsub("*.+-", "", storm)
counties <- hurricaneexposuredata::closest_dist %>%
  dplyr::filter_(~ storm_id == storm) %>%
  dplyr::select_(quote(fips), quote(storm_dist))

flood_data <- county_events(counties = counties$fips,
                            start_year = storm_year,
                            end_year = storm_year,
                            event_type = "flood") %>%
  dplyr::filter_(~ storm_id == storm) %>%
  dplyr::select_(quote(fips)) %>%
  dplyr::mutate_(event = ~ 1) %>%
  dplyr::right_join(counties, by = "fips") %>%
  dplyr::mutate_(event = ~ !is.na(event)) %>%
  dplyr::rename_(flood = ~ event) %>%
  dplyr::select_(quote(-storm_dist))

tornado_data <- county_events(counties = counties$fips,
                              start_year = storm_year,
                              end_year = storm_year,
                              event_type = "tornado") %>%
  dplyr::filter_(~ storm_id == storm) %>%
  dplyr::select_(quote(fips)) %>%
  dplyr::mutate_(event = ~ 1) %>%
  dplyr::right_join(counties, by = "fips") %>%
  dplyr::mutate_(event = ~ !is.na(event)) %>%
  dplyr::rename_(tornado = ~ event) %>%
  dplyr::select_(quote(-storm_dist))


storm_data <- distance_data %>%
  full_join(rain_data, by = "fips") %>%
  full_join(wind_data, by = "fips") %>%
  full_join(flood_data, by = "fips") %>%
  full_join(tornado_data, by = "fips") %>%
  gather(key = metric, value = Exposed, -fips) %>%
  mutate(metric = factor(metric,
                         levels = c("distance", "rain", "wind", "flood", "tornado"),
                         labels = c("Distance-based metric", "Rain-based metric",
                                    "Wind-based metric", "Flood-based metric",
                                    "Tornado-based metric")))

out_data <- hurricaneexposure:::get_eastern_map() %>%
  dplyr::left_join(storm_data, by = "fips")
out <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = out_data,
                        ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                      fill = ~ Exposed, color = ~ Exposed),
                        size = 0.2) +
  ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                        "georgia", "florida", "alabama", "kentucky",
                                        "tennessee", "maryland", "west virginia",
                                        "district of columbia", "pennsylvania",
                                        "new jersey", "delaware", "mississippi",
                                        "louisiana", "texas", "oklahoma", "arkansas",
                                        "new york", "connecticut", "rhode island",
                                        "massachusetts", "new hampshire", "vermont",
                                        "maine", "kansas", "missouri", "iowa", "michigan",
                                        "illinois", "ohio", "wisconsin", "indiana"),
                   colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
  ggplot2::theme_void() +
  ggplot2::coord_map() +
  viridis::scale_fill_viridis(discrete = TRUE, begin = 0.5,
                              end = 1, option = "A", direction = -1,
                              name = "", labels = c("Unexposed", "Exposed")) +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.5,
                               end = 1, option = "A", direction = -1,
                               name = "", labels = c("Unexposed", "Exposed")) +
  facet_wrap(~ metric, ncol = 3) +
  theme(legend.position=c(.8,.15))

katrina <- map_tracks(storm, plot_object = out) +
  ggtitle("Katrina (2005)")

# Seasonality of exposures in Miami
library(hurricaneexposuredata)
library(hurricaneexposure)
library(lubridate)
library(ggplot2)

wind <- county_wind(counties = c("12086", "37055", "28045"), 
                    start_year = 1988,
                    end_year = 2015, wind_limit = 15) %>% 
  mutate(month = month(closest_date, label = TRUE, abbr = FALSE)) %>% 
  group_by(fips, month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(fips = factor(fips, levels = c("12086", "37055", "28045"),
                       labels = c("Miami-Dade, FL", 
                                  "Dare, NC",
                                  "Hancock, MS")))
out <- ggplot(wind, aes(x = month, y = n)) + 
  geom_col() + 
  labs(x = "", y = "# of storm exposures\n(wind-based)") + 
  theme_classic() + 
  ggtitle("Number of storm exposures, 1988--2015") + 
  facet_wrap(~ fips, ncol = 1)
  

# Timeline of storms
library(hurricaneexposuredata)
library(hurricaneexposure)
library(lubridate)
library(ggrepel)

wind <- county_wind(counties = c("12086"), 
                    start_year = 1988,
                    end_year = 2010, wind_limit = 30) %>% 
  filter(year(closest_date) >= 1999) %>% 
  select(storm_id, closest_date) %>% 
  mutate(closest_date = ymd(closest_date),
         year = year(closest_date),
         doy = yday(closest_date))
all_years <- data_frame(year = 1999:2006)
a <- ggplot(wind, aes(x = doy, y = year, label = storm_id)) + 
  geom_segment(data = all_years, aes(y = year, yend = year, label = NA),
               x = 0, xend = 365, alpha = 0.2) + 
  geom_point(aes(color = storm_id)) + 
  geom_text_repel() + 
  scale_y_reverse(breaks = 1999:2006) + 
  xlim(c(1, 365)) +
  labs(x = "Day in year", y = "") +
  theme_classic(base_size = 12) + 
  theme( strip.background  = element_blank(),
         panel.border = element_blank(),
         axis.ticks = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.text.y = element_text(hjust = 1),
         axis.line.y = element_blank(),
         legend.position = "none")

wind_control <- wind %>% 
  bind_rows(wind) %>% 
  mutate(year = c(2001, 1999, 2000, 2006, 2003, 2003))

b <- a + 
  geom_vline(aes(xintercept = doy, color = storm_id), alpha = 0.5) + 
  geom_point(data = wind_control, aes(color = storm_id), fill = "black", shape = 21)

grid.arrange(a + ggtitle("Miami-Dade, FL, storm exposures, 1999--2006"), 
             b + ggtitle("Selecting control days for storm exposures"),
             ncol = 1)
