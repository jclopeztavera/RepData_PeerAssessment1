if (!require(stringdist)) {
  install.packages("stringdist")
}
library(stringdist)

rm(list = ls()[!ls() %in% c("storm_data", "test", "ongoing")])

## Helper function
capwords <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "",
          collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## From the state dataset, creating a table of state names and abbreviations
data(state)
states <- cbind.data.frame(state.abb, state.name)

## Joining the state name-abb table table with
storm_data <- left_join(x = states,
                        y = storm_data,
                        by = c("state.abb" = "STATE"))


begin_date <- paste(storm_data$BGN_DATE, storm_data$BGN_TIME) %>%
  gsub(pattern = "0:00:00",
       replacement = "",
       x = .) %>%
  as.Date(format = "%m/%d/%Y %H%M", tz = storm_data$TIME_ZONE)

official_events <- c(
  "Astronomical Low Tide",
  "Astronomical High Tide",
  "Avalanche",
  "Blizzard",
  "Coastal Flood",
  "Cold/Wind Chill",
  "Dense Fog",
  "Dense Smoke",
  "Drought",
  "Dust Devil",
  "Dust Storm",
  "Excessive Heat",
  "Extreme Cold/Wind Chill",
  "Flash Flood",
  "Flood",
  "Frost/Freeze",
  "Funnel Cloud",
  "Freezing Fog",
  "Hail",
  "Heat",
  "Heavy Rain",
  "Heavy Snow",
  "High Surf",
  "High Wind",
  "Hurricane (Typhoon)",
  "Ice Storm",
  "Lake-Effect Snow",
  "Lakeshore Flood",
  "Lightning",
  "Marine Hail",
  "Marine High Wind",
  "Marine Strong Wind",
  "Marine Thunderstorm Wind",
  "Other",
  "Rip Current",
  "Seiche",
  "Sleet",
  "Storm Surge/Tide",
  "Strong Wind",
  "Thunderstorm Wind",
  "Tornado",
  "Tropical Depression",
  "Tropical Storm",
  "Tsunami",
  "Volcanic Ash",
  "Waterspout",
  "Wildfire",
  "Winter Storm",
  "Winter Weather"
)

original_events <- storm_data$EVTYPE
clean_events <- tolower(original_events)

events <- data_frame(original_events, clean_events)


## General substitutions
events$clean_events <-
  gsub(pattern = "[^[:alpha:]]+",
       replacement = " ",
       x = events$clean_events)
events$clean_events <-
  gsub(pattern = "and",
       replacement = " ",
       x = events$clean_events)


events$clean_events <- gsub(pattern = "tstm",
                            replacement = "thunderstorm",
                            x = events$clean_events)
events$clean_events[grepl(pattern = "fld|urban\\/sml stream fld|urban\\/small stream flooding|stream flood", x = events$clean_events)]  <- "Flood"
events$clean_events <- gsub(pattern = "winds|wnd",
                            replacement = "wind",
                            x = events$clean_events)
events$clean_events[grepl(pattern = "frost|freeze|freezing|frost",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Frost/Freeze"
events$clean_events[grepl(pattern = "cold weather",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Winter Weather"
events$clean_events[grepl(pattern = "cold|chill|record low|cool|low temperature",
                          x = events$clean_events,
                          ignore.case = TRUE)] <-
  "Extreme Cold/Wind Chill"
events$clean_events[grepl(pattern = "hurricane|typhoon",
                          x = events$clean_events,
                          ignore.case = TRUE)] <-
  "Hurricane (Typhoon)"

events$clean_events[grepl(pattern = "ligth snow",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "sleet"
events$clean_events[grepl(pattern = "snow",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Heavy Snow"

events$clean_events[grepl(pattern = "dry microburst|downburst|burst",
                          x = events$clean_events,
                          ignore.case = TRUE)] <-
  "thunderstorm wind"
events$clean_events[grepl(pattern = "wintry mix|glaze|hail",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "hail"
events$clean_events[grepl(pattern = "dry|low rainfall",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Drought"
events$clean_events[grepl(pattern = "record heat|warm|hot|warmth|high temperature record",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Excessive Heat"
events$clean_events[grepl(pattern = "precipitation|rainfall|rain",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Heavy Rain"
events$clean_events[grepl(pattern = "surf",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "High Surf"
events$clean_events[grepl(pattern = "black ice|ice",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "ice storm"
events$clean_events[grepl(pattern = "blowing dust",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "dust storm"
events$clean_events[grepl(pattern = "coastal storm",
                          x = events$clean_events,
                          ignore.case = TRUE)] <-
  "marine thunderstorm"
events$clean_events[grepl(pattern = "funnel",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "funnel cloud"
events$clean_events[grepl(pattern = "river",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Lakeshore Flood"
events$clean_events[grepl(pattern = "fog",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Dense Fog"
events$clean_events[grepl(pattern = "glaze",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "hail"
events$clean_events[grepl(pattern = "beach",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "coastal"


others <- c(
  "apache county",
  "none",
  "summary",
  "unseasonably wet",
  "icy roads",
  "Hypothermia/Exposure",
  "DAM BREAK",
  "ice floes",
  "slide",
  "monthly temperature",
  "record temperature",
  "red flag",
  "temperature record"
)

events$clean_events[grepl(
  pattern = paste(others, collapse = "|"),
  x = events$clean_events,
  ignore.case = TRUE
)] <- "Other"

lcs_dist <- sapply(
  X = tolower(official_events),
  FUN = function(x) {
    stringdist(tolower(events$clean_events), x, method = "lcs")
  }
)

min_lcs <- apply(X = lcs_dist, MARGIN = 1, FUN = which.min)

events$clean_events <- official_events[min_lcs]

ongoing <- storm_data %>% transmute(
  id = REFNUM,
  state = state.name,
  state_abbr = state.abb,
  county = capwords(COUNTYNAME, strict = TRUE),
  begin_date = begin_date,
  event_type = events$clean_events
)
