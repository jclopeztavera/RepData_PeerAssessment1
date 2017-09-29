if (!require(stringdist)) {
  install.packages("stringdist")
}
library(stringdist)

rm(list = ls()[!ls() %in% c("storm_data", "test", "ongoing")])

URL <-
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
path <- paste0(getwd(), "/repdata%2Fdata%2FStormData.csv.bz2")
csvfile <- gsub(pattern = ".bz2",
                replacement = "",
                x = path)
storm_data <- data.table::fread(input = csvfile,
                                showProgress = FALSE,
                                verbose = FALSE)

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

storm_data$BGN_DATE <-
  gsub(
    pattern = " 0:00:00",
    replacement = "",
    x = storm_data$BGN_DATE,
    fixed = TRUE
  )

begin_date <-
  as.Date(x = storm_data$BGN_DATE,
          format = "%m/%d/%Y",
          tz = storm_data$TIME_ZONE)

storm_data$END_DATE <-
  gsub(
    pattern = " 0:00:00",
    replacement = "",
    x = storm_data$END_DATE,
    fixed = TRUE
  )

end_date <-
  as.Date(x = storm_data$END_DATE,
          format = "%m/%d/%Y",
          tz = storm_data$TIME_ZONE)

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
events$clean_events[grepl(pattern = "fld|urban\\/sml stream fld|urban\\/small stream flooding|stream flood", x = events$clean_events)]  <-
  "Flood"
events$clean_events <- gsub(pattern = "winds|wnd",
                            replacement = "wind",
                            x = events$clean_events)
events$clean_events[grepl(pattern = "frost|freeze|freezing|frost|icy roads",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Frost/Freeze"
events$clean_events[grepl(pattern = "cold weather",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Winter Weather"
events$clean_events[grepl(pattern = "cold|chill|record low|cool|low temperature|Hypothermia",
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
events$clean_events[grepl(pattern = "river|slide|dam|ice floes",
                          x = events$clean_events,
                          ignore.case = TRUE)] <- "Flash Flood"
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

human_damage <- storm_data$INJURIES + storm_data$FATALITIES

storm_data$PROPDMGEXP <- tolower(storm_data$PROPDMGEXP)
storm_data$CROPDMGEXP <- tolower(storm_data$CROPDMGEXP)

propdmg_exp_raw <- storm_data$PROPDMGEXP %>% unique
cropdmg_exp_raw <- storm_data$CROPDMGEXP %>% unique

dmg_exp_raw <-
  c(propdmg_exp_raw, cropdmg_exp_raw) %>% unique %>% sort
dmg_exp_raw <-
  dmg_exp_raw[grepl(pattern = "[[:alnum:]]", x = dmg_exp_raw)]

b <- 9
h <- 2
k <- 3
m <- 6
dmg_exp_str <- paste("10^", dmg_exp_raw, sep = "")
dmg_exp <- sapply(parse(text = dmg_exp_str), eval)

prop_dmg_exp_table <-
  data_frame(PROPDMGEXP = dmg_exp_raw, prop_dmg_exp = dmg_exp)
crop_dmg_exp_table <-
  data_frame(CROPDMGEXP = dmg_exp_raw, crop_dmg_exp = dmg_exp)

storm_data <- storm_data %>%
  right_join(x = prop_dmg_exp_table) %>%
  right_join(x = crop_dmg_exp_table)

####################

ongoing <- storm_data %>% transmute(
  id = REFNUM,
  state = state.name,
  state_abbr = state.abb,
  county = capwords(COUNTYNAME, strict = TRUE),
  begin_date = begin_date,
  end_date = end_date,
  event_type = events$clean_events,
  human_damage = human_damage,
  material_damage = (PROPDMG * prop_dmg_exp) + (CROPDMG * crop_dmg_exp)
)
