# Finding the Most Weather Events in the US
Juan C. López Tavera  
9/22/2017  
#### Project Setup


```r
## Loading the required packages for reproducing the report
pkgs <-
  c("data.table",
    "R.utils",
    "tidyverse",
    "knitr",
    "datasets",
    "stringdist")

f <- unlist(lapply(pkgs, require, character.only = TRUE))
if (length(f[!f] > 0)) {
  install.packages(pkgs[!f])
}

invisible(sapply(X = pkgs, FUN = library, character.only = TRUE))


## Setting up all code chunks according to the assignment specifications
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    tidy = TRUE,
    results = "markup",
    include = TRUE,
    message = FALSE,
    warning = FALSE,
    knitr.table.format = "markdown",
    tidy.opts = list(width.cutoff = 80),
    fig.align = "center",
    fig.path = "figure/",
    highlight = TRUE, 
    cache = TRUE
)
```

#### Session Info


```r
sessionInfo()
```

```
## R version 3.4.1 (2017-06-30)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.1
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2       stringdist_0.9.4.6 knitr_1.17        
##  [4] dplyr_0.7.4        purrr_0.2.3        readr_1.1.1       
##  [7] tidyr_0.7.1        tibble_1.3.4       ggplot2_2.2.1     
## [10] tidyverse_1.1.1    R.utils_2.5.0      R.oo_1.21.0       
## [13] R.methodsS3_1.7.1  data.table_1.10.4 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.13     formatR_1.5      cellranger_1.1.0 compiler_3.4.1  
##  [5] plyr_1.8.4       bindr_0.1        forcats_0.2.0    tools_3.4.1     
##  [9] digest_0.6.12    lubridate_1.6.0  jsonlite_1.5     evaluate_0.10.1 
## [13] nlme_3.1-131     gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.1 
## [17] rlang_0.1.2      psych_1.7.8      yaml_2.1.14      parallel_3.4.1  
## [21] haven_1.1.0      xml2_1.1.1       httr_1.3.1       stringr_1.2.0   
## [25] hms_0.3          rprojroot_1.2    grid_3.4.1       glue_1.1.1      
## [29] R6_2.2.2         readxl_1.0.0     foreign_0.8-69   rmarkdown_1.6   
## [33] modelr_0.1.1     reshape2_1.4.2   magrittr_1.5     scales_0.5.0    
## [37] backports_1.1.1  htmltools_0.3.6  rvest_0.3.2      assertthat_0.2.0
## [41] mnormt_1.5-5     colorspace_1.3-2 stringi_1.1.5    lazyeval_0.2.0  
## [45] munsell_0.4.3    broom_0.4.2
```

## Introduction

This is a reproducible research report made with knitr for completing the requirements of the Reproducible Research Course by Johns Hopkins University at Coursera. 

This documents describe the exploration of U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, which tracks weather phenomena, their characteristics and associated casualties. 

The main objective of this data exploration is to answer the questions:

1. Across the USA, which types of events are most harmful with respect to population health?
2. Across the USA, which types of events have the greatest economic consequences?

## Getting and loading the data

First, if necessary, we download the data BZ2 file from the link provided in the course assignment page; then, we read the data into the working environment, and conveniently cache the result of the computation.  


```r
## Downloading the data from the URL in the course assignment page
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
path <- paste0(getwd(), "/repdata%2Fdata%2FStormData.csv.bz2")
csvfile <- gsub(pattern = ".bz2", replacement = "", x = path)

## If the file does not exist, we download it, If it exist and it hasn't been
## uncompressed, we uncompress it Then we read it into the R environment.
if (!file.exists(path)) {
    download.file(url = URL, destfile = path)
    R.utils::bunzip2(path, remove = FALSE)
    storm_data <- data.table::fread(input = csvfile, showProgress = FALSE, verbose = FALSE, 
        na.strings = c("", " ", "NA"))
    
} else if (!file.exists(csvfile)) {
    R.utils::bunzip2(path, remove = FALSE)
    storm_data <- data.table::fread(input = csvfile, showProgress = FALSE, verbose = FALSE, 
        na.strings = c("", " ", "NA"))
    
} else {
    storm_data <- data.table::fread(input = csvfile, showProgress = FALSE, verbose = FALSE, 
        na.strings = c("", " ", "NA"))
}
```

## Dataset Structure

We have a tibble data frame of 902297 rows by 37 columns, with 6645765 missing values, which are 19.91% of the observations.

### Variables of interest

The NOAA storm data set we're using has many variables, but not all of them fall into the scope of this report, which focuses on knowing the human and economic damages caused by reported weather events. Knowing beforehand which variables we're using will speed-up the data processing and analysis.

From the 37 variables in the data set, we're only focusing on: 

1. `REFNUM`: The reference number assigned to each weather event.
2. `STATE`: The state name abbreviation where each weather event occurred. 
3. `BGN_DATE`: The reported date when each weather event was was first noticeable.
4. `END_DATE`: The reported date when each weather event ceased. 
5. `EVTYPE`: The classification of each weather event as one of the 48 types defined by the NOAA.
6. `FATALITIES`: The number of direct and indirect human lives lost due to each weather event. 
7. `INJURIES`: The number of reported injuries caused by each weather event.
8. `PROPDMG`: The cost of property damage in USD (raw)
9. `CROPDMG`: The cost of agricultural losses/damage in USD (raw)

Now, we subset the NOAA data set: 


```r
storm_data <- storm_data %>% select(event_id = REFNUM, STATE, BGN_DATE, END_DATE, 
    EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
```

So we have a tibble data frame —subset of the NOAA data set provided at the Reproducible Research Course— of 902297 rows by 11 columns, with 1327758 missing values, which are 13.38% of the observations.

The classes of our NOAA storm database, `storm_data`, and the proportion of missing values for each column are shown in the following table:


```r
## We make a tbl of column classes, the percentage of NAs; and display it
classes <- sapply(storm_data, class) %>% as_data_frame() %>% rownames_to_column("Variable") %>% 
    select(Variable, Type = value)
## Percentage of missing values, rounded to two decimals
classes$`Percent of NAs` <- paste(sapply(storm_data, function(x) round(mean(is.na(x)) * 
    100, 2)), "%")

## Number of unique values
classes$`Unique Values` <- sapply(storm_data, function(x) length(unique(x)))



## Print table with nice formatting
classes %>% arrange(desc(`Unique Values`), desc(`Percent of NAs`)) %>% kable(x = ., 
    align = c("l", "c", "r", "r"))
```



Variable        Type       Percent of NAs   Unique Values
-----------  -----------  ---------------  --------------
event_id       numeric                0 %          902297
BGN_DATE      character               0 %           16335
END_DATE      character           26.98 %            6663
PROPDMG        numeric                0 %            1390
EVTYPE        character               0 %             985
CROPDMG        numeric                0 %             432
INJURIES       numeric                0 %             200
STATE         character               0 %              72
FATALITIES     numeric                0 %              52
PROPDMGEXP    character           51.64 %              19
CROPDMGEXP    character           68.54 %               9

## Cleaning the data

### Data cleaning tasks 

Looking at the data set, we can notice a couple of major oddities, which should be solved by cleaning the data set: 

1. The number of unique States in `STATE` is 72, while it should be 50.
2. The number of different events `EVTYPE` is 985, while the NOAA has defined just 48.
3. It is already known that `PROPDMG` and `CROPDMG` are not expressed as final USD amounts, but need to be multiplied by the factors `PROPDMG` and `PROPDMG` respectively. `PROPDMG` and `PROPDMG` should be of equal length though. 

What causes such incongruities? How can we make our data set more consistent with the phenomena it characterizes? These are questions that we shall answer in the Data Processing section. 

### Data Processing 

#### Solving the state abbreviations issue

As mentioned above, we need to investigate why to we have 72 states instead of 50. Conveniently, the `datasets` package includes the full and abbreviated names of US states, which we can use to match with our data set abbreviations. Let's take a closer look at the non-matching state abbreviations:


```r
data(state)
states <- cbind.data.frame(state.abb, state.name)

setdiff(x = storm_data$STATE, y = states$state.abb)
```

```
##  [1] "DC" "PR" "ST" "AS" "GU" "MH" "VI" "AM" "LC" "PH" "GM" "PZ" "AN" "LH"
## [15] "LM" "LE" "LS" "SL" "LO" "PM" "PK" "XX"
```

We can tell that some of those abbreviations are US District, Territories, water bodies and regions. We manually record those values and check if there are still any unmatched. 


```r
places <- c(DC = "District of Columbia", PR = "Puerto Rico", AS = "American Samoa", 
    GU = "Guam", MH = "Marshall Islands", VI = "Virgin Islands", LO = "Lake Ontario", 
    LE = "Lake Erie", LS = "Lake Superior", LM = "Lake Michigan", LH = "Lake Huron", 
    LS = "Lake St. Clair", AN = "Atlantic North", AM = "Atlantic South", GM = "Gulf of Mexico", 
    PH = "Hawaii Waters", PZ = "Pacific East")

state_abb <- c(state.abb, names(places))
state_name <- c(state.name, places)
states <- cbind.data.frame(STATE = state.abb, state = state.name)

setdiff(states$STATE, storm_data$STATE)
```

```
## character(0)
```

```r
storm_data <- left_join(x = storm_data, states, by = "STATE")

storm_data <- storm_data %>% select(-STATE)
```

All state (or places, should we say) abbreviations in our data set now have a matching pair.

#### Solving the too many event types issue

There are 985 different values in the `EVTYPE` variable; there should only be 48. We have to, somehow, categorize the remaining 937 unique values as one of the official 48 weather event types. 

A lot of the surplus in event types is due to typos (human errors). Also, there are inconsistencies in data entry, which can be noticed by reading the [data codebook](./docs/storm_data_preparation.pdf) provided by the NOAA; e.g. coding an event as "Microdust", while, in the manual, is categorized as "Thunderstorm Winds". 

We can solve many of these inconsistencies by matching them with the closest official `EVTYPE` string. We do this using the longest common substring method:

"The longest common substring (method='lcs') is defined as the longest string that can be obtained by pairing characters from a and b while keeping the order of characters intact. The lcs-distance is defined as the number of unpaired characters. The distance is equivalent to the edit distance allowing only deletions and insertions, each with weight one."

* van der Loo M (2014). “The stringdist package for approximate string matching.” _The R Journal_, *6*, pp. 111-122. <URL: https://CRAN.R-project.org/package=stringdist>.

In this case, given an official event type, we pair it with the closest element of `EVTYPE`. The drawback of this approach is that we got many "false positives" -- like "fog" being matched to "flood", instead of "dense fog".

Given that there are many values in `EVTYPE` that are semantically close to the official event types, but not string-distance close, and the false positive matches, there was still a lot of manual work to do. 

The series of code chunks below has all the steps to arrive at the clean `EVTYPE` variable. 

We need to define the target `EVTYPE` values, which are the 48 official event types defined by the NOAA: 


```r
official_events <- c("Astronomical Low Tide", "Astronomical High Tide", "Avalanche", 
    "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Dense Fog", "Dense Smoke", "Drought", 
    "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", 
    "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", 
    "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", 
    "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", 
    "Marine Thunderstorm Wind", "Other", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", 
    "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", 
    "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
```

We do some general manual substitutions to make all strings a bit more similar: everything lower case, delete non-alphanumeric characters, and "and" words.  


```r
storm_data <- storm_data %>% mutate(EVTYPE = tolower(EVTYPE), EVTYPE = gsub(pattern = "[^[:alpha:]]+", 
    replacement = " ", x = EVTYPE), EVTYPE = gsub(pattern = "and", replacement = " ", 
    x = EVTYPE, fixed = TRUE))
```

After reading the [data codebook](./docs/storm_data_preparation.pdf), many semantic mismatches were evident: 


```r
## Semantic substitutions, from the codebook tstm --> thunderstorm: very common,
## not close for string matching
storm_data$EVTYPE <- gsub(pattern = "tstm", replacement = "thunderstorm", x = storm_data$EVTYPE)

### Floods homologation of 'flood' variants, including urban and rural floods
storm_data$EVTYPE[grepl(pattern = "fld|urban\\/sml stream fld|urban\\/small stream flooding|stream flood", 
    x = storm_data$EVTYPE)] <- "Flood"

### Homologation of 'wind' to strong wind
storm_data$EVTYPE <- gsub(pattern = "winds|wnd", replacement = "strong wind", x = storm_data$EVTYPE)

## Homologation of frost/freeze variants
storm_data$EVTYPE[grepl(pattern = "frost|freeze|freezing|frost|icy roads", x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "Frost/Freeze"

### cold weather --> winter weather
storm_data$EVTYPE[grepl(pattern = "cold weather", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Winter Weather"

## Homologation of extreme cold wind chill
storm_data$EVTYPE[grepl(pattern = "cold|chill|record low|cool|low temperature|Hypothermia", 
    x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Extreme Cold/Wind Chill"

## Homologation of hurricane (typhoon)
storm_data$EVTYPE[grepl(pattern = "hurricane|typhoon", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Hurricane (Typhoon)"

## Semantic homologation of light snow --> sleet
storm_data$EVTYPE[grepl(pattern = "ligth snow", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "sleet"

## Snow occurrences to heavy snow
storm_data$EVTYPE[grepl(pattern = "snow", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Heavy Snow"

## Semantic homologation of ~burst to thunderstorm winds
storm_data$EVTYPE[grepl(pattern = "dry microburst|downburst|burst", x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "thunderstorm wind"

## Semantic homologation of hail
storm_data$EVTYPE[grepl(pattern = "wintry mix|glaze|hail", x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "hail"

## Drought semantic homologation
storm_data$EVTYPE[grepl(pattern = "dry|low rainfall", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Drought"

## Excessive heat
storm_data$EVTYPE[grepl(pattern = "record heat|warm|hot|warmth|high temperature record", 
    x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Excessive Heat"

## After cleaning drought, substituting synonyms of heavy rain
storm_data$EVTYPE[grepl(pattern = "precipitation|rainfall|rain", x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "Heavy Rain"

## all surf occurrences to high surf
storm_data$EVTYPE[grepl(pattern = "surf", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "High Surf"

# Ice storm
storm_data$EVTYPE[grepl(pattern = "black ice|ice", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "ice storm"

## blowing dust to dust storm
storm_data$EVTYPE[grepl(pattern = "blowing dust", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "dust storm"

## coastal storm to marine thunderstorm
storm_data$EVTYPE[grepl(pattern = "coastal storm|beach", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "marine thunderstorm"

## all funnels to funnel cloud
storm_data$EVTYPE[grepl(pattern = "funnel", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "funnel cloud"

## flood consequences to flash flood (from codebook)
storm_data$EVTYPE[grepl(pattern = "river|slide|dam|ice floes", x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "Flash Flood"

## Manual matching of fog to dense fog
storm_data$EVTYPE[grepl(pattern = "fog", x = storm_data$EVTYPE, ignore.case = TRUE)] <- "Dense Fog"

## there were some ambiguous records, that had to be matched to 'Other'
others <- c("apache county", "none", "summary", "unseasonably wet", "monthly temperature", 
    "record temperature", "red flag", "temperature record")

storm_data$EVTYPE[grepl(pattern = paste(others, collapse = "|"), x = storm_data$EVTYPE, 
    ignore.case = TRUE)] <- "Other"
```

We have a slightly more homogeneous `EVTYPE` variable that is close enough to the official name events. We use the longest common substring method to find how close are each of the values in `EVTYPE` to each of the `official_events`. 


```r
## Distance matrix of official_events by EVTYPE
lcs_dist <- sapply(X = tolower(official_events), FUN = function(x) {
    stringdist(tolower(storm_data$EVTYPE), x, method = "lcs")
})
## Selecting the minimum distance from official_events to EVTYPE ie, the
## official_event type match to the raw EVTYPE
min_lcs <- apply(X = lcs_dist, MARGIN = 1, FUN = which.min)

## Substitutting EVTYPE with the matching official_events
storm_data$event_type <- official_events[min_lcs]

storm_data <- storm_data %>% select(-EVTYPE)
```


#### Estimating the human casualties

Sadly, these weather events cause human deaths and injuries; these numbers are recorded by the NOAA, and will be added to fulfill the definition of "harmful to human health" specified in this document. 


```r
storm_data$human_damage <- storm_data$INJURIES + storm_data$FATALITIES

storm_data <- storm_data %>% select(-c(INJURIES, FATALITIES))
```


#### Calculating the and economic damage to crops and properties

The numbers of human causalities and economic damages are not expressed in their final form; they're rather expressed as coefficients of 10 raised to some power of ten.

`PROPDMG` needs to be multiplied by `PROPDMGEXP` elevated to some power of ten; the same for `CROPDMG * (CROPDMGEXP ^ n)`. 

`PROPDMGEXP` and `CROPDMGEXP` are not explicitly defined as powers of then but rather as some sort abbreviated metric prefixes: "k" (kilo), "m" (mega or millions), "b" (billions). 


```r
## we have unequal lengths of EXP variables, we unify them by transforming them to
## lowercase
storm_data$PROPDMGEXP <- tolower(storm_data$PROPDMGEXP)
storm_data$CROPDMGEXP <- tolower(storm_data$CROPDMGEXP)

## defining their unique values
propdmg_exp_raw <- storm_data$PROPDMGEXP %>% unique
cropdmg_exp_raw <- storm_data$CROPDMGEXP %>% unique

## Merge both variables, get the unique values and sort them alphabetically
dmg_exp_raw <- c(propdmg_exp_raw, cropdmg_exp_raw) %>% unique %>% sort

## Deleting non-alphanumeric characters; +.,-,? are ommited
dmg_exp_raw <- dmg_exp_raw[grepl(pattern = "[[:alnum:]]", x = dmg_exp_raw)]

## Abbreviation for each power, explained
b <- 9
h <- 2
k <- 3
m <- 6

## Scientific notation
dmg_exp_str <- paste("10^", dmg_exp_raw, sep = "")
dmg_exp <- sapply(parse(text = dmg_exp_str), eval)

## tables with computable powers of ten to join to the storm dataset
prop_dmg_exp_table <- dplyr::data_frame(PROPDMGEXP = dmg_exp_raw, prop_dmg_exp = dmg_exp)
crop_dmg_exp_table <- dplyr::data_frame(CROPDMGEXP = dmg_exp_raw, crop_dmg_exp = dmg_exp)

storm_data <- storm_data %>% right_join(x = prop_dmg_exp_table) %>% right_join(x = crop_dmg_exp_table)

storm_data <- storm_data %>% mutate(property_damage = ifelse(is.na(prop_dmg_exp), 
    PROPDMG, PROPDMG * prop_dmg_exp), crop_damage = ifelse(is.na(crop_dmg_exp), CROPDMG, 
    CROPDMG * crop_dmg_exp)) %>% mutate(damages_cost = property_damage + crop_damage) %>% 
    select(-c(contains(match = "dmg", ignore.case = TRUE)), -c(property_damage, crop_damage))
```

#### Usable dates 

```r
storm_data <- storm_data %>% mutate(BGN_DATE = parse_datetime(x = BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), 
    END_DATE = parse_datetime(x = END_DATE, format = "%m/%d/%Y %H:%M:%S")) %>% mutate(year = ifelse((is.na(END_DATE) | 
    year(BGN_DATE) >= year(END_DATE)), year(BGN_DATE), year(END_DATE))) %>% select(-c(BGN_DATE, 
    END_DATE))
```

### Subsetting information-rich years
Not all years the NOAA used to record a wide variety of weather events: before 1993, it only recorded 3 different types of events, which inflates the count of reported events (for Tornado, Thunderstorm Wind and Hail), subsequently biasing the estimates of human and economic damages. 


```r
storm_data <- storm_data %>% filter(year >= 1993)
```

### Ommiting punctual odd reported damage cost observations
There's one damages cost observation which is oddly high: a flood in 2006 in California with an associated damages cost of 115032500000 USD; to put this figure in context, the following reported damages cost was 31300000000 USD, associated to a Storm Surge/Tide in 2005 in Louisiana. It is likely a typo, which we omit. 

Also, there are a couple of suspiciously similar hurricanes (typhoons) in 2005 in Mississippi, both with associated costs of ~7400000000 USD, the difference is that one doesn't have reported human damages. It is likely a repeated observation, which we delete.


```r
## typo in damages cost
storm_data[storm_data$event_id == 605943, ]$damages_cost <- ifelse(test = is.na(storm_data[storm_data$event_id == 
    605943, ]$damages_cost), storm_data[storm_data$event_id == 605943, ]$damages_cost, 
    NA)

## repeated observation
storm_data <- storm_data[storm_data$event_id != 581533, ]
```

#### Adjusting damages costs in USD for inflation
A dollar in 2011 is not the same as a dollar in 1993. We need to adjust for inflation the reported damages costs. 

For this purpose, we use average annual Federal Reserve Bank of St. Louis' Consumer Price Index for All Urban Consumers (CPIAUCSL) from 1993 through 2011 as a measure of inflation. 

The reported damages costs were adjusted for inflation using the scaled CPIAUCSL (taking 2011 CPI as reference, 100). The following chunk shows how this adjustment was performed. 


```r
## Read in the CPIAUCSL data
inflation_data <- read_csv(file = "CPIAUCSL.csv")

## Get the average annual CPI and subset years 1993-2011
annual_cpi_average <- inflation_data %>% mutate(year = year(DATE)) %>% group_by(year) %>% 
    summarise(annual_cpi = mean(CPIAUCSL)) %>% filter((year <= 2011 & year >= 1993))

## Get the value of 2011 CPI as a base
base_cpi <- annual_cpi_average[annual_cpi_average$year == 2011, ]$annual_cpi

## Scaling the annual CPI using the baseline as ref value
annual_cpi_average <- annual_cpi_average %>% mutate(scaled_cpi = (annual_cpi/base_cpi) * 
    100)

## Adjusting for inflation
storm_data <- storm_data %>% left_join(x = ., y = annual_cpi_average, by = "year")
```


## Results

### Most harmful meteorological events

#### Across the United States, which types of events are most harmful with respect to population health?


```r
## Grouping, summarising and desc sorting dmg of weather events
human_damages <- storm_data %>% group_by(event_type) %>% summarise(damages = sum(human_damage, 
    na.rm = TRUE)) %>% arrange(desc(damages))

## Logical vector of the events representing 90% of damages
top90 <- round(cumsum((human_damages$damages/sum(human_damages$damages)) * 100), 
    3) <= 90.5

human_damages <- human_damages[top90, ]
```



### Most costly meteorological events

#### Across the United States, which types of events have the greatest economic consequences?

Using the inflation-adjusted total damages cost, we're able to know the event type which has the greatest economic consequences. 

We group the data by `event_type` and sum the reported damages costs of each event type. We bar plot them to show the most costly event types across US. 


```r
## Grouping, summarising and desc sorting costs of weather events
costs_events <- storm_data %>% group_by(event_type) %>% summarise(cost = sum(damages_cost, 
    na.rm = TRUE)) %>% arrange(desc(cost))

## Logical vector of the events representing 90% of costs
top90 <- round(cumsum((costs_events$cost/sum(costs_events$cost)) * 100), 3) <= 90.5

costs_events <- costs_events[top90, ]
```

For illustrative purposes, the events that represent the 90% of the total damages cost across US are shown (which also happen to be the top 10 most costly wheater event types).


```r
costs_plot <- ggplot(data = costs_events, mapping = aes(x = reorder(event_type, cost), 
    y = cost, fill = cost)) + geom_bar(stat = "identity") + coord_flip() + theme_bw(base_size = 12)

costs_plot
```

<img src="figure/plotting damages costs-1.png" style="display: block; margin: auto;" />





## References


