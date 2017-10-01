# Finding the most severe storms in the US
Juan C. López Tavera  
9/22/2017  



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


## Introduction

This is a reproducible research report made with knitr for completing the requirements of the JHU's Reproducible Research Course. 

This documents describe the exploration of U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, which tracks weather phenomena, their characteristics and casualties caused by them. 

The main objective of this data exploration is to answer the questions:

1. Across the USA, which types of events are most harmful with respect to population health?
2. Across the USA, which types of events have the greatest economic consequences?


## Data reading

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
3. `COUNTYNAME`: The county where each weather event occurred. 
4. `BGN_DATE`: The reported date when each weather event was was first noticeable.
5. `END_DATE`: The reported date when each weather event ceased. 
6. `EVTYPE`: The classification of each weather event as one of the 48 types defined by the NOAA.
7. `FATALITIES`: The number of direct and indirect human lives lost due to ach weather event. 
8. `INJURIES`: The number of reported injuries caused by each weather event.
9. `PROPDMG`: The cost of property damage in USD (raw)
10. `PROPDMGEXP`: The powers of 10 by which `PROPDMG` shall be multiplied to arrive at the final property damage cost in USD.
11. `CROPDMG`: The cost of aggricultural losses/damage in USD (raw)
12. `CROPDMGEXP`: The powers of 10 by which `CROPDMG` shall be multiplied to arrive at the final property damage cost in USD.

Now, we subset the NOAA data set: 


```r
storm_data <- storm_data %>% select(REFNUM, STATE, COUNTYNAME, BGN_DATE, END_DATE, 
    EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
```

So we have a tibble data frame —subset of the NOAA data set provided at the Reproducible Research Course— of 902297 rows by 12 columns, with 1329347 missing values, which are 12.28% of the observations.

The classes of our NOAA storm database, `storm_data`, and the proportion of missing values for each column are shown in the following table:


```r
## We make a tbl of column classes, the percentage of NAs; and display it
classes <- sapply(storm_data, class) %>% as_data_frame() %>% rownames_to_column("Variable") %>% 
    select(Variable, Type = value)

classes$`Percent of NAs` <- paste(sapply(storm_data, function(x) round(mean(is.na(x)) * 
    100, 2)), "%")

classes$`Unique Values` <- sapply(storm_data, function(x) length(unique(x)))

kable(x = classes, align = c("l", "c", "r", "r"))
```



Variable        Type       Percent of NAs   Unique Values
-----------  -----------  ---------------  --------------
REFNUM         numeric                0 %          902297
STATE         character               0 %              72
COUNTYNAME    character            0.18 %           29601
BGN_DATE      character               0 %           16335
END_DATE      character           26.98 %            6663
EVTYPE        character               0 %             985
FATALITIES     numeric                0 %              52
INJURIES       numeric                0 %             200
PROPDMG        numeric                0 %            1390
PROPDMGEXP    character           51.64 %              19
CROPDMG        numeric                0 %             432
CROPDMGEXP    character           68.54 %               9

From the table, we can notice a couple of major oddities: 

1. The number of unique States in `STATE` is 72, while it should be 50.
2. The number of different events `EVTYPE` is 985, while the NOAA has defined just 48.



## Results

### Most harmful meteorological events

Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?




### Most costly meteorological events

Across the United States, which types of events have the greatest economic consequences?







