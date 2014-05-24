---
title: "Analysis of Storm Events"
output:
  html_document:
    self_contained: true
---

Analysis Of Storm Events
========================================================

Two questions to answer:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?


```r
library(stringr)
library(data.table)
library(scales)
library(ggplot2)
```



```r
# read the data - read.csv can handle bz2 files. This is a long running
# opreation, we'll want to cache it.
raw.data <- read.csv(file = "repdata-data-StormData.csv.bz2", strip.white = TRUE)
```



```
##    STATE__   BGN_DATE   BGN_TIME  TIME_ZONE     COUNTY COUNTYNAME 
##          0          0          0          0          0          0 
##      STATE     EVTYPE  BGN_RANGE    BGN_AZI BGN_LOCATI   END_DATE 
##          0          0          0          0          0          0 
##   END_TIME COUNTY_END COUNTYENDN  END_RANGE    END_AZI END_LOCATI 
##          0          0         NA          0          0          0 
##     LENGTH      WIDTH          F        MAG FATALITIES   INJURIES 
##          0          0          3          0          0          0 
##    PROPDMG PROPDMGEXP    CROPDMG CROPDMGEXP        WFO STATEOFFIC 
##          0          0          0          0          0          0 
##  ZONENAMES   LATITUDE  LONGITUDE LATITUDE_E LONGITUDE_    REMARKS 
##          0       3040          0       3051          0          0 
##     REFNUM 
##          0
```

```
## [1] 985
```

```
## [1] 898
```




```r
data <- raw.data


# date type conversions
data$BGN_DATE <- as.Date(data$BGN_DATE, format = "%m/%d/%Y")
data$END_DATE <- as.Date(data$BGN_DATE, format = "%m/%d/%Y")

# what does our weather event distribution look like?
DT <- data.table(data)
results <- DT[, length(BGN_DATE), by = as.numeric(format(BGN_DATE, "%Y"))]
setnames(results, c("Year", "Count"))

p <- ggplot(results, aes(x = Year, y = Count)) + geom_point(color = "blue") + 
    geom_vline(x = 1994, color = "red") + annotate("text", x = 1991, y = 55000, 
    label = "1994", color = "red") + ggtitle("Number of Weather Events Recorded Per Year") + 
    scale_y_continuous(labels = comma)
print(p)
```

![plot of chunk process-raw-data](figure/process-raw-data.png) 

```r

# Event Types are messy.  valid event types from the docs
event.types <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", 
    "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", 
    "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", 
    "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", 
    "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", 
    "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning", "Marine Hail", 
    "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", 
    "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
    "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
    "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
event.types <- toupper(event.types)

# Now clean up a bunch of event types as best we can. We don't want to get
# carried away generalizing, but we should make some effort

# lets ease our data cleanup effort a bit. For now, we'll only look at
# events that have FATALITIES, INJURIES, PROPDMG, CROPDMG as those are the
# areas that we'll be focusing on in this analysis.

data <- subset(data, !is.na(FATALITIES) | !is.na(INJURIES) | !is.na(PROPDMG) | 
    !is.na(CROPDMG))

data$EVTYPE2 <- toupper(str_trim(data$EVTYPE))

data[data$EVTYPE == "AVALANCE", ]$EVTYPE2 <- "AVALANCHE"

data[data$EVTYPE == "WIND", ]$EVTYPE2 <- "HIGH WIND"
data[data$EVTYPE == "HIGH WINDS", ]$EVTYPE2 <- "HIGH WIND"
data[data$EVTYPE == "HIGH WIND AND HIGH TIDES", ]$EVTYPE2 <- "HIGH WIND"

data[data$EVTYPE == "TSTM WIND", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM WINDS", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM WINS", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM WINDS LIGHTNING", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM WINDS/HAIL", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM WINDS HAIL", ]$EVTYPE2 <- "THUNDERSTORM WIND"
data[data$EVTYPE == "THUNDERSTORM", ]$EVTYPE2 <- "LIGHTNING"
data[data$EVTYPE == "LIGHTING", ]$EVTYPE2 <- "LIGHTNING"
data[data$EVTYPE == "LIGHTNING AND HEAVY RAIN", ]$EVTYPE2 <- "LIGHTNING"
data[data$EVTYPE == "LIGHTNING/HEAVY RAIN", ]$EVTYPE2 <- "LIGHTNING"

data[data$EVTYPE == "HEAVY RAINS", ]$EVTYPE2 <- "HEAVY RAIN"
data[data$EVTYPE == "HEAVY RAIN/LIGHTNING", ]$EVTYPE2 <- "HEAVY RAIN"
data[data$EVTYPE == "HIGH WINDS HEAVY RAINS", ]$EVTYPE2 <- "HEAVY RAIN"

data[data$EVTYPE == "FREEZING RAIN", ]$EVTYPE2 <- "WINTER WEATHER"
data[data$EVTYPE == "SNOW", ]$EVTYPE2 <- "WINTER WEATHER"
data[data$EVTYPE == "SNOW/ICE", ]$EVTYPE2 <- "WINTER WEATHER"
data[data$EVTYPE == "BLIZZARD WEATHER", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "HIGH WIND/BLIZZARD", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "BLIZZARD/HIGH WIND", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "HIGH WIND/ BLIZZARD", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "HIGH WIND AND HEAVY SNOW", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "HIGH WIND/BLIZZARD/FREEZING RA", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "HEAVY SNOW/HIGH", ]$EVTYPE2 <- "BLIZZARD"
data[data$EVTYPE == "ICE STORM/FLASH FLOOD", ]$EVTYPE2 <- "ICE STORM"

data[data$EVTYPE == "HURRICANE OPAL/HIGH WINDS", ]$EVTYPE2 <- "HURRICANE (TYPHOON)"
data[data$EVTYPE == "HURRICANE ERIN", ]$EVTYPE2 <- "HURRICANE (TYPHOON)"
data[data$EVTYPE == "HURRICANE OPAL", ]$EVTYPE2 <- "HURRICANE (TYPHOON)"

data[data$EVTYPE == "COLD", ]$EVTYPE2 <- "COLD/WIND CHILL"
data[data$EVTYPE == "FREEZE", ]$EVTYPE2 <- "FROST/FREEZE"
data[data$EVTYPE == "WIND CHILL", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "EXTREME COLD", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "HIGH WIND/LOW WIND CHILL", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "RECORD COLD", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "RECORD LOW", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "RECORD COLD AND HIGH WIND", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"
data[data$EVTYPE == "HIGH WINDS AND WIND CHILL", ]$EVTYPE2 <- "EXTREME COLD/WIND CHILL"

data[data$EVTYPE == "RECORD HIGH TEMPERATURE", ]$EVTYPE2 <- "EXCESSIVE HEAT"
data[data$EVTYPE == "RECORD HIGH", ]$EVTYPE2 <- "EXCESSIVE HEAT"

data[data$EVTYPE == "FLASH FLOODING/THUNDERSTORM WI", ]$EVTYPE2 <- "FLASH FLOOD"
data[data$EVTYPE == "FLASH FLOODING", ]$EVTYPE2 <- "FLASH FLOOD"
data[data$EVTYPE == "BREAKUP FLOODING", ]$EVTYPE2 <- "FLOOD"
data[data$EVTYPE == "FLOODING", ]$EVTYPE2 <- "FLOOD"
data[data$EVTYPE == "RIVER FLOOD", ]$EVTYPE2 <- "FLOOD"
data[data$EVTYPE == "HIGH TIDES", ]$EVTYPE2 <- "STORM SURGE/TIDE"
data[data$EVTYPE == "TORNADO F0", ]$EVTYPE2 <- "TORNADO"
data[data$EVTYPE == "FUNNEL", ]$EVTYPE2 <- "FUNNEL CLOUD"
data[data$EVTYPE == "WALL CLOUD", ]$EVTYPE2 <- "FUNNEL CLOUD"
data[data$EVTYPE == "WALL CLOUD/FUNNEL CLOUD", ]$EVTYPE2 <- "FUNNEL CLOUD"

data[data$EVTYPE == "HAIL 1.75)", ]$EVTYPE2 <- "HAIL"

# x <- unique(data$EVTYPE2) x <- x[order(x)] x

x <- unique(data$EVTYPE2[!(data$EVTYPE2 %in% event.types)])
length(x)
```

```
## [1] 800
```

```r
head(x, 12)
```

```
##  [1] "HEAVY SNOW/HIGH WINDS/FREEZING" "LOW TEMPERATURE RECORD"        
##  [3] "MARINE MISHAP"                  "WIND CHILL/HIGH WIND"          
##  [5] "HIGH WIND/WIND CHILL/BLIZZARD"  "HIGH WIND/WIND CHILL"          
##  [7] "HIGH WIND/HEAVY SNOW"           "HIGH TEMPERATURE RECORD"       
##  [9] "FLOOD WATCH/"                   "RECORD HIGH TEMPERATURES"      
## [11] "HIGH WIND/SEAS"                 "HIGH WINDS/HEAVY RAIN"
```



```r
DT <- data.table(subset(data, data$BGN_DATE >= "1994-01-01"))
results <- DT[, sum(FATALITIES + INJURIES), by = EVTYPE]
results <- results[order(V1, na.last = TRUE, decreasing = TRUE), ]
top25 <- head(results, 25)

p <- ggplot(top25, aes(x = reorder(EVTYPE, V1), y = V1)) + geom_bar(stat = "identity", 
    fill = "blue") + coord_flip() + scale_y_continuous(labels = comma) + ggtitle("Weather Events Affecting the Population - Top 25\nFatalities and Injuries\nJan. 1994 - Nov. 2011") + 
    xlab("Weather Event") + ylab("Nbr Injuries and Fatalities")
print(p)
```

![plot of chunk harm-to-population](figure/harm-to-population.png) 

```r

head(top25, 2)
```

```
##            EVTYPE    V1
## 1:        TORNADO 24164
## 2: EXCESSIVE HEAT  8428
```



```r
# excessive guesswork. those field guys need better data entry software so
# that a bunch of unnecessary hooey isn't flooding our data. (Ha, ha. Get
# it? 'flooding'? ha!)

# PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP EXP: <U+201C>K<U+201D> for
# thousands, <U+201C>M<U+201D> for millions, and <U+201C>B<U+201D> for
# billions

# > unique(data$CROPDMGEXP) [1] M K m B ? 0 k 2 Levels: 0 2 ? B K M k m >
# unique(data$PROPDMGEXP) [1] K M B m + 0 5 6 ? 4 2 3 h 7 H - 1 8 Levels: +
# - 0 1 2 3 4 5 6 7 8 ? B H K M h m

get.multiplier <- function(ch) {
    return(switch(EXPR = ch, K = 1000, M = 1e+06, B = 1e+09, 1))
}
v.get.multiplier <- Vectorize(get.multiplier)

data$PROPDMGEXP <- toupper(str_trim(data$PROPDMGEXP))
data$CROPDMGEXP <- toupper(str_trim(data$CROPDMGEXP))

data$PROPDMG_CALC <- data$PROPDMG * v.get.multiplier(data$PROPDMGEXP)
data$CROPDMG_CALC <- data$CROPDMG * v.get.multiplier(data$CROPDMGEXP)

DT <- data.table(subset(data, data$BGN_DATE >= "1994-01-01"))
results <- DT[, sum(PROPDMG_CALC + CROPDMG_CALC), by = EVTYPE]
results <- results[order(V1, na.last = TRUE, decreasing = TRUE), ]
top25 <- head(results, 25)

p <- ggplot(top25, aes(x = reorder(EVTYPE, V1), y = V1)) + geom_bar(stat = "identity", 
    fill = "blue") + coord_flip() + scale_y_continuous(labels = comma) + ggtitle("Weather Events and Economic Consequences - Top 25\nProperty and Crop Damange\nJan. 1994 - Nov. 2011") + 
    xlab("Weather Event") + ylab("Nbr Injuries and Fatalities")
print(p)
```

![plot of chunk economic-consequences](figure/economic-consequences.png) 

```r

head(top25, 2)
```

```
##               EVTYPE        V1
## 1:             FLOOD 1.497e+11
## 2: HURRICANE/TYPHOON 7.191e+10
```
