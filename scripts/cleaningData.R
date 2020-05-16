
#============ This is the dirty script I used to analyse the data. Not recommended to use. ========================

library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

#Download the file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(fileUrl, destfile='./data/weatherData.bz2')

#load the data using fread (considerably fast): approximately 10 seconds on my system
firstStormdf <- fread('./data/weatherData.bz2',
                      header=T, stringsAsFactors=F, sep=',')

#check for NA values across the data
summary(firstStormdf)

#check structure of the dataframe
str(firstStormdf)

#just a few, negligible NA's. Especially not critical to this analysis
#ignore them, take note.

#EVTYPE variable should have 48 events, according to the Storm data documentation. Check 'Storm Data Event Table'
#Unique(stormdf$EVTYPE) returns over 900 observations. Have to clean that up.

#unique(newStormdf$COUNTYNAME) returns county names. Some have numbers within them. Should not be.
#remove rows with county names that have numbers within them; newStormdf
#thirdStormdf <- subset(firstStormdf, !grepl(pattern='[0-9]', firstStormdf$COUNTYNAME))

#1589 county names are empty. No idea what they are. Mean of those are 0.00176 or 0.176 percent of the data: REMOVE THEM
sum(firstStormdf$COUNTYNAME=='')
mean(firstStormdf$COUNTYNAME=='')

#first check nrows of firstStormdf:
nrow(firstStormdf)

#Removing the above:
firstStormdf <- firstStormdf[firstStormdf$COUNTYNAME!="", ]

#check nrows of firstStormdf after removing the above. Difference should be 1589
nrow(firstStormdf)

#remove leading and trailing whitespaces from EVTYPE; and change them all to upper case letters
firstStormdf$EVTYPE <- toupper(trimws(firstStormdf$EVTYPE))

#The function below takes firstStormdf and changes the EVTYPE columns appropriately
replaceWords <- function(firstStormdf){

    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='.*HURRI.*|.*[TYPHOON]{7,}.*',
                                           replacement='HURRICANE (TYPHOON)')
    
    #if it haS mudslide, landslide, landslump, rock slide in its name, classify as DEBRIS FLOW
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^[MUD|LAND|ROCK]{3,}.*[SLIDE|SLUMP]{5}.*|^REMNANTS.*',
                                           replacement='DEBRIS FLOW')
    
    #Marine thunderstorm wind is tricky because of the presence of Thunderstorm wind. I will change its occurence
    #to ZZZZZ and change it back to Marine Thunderstorm wind when done
    #Substitute for 'MARINE Thunderstorm wind'FOR ZZZZZ
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^MARINE.*TSTM.*|^MARINE.*THUNDERSTORM.*',
                                           replacement='MARINE THUNDERSTORM WIND')
    
    #There are non-TSTM winds. Change them to 'WINDS'. TSTM is thunderstorm
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^NON[-[:space:]].*TSTM.*|^STRONG.*WIND.*',
                                            replacement='STRONG WIND')
    
    #Landspout is a type of TORNADO
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[TORNAD]{6,}.*|.*LANDSPOUT.*|^(WALL|ROTATING).*(CLOUD).*|^(LARGE\\sWALL\\sCLOUD).*',
                                            replacement='TORNADO')
    
    #If any event type has 'FLASH' or starts with 'URBAN or SMALL' in its name, change it to 'FLASH FLOOD'
    #On inspection, all "URBAN's and SMALLs are associated with small floods
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[FLASH]{5,}.*|^URBAN.*|^SMALL.*|^HIGHWAY[[:space:]]FLOOD.*|^LOCAL[[:space:]]FLOOD.*|^MINOR[[:space:]]FLOOD.*|^RURAL[[:space:]]FLOOD.*|^STREET[[:space:]]FLOOD.*|^SML.*',
                                            replacement='FLASH FLOOD')
    
    #if it starts with FLOOD change it to FLOOD
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^FLOOD.*|^MAJOR[[:space:]]FLOOD.*|^RIVER.*FLOOD.*|^STREAM[[:space:]]FLOOD.*|^(HIGH|DAM|RAPIDLY|BREAKUP).*(WATER|FAILURE|BREAK|FLOOD).*|.*DROWNING.*',
                                           replacement='FLOOD')
    
    #if it has 'coastal' or it starts with beach, cstl or erosion, in its name, change it to 'COASTAL FLOOD'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='.*[COASTAL]{7,}.*|^(BEACH|CSTL|EROSION|TIDAL).*(EROSION|FLOOD).*|.*EROSION{6,}.*|.*EROSIN.*',
                                           replacement='COASTAL FLOOD')
    
    #If any event type has 'HAIL' in its name, change it to 'HAIL'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(?!MARINE).*HAIL.*',
                                            replacement='HAIL')
    
    #If any event type has 'VOLCANIC' in its name, change it to 'VOLCANIC ASH'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[VOLCANIC]{8,}.*',
                                            replacement='VOLCANIC ASH')
   
    #if it has 'blizzard' in its name, change it to 'BLIZZARD'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[BLIZZARD]{8}.*',
                                            replacement='BLIZZARD')
    
    #if it has 'high wind' or it starts with 'gusty' in its name, change it to 'HIGH WIND'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(?!EXTREME|MARINE|SEVERE|STRONG|THUNDERSTORM|COLD).*HIGH.*WIND.*|^(GUST|WIND|WHIRL|WND|WAKE|GRADIENT).*',
                                            replacement='HIGH WIND')
  
    #if it starts with 'snow' in its name, change it to 'SNOW'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^SNOW.*|^(HEAVY|DRIFTING|FALLING|MODERATE|MOUNTAIN|NEAR|SEASONAL).*(SNOWFALL|SNOW).*',
                                            replacement='HEAVY SNOW')
   
    #if it starts with something similar to 'LIGHTNING', change it to 'LIGHTNING'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='[LIGHTNING]{9,}.*',
                                            replacement='LIGHTNING')
   
    #if it starts with something similar to 'HEAVY RAIN', change it to 'HEAVY RAIN'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^HEAVY[[:space:]].*RAIN.*|^HEAVY[[:space:]].*SHOWE.*|^HEAVY[[:space:]].*PREC.*|.*RAIN.*|^(ABNORMALLY|EXTREMELY|UNSEASONABLY|WET|EXCESSIVE|MONTHLY|MIXED|NORMAL|RECORD).*(WET|WETNESS|YEAR|WEATHER|PRECIPITATION|MONTH).*',
                                            replacement='HEAVY RAIN')
    
    #if it starts with something similar to 'ICE' or 'ICY', change it to 'ICE STORM'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^ICE.*|^ICY.*',
                                            replacement='ICE STORM')
   
    #if it starts with something similar to 'STORM' or 'WINTER', change it to 'WINTER STORM'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^WINTER[[:space:]][STORM]{5}.*|^GLAZE\\/ICE.*',
                                            replacement='WINTER STORM')
  
    #if it starts with something similar to 'WEEATHER', 'MIX', 'LIGHT/SNOW' or
    #'WINTER', change it to 'WINTER WEATHER'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(WINTER|WINTRY|WINTERY).*(MIX|WEATHER)|.*LATE.*|.*(?=LIGHT)(?!LIGHTNING).*',
                                            replacement='WINTER WEATHER')
    
    #if it has something similar to 'FIRE', change it to WILDFIRE
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*FIRE.*',
                                            replacement='WILDFIRE')
    
    #if it has something to do with frost or freeze, change it to FROST/FREEZE
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^(AGRICULTURAL|DAMAGING|EARLY|HARD|EARLY|RECORD|GLAZE|BLACK|PATCHY).*(FREEZE|FROST|ICE).*|.*FREEZE.*|.*FROST.*|^GLAZE.*',
                                           replacement='FROST/FREEZE')
    
    #some freeze can be classified as FREEZING FOG
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^FREEZING.*(DRIZZLE|FOG|SPRAY).*',
                                           replacement='FREEZING FOG')
    
    #These are related to DENSE FOG
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^FOG.*|^VOG.*|.*DENSE[[:space:]].*FOG.*',
                                           replacement='DENSE FOG')
    
    #These are related to DENSE SMOKE
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='.*SMOKE.*',
                                           replacement='DENSE SMOKE')
    
    # #if it has something similar to 'SPOUT', change it to WATERSPOUT
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE, 
                                           pattern='.*(SPOUT).*',
                                           replacement='WATERSPOUT')
    
    #if it has something similar to 'FUNNEL', change it to FUNNEL CLOUD
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[FUNNEL]{6,}.*',
                                            replacement='FUNNEL CLOUD')
    
    #if it has something similar to 'SURF', change it to HIGH SURF
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*(SURF|SWELL).*',
                                            replacement='HIGH SURF')
    
    #if it has something similar to 'AVALANCHE', change it to AVALANCHE
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*[AVALANCHE]{8,}.*',
                                            replacement='AVALANCHE')
    
    #if it has something similar to 'STORM SURGE', change it to STORM SURGE/TIDE
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(STORM|BLOW-OUT|ASTRONOMICAL)\\s(HIGH|SURGE|FORCE|WIND|TIDE).*|^(HIGH\\sTIDE)',
                                            replacement='STORM SURGE/TIDE')
    
    #if it starts with 'to 'STORM SURGE''TROPICAL', change it to TROPICAL STORM
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^TROPICAL[[:space:]]STORM.*',
                                            replacement='TROPICAL STORM')
   
    #Most of these words are associated with extreme cold. Change them to EXTREME COLD/WIND CHILL
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(EXCESSIVE|EXTRME|EXTREME|EXTENDED|RECORD|UNSEASONAL|UNSEASONABLE|UNUSUALLY|PROLONG|UNSEASONABLY|SEVERE|BLOWING).*(COLD|COLDO|LOW|SNOW|WINDCHILL|WIND|COOL).*|^BITTER.*',
                                            replacement='EXTREME COLD/WIND CHILL')
    
    #Most of these words are associated with NORMAL cold. Change them to COLD/WIND CHILL
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(COOL|COLD).*|^LOW[[:space:]]TEMPER.*|.*HYPOTHERM.*',
                                            replacement='COLD/WIND CHILL')
   
    #Most of these words are associated with DUST STORM. Change to DUST STORM.
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^DUST[[:space:]]STORM.*|^BLOWING[[:space:]]DUST.*|^DUSTSTORM.*',
                                            replacement='DUST STORM')
    
    #Most of these words are associated with DUST DEVIL Change to DUST DEVIL.
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^DUST[[:space:]](DEVEL|DEVIL).*|^SAHARAN[[:space:]]DUST.*',
                                            replacement='DUST DEVIL')
   
    #Most of these words are associated with DROUGHT Change to DROUGHT.
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*DRY.*|^.*DROUGHT.*|^DRIEST.*',
                                            replacement='DROUGHT')
    
    #Most of these words are associated with EXTREME HEAT Change to EXCESSIVE HEAT
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(EXCESSIVE|EXTREME|RECORD|HIGH|ABNORMAL|UNSEASONABLY|UNUSUAL|UNUSUALLY|VERY|WARM|PROLONG|TEMPERATURE).*(HEAT|WEATHER|TEMPERATURE|RECORD|WARMTH|WARM).*|^HEATBURST.*|.*HOT.*|.*HYPERTHERMIA.*',
                                            replacement='EXCESSIVE HEAT')
    
    #Most of these words are associated with normal HEAT Change to HEAT
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^HEAT[[:space:]]WAVE.*|^(MONTHLY\\sTEMPERATURE)',
                                            replacement='HEAT')
    
    #Most of these words are associated with lake-effect snow Change to LAKE-EFFECT SNOW
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='^(EARLY|LATE|MONTHLY|WET|LACK|FIRST).*SNOW.*|^LAKE.*EFFECT.*',
                                            replacement='LAKE-EFFECT SNOW')
    
    #Most of these words are associated with sleet Change to SLEET
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                            pattern='.*(?=SLEET).*',
                                            replacement='SLEET')
    
    #Substitute TSTM for 'Thunderstorm wind'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^(TSTM|SEVERE|THUN|THUD|TUND|GUSTY).*(WIND|WND|DAMAGE|THUNDERSTORM).*|^(TSTM|THUNDERSTORM).*|.*(BURST|THUNDERSNOW).*',
                                           replacement='THUNDERSTORM WIND')
    
    #Substitute TSTM for 'Thunderstorm wind'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^(HEAVY|HIGH|ROUGH).*SEAS',
                                           replacement='TSUNAMI')
    
    #Substitute TSTM for 'Thunderstorm wind'
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^(LAKE\\sFLOOD)',
                                           replacement='LAKESHORE FLOOD')
    
    #Most of these words are associated with what I do not know Change to OTHERS
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='^(RIP\\sCURRENTS).*',
                                           replacement='RIP CURRENT')
    
    #Most of these words are associated with what I do not know Change to OTHERS
    firstStormdf$EVTYPE <- str_replace_all(firstStormdf$EVTYPE,
                                           pattern='.*(?=SUMMARY).*|[?]|^(NONE|OTHER)$',
                                           replacement='OTHERS')
    
    return(firstStormdf)
}

secondStormdf <- replaceWords(firstStormdf)

#' #For easier analysis, create a category of the official event types.
#' #Then, match each observation in the EVTYPE column to an appropriate category.
#' #Note: This is perhaps, laborious and demanding but, maybe, fun.
#' #There are only 48, so...just try
#' 
eventCategories <- c('Astronomical Low Tide', 'Avalanche', 'Blizzard', 'Coastal Flood', 'Cold/Wind Chill',
                     'Debris Flow', 'Dense Fog', 'Dense Smoke', 'Drought', 'Dust Devil', 'Dust Storm',
                     'Excessive Heat', 'Extreme Cold/Wind Chill', 'Flash Flood', 'Flood', 'Frost/Freeze',
                     'Funnel Cloud', 'Freezing Fog', 'Hail', 'Heat', 'Heavy Rain', 'Heavy Snow', 'High Surf',
                     'High Wind', 'Hurricane (Typhoon)', 'Ice Storm', 'Lake-Effect Snow', 'Lakeshore Flood',
                     'Lightning', 'Marine Hail', 'Marine High Wind', 'Marine Strong Wind',
                     'Marine Thunderstorm Wind', 'Rip Current', 'Seiche', 'Sleet', 'Storm Surge/Tide',
                     'Strong Wind', 'Thunderstorm Wind', 'Tornado', 'Tropical Depression', 'Tropical Storm',
                     'Tsunami', 'Volcanic Ash', 'Waterspout', 'Wildfire', 'Winter Storm', 'Winter Weather')

#The function below changes all other events not present in eventCategories to 'OTHERS'
changeOthers <- function(secondStormdf, eventCategories){
    
    eventCategories <- toupper(eventCategories)
    
    for(i in 1:length(secondStormdf$EVTYPE)){
        if (secondStormdf$EVTYPE[i] %in% eventCategories){
            next
        } else {
            secondStormdf$EVTYPE[i] <- 'OTHERS'
        }
    }
    
    return(secondStormdf)
}

thirdStormdf <- changeOthers(secondStormdf, eventCategories)


names(thirdStormdf)

unique(thirdStormdf$PROPDMGEXP)

unique(thirdStormdf$CROPDMGEXP)

#According to the website, each unique observation has a value
#We need to convert these letters and signs to their respective exponential

#This function changes the argument column (the column given) to the appropriate exponentials
changeToExponential <- function(col){
    
    for(i in 1:length(col)){
        if(col[i] %in% c('H', 'h')){
            col[i] <- 10^2
        } else if (col[i] %in% c('K', 'k')){
            col[i] <- 10^3
        } else if (col[i] %in% c('M', 'm')){
            col[i] <- 10^6
        } else if (col[i] %in% c('B', 'b')){
            col[i] <- 10^9
        } else if (col[i] %in% paste(seq(0,8))){
            col[i] <- 10^1
        } else if (col[i] %in% c('+')){
            col[i] <- 1
        } else {
            col[i] <- 0
        }
    }
    return(col)
}

thirdStormdf$PROPDMGEXP <- as.numeric(changeToExponential(thirdStormdf$PROPDMGEXP))
thirdStormdf$CROPDMGEXP <- as.numeric(changeToExponential(thirdStormdf$CROPDMGEXP))

#multiply the PROPDMGEXP and PROPDMG COLUMNS. do the same for CROPDMGEXP AND CROPDMG; CREATE NEW COLUMNS

thirdStormdf$PropertyDamage <- thirdStormdf$PROPDMG * thirdStormdf$PROPDMGEXP
thirdStormdf$CropDamage <- thirdStormdf$CROPDMG * thirdStormdf$CROPDMGEXP

#create a TotalDamage column that sums both
thirdStormdf$TotalDamages <- thirdStormdf$PropertyDamage + thirdStormdf$CropDamage

#typify the BGN_DATE column as date type:
thirdStormdf$BGN_DATE <- mdy_hms(thirdStormdf$BGN_DATE)

#According to NOAA the data recording start from Jan. 1950. At that time they recorded one event type, tornado. 
#They add more events gradually and only from Jan. 1996 they start recording all events type.
#So, remove these dates prior to 1996 for a fairer analysis

fourthStormdf <- thirdStormdf[thirdStormdf$BGN_DATE > '1995-12-31 UTC', ]

#Take out the needed columns for my analysis. I do not need them all. 
#I need the EVTYPE, FATALITIES, INJURIES, PropertyDamage, CropDamage and TotalDamages

tidyStormdf <- fourthStormdf %>% select(EVTYPE, FATALITIES, INJURIES, PropertyDamage, CropDamage, TotalDamages)
tidyStormdf$TotalHealthDamage <- tidyStormdf$FATALITIES + tidyStormdf$INJURIES

#Many TotalDamages = 0. Remove them. 
popHealthDamage <- tidyStormdf %>% group_by(EVTYPE) %>% 
    summarise(TotalHealthDamage=sum(TotalHealthDamage)) %>% arrange(desc(TotalHealthDamage))

#plotting the most harmful event type with respect to population health:
ggplot(popHealthDamage[1:10, ], aes(EVTYPE, TotalHealthDamage, col=EVTYPE, size=TotalHealthDamage)) + 
    geom_point() + theme(axis.text.x=element_text(angle=90)) + 
    ggtitle('Top 10 Most Harmful Event Types to Population Health Across the United States') + 
    xlab('Event Type') + ylab('Total Health Damage')

#
ecoDamage <- tidyStormdf %>% group_by(EVTYPE) %>% 
    summarize(EconomicDamage=sum())

# #This function helped me solve a problem:
# run <- function(firstStormdf, eventCategories){
#     x <- sort(unique(firstStormdf$EVTYPE))
#     y <- toupper(eventCategories)
#     return(intersect(x, y))
# }
# 
# run(firstStormdf, eventCategories)
