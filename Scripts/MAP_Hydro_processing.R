#### Event 1 Water yr: max # of consec. flood and dry events, mean 4 yr WD, Max WD

library(tidyverse)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(fasstr)
library(tidyr)



MAP_Hydro <- read_csv('M1M3M4_WSE.csv',
                       guess_max = 8036,
                       col_types = cols(
                         Date = col_date(format = '%Y-%m-%d')
                       ))


MAP_GE <- read_csv('M1M3M4_GE.csv')



### Gather sites and convert them from horizontal to vertical

MAP_Hydro2 <- MAP_Hydro %>%
  pivot_longer(
    cols = - Date,
    names_to = "Site",
    values_to = "WSE"
  )


# Join water depth and ground elevation

MAP_Hydro_GE <- MAP_Hydro2 %>%
  left_join(MAP_GE, by = "Site")



# Create function to generate water year values,
# with a water year defined as starting in May
wtr_yr <- function(dates, start_month=5) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}



#Calculate calander year based on dates
cal_yr <-function(dates, start_month=1) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 
  # Return the water year
  adj.year
} 


#Calculate water depth (Water surface elevation - digital elevation model 
MAP_WaterDepth <- mutate(MAP_Hydro_GE, 
                          WD = as.numeric(WSE) - GE)





#Add calendar and water years
MAP_WaterDepth <- MAP_WaterDepth %>%
  mutate(WaterYear = wtr_yr(Date),
         CalendarYear = cal_yr(Date))


## Summarize number of water depth observations at each site for the year range
### in the water elevation and water depth file 

sum.test = CSSS_WaterDepth %>% count(Sites, WaterYear)

sumtest2. = sum.test %>% pivot_wider(names_from = WaterYear, values_from = n)



## 3, 7, and 30 day moving average
Hydro_rollingMeans = fasstr::add_rolling_means(CSSS_WaterDepth,
                                               dates = Date,
                                               values = water_depth,
                                               groups = Sites,
                                               roll_days = c(3,7,30),
                                               roll_align = 'right')

Hydro_rollingMeans_WetEvent = select(Hydro_rollingMeans, Date, Sites, X, Y, water_depth, WaterYear, 
                                     CalendarYear, Q3Day)



###Calculate the maximum number of consecutive wet and dry events
numNA = is.na(Hydro_rollingMeans_WetEvent$Q3Day) # number of NAs in 3 day MA. The purpose of this
summary(numNA) # is to check how many NA's are present and how much of the data does is account for
#4,865,130 total observations, 4864020 FALSE, 1110  TRUE. 0.02%; Drop NAs

Hydro_rollingMeans_WetEvent2 = Hydro_rollingMeans_WetEvent %>% drop_na(Q3Day) %>%  
  select(Date, Sites, X, Y, water_depth, WaterYear, CalendarYear, Q3Day)

#### Add sampling years

CSSS_Event1_VegSite_SampleYr = read_csv('E:/CSSS_Event1_VegSite_SampleYr.csv')

Hydro_rollingMeans_WetEvent2 = Hydro_rollingMeans_WetEvent2 %>%
  inner_join(CSSS_Event1_VegSite_SampleYr, by = 'Sites')



rollingMeans_2003_Hydro = Hydro_rollingMeans_WetEvent2 %>%
  filter(Samp_Yr == 2003)


t1 = rollingMeans_2003_Hydro %>%
  filter(WaterYear %in% c(1999,2000,2001,2002))


rollingMeans_2004_Hydro = Hydro_rollingMeans_WetEvent2 %>%
  filter(Samp_Yr == 2004)

t2 = rollingMeans_2004_Hydro %>%
  filter(WaterYear %in% c(2000,2001,2002,2003))



rollingMeans_2005_Hydro = Hydro_rollingMeans_WetEvent2 %>%
  filter(Samp_Yr == 2005)

t3 = rollingMeans_2005_Hydro %>%
  filter(WaterYear %in% c(2001,2002,2003,2004))


Hydro_rollingMeans_WetEvent3 = rbind(t1,t2,t3)




### @@@#@%#$%@@$!$  CHECK POINT @@@#@%#$%@@$!$###################



Event_1_CalYrHydro3DayMean_ = Hydro_rollingMeans_WetEvent3



### Check to see that there are a total of 555 sites

S_bydest = group_by(Event_1_CalYrHydro3DayMean_, Sites)
delay = summarise(S_bydest,
                  count = n())



sum.test = Event_1_CalYrHydro3DayMean_ %>% count(Sites, WaterYear)

sumtest2. = sum.test %>% pivot_wider(names_from = WaterYear, values_from = n)


# Water Year

### Event 1
Event_1_WaterYearHydro3DayMean_ = filter(Hydro_rollingMeans_WetEvent3, WaterYear 
                                         %in% c(1999,2000,2001,2002,2003,2004))

######################################### Max WD PER SITE


Hydro_rollingMeans_WetEvent4 =  Event_1_WaterYearHydro3DayMean_


byMaxWet = group_by(Hydro_rollingMeans_WetEvent4, Sites)
Event_1_WatYr_MaxWD = summarise(byMaxWet,
                                MaxWD = max(Q3Day))


# Event 1 ################################


### Calculate the max number of consecutive wet and dry events (>0 WET; <0 DRY)

#### Wet Events

E1df5 = Event_1_WaterYearHydro3DayMean_$'Q3Day_>_0' <-
  ave(Event_1_WaterYearHydro3DayMean_$Q3Day,Event_1_WaterYearHydro3DayMean_$Sites, FUN = function(vec)
    with(rle(vec>0), max(lengths[values])))



Event_1_WaterYearHydro3DayMean_$'Q3Day_>_0'[Event_1_WaterYearHydro3DayMean_$'Q3Day_>_0' == 1] <- 0



names(Event_1_WaterYearHydro3DayMean_) = c("Date", "Sites", "X", "Y","WD", "WaterYear", "CalendarYear", 
                                           "Q3Day", "Pop", "Samp_Yr", "WetQ3Day")



#### Dry Events



E1df6 = Event_1_WaterYearHydro3DayMean_$'Q3Day_<_0' <-
  ave(Event_1_WaterYearHydro3DayMean_$Q3Day,Event_1_WaterYearHydro3DayMean_$Sites, FUN = function(vec)
    with(rle(vec < 0), max(lengths[values])))

Event_1_WaterYearHydro3DayMean_$'Q3Day_<_0'[Event_1_WaterYearHydro3DayMean_$'Q3Day_<_0' == 1] <- 0
names(Event_1_WaterYearHydro3DayMean_) = c("Date", "Sites",  "X", "Y","water_depth", "WaterYear", "CalendarYear", 
                                           "Q3Day", "Pop", "Samp_Yr", "WetQ3Day","DryQ3Day")


######$@#$@#$@#$@#$#@$ CHECK POINT @#$$@#$@#$#@$@$@$@#$###################

Event_1_WatYr_MaxConsecutiveWetDryEvents = Event_1_WaterYearHydro3DayMean_


byday1 = group_by(Event_1_WatYr_MaxConsecutiveWetDryEvents, Sites, WetQ3Day, DryQ3Day)




Event_1_WatYr_MaxConsecutiveWetDryEvents = summarise(byday1,
                                                     WetQ3Day_F = max(WetQ3Day),
                                                     DryQ3Day_F = max(DryQ3Day))





Event_1_WatYr_MaxConsecutiveWetDryEvents2_Summary = Event_1_WatYr_MaxConsecutiveWetDryEvents %>%
  select("Sites", "WetQ3Day", "DryQ3Day")




### Calculate total number of wet and dry events

#### Wet Events
Event_1_WaterYearHydro3DayMean_$WetEvent = ifelse(Event_1_WaterYearHydro3DayMean_$Q3Day > 0,1,0)

byWet1 = group_by(Event_1_WaterYearHydro3DayMean_, Sites, WetQ3Day, DryQ3Day)
Event_1_WatYr_TotalWetEvents = summarise(byWet1,
                                         TotalWetEvents = sum(WetEvent))

#### Dry Events
Event_1_WaterYearHydro3DayMean_$DryEvent = ifelse(Event_1_WaterYearHydro3DayMean_$Q3Day < 0,1,0)


byDry1 = group_by(Event_1_WaterYearHydro3DayMean_, Sites, WetQ3Day, DryQ3Day)
Event_1_WatYr_TotalDryEvents = summarise(byDry1,
                                         TotalDryEvents = sum(DryEvent))



df_list1 = list(Event_1_WatYr_TotalWetEvents, Event_1_WatYr_TotalDryEvents, Event_1_WatYr_MaxWD)


Event1_WatYr_hydrology_final = df_list1 %>% reduce(full_join, by = 'Sites')


Event1_WatYr_hydrology_final2 = Event1_WatYr_hydrology_final %>%
  select(Sites, WetQ3Day.x, DryQ3Day.x,
         TotalWetEvents, TotalDryEvents, MaxWD)


names(Event1_WatYr_hydrology_final2) = c('Sites','WetQ3Day', 'DryQ3Day', 'TotalWetEvents',
                                         'TotalDryEvents', "MaxWD")

Event1_WatYr_hydrology_final2 = Event1_WatYr_hydrology_final2 %>%
  select(Sites,WetQ3Day,TotalWetEvents,MaxWD)

write.csv(Event1_WatYr_hydrology_final2, 'E:/Event1_WatYr_hydrology_final.csv')







#plots
library(ggplot2)

den = density(Event1_WatYr_hydrology_final2$WetQ3Day)

plot(den, frame=FALSE, col = "blue", main="Max # of consecutive flooding events density plot")



den3 = density(Event1_WatYr_hydrology_final2$TotalWetEvents)
plot(den3, frame=FALSE, col = "blue", main="Total # of wet events density plot")



den5 = density(Event1_WatYr_hydrology_final2$MaxWD)
plot(den5, frame=FALSE, col = "blue", main="Max water depth density plot")


library(hrbrthemes)

p <- ggplot(Event1_WatYr_hydrology_final2, aes(x=x) ) +
  geom_density( aes(x = WetQ3Day, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1500, y=0.006, label="WetQ3Day"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = DryQ3Day, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1500, y=-0.006, label="DryQ3Day"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")

t <- ggplot(Event1_WatYr_hydrology_final2, aes(x=x) ) +
  geom_density( aes(x = TotalWetEvents, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=1500, y=0.0012, label="TotalWetEvents"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = TotalDryEvents, y = -..density..), fill= "#404080") +
  geom_label( aes(x=1500, y=-0.0012, label="TotalDryEvents"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")



Event1_WatYr_hydrology_final2 %>%
  ggplot(aes(x=MaxWD)) + geom_density(fill = "#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Max. water depth during Event 1 in the CSSS habitats") +
  theme_ipsum()





















