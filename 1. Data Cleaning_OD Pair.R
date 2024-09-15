########## Merger Impact on Airline Delays - With Origin Destination Pairs ###############

########### 1. INITIAL SETUP - UNZIP DOT Data ###############
library(plyr) # DEACTIVATE this library in Step 2 once Step 1 is done. It confuses dplyr
library(dplyr)
library(readr)

# get all the zip files
# data are downloaded from DOT 
zipF <- list.files(path = "/Users/kuang/Data/DOT", pattern = "*.zip", 
                   full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = "output")

# get the csv files
csv_files <- list.files(path = "/Users/kuang/Data/O1", pattern = "*.csv")

# read the csv files
my_data_1<- ldply(.data = csv_files, .fun = read.csv)

#################### 2. COMBINE DATA #################
library(dplyr)
library(readr)

# Test
# read in every two years as the file is too big using funtion not working due to memory limit.
df1 <- list.files(path="/Users/kuang/Data/O1", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

test = head(df1, 5000)
colnames(test)

test = test[, c("Year","Quarter", 
                "Month", "DayofMonth", "DayOfWeek", 
                "FlightDate", "Reporting_Airline",
                "Origin", "Dest", 
                "DepDelay", "DepDelayMinutes",
                "TaxiOut", "TaxiIn", 
                "ArrDelay", "ArrDelayMinutes", 
                "CarrierDelay", "WeatherDelay",                
                "NASDelay", "SecurityDelay","LateAircraftDelay",
                "Distance" )]


test1 = test %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))



##### 2.1 First Chunk #####
library(dplyr)
library(readr)
df1 <- list.files(path="/Users/kuang/Data/O1", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df1a= df1[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data1 = df1a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data1, file = "data1_24_OD.csv")


##### 2.2 Second Chunk #####
library(dplyr)
library(readr)
df2 <- list.files(path="/Users/kuang/Data/O2", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

df2a= df2[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data2 = df2a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data2, file = "data2_24_OD.csv")


##### 2.3  Third Chunk #######
library(dplyr)
library(readr)
df3 <- list.files(path="/Users/kuang/Data/O3", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

df3a= df3[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data3 = df3a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data3, file = "data3_24_OD.csv")


##### 2.4 Fourth Chunk #####
library(dplyr)
library(readr)
df4 <- list.files(path="/Users/kuang/Data/O4", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

df4a= df4[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data4 = df4a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline,Origin,Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data4, file = "data4_24_OD.csv")

##### 2.5 Fifth Chunk #####
library(dplyr)
library(readr)
df5 <- list.files(path="/Users/kuang/Data/O5", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

df5a= df5[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data5 = df5a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data5, file = "data5_24_OD.csv")


##### 2.6 Sixth Chunk #####
library(dplyr)
library(readr)
df6 <- list.files(path="/Users/kuang/Data/O6", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

df6a= df6[, c("Year","Quarter", 
              "Month", "DayofMonth", "DayOfWeek", 
              "FlightDate", "Reporting_Airline",
              "Origin", "Dest", 
              "DepDelay", "DepDelayMinutes",
              "TaxiOut", "TaxiIn", 
              "ArrDelay", "ArrDelayMinutes", 
              "CarrierDelay", "WeatherDelay",                
              "NASDelay", "SecurityDelay","LateAircraftDelay",
              "Distance" )]

data6 = df6a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                          FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data6, file = "data6_24_OD.csv")

##### 2.7 Year 2009 #####
# Separately handle 2009 as there is a mismatch of columns
library(dplyr)
library(readr)
setwd("/Users/kuang/Data/O31")

jan = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_1.csv")[,1:61]
feb = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_2.csv")[,1:61]
mar = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_3.csv")[,1:61]
apr = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_4.csv")[,1:61]
may = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_5.csv")[,1:61]
jun = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_6.csv")[,1:61]
jul = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_7.csv")[,1:61]
aug = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_8.csv")[,1:61]
sep = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_9.csv")[,1:61]
oct = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_10.csv")[,1:61]
nov = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_11.csv")[,1:61]
dec = read.csv("On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2009_12.csv")[,1:61]

data2009  =dplyr::bind_rows(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

df2009a= data2009[, c("Year","Quarter", 
                      "Month", "DayofMonth", "DayOfWeek", 
                      "FlightDate", "Reporting_Airline",
                      "Origin", "Dest", 
                      "DepDelay", "DepDelayMinutes",
                      "TaxiOut", "TaxiIn", 
                      "ArrDelay", "ArrDelayMinutes", 
                      "CarrierDelay", "WeatherDelay",                
                      "NASDelay", "SecurityDelay","LateAircraftDelay",
                      "Distance" )]

data09 = df2009a %>% group_by(Year, Quarter,Month, DayofMonth, DayOfWeek, 
                              FlightDate, Reporting_Airline, Origin, Dest) %>%
  summarise(TotalFlights = n(),
            TaxiOut_T = sum(TaxiOut, na.rm = T), 
            TaxiIn_T = sum(TaxiIn, na.rm = T),
            DepDelay_T = sum(DepDelay, na.rm = T),
            DepDelayMinutes_T = sum(DepDelayMinutes, na.rm = T),
            ArrDelay_T = sum(ArrDelay, na.rm = T), 
            ArrDelayMinutes_T = sum(ArrDelayMinutes, na.rm = T),
            CarrierDelay_T = sum(CarrierDelay, na.rm = T),
            WeatherDelay_T = sum(WeatherDelay, na.rm = T), 
            NASDelay_T = sum(NASDelay, na.rm = T), 
            SecurityDelay_T = sum(SecurityDelay, na.rm = T), 
            LateAircraftDelay_T = sum(LateAircraftDelay, na.rm = T),
            Dist = mean(Distance))


write.csv(data09, file = "data09_24_OD.csv")


################# 3. Read in Cleaned DATA ######################

library(dplyr)
library(readr)
setwd("/Users/kuang")

# read different chunks #####
chunk1 = read.csv("data1_24_OD.csv")[,-1]
chunk2 = read.csv("data2_24_OD.csv")[,-1]
chunk09 = read.csv("data09_24_OD.csv")[,-1]
chunk3 = read.csv("data3_24_OD.csv")[,-1]
chunk4 = read.csv("data4_24_OD.csv")[,-1]
chunk5 = read.csv("data5_24_OD.csv")[,-1]
chunk6 = read.csv("data6_24_OD.csv")[,-1]


datafinal = bind_rows(chunk1, chunk2, chunk09,
                      chunk3, chunk4, chunk5, chunk6)

# unique routes and carriers
length(unique(datafinal$Reporting_Airline)) # 27 unique carriers

# create origin-destination unique pair
datafinal$OD_pair = paste(datafinal$Origin, datafinal$Dest, sep = "-")
OD_list = data.frame(unique(datafinal$OD_pair))


OD_list$OD_Dummy = seq(1:10007)
colnames(OD_list) = c("OD_pair", "OD_Dummy")

# join back to datafinal
datafinal = datafinal %>% left_join(OD_list, by= "OD_pair")

# save file
# write.csv(datafinal, file = "data_final_24_OD.csv", row.names = F)

######## 4. Add Load Factor ##############
#  T1 Enplaned Passengers Load Factor Monthly ##########

t1= read.csv("T1.csv", stringsAsFactors =  FALSE)

# subset Only US Domestic and Z service class (All included). Refer to T1_Service_Class table
t1= subset(t1, REGION == "D" & SERVICE_CLASS == "Z")
t1 = subset(t1,YEAR >= 2004)
colnames(t1)
unique(t1$UNIQUE_CARRIER)

dlist =c("YEAR","QUARTER", "MONTH",
         "UNIQUE_CARRIER_NAME","CARRIER",
         "REV_PAX_ENP_110", 
         "REV_PAX_MILES_140","AVL_SEAT_MILES_320", 
         "REV_TON_MILES_240", "AVL_TON_MILES_280",
         "REV_ACRFT_MILES_FLOWN_410","REV_ACRFT_MILES_SCH_430",
         "REV_ACRFT_HRS_AIRBORNE_610","ACRFT_HRS_RAMPTORAMP_630")

t1new = t1[,dlist]

# rename column names
colnames(t1new) = c("Year","Quarter", "Month",
                    "CARRIER_NAME", "CARRIER",
                    "RevPaxEnplaned", 
                    "RevPaxMiles","AvlSeatMiles",
                    "RevTonMiles", "AvlTonMiles", 
                    "RevMilesFlown","RevMileSched",
                    "RevAirHours","AirHourRamp2Ramp")

# delete cargo airlines
t1new =t1new[!grepl("Cargo", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Parcel", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Alaska Central Express", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Alaska Seaplane Service", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Aloha Island Air", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Frontier Flying Service", t1new$CARRIER_NAME),]
t1new =t1new[!grepl("Frontier Horizon Inc.", t1new$CARRIER_NAME),]

### name changes
t1_q = t1new
library(stringr)

t1_q$CARRIER_NAME = toupper(t1_q$CARRIER_NAME)
t1_q$CARRIER_NAME <- str_replace(t1_q$CARRIER_NAME,"AMERICAN EAGLE","ENVOY")
t1_q$CARRIER_NAME <- str_replace(t1_q$CARRIER_NAME,"ATLANTIC COAST","INDEPENDENCE")
t1_q$CARRIER_NAME <- str_replace(t1_q$CARRIER_NAME,"PINNACLE","ENDEAVOR")

# remove for extraction of first word
t1_q =t1_q[!grepl("CONTINENTAL MICRONESIA", t1_q$CARRIER_NAME),]

#### clean airline names by extracting the first word
library(stringr)
t1_q$airline = word(t1_q$CARRIER_NAME,1) 
t1_q$airline = toupper(t1_q$airline)

t1_q$airline <- str_replace(t1_q$airline,"ATLANTIC","ATLANTIC SOUTHEAST")
t1_q$airline = gsub("\\AMERICA\\>","AMERICA WEST",t1_q$airline)

t1_q$CARRIER_NAME = NULL

t1_q$Loadf = t1_q$RevPaxMiles/t1_q$AvlSeatMiles
t1_lf = t1_q %>% select(Year, Quarter, Month, 
                        CARRIER, Loadf)

### save files
write.csv(t1_q, file = "t1_month_24.csv")
write.csv(t1_lf, file = "t1__month_24_lf.csv", row.names = F)
colnames(datafinal)

#### 5. Add Load Factor #####
datafinal = read.csv( "data_final_24_OD.csv")
t1_q = read.csv("t1_month_24.csv")
t1_trun = t1_q %>% select(Year, Quarter, Month,
                        CARRIER, RevPaxEnplaned, Loadf)


datafinal = datafinal %>% left_join(t1_trun, by = c("Year", "Quarter", "Month", 
                                             "Reporting_Airline" = "CARRIER"))

write.csv(datafinal, file = "datafinal_24_OD.csv", 
          row.names = F)

# name changed to "Essay3_OD_Cleaned.csv" in Master Main.R code to keep the original copy
