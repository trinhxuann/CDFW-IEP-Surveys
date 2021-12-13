# ######################################################################
# Bay Study - Field Data QC #
# Based on queries in the 2021 Access database (2021 BS Field Entry.mdb)
# K.Alstad Dec 6, 2021
#
# NOTE: BAY STUDY EVALUATES THE FIELD ENTRY DATA IN SEPARTE YEARLY FILES:
# The protocols are described in a file called Bay Study_Datasheet Proofing_How To.doc
# located in the folder: U:\LTM\BayStudy_Lab\Bay Study SOP\SOP Revision\Referenced Docs
# The queries coded below follow the procedures in the "First Proof by an Environmental Scientist"
# section of this protocol.
# The data used for this initial test was from the 2021 Access db
# U:\LTM\BayStudy_Lab\2021_Data\2021 BS Field Entry.mdb
# The data from this Access db was extracted using a separate program (Boat.R)
# which is run from a separate program (readAccess1.R)
# These Access read are derived from the read programs that Trinh provided
# but reading from local drives to mimic the QC query process.
# The Field Entry queries in this program the only the initial QC anlayses in the Bay Study protocols;
# Other Access queries are made in separate db for Fish, Crab, Shrimp and Water Quality.
# ######################################################################

library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(hms)


setwd("C:/Users/KAlstad/Kar_Docs/Data_Reporting/BS_Rprocess/AccessOut/")
getwd()
data <- readRDS(file.path("data-raw", "Boat", "BAYTables.rds"))
QC <- list()


#-------------------------------------------------------#
# Meter by date and time

# SELECT Tow.Survey, Station.Date, Tow.Station, Station.Depth, Tow.Net, Tow.Time, Tow.[Tow Duration], Tow.[Start Meter], Tow.[End Meter], Tow![End Meter]-Tow![Start Meter]
# AS Meter, Tow.Distance, Tow.Tide, Tow.Direction
# FROM Station LEFT JOIN Tow ON (Station.Station = Tow.Station)
# AND (Station.Survey = Tow.Survey) AND (Station.Year = Tow.Year)
# WHERE (((Tow.Survey)=11))
# ORDER BY Tow.Survey, Station.Date, Tow.Time;

QC$Meterbydatetime <- left_join(data$Station, data$Tow, by = c("Station","Survey","Year")) %>%
  arrange(Date,Time) %>%
  filter(Survey == 11)  %>%
  mutate(Meter = End.Meter-Start.Meter) %>%
  mutate(Time = format(Time, format = "%H:%M")) %>%
  mutate(Date = gsub("\\s", "", format(Date, format ="%m/%e/%Y"))) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  select(Date, Station, Time,  Net,  Depth,Tow.Duration, Start.Meter, End.Meter, Meter, Tide, Direction, Distance)

test1 <- QC$Meterbydatetime
head(test1)
tail(test1)

# Test against Access table that was exported to .csv
# Note: this exercise demonstrated that the Access export changes the date and times when exporting to .csv
# I do not know if the Access export code can be modified to control date/time formating.
# Here I reformat dates and times in order to compare the R table derivation against the Access tables
test2 <- read_csv(file.path("data-raw", "Boat", "Meter by date and time.csv"),
                  col_names = TRUE, show_col_types = FALSE)
colnames(test2) <- gsub("X", "", colnames(test2))
test2$Time = format(test2$Time, format = "%H:%M")
test2$Date = gsub("\\s", "", format(test2$Date, format ="%m/%e/%Y"))
test2 <- as.data.frame(test2)
test2 <- test2 %>% select(Date, Station, Time,  Net,  Depth,Tow.Duration, Start.Meter, End.Meter, Meter, Tide, Direction, Distance)

head(test2)
tail(test2)

all.equal(test1, test2)
# TRUE
# (tables match)

################################################################
# Other Test methods
################################################################
library(janitor)
janitor::compare_df_cols(test1, test2)
library(diffdf)
diffdf::diffdf(test1, test2)
require(sqldf)
sqldf('SELECT * FROM test1 EXCEPT SELECT * FROM test3')
#write.csv(test,"Test\\test.csv", row.names = F)
################################################################
################################################################


#-------------------------------------------------------#
# Qry Meters Small or Large

# SELECT Tow.Year, Tow.Survey, Tow.Station, Tow.Net, Tow.[Tow Duration], Tow.Distance, Tow.Tide,
#         Tow.[Start Meter], Tow.[End Meter], Abs(Tow![START METER]-Tow![END METER]) AS Meter
# FROM Tow
# WHERE (((Tow.Survey)=11) AND ((Abs([Tow]![START METER]-[Tow]![END METER]))<14000
#                               Or (Abs([Tow]![START METER]-[Tow]![END METER]))>30000))
# ORDER BY Tow.Survey, Tow.Station, Tow.Net;

QC$MetersSmallorLarge <- data$Tow %>%
  filter(Survey == 11)  %>%
  mutate(Meter = abs(End.Meter-Start.Meter)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  filter((Survey==11) & (abs(Meter)<14000) | (abs(Meter)>30000)) %>%
  select(Year,Survey, Station, Net, Tow.Duration, Distance, Tide, Start.Meter, End.Meter, Meter)%>%
  arrange(Survey, Station,Net)

test1 <- QC$MetersSmallorLarge

test2 <- read_csv(file.path("data-raw", "Boat", "Meters Small or Large.csv"),
                  col_names = TRUE, show_col_types = FALSE)
test2 <- as.data.frame(test2)

head(test1)
head(test2)

all.equal(test1, test2)
# TRUE
# diffdf::diffdf(test1, test2)
# janitor::compare_df_cols(test1, test2)


#-------------------------------------------------------#
# Qry Distance High or Low

# SELECT Tow.Year, Tow.Survey, Tow.Station, Tow.Net, Tow.Tow, Tow.Tide, Tow.Direction, Tow.[Tow Duration], Tow.Distance
# FROM Tow
# WHERE (((Tow.Survey)=11) AND ((Tow.Net)=1) AND ((Tow.Tow)<>57) AND ((Tow.Distance)<0.33
#                                                                     Or (Tow.Distance)>0.8
#                                                                     Or (Tow.Distance) Is Null))
# OR (((Tow.Survey)=11) AND ((Tow.Net)=2) AND ((Tow.Tow)<>57) AND ((Tow.Distance)<0.06
#                                                                  Or (Tow.Distance)>0.23
#                                                                  Or (Tow.Distance) Is Null))
# ORDER BY Tow.Survey, Tow.Station, Tow.Net;

#Distance High or Low

QC$DistanceHighorLow <- data$Tow %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    filter((Survey == 11 & Net==1 & Tow!=57 & (Distance<0.33 | Distance>0.8 | is.na(Distance) ))  |
          (Survey == 11 & Net==2 & Tow!=57 & (Distance<0.06 | Distance>0.23| is.na(Distance) ))) %>%
    select(Year, Survey, Station, Net, Tow, Tide, Direction, Tow.Duration, Distance) %>%
    arrange(Survey, Station, Net)

test1 <- QC$DistanceHighorLow

test2 <- read_csv(file.path("data-raw", "Boat", "Distance High or Low.csv"),
                      col_names = TRUE, show_col_types = FALSE)
test2 <- as.data.frame(test2)
test2 <- test2 %>% arrange(Survey, Station, Net)

head(test1)
head(test2)

all.equal(test1, test2)
janitor::compare_df_cols(test1, test2)




#-------------------------------------------------------#
# Calculated distance and bearing


# SELECT Tow.Year, Tow.Survey, Tow.Station, Tow.Net, Tow.Tow, Tow.[Tow Duration], Tow.Bearing, Tow.StartLat,
# Tow.StartLong, Tow.EndLat, Tow.EndLong,


# Round(IIf(([ENDLONG]=[STARTLONG]) Or ([ENDLAT]=[STARTLAT]),

#         (Switch([ENDLONG]=[STARTLONG] And [ENDLAT]>[STARTLAT],0,
#         [ENDLONG]=[STARTLONG] And [ENDLAT]<[STARTLAT],180,
#         [ENDLAT]=[STARTLAT] And [ENDLONG]>[STARTLONG],270,
#         [ENDLAT]=[STARTLAT] And [ENDLONG]<[STARTLONG],90)),

#     (IIf([STARTLAT] Is Null
#         Or [STARTLONG] Is Null
#         Or [ENDLAT] Is Null
#         Or [ENDLONG] Is Null,999,

#        (Switch(([ENDLAT]-[STARTLAT])>0 And ([ENDLONG]-[STARTLONG])>0
#          Or ([ENDLAT]-[STARTLAT])<0 And ([ENDLONG]-[STARTLONG])>0,
#            270+57.3*(Atn((([ENDLAT]-[STARTLAT])*0.0098)/(([ENDLONG]-[STARTLONG])*0.008))),

#       ([ENDLAT]-[STARTLAT])<0 And ([ENDLONG]-[STARTLONG])<0
#          Or ([ENDLAT]-[STARTLAT])>0 And ([ENDLONG]-[STARTLONG])<0,
#            90+57.3*(Atn((([STARTLAT]-[ENDLAT])*0.0098)/(([STARTLONG]-[ENDLONG])*0.008)))))))),0)

# AS BearCalc, Tow.Distance,

# IIf([STARTLAT] Is Null
#       Or [STARTLONG] Is Null
#       Or [ENDLAT] Is Null
#       Or [ENDLONG] Is Null,0,(Sqr(((([STARTLAT]*100-[ENDLAT]*100)*0.0098)^2)+((([STARTLONG]*100-[ENDLONG]*100)*0.008)^2))))
# AS DistCalc, Tow.Tide, Tow.Direction

# FROM Tow
# ORDER BY Tow.Survey, Tow.Station, Tow.Net;


# Acceess functions:
## IIf ( expr , truepart , falsepart )
# expr = Expression you want to evaluate.
# truepart =  Value or expression returned if expr is True.
# falsepart =  Value or expression returned if expr is False.
# The Switch function evaluates a list of expressions and returns the corresponding value for the first expression in the list that is TRUE.
# Switch ( expression1, value1, expression2, value2, ... expression_n, value_n )
# Round (210.67, 0) Result: 211
# The Atn function takes the ratio of two sides of a right triangle (number) and returns the corresponding angle in radians.
# The ratio is the length of the side opposite the angle divided by the length of the side adjacent to the angle.
# The range of the result is pi/2 to pi/2 radians.
# R conversion = atan(): atan is the general form of inverse tangent that gets a value and returns the associated angle in radian.
# But atan2 gets two values of y and x and assumes a complex number as x + iy and returns its phase.
# Note: Access * = R ** or ^



QC$Calc_dist_bearing <- data$Tow  %>%
  select(Year,Survey,Station,Net,Tow,Tow.Duration, Bearing, StartLat,StartLong, EndLat, EndLong, Distance, Direction, Tide) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  mutate(BearCalc =

        if_else ((EndLong == StartLong | EndLat == StartLat),

            (case_when((EndLong=StartLong & EndLat>StartLat) ~ 0,
                   (EndLong=StartLong & EndLat<StartLat) ~ 180,
                   (EndLat=StartLat & EndLong>StartLong) ~ 270,
                   (EndLat=StartLat & EndLong<StartLong) ~ 90,
                   TRUE ~ -9999)),

       if_else ((StartLat == 'Null' |  StartLong == 'Null' | EndLat == 'Null' | EndLong == 'Null'),

            999,

            case_when(((EndLat-StartLat)>0 & (EndLong-StartLong)>0)
                              | ((EndLat-StartLat)<0 & (EndLong-StartLong)>0)
                              ~ 270+57.3*(atan(((EndLat-StartLat)*0.0098)/((EndLong-StartLong)*0.008))),

                              ((EndLat-StartLat)<0 & (EndLong-StartLong)<0)
                              | ((EndLat-StartLat)>0 & (EndLong-StartLong)<0)
                              ~ 90+57.3*(atan(((StartLat-EndLat)*0.0098)/((StartLong-EndLong)*0.008))),
                              TRUE ~ -9900)))) %>%

  mutate( DistCalc =
        if_else((is.na(StartLat) | is.na(StartLong) | is.na(EndLat)| is.na(EndLong)),0,
        (sqrt( ((StartLat*100-EndLat*100)*0.0098)^2 + ((StartLong*100-EndLong*100)*0.008)^2)))) %>%
  mutate(BearCalc = round(BearCalc,0)) %>%
  arrange(Survey, Station, Net)

head(QC$Calc_dist_bearing)

test1 <- QC$Calc_dist_bearing

test2 <- read_csv(file.path("data-raw", "Boat", "Calculated distance and bearing.csv"),
                  col_names = TRUE, show_col_types = FALSE)
test2 <- as.data.frame(test2)
test3 <- test2 %>% select(Year, Survey, Station, Net, Tow, Tow.Duration, Bearing, StartLat, StartLong,   EndLat,  EndLong, Distance, Direction, Tide, BearCalc,  DistCalc)

head(test1)
head(test3)

all.equal(test1, test3)
# TRUE
janitor::compare_df_cols(test1, test3)
diffdf::diffdf(test1, test2)



#-------------------------------------------------------#
# Distance Outliers (uses Calculated distance and bearing)

# SELECT [Calculated distance and bearing].Year, [Calculated distance and bearing].Survey, [Calculated distance and bearing].Station,
#   [Calculated distance and bearing].Net, [Calculated distance and bearing].Tow, [Calculated distance and bearing].StartLat,
#   [Calculated distance and bearing].StartLong, [Calculated distance and bearing].EndLat, [Calculated distance and bearing].EndLong,
#   [Calculated distance and bearing].Distance, [Calculated distance and bearing].DistCalc, Abs([Distance]-[DistCalc])
# AS DistDiff
# FROM [Calculated distance and bearing]
# WHERE ((([Calculated distance and bearing].Survey)=11) AND ((Abs([Distance]-[DistCalc]))>0.04 Or (Abs([Distance]-[DistCalc])) Is Null))
# ORDER BY [Calculated distance and bearing].Survey, [Calculated distance and bearing].Station, [Calculated distance and bearing].Net;

QC$DistanceOutliers <- data$'Calculated distance and bearing' %>%
  mutate(DistDiff = abs(Distance-DistCalc))%>%
  mutate(across(where(is.integer), as.numeric)) %>%
  filter((Survey==11 &  DistDiff >0.04) | is.na(DistDiff))%>%
  select(Year,Survey,Station,Net,Tow,StartLat,StartLong,EndLat,EndLong,Distance,DistCalc,DistDiff)%>%
  arrange(Survey,Station,Net)

test1 <- QC$DistanceOutliers

test2 <- read_csv(file.path("data-raw", "Boat", "Distance Outliers.csv"),
                  col_names = TRUE, show_col_types = FALSE)
test2 <- as.data.frame(test2)
test2 <- test2 %>% arrange(Survey, Station, Net)

head(test1)
head(test2)

all.equal(test1, test2)
# TRUE
janitor::compare_df_cols(test1, test2)









#-------------------------------------------------------#
# Bearing Outliers (uses Calculated distance and bearing)

# SELECT [Calculated distance and bearing].Year, [Calculated distance and bearing].Survey,
# [Calculated distance and bearing].Station, [Calculated distance and bearing].Net,
# [Calculated distance and bearing].StartLat, [Calculated distance and bearing].StartLong,
# [Calculated distance and bearing].EndLat, [Calculated distance and bearing].EndLong,
# [Calculated distance and bearing].Bearing, [Calculated distance and bearing].BearCalc, Abs([BEARING]-[BEARCALC]) AS BearDiff
# FROM [Calculated distance and bearing]
# WHERE ((([Calculated distance and bearing].Survey)=11) AND ((Abs([BEARING]-[BEARCALC]))>45 And (Abs([BEARING]-[BEARCALC]))<330));


QC$BearingOutliers <- data$'Calculated distance and bearing' %>%
  mutate(BearDiff = abs(Bearing-BearCalc))%>%
  select(Year,Survey,Station,Net,StartLat,StartLong,EndLat,EndLong,Bearing,BearCalc,BearDiff)%>%
  filter(Survey==11 & (BearDiff>45) & (BearDiff<330))

# This matches the lack of outliers in 2021 data,
# But perhaps check using another year that has outliers.



#-------------------------------------------------------#
# Tide Bearing Crosstab

# TRANSFORM Avg(Tow.Bearing) AS [The Value]
# SELECT Tow.Station, Tow.Net, Tow.Tide, Avg(Tow.Bearing) AS [Total Of Bearing]
# FROM Tow
# GROUP BY Tow.Station, Tow.Net, Tow.Tide, Tow.Direction
# ORDER BY Tow.Station, Tow.Net, Tow.Tide
# PIVOT Tow.Survey;

QC$Tide_Bearing_Crosstab <- data$Tow %>%
  select(Station, Net, Tide, Survey, Bearing, Direction)%>%
  arrange(Station, Net, Tide, Direction) %>%
  spread(Survey,Bearing) %>%
  select(-'Direction') %>%
  mutate("Total.Of.Bearing"=rowMeans(.[,4:13], na.rm = TRUE))%>%
  relocate(Total.Of.Bearing, .before = "2")

test1 <- QC$Tide_Bearing_Crosstab
head(test1)

# Test against Access table
test2 <- read_csv(file.path("data-raw", "Boat", "Tide_Bearing_Crosstab.csv"),
                  col_names = TRUE, show_col_types = FALSE)
colnames(test2) <- gsub("X", "", colnames(test2))
test2 <- as.data.frame(test2)
head(test2)

all.equal(test1, test2)
# TRUE



#-------------------------------------------------------#
# DepthCrosstab

# QRY: Depth_Crosstab
# TRANSFORM Max(Station.Depth) AS MaxOfDepth
# SELECT Station.Station
# FROM Station
# GROUP BY Station.Station
# PIVOT Station.Survey;

QC$Depth_Crosstab <- data$Station %>% group_by(Survey,Station) %>%
  mutate(MaxOfDepth= max(Depth)) %>%
  select(Station, Survey, MaxOfDepth) %>%
  spread(Survey,MaxOfDepth)
head(QC$Depth_Crosstab)


# Test against Access table
Depth_Crosstab <- read_csv(file.path("data-raw", "Boat", "Depth_Crosstab.csv"),
                           col_names = TRUE, show_col_types = FALSE)

test <- as.data.frame(QC$Depth_Crosstab)
str(test)
test2 <- as.data.frame(Depth_Crosstab)
# Access table puts an X in front of the station numbers
colnames(test2) <- gsub("X", "", colnames(test2))
str(test2)

all.equal(test, test2)
# All equal = TRUE




#-------------------------------------------------------#
# Substrate Crosstab

# TRANSFORM Last(Station.Substrate) AS LastOfSubstrate
# SELECT Station.Station
# FROM Station
# GROUP BY Station.Station
# PIVOT Station.Survey;

QC$Substrate_Crosstab <- data$Station %>% group_by(Survey, Station) %>%
  mutate(LastOfSubstrate= last(Substrate)) %>%
  select(Station, Survey, LastOfSubstrate) %>%
  spread(Survey,LastOfSubstrate)
head(QC$Substrate_Crosstab)

# Test against Access table
Substrate_Crosstab <- read_csv(file.path("data-raw", "Boat", "Substrate_Crosstab.csv"),
                               col_names = TRUE, show_col_types = FALSE)

test <- as.data.frame(QC$Substrate_Crosstab)
str(test)
test2 <- as.data.frame(Substrate_Crosstab)
colnames(test2) <- gsub("X", "", colnames(test2))
str(test2)

all.equal(test, test2)
# TRUE


#-------------------------------------------------------#
# Secchi Crosstab

# TRANSFORM Max(Station.Secchi) AS MaxOfSecchi
# SELECT Station.Station
# FROM Station
# GROUP BY Station.Station
# ORDER BY Station.Station, Station.Survey
# PIVOT Station.Survey;

QC$Secchi_Crosstab <- data$Station %>% group_by(Survey,Station) %>%
  mutate(MaxOfSecchi= max(Secchi)) %>%
  select(Station, Survey, MaxOfSecchi) %>%
  spread(Survey,MaxOfSecchi)
head(QC$Secchi_Crosstab)

# Test against Access table
Secchi_Crosstab <- read_csv(file.path("data-raw", "Boat", "Secchi_Crosstab.csv"),
                            col_names = TRUE, show_col_types = FALSE)

test <- as.data.frame(QC$Secchi_Crosstab)
str(test)
test2 <- as.data.frame(Secchi_Crosstab)
colnames(test2) <- gsub("X", "", colnames(test2))
str(test2)

all.equal(test, test2)
# All equal = TRUE



writexl::write_xlsx(QC, "QC.xlsx")
