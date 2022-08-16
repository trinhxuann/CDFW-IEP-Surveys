library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggdark)
library(lubridate)
library(ggsci)
library(caret)
library(randomForest)
library(doParallel)
library(rgdal)
library(stringr)

# Themes ------------------------------------------------------------------

myTheme <- dark_theme_bw(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2c2828", color = NA),
        panel.background = element_blank(),
        # panel.grid.major = element_blank(),
        panel.grid.major = element_line(color = "#646464", size = 0.2),
        legend.background = element_blank(),
        legend.key = element_blank())

theme_set(myTheme)


# Read in the data --------------------------------------------------------

data <- read_csv(file.path("data-raw", "20mm", "TMM.csv"), 
                 col_types = cols(
                   Source = col_character(),
                   Station = col_double(),
                   Latitude = col_double(),
                   Longitude = col_double(),
                   Date = col_date(format = ""),
                   Datetime = col_datetime(format = ""),
                   Survey = col_double(),
                   TowNum = col_double(),
                   Depth = col_double(),
                   SampleID = col_character(),
                   Method = col_character(),
                   Tide = col_character(),
                   Sal_surf = col_double(),
                   Temp_surf = col_double(),
                   Secchi = col_double(),
                   Tow_volume = col_double(),
                   Cable_length = col_double(),
                   Taxa = col_character(),
                   Length = col_double(),
                   Count = col_double(),
                   Length_NA_flag = col_character(),
                   Notes_survey = col_character(),
                   Notes_station = col_character(),
                   Notes_gear = col_character()
                 ))

produceClusters <- function(data, 
                            taxa = "Hypomesus transpacificus",
                            distanceMethod = "euclidean",
                            clusterMethod = "average",
                            months = 3:7) {
  
  dataFiltered <- data %>% 
    filter(Taxa == taxa) %>% 
    group_by(Month = month(Date)) %>% 
    mutate(lengthScaled = scale(Length)) %>% 
    ungroup() %>% 
    group_split(Month, .keep = T)
  
  names(dataFiltered) <- sapply(dataFiltered, function(x) unique(x$Month))
  
  distanceMatrix <- lapply(as.character(months), function(x) dist(dataFiltered[[x]]$lengthScaled, method = distanceMethod))
  hclusters <- lapply(distanceMatrix, hclust, method = clusterMethod) %>% 
    setNames(as.character(months))
  
  finalClusters <- lapply(as.character(months), function(x) {
    mutate(dataFiltered[[x]], cluster = cutree(hclusters[[x]], 2))
  }) %>%
    bind_rows(.id = "ID")
  
  finalMonths <- month(data$Date) %>% 
    unique()
  
  missingMonth <- finalMonths[which(!finalMonths %in% months)] %>% 
    as.character()
  
  finalData <- bind_rows(finalClusters,
                         dataFiltered[[missingMonth]])
  if (nrow(finalData) %in% nrow(data %>% 
                                filter(Taxa == taxa))) {
    cat("The missing month here was", missingMonth, "which will need to be manually classified with your preferred cluster.")
    finalData
  } else {
    stop("Final data frame does not account for all instances of catch for this species.")
  }
}

dataset <- produceClusters(data) %>% 
  # Need to fill in the missing month
  mutate(cluster = ifelse(Month == 8, 1, cluster),
         cluster = factor(ifelse(cluster == 1, "YOY", "Adults"), 
                          levels = c("YOY", "Adults")))

set.seed(135)
dataset %>% 
  ggplot(aes(Month, Length, color = cluster)) +
  geom_jitter() +
  scale_color_npg()

# Create a quick RF model -------------------------------------------------

cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

# Creating list of seeds
# 10 folds 3 repeats = 30 elements + 1 element for final model
seeds <- lapply(1:31, function(x) {
  set.seed(135)
  if (x != 31) {
    sample(1:1000000, 2)
  } else {
    sample(1:1000000, 1)
  }
})

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        verboseIter = F,
                        allowParallel = T,
                        classProbs = T,
                        seeds = seeds,
                        savePredictions = "final")

rfModel <- train(cluster ~ Month + Length,
                 data = dataset,
                 preProcess = c("center", "scale"),
                 method = "rf",
                 metric = "Kappa",
                 tuneGrid = expand.grid(mtry = seq(1, 2, 1)),
                 trControl = control)

stopCluster(cl)

# Check to see optimal tune, although this is silly as there is only 2 features
ggplot(rfModel)
# Did the model stabilize at 500 trees
plot(rfModel$finalModel)

# Simulating to find threshold per month
seq(min(dataset$Length), max(dataset$lengthScaled), length = 1000)
simulatedDF <- bind_rows(lapply(3:8, function(x) data.frame(Month = rep(x, 1000),
                                           Length = seq(min(dataset$Length), 
                                                        max(dataset$Length), 
                                                        length = 1000))) %>% 
                    setNames(3:8), .id = "Month") %>% 
  mutate(Month = as.numeric(Month)) %>% 
  mutate(predicted = predict(rfModel, .)) %>% 
  bind_cols(predict(rfModel, ., type = "prob"))

thresholdDF_50 <- simulatedDF %>% 
  filter(predicted == "YOY") %>% 
  group_by(Month) %>% 
  slice_max((Length)) %>% 
  rename(LengthCutoff = Length) %>% 
  bind_rows(data.frame(Month = 9)) %>% 
  ungroup() %>% 
  fill(LengthCutoff, predicted) %>% 
  mutate(LengthCutoff = round(LengthCutoff))

thresholdDF_95 <- simulatedDF %>% 
  filter(Adults < 0.95) %>% 
  group_by(Month) %>% 
  slice_max((Length)) %>% 
  rename(LengthCutoff = Length) %>% 
  bind_rows(data.frame(Month = 9)) %>% 
  ungroup() %>% 
  fill(LengthCutoff, predicted) %>% 
  mutate(LengthCutoff = round(LengthCutoff))

# Plotting it
set.seed(135)
dataset %>% 
  left_join(thresholdDF_50, by = "Month") %>% 
  mutate(predictions = factor(ifelse(Length <= LengthCutoff, "YOY", "Adults"),
                              levels = c("YOY", "Adults"))) %>% 
  ggplot(aes(Month, Length)) +
  geom_step(data = thresholdDF_50, 
            aes(x = Month - 0.5, y = LengthCutoff), color = "white", size = 1.1) +
  geom_jitter(aes(color = predictions)) +
  geom_text(data = thresholdDF_50,
            aes(y = -2, label = paste0(LengthCutoff, " mm")), size = 5) +
  scale_color_npg() +
  scale_x_continuous(breaks = 3:8)

data %>% 
  filter(month(Date) == 4, Taxa == "Hypomesus transpacificus", Length > 52.5) %>% 
  View()

# Mixing model ------------------------------------------------------------

library(mixdist)

adults <- dataset %>% 
  filter((Length > 50 & Month %in% c(3:5))|
         (Length > 60 & Month %in% c(6:8)))
YOY <- dataset %>% 
  filter((Length <= 50 & Month %in% c(3:5))|
           (Length <= 60 & Month %in% c(6:8)))

fitMix <- function(Month = 3:7, 
                   Year = NULL,
                   mu, sigma, pi) {
 
  if (!is.null(Year)) {
    YOY <- filter(YOY, year(Date) == !!Year)
    adults <- filter(adults, year(Date) == !!Year)
  }
  
  datasetMixed <- YOY %>% 
    filter(Month == !!Month) %>% 
    bind_rows(adults) %>% 
    uncount(as.integer(Count)) %>% 
    group_by(Length) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(Length) %>% 
    rename(length = Length,
           freq = n) %>% 
    tidyr::complete(length = seq(min(length), max(length), by = 1)) %>% 
    add_row(length = Inf, freq = 1) %>% 
    replace_na(list(freq = 0)) %>% 
    as.mixdata()
  
  fitYOY <- mix(datasetMixed,
                mixpar = mixparam(mu = mu, 
                                  sigma = sigma, 
                                  pi = pi),
                dist = "norm",
                mixconstr(consigma = "SEQ"),
                iterlim = 150)
  
  list(datasetMixed = datasetMixed,
       fitYOY = fitYOY)
}

fitList <- list()
fitList <- mapply(fitMix,
                  Month = list(3, 4, 5, 6, 7),
                  mu = list(c(10, 70),
                            c(10, 70),
                            c(20, 70),
                            c(20, 40, 70),
                            c(25, 40, 70)),
                  sigma = list(c(5, 5),
                               c(5, 5),
                               c(5, 5),
                               c(5, 5, 5),
                               c(5, 5, 5)),
                  pi = list(c(0.8, 0.2),
                            c(0.95, 0.05),
                            c(0.99, 0.01),
                            c(0.7, 0.2, 0.1),
                            c(0.55, 0.45, 0.05)),
                  SIMPLIFY = F) %>% 
  setNames(month.abb[3:7])
# warnings in months 4 and 5, during which the number of YOY and adults are VASTLY different

par(mfrow=c(3,2))
lapply(month.abb[3:7], function(x) plot(fitList[[x]]$fitYOY, main = x))

# Finding the transition points per class per month:
lengthCutOffMixture <- sapply(month.abb[3:7], function(x) {
  df <- bind_cols(fitList[[x]]$datasetMixed, fitted.mix(fitList[[x]]$fitYOY)$conditprob)
  
  # As it stands, the last column will be always be the adults
  adultColumnPosition <- ncol(df)
  df %>% 
    mutate(adultProbability = !!sym(names(df)[adultColumnPosition])) %>% 
    filter(adultProbability > 0.95) %>% 
    slice(1) %>% 
    pull(length)
})

# Plotting it
# Agree'd upon threshold df
thresholdDF <- data.frame(Month = 3:8,
                          LengthCutoff = c(50, 50, 50, 60, 60, 60))

set.seed(135)
dataset %>% 
  left_join(thresholdDF, by = "Month") %>% 
  mutate(predictions = factor(ifelse(Length <= LengthCutoff, "YOY", "Adults"),
                              levels = c("YOY", "Adults"))) %>% 
  ggplot(aes(Month, Length)) +
  geom_step(data = thresholdDF_50 %>% 
              rename(randomForest50 = LengthCutoff) %>% 
              mutate(mixture95 = c(lengthCutOffMixture, 64, 64)) %>% 
              pivot_longer(c(randomForest50, mixture95),
                           names_to = "model",
                           values_to = "lengthCutoff"),
            aes(x = Month - 0.5, y = lengthCutoff, color = model), size = 1.1) +
  geom_jitter(aes(color = predictions)) +
  geom_text(data = thresholdDF,
            aes(y = -2, label = paste0(round(LengthCutoff, 1), " mm")), size = 5) +
  scale_color_npg(breaks = c("Adults", "YOY", "randomForest50", "mixture95")) +
  scale_x_continuous(breaks = 3:8, labels = month.abb[3:8])

# Checking distribution per year
# 1995
# June
plot(fitMix(Month = 6, Year = 1995, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
# July
plot(fitMix(Month = 7, Year = 1995, mu = c(20, 40, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 1996
plot(fitMix(Month = 6, Year = 1996, mu = c(20, 40, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 1996, mu = c(20, 40, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 1997
plot(fitMix(Month = 6, Year = 1997, mu = c(20, 40, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 1997, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2

# 1998
plot(fitMix(Month = 6, Year = 1998, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 1998, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
# Not alot of adults in this year...

# 1999
plot(fitMix(Month = 6, Year = 1999, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 1999, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2000
plot(fitMix(Month = 6, Year = 2000, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2000, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2001
plot(fitMix(Month = 6, Year = 2001, mu = c(30, 70), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
# plot(fitMix(Month = 7, Year = 2001, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 1, errr this bad year?

# 2002
plot(fitMix(Month = 6, Year = 2002, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
# plot(fitMix(Month = 7, Year = 2002, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 1, errr this bad year?

# 2003
plot(fitMix(Month = 6, Year = 2003, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2003, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2004
plot(fitMix(Month = 6, Year = 2004, mu = c(15, 30, 60), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2004, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2005
plot(fitMix(Month = 6, Year = 2005, mu = c(20, 45, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2005, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3, weird year though

# 2006
plot(fitMix(Month = 6, Year = 2006, mu = c(20, 45, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2006, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2007
plot(fitMix(Month = 6, Year = 2007, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
plot(fitMix(Month = 7, Year = 2007, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2008
plot(fitMix(Month = 6, Year = 2008, mu = c(30, 50, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2008, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2009
plot(fitMix(Month = 6, Year = 2009, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
plot(fitMix(Month = 7, Year = 2009, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2

# 2010
plot(fitMix(Month = 6, Year = 2010, mu = c(30, 50, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2010, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2011
plot(fitMix(Month = 6, Year = 2011, mu = c(30, 50, 70), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3
plot(fitMix(Month = 7, Year = 2011, mu = c(30, 40, 80), sigma = c(5, 5, 5), pi = c(0.8, 0.15, 0.05))$fitYOY)
# 3

# 2012
plot(fitMix(Month = 6, Year = 2012, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
plot(fitMix(Month = 7, Year = 2012, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2, weird year

# 2013
plot(fitMix(Month = 6, Year = 2013, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
plot(fitMix(Month = 7, Year = 2013, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2, weird year

# 2014
plot(fitMix(Month = 6, Year = 2014, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2
# plot(fitMix(Month = 7, Year = 2014, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2, weird year

# 2015
plot(fitMix(Month = 6, Year = 2015, mu = c(30), sigma = c(5), pi = c(1))$fitYOY)
# 1
# plot(fitMix(Month = 7, Year = 2015, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# NA, only 1 fish caught

# 2016
plot(fitMix(Month = 6, Year = 2016, mu = c(30), sigma = c(5), pi = c(1))$fitYOY)
# 2
# plot(fitMix(Month = 7, Year = 2016, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# NA not enough fish caught, only 3 fish

# 2017
plot(fitMix(Month = 6, Year = 2017, mu = c(30), sigma = c(5), pi = c(1))$fitYOY)
# 2
# plot(fitMix(Month = 7, Year = 2017, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# 2, weird year

# 2018
plot(fitMix(Month = 6, Year = 2018, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# NA, no fish
plot(fitMix(Month = 7, Year = 2018, mu = c(20, 75), sigma = c(5, 5), pi = c(0.9, 0.1))$fitYOY)
# NA, too little fish

# 2019-2021 no fish

mixModelComponents <- read_csv(file.path("docs", "20 mm index", "mixtureModelComponents.csv"))

# Looking at spatial pattern ----------------------------------------------

strata <- readOGR(file.path("..", "WRCML", "raw data", "wrcml_subregions.shp")) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))

joinTo_strata <- function(df, strata) {
  # If dataframe doesn't have long/lat columns, stop
  if (!any(names(df) %in% c("longitude", "latitude",
                            "Longitude", "Latitude",
                            "LONGITUDE", "LATITUDE"))) 
    stop("Dataframe does not have longitude/latitude columns (lower, upper, sentence case).", call. = F)

  # Changing all names to lower case to make processing more consistent
  # Isolating lat/long dataframe to find strata matches before rebinding to original df
  df <- df %>% 
    rename_with(.cols = contains(c("longitude", "latitude",
                                   "Longitude", "Latitude",
                                   "LONGITUDE", "LATITUDE")), ~str_to_lower(.)) %>% 
    mutate(rowIndex = row_number())
  
  dfLoc <- df %>% 
    dplyr::select(rowIndex, Station, longitude, latitude)
  
  dfLocNoNA <- dfLoc %>% 
    filter(!is.na(latitude), !is.na(longitude))
  
  dfLocNA <- dfLoc %>% 
    filter(is.na(latitude), is.na(longitude))
  
  # Assigning spatial coordinates of the DF to be joined into strata
  sp::coordinates(dfLocNoNA) <- ~longitude + latitude
  sp::proj4string(dfLocNoNA) <- proj4string(strata)
  
  overDF <- over(dfLocNoNA, strata)
  
  dfLocNoNA <- df %>% 
    filter(!is.na(latitude), !is.na(longitude)) %>% 
    mutate(region = overDF[[1]],
           subRegion = overDF[[2]]) %>% 
    bind_rows(df %>% 
                filter(is.na(latitude), is.na(longitude)))
  
  # Specifying order, based entirely on latitude, with higher values = more downstream
  regionOrder <- c("San Pablo Bay", "Far West Delta", "West Delta", "North Delta", "South Delta")
  
  subRegionOrder <- dfLocNoNA %>% 
    mutate(region = factor(region, levels = regionOrder)) %>% 
    distinct(region, subRegion) %>% 
    arrange(region) %>% 
    pull(subRegion)
  
  dfLocNoNA %>% 
    mutate(subRegion = factor(subRegion, levels = subRegionOrder))
}

YOY <- joinTo_strata(YOY, strata)
adults <- joinTo_strata(adults, strata)

lengthCutoffProb <- sapply(month.abb[3:7], function(x) {
  df <- bind_cols(fitList[[x]]$datasetMixed, fitted.mix(fitList[[x]]$fitYOY)$conditprob)
})

dfList <- bind_rows(YOY, adults) %>% 
  split(f = as.factor(.$Month))
 
# All months 
bind_rows(dfList$`3` %>% 
            left_join(lengthCutoffProb$Mar, by = c("Length" = "length")) %>% 
            mutate(prediction = ifelse(`...4` >= 0.95, "Adult", "YOY1"),
                   Month = "Mar") %>% 
            group_by(region, subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion)),
          dfList$`4` %>% 
            left_join(lengthCutoffProb$Apr, by = c("Length" = "length")) %>% 
            mutate(prediction = ifelse(`...4` >= 0.95, "Adult", "YOY1"),
                   Month = "Apr") %>% 
            group_by(region, subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion)),
          dfList$`5` %>% 
            left_join(lengthCutoffProb$May, by = c("Length" = "length")) %>% 
            mutate(prediction = ifelse(`...4` >= 0.95, "Adult", "YOY1"),
                   Month = "May") %>% 
            group_by(region, subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion)),
          dfList$`6` %>% 
            left_join(lengthCutoffProb$Jun, by = c("Length" = "length")) %>% 
            mutate(prediction = case_when(`...3` > `...4` & `...3` > `...5` ~ "YOY1",
                                          `...4` > `...3` & `...4` > `...5` ~ "YOY2",
                                          `...5` > `...4` & `...5` > `...3` ~ "Adult"),
                   Month = "Jun") %>% 
            group_by(region, subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion)),
          dfList$`7` %>% 
            left_join(lengthCutoffProb$Jul, by = c("Length" = "length")) %>% 
            mutate(prediction = case_when(`...3` > `...4` & `...3` > `...5` ~ "YOY1",
                                          `...4` > `...3` & `...4` > `...5` ~ "YOY2",
                                          `...5` > `...4` & `...5` > `...3` ~ "Adult"),
                   Month = "Jul") %>% 
            group_by(region, subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion))) %>% 
  ggplot(aes(subRegion, n, fill = factor(prediction, levels = c("YOY1", "YOY2", "Adult")))) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(Month, levels = month.abb)) +
  labs(fill = " ") +
  theme(axis.text.x = element_blank()) +
  scale_fill_npg()

# Jun/July
bind_rows(dfList$`6` %>% 
            left_join(lengthCutoffProb$Jun, by = c("Length" = "length")) %>% 
            mutate(prediction = case_when(`...3` > `...4` & `...3` > `...5` ~ "YOY1",
                                          `...4` > `...3` & `...4` > `...5` ~ "YOY2",
                                          `...5` > `...4` & `...5` > `...3` ~ "Adult"),
                   Month = "Jun") %>% 
            group_by(subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion)),
          dfList$`7` %>% 
            left_join(lengthCutoffProb$Jul, by = c("Length" = "length")) %>% 
            mutate(prediction = case_when(`...3` > `...4` & `...3` > `...5` ~ "YOY1",
                                          `...4` > `...3` & `...4` > `...5` ~ "YOY2",
                                          `...5` > `...4` & `...5` > `...3` ~ "Adult"),
                   Month = "Jul") %>% 
            group_by(subRegion, prediction, Month) %>% 
            tally() %>% 
            filter(!is.na(subRegion))) %>% 
  ggplot(aes(subRegion, n, fill = factor(prediction, levels = c("YOY1", "YOY2", "Adult")))) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(Month, levels = month.abb)) +
  labs(fill = " ") +
  theme(axis.text.x = element_blank()) +
  scale_fill_npg()

dfList$`6` %>% 
  left_join(lengthCutoffProb$Jun, by = c("Length" = "length")) %>% 
  mutate(prediction = case_when(`...3` > `...4` & `...3` > `...5` ~ "YOY1",
                                `...4` > `...3` & `...4` > `...5` ~ "YOY2",
                                `...5` > `...4` & `...5` > `...3` ~ "Adult"),
         Month = "Jun") %>% 
  group_by(subRegion, prediction, Month) %>% 
  tally() %>% 
  filter(!is.na(subRegion)) %>% 
  ggplot(aes(subRegion, n, fill = factor(prediction, levels = c("YOY1", "YOY2", "Adult")))) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(Month, levels = month.abb)) +
  labs(fill = " ") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_npg()

