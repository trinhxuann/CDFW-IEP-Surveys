library(tidyverse); library(wordcloud); library(stringr); library(tm)

df <- LTMRdata::SLS %>% 
  mutate(Notes_tow = str_remove_all(Notes_tow, "NA|[C|c]rew|;|:|,")) %>% 
  distinct(Notes_tow)

corpus <- Corpus(VectorSource(df$Notes_tow)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

words <- sort(rowSums(corpus), decreasing = T)

dfWC = data.frame(word = names(words),
                  freq = words)

wordcloud(words = dfWC$word, freq = dfWC$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=ggsci::pal_jco()(8))


# Checking relevant words -------------------------------------------------
# They are: 
# c("retow", "engine", "peat", "weeds", "error", "removed",
#   "missed", "debris", "retowed", "accidently", "veg")

LTMRdata::SLS %>% 
  filter(str_detect(Notes_tow, c("retow | engine | peat | weeds | error | removed | 
                                 missed | debris | retowed | accidently | veg "))) %>%
  pull(Notes_tow) %>% 
  unique()
# Nothing about missing station in these key words
# Trying to look at Lat/Lon now as suggest by Adam

LTMRdata::SLS %>% 
  filter(str_detect(Notes_tow, c("lat | lon | long | latitude | longitude"))) %>%
  pull(Notes_tow) %>% 
  unique()

# There is no useful information on the flowmeter comments
LTMRdata::SLS %>% 
  pull(Notes_flowmeter) %>% 
  unique()

# For when tows did not catch any fish, nothing in the comments either
LTMRdata::SLS %>% 
  filter(Length_NA_flag %in% "No fish caught") %>% 
  pull(Notes_tow) %>% 
  unique()

# Try an overall look at ALL comments
LTMRdata::SLS %>% 
  select(Notes_tow) %>% 
  View()
# Checked twice and could not find information about missed tows


# Verdict -----------------------------------------------------------------

# Seems like only tows that occurred have any recording in the database.
# Steps to reach this conclusion:
# 1) used a word cloud approach to find most common words and work with Adam to ID potential words
# to look through. Looked through comments with those words. Result = no additional info
# 2) Adam suggested looking at lat/lon comments. Result = no additional info
# 3) looked through flow meter comments in the database. Result = no additional info
# 4) looked through comments for tows that caught 0 fish (of any species). Result = no additional info
# 5) checked through ALL comments in the database for information about missed tows
# Final result: no additional information about missed tows. 
# Seems like only tows that occurred have any recording in the database.
