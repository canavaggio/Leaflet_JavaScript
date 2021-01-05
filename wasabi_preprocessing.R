# TOPIC :  US CHLOROMAP THAT PLOT THE DENSITY/FREQUENCY OF ARTIST GENRE BY STATE #

# some ressources :
# https://datahub.io/core/world-cities

# DATASET 
wasabi_artists <- read.csv(file = '/home/charles/M1/D3_project/project/wasabi_data/wasabi_artists.csv/wasabi_artists.csv')

# JSON FILE // That contain the sahpe of each us state and his name
library(rjson)
json_file <- fromJSON(file = "/home/charles/M1/D3_project/project/StatesData.json")

# Which state is containing in the file
json_file_state <- c()
for(i in 1:length(json_file$features)){
  json_file_state <- append(json_file_state, json_file$features[[i]]$properties$name)
}
json_file_state

# SOME DATA PREPROCESSING / FILTER ONLY RELEVANT COLUMN
library(tidyverse)
data <- wasabi_artists %>% select(location,
                                  location.city,
                                  location.country,  
                                  locationInfo,
                                  genres,
                                  name,
                                  lifeSpan.begin) %>% filter(# lifeSpan.begin !="",
                                                            # location.city !="",
                                                             genres !="[]")

# SOME DATA PROCESSING // purpose : split this column into three columns 
data$locationInfo <- str_replace(data$locationInfo, fixed("["), "")
data$locationInfo <- str_replace(data$locationInfo, fixed("]"), "")
data$locationInfo <- str_replace_all(data$locationInfo, fixed('\"'),"")

# DATA FILTER FOR US ARTIST
data <- data %>%
  filter(str_detect(data$locationInfo, "United States"))

# SOME DATA PREPROCESSING
data <- data %>% separate(locationInfo, c("Country", "State", "City"), sep = ",")

# DELETING NA VALUE
data <- data %>% drop_na(State) 

# I actually first decide to use location.city column but it was not really accurate
# So I change my mind to locationInfo where it looked like more accuarate BUT like always in this dataset there are
# mistakes
# I notice geojson_file_state contains 52 states but data contains 57 states. Let's figure it out

# sort(json_file_state)
# length(json_file_state)


# sort(unique(data$State))
# length(unique(data$State))

# Let's use a mask to identify which country do not belong in the US
mask <- unique(data$State) %in% json_file_state
clean_state <- unique(data$State)[mask]
# It looks like that one state do not have data about artist 
# Let figure out which on it is
sort(clean_state) # There are no data about puerto rico / it's not there no data but for some obscure reason 

data <- data %>%
  filter(data$State %in% clean_state)




# SOME FILTER FOR GENRE WITH THEIR FREQUENCY BY STATE
nb_Hip_Hop <- data %>%
  filter(str_detect(data$genres, 'Hip Hop')) %>%
  group_by(State) %>%
  summarise(nb = n()) %>%
  mutate(frequence = nb/sum(nb))

nb_Rock <- data %>%
  filter(str_detect(data$genres, 'Rock')) %>%
  group_by(State) %>%
  summarise(nb = n()) %>%
  mutate(frequence = nb/sum(nb))

nb_Country <- data %>%
  filter(str_detect(data$genres, 'Country')) %>%
  group_by(State) %>%
  summarise(nb = n()) %>%
  mutate(frequence = nb/sum(nb))

nb_Jazz <- data %>%
  filter(str_detect(data$genres, 'Jazz')) %>%
  group_by(State) %>%
  summarise(nb = n()) %>%
  mutate(frequence = nb/sum(nb))

nb_Electronic <- data %>%
  filter(str_detect(data$genres, 'Electronic')) %>%
  group_by(State) %>%
  summarise(nb = n()) %>%
  mutate(frequence = nb/sum(nb))




# Let assume our data are pretty well clean
# Let add the frequence of some genre on the geojson datafile, so we could use it in our visualization

# This loop ad a property frequence_Hip_Hop to my geojson file
# This loop work with thee json file format 
for( i in 1:52){
  for(j in nb_Hip_Hop$State){
    if(json_file$features[[i]]$properties$name == j){
      tmp <- which(nb_Hip_Hop$State == j)
      json_file$features[[i]]$properties$frequence_Hip_Hop <- nb_Hip_Hop$frequence[tmp]
      json_file$features[[i]]$properties$nb_Hip_Hop_artist <- nb_Hip_Hop$nb[tmp]
    }
  }
}

# This loop ad a property frequence_Rock to my geojson file
for( i in 1:52){
  for(j in nb_Rock$State){
    if(json_file$features[[i]]$properties$name == j){
      tmp <- which(nb_Rock$State == j)
      json_file$features[[i]]$properties$frequence_Rock <- nb_Rock$frequence[tmp]
      json_file$features[[i]]$properties$nb_Rock_artist <- nb_Rock$nb[tmp]
    }
  }
}

# This loop ad a property frequence_Country to my geojson file
for( i in 1:52){
  for(j in nb_Country$State){
    if(json_file$features[[i]]$properties$name == j){
      tmp <- which(nb_Country$State == j)
      json_file$features[[i]]$properties$frequence_Country <- nb_Country$frequence[tmp]
      json_file$features[[i]]$properties$nb_Country_artist <- nb_Country$nb[tmp]
    }
  }
}

# This loop ad a property frequence_Jazz to my geojson file
for( i in 1:52){
  for(j in nb_Jazz$State){
    if(json_file$features[[i]]$properties$name == j){
      tmp <- which(nb_Jazz$State == j)
      json_file$features[[i]]$properties$frequence_Jazz <- nb_Jazz$frequence[tmp]
      json_file$features[[i]]$properties$nb_Jazz_artist <- nb_Jazz$nb[tmp]
    }
  }
}

# This loop ad a property frequence_Jazz to my geojson file
for( i in 1:52){
  for(j in nb_Electronic$State){
    if(json_file$features[[i]]$properties$name == j){
      tmp <- which(nb_Electronic$State == j)
      json_file$features[[i]]$properties$frequence_Electronic <- nb_Electronic$frequence[tmp]
      json_file$features[[i]]$properties$nb_Electronic_artist <- nb_Electronic$nb[tmp]
    }
  }
}





# Note a moi meme : La librairie rjson et jsonlite ont des fonctions portant le mem nom, 
# ce qui peut provoquer un probleme de compatibilite 
exportJSON <- toJSON(json_file) # convert my list into a json format
write(exportJSON, "StatesDataGenres2.json") # export is as json format so I can use it 
getwd()
