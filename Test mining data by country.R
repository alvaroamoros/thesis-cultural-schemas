# Cultural schemas english
# Test mining data by country
# 07/07/21

# Libraries
library(usethis)
library(devtools)
library(rtweet)
library(tidyverse)
library(dplyr)
library(CMDist)
library(text2vec)
library(gensim)
library(word2vec)
library(tools)
library(reticulate)
library(gridExtra)
library(ggpubr)
library(corclass)
library(reshape2)
library(qgraph)
library(ggcorrplot)
library(RCA)
library(sf)
library(giscoR)
library(tidytext)
library(textrecipes)
library(readr)


# FUNCTIONS
# Function. Obtains cordenates of specific country. By Diego H., dieghernan.
lookup_coords_nominatim <- function(address, ...) {
  if (missing(address)) stop("must supply address", call. = FALSE)
  stopifnot(is.atomic(address))
  place <- address
  if (grepl("^us$|^usa$|^united states$|^u\\.s",
            address,
            ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -124.848974,
      sw.lat = 24.396308,
      ne.lng = -66.885444,
      ne.lat = 49.384358
    )
    point <- c(
      lat = 36.89,
      lng = -95.867
    )
  } else if (grepl("^world$|^all$|^globe$|^earth$",
                   address,
                   ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -180,
      sw.lat = -90,
      ne.lng = 180,
      ne.lat = 90
    )
    point <- c(
      lat = 0,
      lng = 0
    )
  } else {
    ## encode address
    address <- gsub(" ", "+",  address)
    ## compose query
    params <- list(
      q = address,
      format = "json",
      limit = 1
    )
    params <- params[!vapply(params, is.null, logical(1))]
    params <- paste0(
      mapply(
        function(x, y) paste0(x, "=", y),
        names(params), params
      ),
      collapse = "&"
    )
    ## build URL - final name in English
    geourl <- paste0(
      "https://nominatim.openstreetmap.org/search?",
      params,
      "&accept-language=en"
    )
    ## read and convert to list obj
    r <- jsonlite::fromJSON(geourl)
    ## extract and name box and point data frames
    bbox <- as.double(unlist(r$boundingbox))
    boxp <- c(
      sw.lng = bbox[3],
      sw.lat = bbox[1],
      ne.lng = bbox[4],
      ne.lat = bbox[2]
    )
    point <- c(
      lat = as.double(r$lat),
      lng = as.double(r$lon)
    )
    # Full name from Nominatim
    place <- r$display_name
  }
  rtweet:::as.coords(place = place, box = boxp, point = point) # call an internal function
}

# Function for generating a DTM sparece matrix
dtm_simple_sparse  <- function(text, doc_id){
  tokns <- strsplit(text, " ") 
  vects <- unlist(tokns)
  vocab <- sort(unique(vects))
  lens <- sapply(tokns, length)
  dtm <- Matrix::sparseMatrix(i=rep(seq_along(lens), lens), 
                              j=match(vects, vocab), x=1L,
                              dimnames = list(doc_id, vocab))
}

# Twitter API
twitter_token <- create_token(
  app = "r-project-corruption",
  consumer_key = "fRNnEe4DOLxaQtE9NAe7NTyr0",
  consumer_secret = "12oVmuRj01fcXLy1JNgoA510nX7Hq8b6BrwY9j55ndGyZM10Mc",
  set_renv = TRUE)


# EXTRACTING TWEETS


# South Africa  OK !
south_africa <- "south africa"
coords_south_africa <- lookup_coords_nominatim(south_africa)
corruption_tweets_south_africa <- search_tweets("corruption", n = 7000, include_rts = FALSE, lang = "en",
                                   geocode = coords_south_africa)

# India OK !
india <- "india"
coords_india <- lookup_coords_nominatim(india)
corruption_tweets_india <- search_tweets("corruption", n = 7000, include_rts = FALSE, lang = "en",
                                                geocode = coords_india)

# UK OK ! 
uk <- "united kingdom"
coords_uk<- lookup_coords_nominatim(uk)
corruption_tweets_uk <- search_tweets("corruption", n = 7000, include_rts = FALSE, lang = "en",
                                                geocode = coords_uk)

# USA OK !
usa <- "usa"
coords_usa <- lookup_coords_nominatim(usa)
corruption_tweets_usa <- search_tweets("corruption", n = 7000, include_rts = FALSE, lang = "en",
                                                geocode = coords_usa)

# CLEAN AND PREPARE DATA

# Generate variable to register country

corruption_tweets_uk <- corruption_tweets_uk %>%
  mutate(country = "UK")

corruption_tweets_south_africa <- corruption_tweets_south_africa %>%
  mutate(country = "South Africa")

corruption_tweets_usa <- corruption_tweets_usa %>%
  mutate(country = "USA")

corruption_tweets_india <- corruption_tweets_india %>%
  mutate(country = "India")

x <- as.data.frame(corruption_tweets_uk)

write_csv(corruption_tweets_uk, "~/Documents/projects/r-projects/cultural-schemas-english-2/corruption_tweets_uk_2.csv")
write_csv(corruption_tweets_usa, "~/Documents/projects/r-projects/cultural-schemas-english-2/corruption_tweets_usa_2.csv")
write_csv(corruption_tweets_south_africa, "~/Documents/projects/r-projects/cultural-schemas-english-2/corruption_tweets_south_africa_2.csv")
write_csv(corruption_tweets_india, "~/Documents/projects/r-projects/cultural-schemas-english-2/corruption_tweets_india_2.csv")




# TEST GEOLOCATIONS

# South Africa
test_coords <- lookup_coords_nominatim(south_africa)
bbox <- test_coords$box
class(bbox) <- "bbox"
bbox <- bbox %>% st_as_sfc() %>% st_set_crs(4326)
country_sf <- gisco_get_countries(country = south_africa, epsg = 4326)
par(mar=c(0,0,0,0))
plot(bbox, border = "red")
plot(st_geometry(country_sf), add = TRUE)

# UK
test_coords <- lookup_coords_nominatim(uk)
bbox <- test_coords$box
class(bbox) <- "bbox"
bbox <- bbox %>% st_as_sfc() %>% st_set_crs(4326)
country_sf <- gisco_get_countries(country = uk, epsg = 4326)
par(mar=c(0,0,0,0))
plot(bbox, border = "red")
plot(st_geometry(country_sf), add = TRUE)

# usa
test_coords <- lookup_coords_nominatim(usa)
bbox <- test_coords$box
class(bbox) <- "bbox"
bbox <- bbox %>% st_as_sfc() %>% st_set_crs(4326)
country_sf <- gisco_get_countries(country = usa, epsg = 4326)
par(mar=c(0,0,0,0))
plot(bbox, border = "red")
plot(st_geometry(country_sf), add = TRUE)
