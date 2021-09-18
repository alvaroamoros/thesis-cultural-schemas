# Concept Class Analysis

# Generate sparece matrix
twitter_sparce_matrix <- dtm_simple_sparse(corruption_tweets_clean$clean_text, corruption_tweets_clean$user_id)

# Load word embeddings 
word_embeddings <- readRDS("~/Documents/projects/r-projects/cultural-schemas/ft.cc.en.300D.2M.Rds")


# build juxtaposed pairs for each semantic directions
authority <- data.frame(additions  = c("vertical" ),
                        substracts = c("horizontal") )

 structure <- data.frame(additions  = c("network"),
                  substracts = c("pair") )

type <- data.frame(additions  = c("monetary"),
                   substracts = c("favour") )

type_2 <- data.frame(additions  = c("legal"),
                   substracts = c("ilegal") )

level <- data.frame(additions  = c("goverment"),
                        substracts = c("citizen", ) )

level_2 <- data.frame(additions  = c("system"),
                    substracts = c("individual") ) 

level_3 <- data.frame(additions  = c("private"),
                      substracts = c("public") ) 

ideology <- data.frame(additions  = c("solidarity"),
                          substracts = c("competition") )

ideology_2 <- data.frame(additions  = c("nation"),
                       substracts = c("class") )

time <- data.frame(additions  = c("inmediate"),
                   substracts = c("eventually") )

space <- data.frame(additions  = c("center"),
                   substracts = c("periphery") )

space <- data.frame(additions  = c("moral"),
                    substracts = c("inmoral") )

democracy <- data.frame(additions  = c("democratic"),
                    substracts = c("undemocratic") )

#########################################
#########################################

uno <- data.frame(additions  = c("legal"),
                     substracts = c("ilegal") )

dos <- data.frame(additions  = c("moral"),
                    substracts = c("amoral") )

tres <- data.frame(additions  = c("democratic"),
                        substracts = c("undemocratic") )

cuatro <- data.frame(additions  = c("system"),
                    substracts = c("individual") ) 

cinco <- data.frame(additions  = c("private"),
                      substracts = c("public") ) 

seis <- data.frame(additions  = c("solidarity"),
                       substracts = c("competition") )

siete <- data.frame(additions  = c("duty"),
                   substracts = c("irresponsibility") )

ocho <- data.frame(additions  = c("exoneration"),
                         substracts = c("punishment") )

#########################################
#########################################
uno <- data.frame(additions  = c("surprise"),
                    substracts = c("expectation") )

dos <- data.frame(additions  = c("sadness"),
                    substracts = c("hapiness") )

tres <- data.frame(additions  = c("anger"),
                    substracts = c("calmness") )

cuatro <- data.frame(additions  = c("fear"),
                    substracts = c("courage") )

cinco <- data.frame(additions  = c("disgust"),
                    substracts = c("linking") )

seis <- data.frame(additions  = c("anticipation"),
                    substracts = c("ignorance") )

siete <- data.frame(additions  = c("trust"),
                    substracts = c("distrust") )

ocho <- data.frame(additions  = c("joy"),
                    substracts = c("missery") )




#########################################
#########################################

# Final
issue <- data.frame(additions  = c("legal"),
                    substracts = c("ilegal") )

issue_2 <- data.frame(additions  = c("moral"),
                      substracts = c("amoral") )

issue_3 <- data.frame(additions  = c("democratic"),
                      substracts = c("undemocratic") )

level <- data.frame(additions  = c("system"),
                    substracts = c("individual") ) 

level_2 <- data.frame(additions  = c("private"),
                      substracts = c("public") ) 

ideology <- data.frame(additions  = c("solidarity"),
                       substracts = c("competition") )

ideology_2 <- data.frame(additions  = c("duty"),
                         substracts = c("irresponsibility") )

ideology_3 <- data.frame(additions  = c("exoneration"),
                         substracts = c("punishment") )



# get the vectors for each direction
sd.01 <- get_direction(issue, word_embeddings)
sd.02 <- get_direction(issue_2, word_embeddings)
sd.03 <- get_direction(issue_3, word_embeddings)
sd.04 <- get_direction(level, word_embeddings)
sd.05 <- get_direction(level_2, word_embeddings)
sd.06 <- get_direction(ideology, word_embeddings)
sd.07 <- get_direction(ideology_2, word_embeddings)
sd.08 <- get_direction(ideology_3, word_embeddings)




# row bind each direction
sem.dirs <- rbind(sd.01, sd.02, sd.03, sd.04, sd.05, sd.06, sd.07, sd.08)

classes <- CMDist::CoCA(twitter_sparce_matrix, wv = word_embeddings, directions = sem.dirs)


plot(classes, module=1)
plot(classes, module=2)
plot(classes, module=3)
plot(classes, module=4)
plot(classes, module=5)


# PLOTS
corruption_tweets_clean <- corruption_tweets_clean %>%
  mutate(coca_class = classes[["membership"]])

# By country
g_1 <- corruption_tweets_clean %>%
  filter(country == "usa") %>%
  ggplot(aes(coca_class)) +
  geom_bar() +
  ggtitle("EEUU")

g_2 <- corruption_tweets_clean %>%
  filter(country == "uk") %>%
  ggplot(aes(coca_class)) +
  geom_bar() +
  ggtitle("UK")

g_3 <- corruption_tweets_clean %>%
  filter(country == "south africa") %>%
  ggplot(aes(coca_class)) +
  geom_bar() +
  ggtitle("South Africa")

g_4 <- corruption_tweets_clean %>%
  filter(country == "india") %>%
  ggplot(aes(coca_class)) +
  geom_bar() +
  ggtitle("India") 

grid.arrange(g_1, g_2, g_3, g_4,
             top = ("CoCA classes Log by country"))

# By class
g_5 <- corruption_tweets_clean %>%
  filter(coca_class == 1) %>%
  ggplot(aes(country)) +
  geom_bar() +
  ggtitle("1")
g_6 <-  corruption_tweets_clean %>%
   filter(coca_class == 2) %>%
   ggplot(aes(country)) +
   geom_bar() +
   ggtitle("2")
g_7 <-  corruption_tweets_clean %>%
   filter(coca_class == 3) %>%
   ggplot(aes(country)) +
   geom_bar() +
   ggtitle("3")
g_8 <-  corruption_tweets_clean %>%
   filter(coca_class == 4) %>%
   ggplot(aes(country)) +
   geom_bar() +
   ggtitle("4")


grid.arrange(g_5, g_6, g_7, g_8,
             top = ("CoCA classes Log by country"))

## Regressions

# Dummies for countries
corruption_tweets_clean <- corruption_tweets_clean %>%
  mutate(uk_dummie = ifelse(country == "uk", 1, 0))

corruption_tweets_clean <- corruption_tweets_clean %>%
  mutate(usa_dummie = ifelse(country == "usa", 1, 0))

corruption_tweets_clean <- corruption_tweets_clean %>%
  mutate(south_africa_dummie = ifelse(country == "south africa", 1, 0))

corruption_tweets_clean <- corruption_tweets_clean %>%
  mutate(india_dummie = ifelse(country == "india", 1, 0))

corruption_tweets_clean %>%
  mutate(coca_class = as.character(coca_class)) %>%
  glm(south_africa_dummie ~ coca_class , family = "binomial", data = .)
