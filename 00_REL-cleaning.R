##########################################
##  SIBS Personality Change             ##
##  Sibling relationship data cleaning  ##
##  Linh Nguyen                         ##
##  2021-01-21                          ##
##########################################

# I. Meta ----

# load libraries
libraries <- c("rio",          # import export
               "dplyr")        # general wrangling

lapply(libraries, require, character.only = TRUE)

# Read in sibling relationship files
rel_IN <- rio::import(file = "./Data/AXIKQ_SiblingRel_In.sav")
rel_FU1 <- rio::import(file = "./Data/AX1KQ_SiblingRel_FU1.sav")

# Read in dictionary
dict <- rio::import(file = "dictionary.xlsx", which = "Sibling relationship")

# II. Cleaning ----

# only keep ID, IDFAMYR, and REL questions
rel_IN <- rel_IN %>%
  select(ID, IDYRFAM, Q1:Q48)
rel_FU1 <- rel_FU1 %>%
  select(ID, IDYRFAM, Q1:Q48)

# scale scores
for (dim in unique(na.omit(dict$scale))) {
  items <- dict %>% filter(scale == dim) %>%
    pull(variable)
  assign(x = dim, value = items)
  
}
