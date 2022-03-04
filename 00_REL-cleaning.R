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
  
  # pull item names for each sub scale
  items <- dict %>% filter(scale == dim) %>%
    pull(variable)
  assign(x = "scale", value = items)
  
  # add column to rel_IN data frame and rename using scale_short from dict
  rel_IN <- cbind(rel_IN,
                  rowMeans(select(rel_IN, all_of(scale)),
                           na.rm = T))
  names(rel_IN)[ncol(rel_IN)] <- dict[which(dict$scale == dim), 
                                      "scale_short"][1]
  
  # add column to rel_FU1 data frame and rename using scale_short from dict
  rel_FU1 <- cbind(rel_FU1,
                   rowMeans(select(rel_FU1, all_of(scale)),
                            na.rm = T))
  names(rel_FU1)[ncol(rel_FU1)] <- dict[which(dict$scale == dim), 
                                        "scale_short"][1]
}

# check to see who has data at FU1 but not IN
rel_FU1[which(!rel_FU1$ID %in% rel_IN$ID), "ID"]

# III. Export ----

# only keep ID, IDFAMYR, and REL scale scores
rel_IN  <- rel_IN  %>% select(-c(Q1:Q48))
rel_FU1 <- rel_FU1 %>% select(-c(Q1:Q48))

rio::export(rel_IN, file = "./Data/rel_IN.sav")
rio::export(rel_FU1, file = "./Data/rel_FU1.sav")