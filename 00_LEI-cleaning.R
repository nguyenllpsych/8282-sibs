################################
##  SIBS Personality Change   ##
##  Life Event data cleaning  ##
##  Linh Nguyen               ##
##  2021-01-21                ##
################################

# I. META ----

# load libraries
libraries <- c("rio",          # import export
               "labelled",     # var_label
               "reshape2",     # reshape data wide long
               "dplyr",        # general wrangling
               "plyr")         # rbind.fill

lapply(libraries, require, character.only = TRUE)

# read in dictionary
dict_IN  <- rio::import("dictionary.xlsx", which = "Life events IN")
dict_FU1 <- rio::import("dictionary.xlsx", which = "Life events FU1")
dict_FU2 <- rio::import("dictionary.xlsx", which = "Life events FU2")

# list of applicable LEI items
# Family Life Events (FAM)
FAM_15_IN   <- dict_IN  %>% 
  filter(scale == "Family Life Events") %>%
  filter(IN_15 != "NA") %>%
  pull(variable_IN)
FAM_16_IN   <- dict_IN  %>% 
  filter(scale == "Family Life Events") %>%
  pull(variable_IN)
FAM_15_FU1  <- dict_FU1 %>% 
  filter(scale == "Family Life Events") %>%
  filter(FU1_15 != "NA") %>%
  pull(variable_FU1)
FAM_16_FU1  <- dict_FU1 %>% filter(scale == "Family Life Events") %>%
  filter(FU1_16 != "NA") %>%
  pull(variable_FU1)
FAM_FU2  <- dict_FU2 %>% filter(scale == "Family Life Events") %>%
  pull(variable_FU2)

# Independent Non-Family Life Events (INF)
INF_15_IN   <- dict_IN  %>% 
  filter(scale == "Independent Non-Family Life Events") %>%
  filter(IN_15 != "NA") %>%
  pull(variable_IN)
INF_16_IN   <- dict_IN  %>% 
  filter(scale == "Independent Non-Family Life Events") %>%
  pull(variable_IN)
INF_15_FU1  <- dict_FU1 %>% 
  filter(scale == "Independent Non-Family Life Events") %>%
  filter(FU1_15 != "NA") %>%
  pull(variable_FU1)
INF_16_FU1  <- dict_FU1 %>% 
  filter(scale == "Independent Non-Family Life Events") %>%
  pull(variable_FU1)
INF_FU2  <- dict_FU2 %>% 
  filter(scale == "Independent Non-Family Life Events") %>%
  pull(variable_FU2)

# Nonindependent Non-Family Life Events (NINF)
NINF_15_IN   <- dict_IN  %>% 
  filter(scale == "Nonindependent Non-Family Life Events") %>%
  filter(IN_15 != "NA") %>%
  pull(variable_IN)
NINF_16_IN   <- dict_IN  %>% 
  filter(scale == "Nonindependent Non-Family Life Events") %>%
  pull(variable_IN)
NINF_15_FU1  <- dict_FU1 %>% 
  filter(scale == "Nonindependent Non-Family Life Events") %>%
  filter(FU1_15 != "NA") %>%
  pull(variable_FU1)
NINF_16_FU1  <- dict_FU1 %>% 
  filter(scale == "Nonindependent Non-Family Life Events") %>%
  pull(variable_FU1)
NINF_FU2  <- dict_FU2 %>% 
  filter(scale == "Nonindependent Non-Family Life Events") %>%
  pull(variable_FU2)

# read in LEI files with applicable items
lei15_IN <- rio::import(file = "./Data/SIBS_LEI_15_INTAKE.sav") %>%
  select(ID, IDYRFAM, all_of(c(FAM_15_IN, INF_15_IN, NINF_15_IN)))
lei16_IN <- rio::import(file = "./Data/SIBS_LEI_16_INTAKE.sav") %>%
  select(ID, IDYRFAM, all_of(c(FAM_16_IN, INF_16_IN, NINF_16_IN)))

lei15_FU1 <- rio::import(file = "./Data/SIBS_LEI_15_FU1.sav") %>%
  select(ID, IDYRFAM, all_of(c(FAM_15_FU1, INF_15_FU1, NINF_15_FU1)))
lei16_FU1 <- rio::import(file = "./Data/SIBS_LEI_16_FU1.sav") %>%
  select(ID, IDYRFAM, all_of(c(FAM_16_FU1, INF_16_FU1, NINF_16_FU1)))

lei_FU2 <- rio::import(file = "./Data/SIBS_LEI_FU2.sav") %>% 
  select(ID, IDYRFAM, all_of(c(FAM_FU2, INF_FU2, NINF_FU2)))

# II. CLEANING ----

# remove YN0 and YN3 in names
names(lei15_IN) <- gsub(pattern = "YN0", 
                        replacement = "",
                        x = names(lei15_IN))
names(lei16_IN) <- gsub(pattern = "YN0", 
                        replacement = "",
                        x = names(lei16_IN))
names(lei15_FU1) <- gsub(pattern = "YN0", 
                        replacement = "",
                        x = names(lei15_FU1))
names(lei15_FU1) <- gsub(pattern = "YN3", 
                         replacement = "",
                         x = names(lei15_FU1))
names(lei16_FU1) <- gsub(pattern = "YN0", 
                        replacement = "",
                        x = names(lei16_FU1))
names(lei16_FU1) <- gsub(pattern = "YN3", 
                         replacement = "",
                         x = names(lei16_FU1))
names(lei_FU2)   <- gsub(pattern = "YN0", 
                         replacement = "",
                         x = names(lei_FU2))
names(lei_FU2)   <- gsub(pattern = "YN3", 
                         replacement = "",
                         x = names(lei_FU2))

# remove YN0 and YN3 in dict
FAM_15_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = FAM_15_IN)
FAM_16_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = FAM_16_IN)
FAM_15_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = FAM_15_FU1)
FAM_16_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = FAM_16_FU1)
FAM_15_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = FAM_15_FU1)
FAM_16_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = FAM_16_FU1)
FAM_FU2 <- gsub(pattern = "YN0",
                replacement = "",
                x = FAM_FU2)
FAM_FU2 <- gsub(pattern = "YN3",
                replacement = "",
                x = FAM_FU2)

INF_15_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = INF_15_IN)
INF_16_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = INF_16_IN)
INF_15_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = INF_15_FU1)
INF_16_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = INF_16_FU1)
INF_15_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = INF_15_FU1)
INF_16_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = INF_16_FU1)
INF_FU2 <- gsub(pattern = "YN0",
                replacement = "",
                x = INF_FU2)
INF_FU2 <- gsub(pattern = "YN3",
                replacement = "",
                x = INF_FU2)

NINF_15_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = NINF_15_IN)
NINF_16_IN <- gsub(pattern = "YN0",
                  replacement = "",
                  x = NINF_16_IN)
NINF_15_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = NINF_15_FU1)
NINF_16_FU1 <- gsub(pattern = "YN0",
                   replacement = "",
                   x = NINF_16_FU1)
NINF_15_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = NINF_15_FU1)
NINF_16_FU1 <- gsub(pattern = "YN3",
                   replacement = "",
                   x = NINF_16_FU1)
NINF_FU2 <- gsub(pattern = "YN0",
                replacement = "",
                x = NINF_FU2)
NINF_FU2 <- gsub(pattern = "YN3",
                replacement = "",
                x = NINF_FU2)

# rename Q46A -> Q46P
lei_FU2 <- dplyr::rename(lei_FU2, Q46P_ = Q46A_)
lei16_IN <- dplyr::rename(lei16_IN, Q46P_ = Q46A_)
lei16_FU1 <- dplyr::rename(lei16_FU1, Q46P_ = Q46A_)

FAM_16_IN <- gsub(pattern = "Q46A_", replacement = "Q46P_",
                  x = FAM_16_IN)
FAM_16_FU1 <- gsub(pattern = "Q46A_", replacement = "Q46P_",
                   x = FAM_16_FU1)
FAM_FU2 <- gsub(pattern = "Q46A_", replacement = "Q46P_",
                x = FAM_FU2)

# general lei variables list
FAM  <- unique(c(FAM_15_IN, FAM_15_FU1, 
                 FAM_16_IN, FAM_16_FU1, FAM_FU2))
INF  <- unique(c(INF_15_IN, INF_15_FU1, 
                 INF_16_IN, INF_16_FU1, INF_FU2))
NINF <- unique(c(NINF_15_IN, NINF_15_FU1, 
                 NINF_16_IN, NINF_16_FU1, NINF_FU2))

# remove _ in variable lists
FAM   <- gsub(pattern = "_", replacement = "", FAM)
INF   <- gsub(pattern = "_", replacement = "", INF)
NINF  <- gsub(pattern = "_", replacement = "", NINF)

# combine 15 and 16
lei_IN  <- rbind.fill(lei15_IN, lei16_IN)
lei_FU1 <- rbind.fill(lei15_FU1, lei16_FU1)

# time indicator
lei_IN$time  <- "IN"
lei_FU1$time <- "FU1"
lei_FU2$time <- "FU2"

# combine all waves to long dataset
lei <- rbind.fill(lei_IN, lei_FU1)
lei <- rbind.fill(lei, lei_FU2)
names(lei) <- gsub(pattern = "_", replacement = "", names(lei))
lei <- lei[c(1,2,54,3:53,55)]

# long to wide
lei_w <- melt(lei, id = c("ID", "IDYRFAM", "time"))
lei_w <- dcast(lei_w, ID + IDYRFAM ~ time + variable,
               value.var = "value")

# variable list
varlist <- variable.names(lei)[-c(1,2)]

# create overall indicators
lei_w <- lei_w %>%
  mutate(Q1B = as.numeric(IN_Q1B == "1" | FU1_Q1B == "1" | FU2_Q1B == "1"),
         Q3A = as.numeric(IN_Q3A == "1" | FU1_Q3A == "1" | FU2_Q3A == "1"),
         Q10 = as.numeric(IN_Q10 == "1" | FU1_Q10 == "1" | FU2_Q10 == "1"),
         Q11A = as.numeric(IN_Q11A == "1" | FU1_Q11A == "1" | FU2_Q11A == "1"),
         Q17 = as.numeric(IN_Q17 == "1" | FU1_Q17 == "1" | FU2_Q17 == "1"),
         Q17A = as.numeric(IN_Q17A == "1" | FU1_Q17A == "1" | FU2_Q17A == "1"),
         Q18B = as.numeric(IN_Q18B == "1" | FU1_Q18B == "1" | FU2_Q18B == "1"),
         Q19AP = as.numeric(IN_Q19AP == "1" | FU1_Q19AP == "1" | FU2_Q19AP == "1"),
         Q20AP = as.numeric(IN_Q20AP == "1" | FU1_Q20AP == "1" | FU2_Q20AP == "1"),
         Q21A = as.numeric(IN_Q21A == "1" | FU1_Q21A == "1" | FU2_Q21A == "1"),
         Q21B = as.numeric(IN_Q21B == "1" | FU1_Q21B == "1" | FU2_Q21B == "1"),
         Q15 = as.numeric(IN_Q15 == "1" | FU1_Q15 == "1" | FU2_Q15 == "1"),
         Q22S = as.numeric(IN_Q22S == "1" | FU1_Q22S == "1" | FU2_Q22S == "1"),
         Q35 = as.numeric(IN_Q35 == "1" | FU1_Q35 == "1" | FU2_Q35 == "1"),
         Q41D = as.numeric(IN_Q41D == "1" | FU1_Q41D == "1" | FU2_Q41D == "1"),
         Q49A = as.numeric(IN_Q49A == "1" | FU1_Q49A == "1" | FU2_Q49A == "1"),
         Q49B = as.numeric(IN_Q49B == "1" | FU1_Q49B == "1" | FU2_Q49B == "1"),
         Q41C = as.numeric(IN_Q41C == "1" | FU1_Q41C == "1" | FU2_Q41C == "1"),
         Q7 = as.numeric(IN_Q7 == "1" | FU1_Q7 == "1" | FU2_Q7 == "1"),
         Q8 = as.numeric(IN_Q8 == "1" | FU1_Q8 == "1" | FU2_Q8 == "1"),
         Q9 = as.numeric(IN_Q9 == "1" | FU1_Q9 == "1" | FU2_Q9 == "1"),
         Q24 = as.numeric(IN_Q24 == "1" | FU1_Q24 == "1" | FU2_Q24 == "1"),
         Q25 = as.numeric(IN_Q25 == "1" | FU1_Q25 == "1" | FU2_Q25 == "1"),
         Q26 = as.numeric(IN_Q26 == "1" | FU1_Q26 == "1" | FU2_Q26 == "1"),
         Q27 = as.numeric(IN_Q27 == "1" | FU1_Q27 == "1" | FU2_Q27 == "1"),
         Q29EM = as.numeric(IN_Q29EM == "1" | FU1_Q29EM == "1" | FU2_Q29EM == "1"),
         Q29EF = as.numeric(IN_Q29EF == "1" | FU1_Q29EF == "1" | FU2_Q29EF == "1"),
         Q51B = as.numeric(IN_Q51B == "1" | FU1_Q51B == "1" | FU2_Q51B == "1"),
         Q4A = as.numeric(IN_Q4A == "1" | FU1_Q4A == "1" | FU2_Q4A == "1"),
         Q5 = as.numeric(IN_Q5 == "1" | FU1_Q5 == "1" | FU2_Q5 == "1"),
         Q6A = as.numeric(IN_Q6A == "1" | FU1_Q6A == "1" | FU2_Q6A == "1"),
         Q14B = as.numeric(IN_Q14B == "1" | FU1_Q14B == "1" | FU2_Q14B == "1"),
         Q20B = as.numeric(IN_Q20B == "1" | FU1_Q20B == "1" | FU2_Q20B == "1"),
         Q28B1 = as.numeric(IN_Q28B1 == "1" | FU1_Q28B1 == "1" | FU2_Q28B1 == "1"),
         Q29BM = as.numeric(IN_Q29BM == "1" | FU1_Q29BM == "1" | FU2_Q29BM == "1"),
         Q29BF = as.numeric(IN_Q29BF == "1" | FU1_Q29BF == "1" | FU2_Q29BF == "1"),
         Q29CM = as.numeric(IN_Q29CM == "1" | FU1_Q29CM == "1" | FU2_Q29CM == "1"),
         Q29CF = as.numeric(IN_Q29CF == "1" | FU1_Q29CF == "1" | FU2_Q29CF == "1"),
         Q29DM = as.numeric(IN_Q29DM == "1" | FU1_Q29DM == "1" | FU2_Q29DM == "1"),
         Q29DF = as.numeric(IN_Q29DF == "1" | FU1_Q29DF == "1" | FU2_Q29DF == "1"),
         Q38B = as.numeric(IN_Q38B == "1" | FU1_Q38B == "1" | FU2_Q38B == "1"),
         Q38H = as.numeric(IN_Q38H == "1" | FU1_Q38H == "1" | FU2_Q38H == "1"),
         Q38C = as.numeric(IN_Q38C == "1" | FU1_Q38C == "1" | FU2_Q38C == "1"),
         Q43F = as.numeric(IN_Q43F == "1" | FU1_Q43F == "1" | FU2_Q43F == "1"),
         Q46P = as.numeric(IN_Q46P == "1" | FU1_Q46P == "1" | FU2_Q46P == "1"),
         Q13 = as.numeric(IN_Q13 == "1" | FU1_Q13 == "1" | FU2_Q13 == "1"),
         Q30A = as.numeric(IN_Q30A == "1" | FU1_Q30A == "1" | FU2_Q30A == "1"),
         Q31 = as.numeric(IN_Q31 == "1" | FU1_Q31 == "1" | FU2_Q31 == "1"),
         Q16F = as.numeric(IN_Q16F == "1" | FU1_Q16F == "1" | FU2_Q16F == "1"),
         Q16D = as.numeric(IN_Q16D == "1" | FU1_Q16D == "1" | FU2_Q16D == "1"),
         Q16B = as.numeric(IN_Q16B == "1" | FU1_Q16B == "1" | FU2_Q16B == "1"),
         Q22P = as.numeric(IN_Q22P == "1" | FU1_Q22P == "1" | FU2_Q22P == "1"))

# select only tallied variables
lei <- lei_w %>%
  select(ID, IDYRFAM, Q1B:Q22P)

# create scale scores
lei$FAM <- lei %>%
  select(all_of(FAM)) %>%
  rowSums(na.rm = T)
lei$INF <- lei %>%
  select(all_of(INF)) %>%
  rowSums(na.rm = T)
lei$NINF <- lei %>%
  select(all_of(NINF)) %>%
  rowSums(na.rm = T)

# combine M/F items
lei <- lei %>%
  mutate(Q29E = as.numeric(Q29EM == "1" | Q29EF == "1"),
         Q29B = as.numeric(Q29BM == "1" | Q29BF == "1"),
         Q29C = as.numeric(Q29CM == "1" | Q29CF == "1"),
         Q29D = as.numeric(Q29DM == "1" | Q29DF == "1"))
lei <- lei %>%
  select(-c(Q29EM, Q29EF, Q29BM, Q29BF, Q29CM, Q29CF, Q29DM, Q29DF)) %>%
  select(ID, IDYRFAM, Q1B:Q22P, Q29E:Q29D, FAM, INF, NINF)

# III. EXPORT ----
var_label(lei) <- list(
  Q1B     = "Family moved to new neighborhood",
  Q3A     = "Changed school due to moving",
  Q10     = "Family pet died",
  Q11A    = "Close relative died",
  Q17     = "Family had money problem",
  Q17A    = "Family received govt assistance",
  Q18B    = "Parents argued a lot",
  Q19AP   = "Parents lived apart",
  Q20AP   = "Parents dated others",
  Q21A    = "New adult lived with family",
  Q21B    = "Moved to other guardian or foster",
  Q15     = "Siblings ran away",
  Q22S    = "Siblings unavailable",
  Q35     = "Family had drug/alcohol trouble",
  Q41D    = "Family sent to jail",
  Q49A    = "Family attempted suicide",
  Q49B    = "Family committed suicide",
  Q41C    = "Family got arrested",
  Q7      = "Close friend moved away",
  Q8      = "Close friend seriously ill/hurt",
  Q9      = "Close friend died",
  Q24     = "Body changed due to puberty",
  Q25     = "Was teased due to body changing",
  Q26     = "Wore braces",
  Q27     = "Got pimples",
  Q29E    = "Partner/self had miscarriage",
  Q51B    = "Got mugged or robbed",
  Q4A     = "Got suspended or expelled",
  Q5      = "Failed to make after-school activity",
  Q6A     = "Had serious problem with close friend",
  Q14B    = "Ran away from home",
  Q20B    = "Started dating",
  Q28B1   = "Broke up with romantic partner",
  Q29B    = "Got (a girl) pregnant",
  Q29C    = "(Partner) gave birth",
  Q29D    = "(Partner) got an abortion",
  Q38B    = "Had trouble with police",
  Q38H    = "Went to court",
  Q38C    = "Sent to juvenile detention",
  Q43F    = "Family got treated for mental problem",
  Q46P    = "Family got hospitalized for mental problem",
  Q13     = "Moved away from home",
  Q30A    = "Moved in with romantic partner",
  Q31     = "Got married",
  Q16F    = "Failed to get a job",
  Q16D    = "Started a job",
  Q16B    = "Lost a job",
  Q22P    = "Parents unavailable",
  FAM     = "Count of family-related events",
  INF     = "Count of independent non-family events",
  NINF    = "Count of non-independent non-family events"
)
rio::export(x = lei, file = "./Data/lei.sav")
