################################
##  SIBS Personality Change   ##
##  Life Event data cleaning  ##
##  Linh Nguyen               ##
##  2021-01-21                ##
################################

# I. Meta ----

# load libraries
libraries <- c("rio",          # import export
               "reshape2",     # reshape data wide long
               "dplyr")        # general wrangling

lapply(libraries, require, character.only = TRUE)


# Read in lie files
# select only ID and LIE questions
lie15_IN <- rio::import(file = "./Data/SIBS_LIE_15_INTAKE.sav") %>%
  select(ID, Q1B_YN0:QMETF_LY)
lie16_IN <- rio::import(file = "./Data/SIBS_LIE_16_INTAKE.sav") %>%
  select(ID, Q1B_YN0:QMETF_LY)

lie15_FU1 <- rio::import(file = "./Data/SIBS_LIE_15_FU1.sav") %>%
  select(ID, Q1B_YN3: QMETF_LY)
lie16_FU1 <- rio::import(file = "./Data/SIBS_LIE_16_FU1.sav") %>%
  select(ID, Q1B_YN3: QMETF_LY)

lie_FU2 <- rio::import(file = "./Data/SIBS_LIE_FU2.sav") %>% 
  select(ID, Q13_YN3:QMETOOR3)


# II. Cleaning ----

## reconcile 15 and 16 ----
unique_15IN <- unique_16IN <- unique_15FU1 <- unique_16FU1 <- c()

# variables in 15_intake but not in 16_intake
for(name in names(lie15_IN)) {
  if (!name %in% names(lie16_IN)) {
    unique_15IN <- append(unique_15IN, name)
  }
}
unique_15IN

# variables in 16_intake but not in 15_intake
for(name in names(lie16_IN)) {
  if (!name %in% names(lie15_IN)) {
    unique_16IN <- append(unique_16IN, name)
  }
}
unique_16IN