#########################################
##     SIBS Personality Change         ##
##          Linh Nguyen                ##
##      Created: 29-Nov-2020           ##
##    Last updated: 07-Dec-2020        ##
#########################################

# META ==================================
# > Library ----
library(tidyverse)
library(codebook)
library(renv)
library(future) #reliability
library(ufs) #reliability
library(reshape2)
library(cowplot) #plotgrid
library(haven) #export spss
library(lmerTest)
library(lme4)
library(nlme)

set.seed(8282)
options(scipen = 999)

renv::restore() #package version control

# > Data ----
dict <- read.csv(file = "./Data/Dictionary.csv") #dictionary

data <- read.csv(file = './Data/DF_LINH_MPQ.csv', sep = "") %>% 
  select(ID, IDYRFAM, IDSEX, IDAB, IDFAMAB, BDAY, 
         IN_MPQ_AGE, FU1_MPQ_AGE, FU3_MPQ_AGE, IN_NCDATE, FU1_NCDATE, FU3_NCDATE,

         #social potency
         IN_Q1, IN_Q14, IN_Q30, IN_Q54, IN_Q61, IN_Q77, IN_Q91, IN_Q119, IN_Q142, IN_Q153, IN_Q170, IN_Q184,
         FU1_Q1, FU1_Q14, FU1_Q30, FU1_Q54, FU1_Q61, FU1_Q77, FU1_Q91, FU1_Q119, FU1_Q142, FU1_Q153, FU1_Q170, FU1_Q184,
         FU3_SP_1, FU3_SP_25, FU3_SP_47_R, FU3_SP_94, FU3_SP_105, FU3_SP_129, FU3_SP_150_R, FU3_SP_187_R, FU3_SP_218_R, FU3_SP_233, FU3_SP_257_R, FU3_SP_278,
         
         #achievement
         IN_Q8, IN_Q32, IN_Q46, IN_Q56, IN_Q65, IN_Q74, IN_Q82, IN_Q115, IN_Q125, IN_Q137, IN_Q145, IN_Q197,
         FU1_Q8, FU1_Q32, FU1_Q46, FU1_Q56, FU1_Q65, FU1_Q74, FU1_Q82, FU1_Q115, FU1_Q125, FU1_Q137, FU1_Q145, FU1_Q197,
         FU3_AC_10, FU3_AC_55, FU3_AC_80, FU3_AC_98, FU3_AC_110_R, FU3_AC_124, FU3_AC_136, FU3_AC_180, FU3_AC_196_R, FU3_AC_213, FU3_AC_223, FU3_AC_294,
         
         #aggression
         IN_Q13, IN_Q38, IN_Q47, IN_Q55, IN_Q67, IN_Q75, IN_Q98, IN_Q109, IN_Q128, IN_Q152, IN_Q173, IN_Q196,
         FU1_Q13, FU1_Q38, FU1_Q47, FU1_Q55, FU1_Q67, FU1_Q75, FU1_Q98, FU1_Q109, FU1_Q128, FU1_Q152, FU1_Q173, FU1_Q196,
         FU3_AG_22, FU3_AG_66, FU3_AG_82, FU3_AG_97, FU3_AG_112, FU3_AG_127_R, FU3_AG_158, FU3_AG_172, FU3_AG_202, FU3_AG_232, FU3_AG_261, FU3_AG_293,
         
         #control
         IN_Q15, IN_Q31, IN_Q37, IN_Q129, IN_Q53, IN_Q60, IN_Q69, IN_Q92, IN_Q102, IN_Q149, IN_Q159, IN_Q180,
         FU1_Q15, FU1_Q31, FU1_Q37, FU1_Q129, FU1_Q53, FU1_Q60, FU1_Q69, FU1_Q92, FU1_Q102, FU1_Q149, FU1_Q159, FU1_Q180,
         FU3_CON_26, FU3_CON_51, FU3_CON_64_R, FU3_CON_78, FU3_CON_90, FU3_CON_104_R, FU3_CON_115, FU3_CON_151_R, FU3_CON_162, FU3_CON_228, FU3_CON_239_R, FU3_CON_274,
         
         #harm avoidance
         IN_Q21, IN_Q45, IN_Q62, IN_Q71, IN_Q84, IN_Q90, IN_Q100, IN_Q108, IN_Q117, IN_Q147, IN_Q171, IN_Q194,
         FU1_Q21, FU1_Q45, FU1_Q62, FU1_Q71, FU1_Q84, FU1_Q90, FU1_Q100, FU1_Q108, FU1_Q117, FU1_Q147, FU1_Q171, FU1_Q194,
         FU3_HA_33_R, FU3_HA_77_R, FU3_HA_106_R, FU3_HA_119, FU3_HA_139, FU3_HA_149_R, FU3_HA_160, FU3_HA_171_R, FU3_HA_183, FU3_HA_225, FU3_HA_259, FU3_HA_290_R,
         
         #traditionalism
         IN_Q7, IN_Q36, IN_Q52, IN_Q58, IN_Q105, IN_Q113, IN_Q126, IN_Q150, IN_Q168, IN_Q174, IN_Q181, IN_Q190,
         FU1_Q7, FU1_Q36, FU1_Q52, FU1_Q58, FU1_Q105, FU1_Q113, FU1_Q126, FU1_Q150, FU1_Q168, FU1_Q174, FU1_Q181, FU1_Q190,
         FU3_TR_9, FU3_TR_63, FU3_TR_89, FU3_TR_100_R, FU3_TR_167, FU3_TR_177, FU3_TR_199, FU3_TR_230, FU3_TR_253, FU3_TR_262, FU3_TR_275_R, FU3_TR_285) %>% 
  filter(IDAB != 3) #, is.na(IN_MPQ_AGE) == F
  
# CLEANING ==============================

# > Basic cleaning ----
## Missing values
data[data == -98 | data == -97 | data == -96 | data == -95 | data == -94] <- NA

## Duplicated 
data[duplicated(data$ID),] %>% pull(IDYRFAM)

### delete both dyads
data <- data %>% filter(IDYRFAM != 83870 & IDYRFAM != 86794)

## person dummy var
data <- data[order(data$ID),]

data$P <- c(rep(c(1:2), times = nrow(data)/2))
  
## young/old dummy var
yo <- data %>% select(IDYRFAM, BDAY, P) %>% 
  mutate(BDAY = as.character(BDAY),
         P = as.character(P))

yo <- reshape2::dcast(yo, IDYRFAM ~ P, value.var = "BDAY")
yo <- yo %>% mutate(`1` = as.Date(`1`),
                    `2` = as.Date(`2`))
yo <- yo %>% mutate(older = ifelse(`1`<`2`, "P1", "P2")) %>% 
  select(IDYRFAM, older)

data <- merge(data,yo)
data <- data %>% 
  mutate(young = ifelse(P == 1 & older == "P2" | P == 2 & older == "P1", 1, 0),
         old = ifelse(P == 1  & older == "P1" | P == 2 & older == "P2", 1, 0)) %>% 
  select(-P, -older)

rm(yo)

data <- data %>% 
  mutate(yo = ifelse(young == 0, "o","y"))

## Variable types 
names <- dict %>% 
  filter(type == "date") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.Date)

names <- dict %>% 
  filter(type == "character") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.character)

names <- dict %>% 
  filter(type == "factor") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.numeric) #factor variables are coded as numeric for codebook purposes

names <- dict %>% 
  filter(type == "numeric") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.numeric)

rm(names)

## Variable labels
var_label(data) <- dict %>% 
  dplyr::select(variable, label) %>% 
  dict_to_list()

## value labels demographics
val_labels(data$IDSEX) <- c("male" = 1,
                            "female" = 2) 

val_labels(data$IDAB) <- c("adoptive" = 1,
                            "biological" = 2) 

val_labels(data$IDFAMAB) <- c("adoptive" = 1,
                              "biological" = 2,
                              "mixed" = 3) 

## value labels scales
likert <- dict %>% 
  filter (value_label == "1 = T, 2 = t, 3 = f, 4 = F") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Definitely True" = 1,
                     "Probably True" = 2,
                     "Probably False" = 3,
                     "Definitely False" = 4)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

likert <- dict %>% 
  filter (value_label == "1 = A, 2 = a, 3 = b, 4 = B") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Definitely A" = 1,
                     "Probably A" = 2,
                     "Probably B" = 3,
                     "Definitely B" = 4)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

rm(likert, add_likert)

## Reverse-scoring 
reversed_items <- dict %>%  #make a list of reversed items
  filter (keying == -1) %>% 
  pull(variable)

data <- data %>%  #reverse values in data
  mutate_at(reversed_items,
            reverse_labelled_values)

rm(reversed_items)

# > Scale scoring ----

## create list of items
AC_1 <- dict %>% 
  filter (scale == "Achievement" & wave == "IN") %>% 
  pull(variable)
AG_1 <- dict %>% 
  filter (scale == "Aggression" & wave == "IN") %>% 
  pull(variable)
CON_1 <- dict %>% 
  filter (scale == "Control" & wave == "IN") %>% 
  pull(variable)
HA_1 <- dict %>% 
  filter (scale == "Harm Avoidance" & wave == "IN") %>% 
  pull(variable)
SP_1 <- dict %>% 
  filter (scale == "Social Potency" & wave == "IN") %>% 
  pull(variable)
TR_1 <- dict %>% 
  filter (scale == "Traditionalism" & wave == "IN") %>% 
  pull(variable)

AC_2 <- dict %>% 
  filter (scale == "Achievement" & wave == "FU1") %>% 
  pull(variable)
AG_2 <- dict %>% 
  filter (scale == "Aggression" & wave == "FU1") %>% 
  pull(variable)
CON_2 <- dict %>% 
  filter (scale == "Control" & wave == "FU1") %>% 
  pull(variable)
HA_2 <- dict %>% 
  filter (scale == "Harm Avoidance" & wave == "FU1") %>% 
  pull(variable)
SP_2 <- dict %>% 
  filter (scale == "Social Potency" & wave == "FU1") %>% 
  pull(variable)
TR_2 <- dict %>% 
  filter (scale == "Traditionalism" & wave == "FU1") %>% 
  pull(variable)

AC_3 <- dict %>% 
  filter (scale == "Achievement" & wave == "FU3") %>% 
  pull(variable)
AG_3 <- dict %>% 
  filter (scale == "Aggression" & wave == "FU3") %>% 
  pull(variable)
CON_3 <- dict %>% 
  filter (scale == "Control" & wave == "FU3") %>% 
  pull(variable)
HA_3 <- dict %>% 
  filter (scale == "Harm Avoidance" & wave == "FU3") %>% 
  pull(variable)
SP_3 <- dict %>% 
  filter (scale == "Social Potency" & wave == "FU3") %>% 
  pull(variable)
TR_3 <- dict %>% 
  filter (scale == "Traditionalism" & wave == "FU3") %>% 
  pull(variable)

## reorder items within list so reversed items are not first
HA_1 <- HA_1[c(4,1:3,5:12)]
HA_2 <- HA_2[c(4,1:3,5:12)]
HA_3 <- HA_3[c(4,1:3,5:12)]

## create aggregated variables 
data$AC_1 <- data %>% 
  dplyr::select(all_of(AC_1)) %>% 
  aggregate_and_document_scale()
data$AG_1 <- data %>% 
  dplyr::select(all_of(AG_1)) %>% 
  aggregate_and_document_scale()
data$CON_1 <- data %>% 
  dplyr::select(all_of(CON_1)) %>% 
  aggregate_and_document_scale()
data$HA_1 <- data %>% 
  dplyr::select(all_of(HA_1)) %>% 
  aggregate_and_document_scale()
data$SP_1 <- data %>% 
  dplyr::select(all_of(SP_1)) %>% 
  aggregate_and_document_scale()
data$TR_1 <- data %>% 
  dplyr::select(all_of(TR_1)) %>% 
  aggregate_and_document_scale()

data$AC_2 <- data %>% 
  dplyr::select(all_of(AC_2)) %>% 
  aggregate_and_document_scale()
data$AG_2 <- data %>% 
  dplyr::select(all_of(AG_2)) %>% 
  aggregate_and_document_scale()
data$CON_2 <- data %>% 
  dplyr::select(all_of(CON_2)) %>% 
  aggregate_and_document_scale()
data$HA_2 <- data %>% 
  dplyr::select(all_of(HA_2)) %>% 
  aggregate_and_document_scale()
data$SP_2 <- data %>% 
  dplyr::select(all_of(SP_2)) %>% 
  aggregate_and_document_scale()
data$TR_2 <- data %>% 
  dplyr::select(all_of(TR_2)) %>% 
  aggregate_and_document_scale()

data$AC_3 <- data %>% 
  dplyr::select(all_of(AC_3)) %>% 
  aggregate_and_document_scale()
data$AG_3 <- data %>% 
  dplyr::select(all_of(AG_3)) %>% 
  aggregate_and_document_scale()
data$CON_3 <- data %>% 
  dplyr::select(all_of(CON_3)) %>% 
  aggregate_and_document_scale()
data$HA_3 <- data %>% 
  dplyr::select(all_of(HA_3)) %>% 
  aggregate_and_document_scale()
data$SP_3 <- data %>% 
  dplyr::select(all_of(SP_3)) %>% 
  aggregate_and_document_scale()
data$TR_3 <- data %>% 
  dplyr::select(all_of(TR_3)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$AC_1) <- "Achievement at intake - 12 MPQ items aggregated by rowMeans"
var_label(data$AG_1) <- "Aggression at intake - 12 MPQ items aggregated by rowMeans"
var_label(data$CON_1) <- "Control at intake - 12 MPQ items aggregated by rowMeans"
var_label(data$HA_1) <- "Harm Avoidance at intake - 12 MPQ items aggregated by rowMeans"
var_label(data$SP_1) <- "Social Potency at intake - 12 MPQ items aggregated by rowMeans"
var_label(data$TR_1) <- "Traditionalism at intake - 12 MPQ items aggregated by rowMeans"

var_label(data$AC_2) <- "Achievement at first follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$AG_2) <- "Aggression at first follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$CON_2) <- "Control at first follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$HA_2) <- "Harm Avoidance at first follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$SP_2) <- "Social Potency at first follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$TR_2) <- "Traditionalism at first follow-up - 12 MPQ items aggregated by rowMeans"

var_label(data$AC_3) <- "Achievement at third follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$AG_3) <- "Aggression at third follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$CON_3) <- "Control at third follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$HA_3) <- "Harm Avoidance at third follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$SP_3) <- "Social Potency at third follow-up - 12 MPQ items aggregated by rowMeans"
var_label(data$TR_3) <- "Traditionalism at third follow-up - 12 MPQ items aggregated by rowMeans"

rm(AC_1, AG_1, CON_1, HA_1, SP_1, TR_1, AC_2, AG_2, CON_2, HA_2, SP_2, TR_2, AC_3, AG_3, CON_3, HA_3, SP_3, TR_3)

# > Cleaned data file ----
data <- data %>% select(
  ID, IDYRFAM, IDSEX, IDAB, IDFAMAB, young, old, yo, BDAY, 
  IN_MPQ_AGE, AC_1, AG_1, CON_1, HA_1, SP_1, TR_1, 
  FU1_MPQ_AGE, AC_2, AG_2, CON_2, HA_2, SP_2, TR_2, 
  FU3_MPQ_AGE, AC_3, AG_3, CON_3, HA_3, SP_3, TR_3) %>% 
  filter(is.na(young) == FALSE) #select only full dyads
rm(dict)

data <- data %>% dplyr::rename(
  age_1 = IN_MPQ_AGE,
  age_2 = FU1_MPQ_AGE,
  age_3 = FU3_MPQ_AGE
)

data <- data %>% mutate(age_3 = age_3 - age_1,
                        age_2 = age_2 - age_1,
                        age_1 = 0)

# > Long format ----
# center age to start at baseline
long <- reshape(data, direction = "long",
                varying = list(c("age_1", "age_2", "age_3"),
                               c("AC_1", "AC_2", "AC_3"),
                               c("AG_1", "AG_2", "AG_3"),
                               c("CON_1", "CON_2", "CON_3"),
                               c("HA_1", "HA_2", "HA_3"),
                               c("SP_1", "SP_2", "SP_3"),
                               c("TR_1", "TR_2", "TR_3")),
                timevar = "time",
                times = c(0,1,2),
                v.names = c("age","AC","AG","CON","HA","SP","TR"),
                idvar = c("ID"))
row.names(long) <- 1:nrow(long)

# > Export data ----
haven::write_sav(data, "./Data/wide.sav")
haven::write_sav(long, "./Data/long.sav")

write.csv(data, "./Data/wide.csv")
write.csv(long, "./Data/long.csv")

# DESCRIPTIVES ====

#random sample for plots
sample = sample(data$ID, size = 100)
sample <- data %>% filter(ID %in% c(sample))

sample <- reshape(sample, direction = "long",
                varying = list(c("age_1", "age_2", "age_3"),
                               c("AC_1", "AC_2", "AC_3"),
                               c("AG_1", "AG_2", "AG_3"),
                               c("CON_1", "CON_2", "CON_3"),
                               c("HA_1", "HA_2", "HA_3"),
                               c("SP_1", "SP_2", "SP_3"),
                               c("TR_1", "TR_2", "TR_3")),
                timevar = "time",
                times = c(0,1,2),
                v.names = c("age","AC","AG","CON","HA","SP","TR"),
                idvar = c("ID"))
row.names(sample) <- 1:nrow(sample)

# > Plot of change over timepoints ----
(pAC <- ggplot(data = sample,
               aes(x = time, y = AC, group = ID)) + 
   geom_line (linetype = "dashed")+ 
   geom_point(size = 0.5) + 
   theme_bw () +
   stat_summary(aes(data=sample$AC,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

(pAG <- ggplot(data = sample,
               aes(x = time, y = AG, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.5) +
    theme_bw () +
    stat_summary(aes(data=sample$AG,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

(pCON <- ggplot(data = sample,
                aes(x = time, y = CON, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.5) +
    theme_bw () +
    stat_summary(aes(data=sample$CON,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

(pHA <- ggplot(data = sample,
               aes(x = time, y = HA, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.5) +
    theme_bw () +
    stat_summary(aes(data=sample$HA,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

(pSP <- ggplot(data = sample,
               aes(x = time, y = SP, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.5) +
    theme_bw () +
    stat_summary(aes(data=sample$SP,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

(pTR <- ggplot(data = sample,
               aes(x = time, y = TR, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.5) +
    theme_bw () +
    stat_summary(aes(data=sample$TR,group=1),fun=mean,geom="line",lwd = 1.5, color= "red"))

cowplot::plot_grid(pAC, pAG, pCON, pHA, pSP, pTR,
                   nrow = 3, ncol = 2)

# > Plot of change with age ----
(paAC <- ggplot(data = sample,
               aes(x = age, y = AC, group = ID)) + 
   geom_line (linetype = "dashed")+ 
   geom_point(size = 0.25) + 
   theme_bw () +
   stat_smooth(aes(data=sample$AC,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

(paAG <- ggplot(data = sample,
               aes(x = age, y = AG, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.25) + 
    theme_bw () +
    stat_smooth(aes(data=sample$AG,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

(paCON <- ggplot(data = sample,
                aes(x = age, y = CON, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.25) +
    theme_bw () +
    stat_smooth(aes(data=sample$CON,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

(paHA <- ggplot(data = sample,
               aes(x = age, y = HA, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.25) +
    theme_bw () +
    stat_smooth(aes(data=sample$HA,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

(paSP <- ggplot(data = sample,
               aes(x = age, y = SP, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.25) +
    theme_bw () +
    stat_smooth(aes(data=sample$SP,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

(paTR <- ggplot(data = sample,
               aes(x = age, y = TR, group = ID)) + 
    geom_line (linetype = "dashed")+ 
    geom_point(size = 0.25) +
    theme_bw () +
    stat_smooth(aes(data=sample$TR,group=1),method="lm",formula=y ~ poly(x, 2),lwd = 1.5, color= "red"))

cowplot::plot_grid(paAC, paAG, paCON, paHA, paSP, paTR,
                   nrow = 3, ncol = 2)

# ANALYSIS ====
# > Ignore dyadic structure ----
# >> Mean-level change ----
## Achievement -- best model is m2AC
m1AC <- lmer(AC ~ age + (1|ID), data = long, REML = FALSE)
summary(m1AC)

m2AC <- lmer(AC ~ age + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2AC)

m3AC <- lmer(AC ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3AC)

## Aggression -- best model is m3AG
m1AG <- lmer(AG ~ age + (1|ID), data = long, REML = FALSE)
summary(m1AG)

m2AG <- lmer(AG ~ age + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2AG)

m3AG <- lmer(AG ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3AG)
anova(m2AG, m3AG)

## Control -- best model is m1CON
m1CON <- lmer(CON ~ age + (1|ID), data = long, REML = FALSE)
summary(m1CON)

m2CON <- lmer(CON ~ age + (1+age|ID), data = long, REML = FALSE,
              control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2CON)

m3CON <- lmer(CON ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3CON)

## Harm Avoidance -- best model is m1HA
m1HA <- lmer(HA ~ age + (1|ID), data = long, REML = FALSE)
summary(m1HA)

m2HA <- lmer(HA ~ age + (1+age|ID), data = long, REML = FALSE,
              control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2HA)

m3HA <- lmer(HA ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3HA)

## Social Potency -- best model is m1SP
m1SP <- lmer(SP ~ age + (1|ID), data = long, REML = FALSE)
summary(m1SP)

m2SP <- lmer(SP ~ age + (1+age|ID), data = long, REML = FALSE,
              control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2SP)

m3SP <- lmer(SP ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3SP)


## Traditionalism -- best model is m1TR
m1TR <- lmer(TR ~ age + (1|ID), data = long, REML = FALSE)
summary(m1TR)

m2TR <- lmer(TR ~ age + (1+age|ID), data = long, REML = FALSE,
              control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2TR)

m3TR <- lmer(TR ~ poly(age, degree = 2, raw = TRUE) + (1+age|ID), data = long, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(m3TR)

# > Distinguishable dyads ----
# >> Mean-level change ----
## Achievement 
mACd <- lme(fixed = AC ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old| IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mACd)

## Aggression 
mAGd <- lme(fixed = AG ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mAGd)

## Control 
mCONd <- lme(fixed = CON ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mCONd)

## Harm Avoidance
mHAd <- lme(fixed = HA ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mHAd)

## Social Potency
mSPd <- lme(fixed = SP ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mSPd)

## Traditionalism
mTRd <- lme(fixed = TR ~ -1 + young + young:age + old + old:age,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mTRd)

# >> Gender as moderators ----
#male = 0, female = 1
long <- long %>% mutate(
  IDSEX = ifelse(IDSEX == 1, 0, 1)) 

## Achievement 
mgACd <- lme(fixed = AC ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgACd)
anova(mACd, mgACd)

## Aggression 
mgAGd <- lme(fixed = AG ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgAGd)

## Control 
mgCONd <- lme(fixed = CON ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgCONd)

## Harm Avoidance
mgHAd <- lme(fixed = HA ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgHAd)

## Social Potency
mgSPd <- lme(fixed = SP ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgSPd)

## Traditionalism
mgTRd <- lme(fixed = TR ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(mgTRd)

# >> Adoption status as moderators ----
#bio = 0, adop = 1
long <- long %>% mutate(
  IDAB = ifelse(IDAB == 1, 1, 0)) 

## Achievement 
maACd <- lme(fixed = AC ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maACd)

## Aggression 
maAGd <- lme(fixed = AG ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maAGd)

## Control 
maCONd <- lme(fixed = CON ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maCONd)

## Harm Avoidance
maHAd <- lme(fixed = HA ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maHAd)

## Social Potency
maSPd <- lme(fixed = SP ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maSPd)

## Traditionalism
maTRd <- lme(fixed = TR ~ -1 + young + young:age + old + old:age + young:age:IDSEX + old:age:IDSEX,
            random = ~ -1 + young + old | IDYRFAM,
            correlation = corAR1(), 
            weights=varIdent(form = ~1 | yo), 
            na.action = "na.omit",
            data = long,
            control = list(maxIter = 1000),
            method = "ML")
summary(maTRd)