
# load -------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

load("PROCESSED DATA/cohort1.rda")
load("PROCESSED DATA/cohort2.rda")

# MISC CHARACTERISTICS CLEANING -----------------------------------------------

cohort2 <- cohort2[, !(c("schoollunchtaken"))]

cohort1[is.na(sourcetable)]$sourcetable <- "SpringCensus"
cohort1[sourcetable == "PupilsOnRoll"]$sourcetable <- "SpringCensus"

cohort2[is.na(sourcetable)]$sourcetable <- "SpringCensus"
cohort2[sourcetable == "PupilsOnRoll"]$sourcetable <- "SpringCensus"

cohort1$eth.2 <- cohort1$ethnicgroupminor
cohort1[ethnicgroupminor %in% c("N/A ", "NOBT", "REFU", "MISS")]$eth.2 <- "unknown"

cohort2$eth.2 <- cohort2$ethnicgroupminor
cohort2[ethnicgroupminor %in% c("N/A ", "NOBT", "REFU", "MISS")]$eth.2 <- "unknown"

cohort1[languagegroupminor == ""]$languagegroupminor <- NA
cohort1$lang <- factor("unknown", levels = c("eng", "oth", "unknown"))
cohort1[languagegroupminor %in% c("ENB", "ENG") | languagegroupmajor == "1_ENG"]$lang <- "eng"
cohort1[languagegroupminor %in% c("OTB", "OTH") | languagegroupmajor == "2_OTH"]$lang <- "oth"
cohort1[year == 1 & firstlanguage %in% c("ENB", "ENG")]$lang <- "eng"
cohort1[year == 1 & firstlanguage %in% c("OTB", "OTH")]$lang <- "oth"

cohort2[languagegroupminor == ""]$languagegroupminor <- NA
cohort2$lang <- factor("unknown", levels = c("eng", "oth", "unknown"))
cohort2[languagegroupminor %in% c("ENB", "ENG") | languagegroupmajor == "1_ENG"]$lang <- "eng"
cohort2[languagegroupminor %in% c("OTB", "OTH") | languagegroupmajor == "2_OTH"]$lang <- "oth"
cohort2[year == 0 & firstlanguage %in% c("ENB", "ENG")]$lang <- "eng"
cohort2[year == 0 & firstlanguage %in% c("OTB", "OTH")]$lang <- "oth"

cohort1[gender %in% c(0, 9)]$gender <- NA
cohort1$female <- NA
cohort1[gender == "M" | gender == "1"]$female <- 0
cohort1[gender == "F" | gender == "2"]$female <- 1

cohort2[gender %in% c(0, 9)]$gender <- NA
cohort2$female <- NA
cohort2[gender == "M" | gender == "1"]$female <- 0
cohort2[gender == "F" | gender == "2"]$female <- 1

cohort1[, row_per_child := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
cohort2[, row_per_child := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

cohort1[, row_per_child_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous, year)]
cohort2[, row_per_child_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous, year)]

mode.fun <- function(x) {
  v <- x[x != "unknown" & !is.na(x)]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  if (length(md) == 1) {
    return(md)
  } else {
    return(md[which(rmultinom(1, 1, rep(1/length(md), length(md))) == 1)])
  }
}

# gender
cohort1[, sex.min := min(female), by = "pupilmatchingrefanonymous"]
cohort1[, sex.max := max(female), by = "pupilmatchingrefanonymous"]
cohort1$sex.comp <- cohort1$sex.min != cohort1$sex.max
table(cohort1[row_per_child == 1]$sex.comp)

set.seed(100)
cohort1[, female_clean := mode.fun(female), by = "pupilmatchingrefanonymous"]

cohort1[, sex.min := min(female_clean), by = "pupilmatchingrefanonymous"]
cohort1[, sex.max := max(female_clean), by = "pupilmatchingrefanonymous"]
cohort1$sex.comp <- cohort1$sex.min != cohort1$sex.max
table(cohort1$sex.comp)

cohort1[, sex.comp := NULL]
cohort1[, sex.min := NULL]
cohort1[, sex.max := NULL]

cohort2[, sex.min := min(female), by = "pupilmatchingrefanonymous"]
cohort2[, sex.max := max(female), by = "pupilmatchingrefanonymous"]
cohort2$sex.comp <- cohort2$sex.min != cohort2$sex.max
table(cohort2[row_per_child == 1]$sex.comp)

set.seed(100)
cohort2[, female_clean := mode.fun(female), by = "pupilmatchingrefanonymous"]

cohort2[, sex.min := min(female_clean), by = "pupilmatchingrefanonymous"]
cohort2[, sex.max := max(female_clean), by = "pupilmatchingrefanonymous"]
cohort2$sex.comp <- cohort2$sex.min != cohort2$sex.max
table(cohort2$sex.comp)

cohort2[, sex.comp := NULL]
cohort2[, sex.min := NULL]
cohort2[, sex.max := NULL]

# ethnicity
cohort1[, eth.2.int := as.integer(as.factor(eth.2))]
cohort1[, eth.2.min := min(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort1[, eth.2.max := max(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort1$eth.check <- cohort1$eth.2.min != cohort1$eth.2.max
table(cohort1[row_per_child == 1]$eth.check) 

set.seed(1)
cohort1[, eth_clean := mode.fun(eth.2), by = "pupilmatchingrefanonymous"]
cohort1[is.na(eth_clean)]$eth_clean <- "unknown"

cohort1[, eth.2.int := as.integer(as.factor(eth_clean))]
cohort1[, eth.2.min := min(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort1[, eth.2.max := max(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort1$eth.check <- cohort1$eth.2.min != cohort1$eth.2.max
table(cohort1$eth.check) 

cohort1[, eth.check := NULL]
cohort1[, eth.2.int := NULL]
cohort1[, eth.2.min := NULL]
cohort1[, eth.2.max := NULL]

cohort2[, eth.2.int := as.integer(as.factor(eth.2))]
cohort2[, eth.2.min := min(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort2[, eth.2.max := max(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort2$eth.check <- cohort2$eth.2.min != cohort2$eth.2.max
table(cohort2[row_per_child == 1]$eth.check) # 72985

set.seed(1)
cohort2[, eth_clean := mode.fun(eth.2), by = "pupilmatchingrefanonymous"]
cohort2[is.na(eth_clean)]$eth_clean <- "unknown"

cohort2[, eth.2.int := as.integer(as.factor(eth_clean))]
cohort2[, eth.2.min := min(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort2[, eth.2.max := max(eth.2.int), by = "pupilmatchingrefanonymous"]
cohort2$eth.check <- cohort2$eth.2.min != cohort2$eth.2.max
table(cohort2$eth.check) # 

cohort2[, eth.check := NULL]
cohort2[, eth.2.int := NULL]
cohort2[, eth.2.min := NULL]
cohort2[, eth.2.max := NULL]

# language
cohort1[, lang.int := as.integer(lang)]
cohort1[, lang.min := min(lang.int), by = "pupilmatchingrefanonymous"]
cohort1[, lang.max := max(lang.int), by = "pupilmatchingrefanonymous"]
cohort1$lang.check <- cohort1$lang.min != cohort1$lang.max
table(cohort1[row_per_child == 1]$lang.check) # 

set.seed(753)
cohort1[, language_clean := mode.fun(lang), by = "pupilmatchingrefanonymous"]
cohort1[is.na(language_clean)]$language_clean <- "unknown"

cohort1[, lang.int := as.integer(language_clean)]
cohort1[, lang.min := min(lang.int), by = "pupilmatchingrefanonymous"]
cohort1[, lang.max := max(lang.int), by = "pupilmatchingrefanonymous"]
cohort1$lang.check <- cohort1$lang.min != cohort1$lang.max
table(cohort1$lang.check)

cohort1[, lang.check := NULL]
cohort1[, lang.int := NULL]
cohort1[, lang.min := NULL]
cohort1[, lang.max := NULL]

cohort2[, lang.int := as.integer(lang)]
cohort2[, lang.min := min(lang.int), by = "pupilmatchingrefanonymous"]
cohort2[, lang.max := max(lang.int), by = "pupilmatchingrefanonymous"]
cohort2$lang.check <- cohort2$lang.min != cohort2$lang.max
table(cohort2[row_per_child == 1]$lang.check) # 

set.seed(753)
cohort2[, language_clean := mode.fun(lang), by = "pupilmatchingrefanonymous"]
cohort2[is.na(language_clean)]$language_clean <- "unknown"

cohort2[, lang.int := as.integer(language_clean)]
cohort2[, lang.min := min(lang.int), by = "pupilmatchingrefanonymous"]
cohort2[, lang.max := max(lang.int), by = "pupilmatchingrefanonymous"]
cohort2$lang.check <- cohort2$lang.min != cohort2$lang.max
table(cohort2$lang.check)

cohort2[, lang.check := NULL]
cohort2[, lang.int := NULL]
cohort2[, lang.min := NULL]
cohort2[, lang.max := NULL]

rm(mode.fun)

cohort1$senprovision_3_groups <- factor("None", levels = c("None", "AAPS", "SEHCP"))
cohort1[senprovision %in% c("A", "P", "K")]$senprovision_3_groups <- "AAPS"
cohort1[senprovision %in% c("S", "E")]$senprovision_3_groups <- "SEHCP"

cohort2$senprovision_3_groups <- factor("None", levels = c("None", "AAPS", "SEHCP"))
cohort2[senprovision %in% c("A", "P", "K")]$senprovision_3_groups <- "AAPS"
cohort2[senprovision %in% c("S", "E")]$senprovision_3_groups <- "SEHCP"

# order
cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, sch_n_in_year)]
cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

# COMPLETE RECORD ---------------------------------------------------------

cohort1$complete_record <-
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 1]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 2]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 3]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 4]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 5]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 6]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 7]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 8]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 9]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 10]$pupilmatchingrefanonymous &
  cohort1$pupilmatchingrefanonymous %in% cohort1[year == 11]$pupilmatchingrefanonymous

cohort2$complete_record <-
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 0]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 1]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 2]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 3]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 4]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 5]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 6]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 7]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 8]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 9]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 10]$pupilmatchingrefanonymous &
  cohort2$pupilmatchingrefanonymous %in% cohort2[year == 11]$pupilmatchingrefanonymous

# IMD ---------------------------------------------------------------------

# This part first cleans the LSOA variable in both cohorts and then imports IDACI data from the English indices of deprivation.
# IDACI quintile variables are then created. The following scheme was used to assign different versions of IDACI to different
# years' activity:
# 
# Activity            IMD   c1    c2
# to Aug '07          2007  1-2   r-1
# Sep '07 to Aug '10  2010  3-5   2-4
# Sep '10 to Aug '15  2015  6-10  5-9
# Sep '15 to Aug '17  2019  11    10-11

# * fix LSOAs ---------------------------------------------------------------

# ** cohort 1 --------------------------------------------------------------

cohort1[is.na(llsoa)]$llsoa <- cohort1[is.na(llsoa)]$lsoa01
cohort1$lsoa_clean <- as.character(NA)

# years 1 to 5, LLSOA is fine
cohort1[year %in% 1:5]$lsoa_clean <- cohort1[year %in% 1:5]$llsoa

# years 6 to 11, need LSOA11
# years 8 and 9, alreadyd have LSOA11
cohort1[year %in% 8:9]$lsoa_clean <- cohort1[year %in% 8:9]$lsoa11

# therefore need to fix 6, 7, 10 and 11

# year 11: we have idaci 2015 and so can get LSOA11
idaci_2015 <- data.table(read.csv("IDACI/idaci_2015.csv"))
idaci_2015 <- idaci_2015[!duplicated(idaci_rank_1_most_deprived)]
ir15 <- data.table(
  idacirank_15 = cohort1[year == 11]$idacirank_15
)

ir15 <- ir15[!is.na(idacirank_15)] # because of tied ranks - this is the best way of dealing with them

ir15 <- merge(ir15,
              idaci_2015[, c("idaci_rank_1_most_deprived", "lsoa_2011")],
              by.x = "idacirank_15",
              by.y = "idaci_rank_1_most_deprived",
              all.x = T,
              sort = F)

cohort1[year == 11 & !is.na(idacirank_15)]$lsoa_clean <- ir15$lsoa_2011
rm(ir15)

# year 10: we have idaci rank (2010) and so can get LSOA01, then need to convert to 2011
idaci_2010 <- data.table(read.csv("IDACI/idaci_2010.csv"))
idaci_2010 <- idaci_2010[!duplicated(idaci_rank_1_most_deprived)]
ir10 <- data.table(
  idacirank = cohort1[year == 10]$idacirank
)

ir10 <- ir10[!is.na(idacirank)]

ir10 <- merge(ir10,
              idaci_2010[, c("idaci_rank_1_most_deprived", "lsoa_2001")],
              by.x = "idacirank",
              by.y = "idaci_rank_1_most_deprived",
              all.x = T,
              sort = F)

cohort1[year == 10 & !is.na(idacirank)]$llsoa <- ir10$lsoa_2001
rm(ir10)

# now convert year 10 to LSOA11 along with years 6 and 7 to LSOA2011
lkp <- data.table(read.csv("IDACI/LSOA01_LSOA11_EW_LUv2.csv"))

# need to decide what to do with the split ones - just take the first one
lkp <- lkp[!duplicated(LSOA01CD)]

# year 10
tmp <- data.table(
  llsoa = cohort1[year == 10]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort1[year == 10]$lsoa_clean <- tmp$LSOA11CD

# year 6
tmp <- data.table(
  llsoa = cohort1[year == 6]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort1[year == 6]$lsoa_clean <- tmp$LSOA11CD

# year 7
tmp <- data.table(
  llsoa = cohort1[year == 7]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort1[year == 7]$lsoa_clean <- tmp$LSOA11CD

rm(tmp, lkp, idaci_2015, idaci_2010)

table(is.na(cohort1$lsoa_clean), cohort1$year)

# ** cohort 2 --------------------------------------------------------------

# Activity            IMD   c1    c2
# to Aug '07          2007  1-2   r-1
# Sep '07 to Aug '10  2010  3-5   2-4
# Sep '10 to Aug '15  2015  6-10  5-9
# Sep '15 to Aug '17  2019  11    10-11

cohort2[is.na(llsoa)]$llsoa <- cohort2[is.na(llsoa)]$lsoa01
cohort2$lsoa_clean <- as.character(NA)

# years 0 to 4, LLSOA is fine
cohort2[year %in% 0:4]$lsoa_clean <- cohort2[year %in% 0:4]$llsoa

table(is.na(cohort2$lsoa_clean), cohort2$year)

# years 5 to 11, need LSOA11
# years 7, 8 and 11, already have LSOA11
cohort2[year %in% c(7:8, 11)]$lsoa_clean <- cohort2[year %in% c(7:8, 11)]$lsoa11

# therefore need to fix 5, 6, 9 and 10

# year 10: we have idaci rank15 and so can get LSOA01, then need to convert to 2011
idaci_2015 <- data.table(read.csv("IDACI/idaci_2015.csv"))
idaci_2015 <- idaci_2015[!duplicated(idaci_rank_1_most_deprived)]
ir15 <- data.table(
  idacirank_15 = cohort2[year == 10]$idacirank_15
)

ir15 <- ir15[!is.na(idacirank_15)] # because of tied ranks - this is the best way of dealing with them

ir15 <- merge(ir15,
              idaci_2015[, c("idaci_rank_1_most_deprived", "lsoa_2011")],
              by.x = "idacirank_15",
              by.y = "idaci_rank_1_most_deprived",
              all.x = T,
              sort = F)

cohort2[year == 10 & !is.na(idacirank_15)]$lsoa_clean <- ir15$lsoa_2011
rm(ir15)


# year 9: we have idaci rank (2010) and so can get LSOA01, then need to convert to 2011
idaci_2010 <- data.table(read.csv("IDACI/idaci_2010.csv"))
idaci_2010 <- idaci_2010[!duplicated(idaci_rank_1_most_deprived)]
ir10 <- data.table(
  idacirank = cohort2[year == 9]$idacirank
)

ir10 <- ir10[!is.na(idacirank)]

ir10 <- merge(ir10,
              idaci_2010[, c("idaci_rank_1_most_deprived", "lsoa_2001")],
              by.x = "idacirank",
              by.y = "idaci_rank_1_most_deprived",
              all.x = T,
              sort = F)

cohort2[year == 9 & !is.na(idacirank)]$llsoa <- ir10$lsoa_2001
rm(ir10)

# now convert year 9 to LSOA11 along with years 5 and 6 to LSOA2011
lkp <- data.table(read.csv("IDACI/LSOA01_LSOA11_EW_LUv2.csv"))

# need to decide what to do with the split ones - just take the first one
lkp <- lkp[!duplicated(LSOA01CD)]

# year 9
tmp <- data.table(
  llsoa = cohort2[year == 9]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort2[year == 9]$lsoa_clean <- tmp$LSOA11CD


# year 5
tmp <- data.table(
  llsoa = cohort2[year == 5]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort2[year == 5]$lsoa_clean <- tmp$LSOA11CD

# year 6
tmp <- data.table(
  llsoa = cohort2[year == 6]$llsoa
)

tmp <- merge(tmp,
             lkp[, c("LSOA01CD", "LSOA11CD")],
             by.x = "llsoa",
             by.y = "LSOA01CD",
             all.x = T,
             sort = F)

cohort2[year == 6]$lsoa_clean <- tmp$LSOA11CD

rm(tmp, lkp, idaci_2015, idaci_2010)

table(is.na(cohort2$lsoa_clean), cohort2$year)

# * assign IDACI -----------------------------------------------------------

# Activity            IMD   c1    c2
# to Aug '07          2007  1-2   r-1
# Sep '07 to Aug '10  2010  3-5   2-4
# Sep '10 to Aug '15  2015  6-10  5-9
# Sep '15 to Aug '17  2019  11    10-11

idaci_2007 <- data.table(read.csv("IDACI/idaci_2007.csv"))
idaci_2010 <- data.table(read.csv("IDACI/idaci_2010.csv"))
idaci_2015 <- data.table(read.csv("IDACI/idaci_2015.csv"))
idaci_2019 <- data.table(read.csv("IDACI/idaci_2019.csv"))

# ** cohort 1 -------------------------------------------------------------

cohort1$idaci_rank_clean <- as.numeric(NA)

# years 1 and 2 - IDACI 2007
tmp <- data.table(
  lsoa_clean = cohort1[year %in% 1:2]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2007[, c("lsoa_2001", "idaci_rank")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2001",
             all.x = T,
             sort = F)

cohort1[year %in% 1:2]$idaci_rank_clean <- tmp$idaci_rank
rm(tmp)

# years 3 to 5 - IDACI 2010
cohort1[year %in% 3:5]$idaci_rank_clean <- cohort1[year %in% 3:5]$idacirank

# years 6 to 10 - IDACI 2015
tmp <- data.table(
  lsoa_clean = cohort1[year %in% 6:10]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2015[, c("lsoa_2011", "idaci_rank_1_most_deprived")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2011",
             all.x = T,
             sort = F)

cohort1[year %in% 6:10]$idaci_rank_clean <- tmp$idaci_rank_1_most_deprived
rm(tmp)

# year 11 - IDACI 2019
tmp <- data.table(
  lsoa_clean = cohort1[year == 11]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2019[, c("lsoa_2011", "idaci_rank_1_most_deprived")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2011",
             all.x = T,
             sort = F)

cohort1[year == 11]$idaci_rank_clean <- tmp$idaci_rank_1_most_deprived
rm(tmp)

table(is.na(cohort1$idaci_rank_clean), cohort1$year)

cohort1$idaci_deciles <- as.integer(NA)

# years 1 to 6 - LSOA01
cohort1[!is.na(idaci_rank_clean) & year < 6]$idaci_deciles <- as.integer(cut(cohort1[!is.na(idaci_rank_clean) & year < 6]$idaci_rank_clean,
                                                                             breaks = quantile(1:32482, probs = seq(0, 1, 0.1)),
                                                                             include.lowest = T,
                                                                             right = T))

# years 7 to 11 - LSOA11
cohort1[!is.na(idaci_rank_clean) & year >= 6]$idaci_deciles <- as.integer(cut(cohort1[!is.na(idaci_rank_clean) & year >= 6]$idaci_rank_clean,
                                                                              breaks = quantile(1:32844, probs = seq(0, 1, 0.1)),
                                                                              include.lowest = T,
                                                                              right = T,
                                                                              ordered_result = T))

table(cohort1$idaci_deciles, cohort1$year)

# quintiles
cohort1$idaci_quintiles <- as.integer(NA)
cohort1[idaci_deciles %in% 1:2]$idaci_quintiles <- 1
cohort1[idaci_deciles %in% 3:4]$idaci_quintiles <- 2
cohort1[idaci_deciles %in% 5:6]$idaci_quintiles <- 3
cohort1[idaci_deciles %in% 7:8]$idaci_quintiles <- 4
cohort1[idaci_deciles %in% 9:10]$idaci_quintiles <- 5

table(cohort1$idaci_quintiles, cohort1$year)

# ** cohort 2 -------------------------------------------------------------

cohort2$idaci_rank_clean <- as.numeric(NA)

# years r to 2 - IDACI 2007
tmp <- data.table(
  lsoa_clean = cohort2[year %in% 0:2]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2007[, c("lsoa_2001", "idaci_rank")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2001",
             all.x = T,
             sort = F)

cohort2[year %in% 0:2]$idaci_rank_clean <- tmp$idaci_rank
rm(tmp)

# years 3 to 5 - IDACI 2010
cohort2[year %in% 3:5]$idaci_rank_clean <- cohort2[year %in% 3:5]$idacirank

# years 6 to 10 - IDACI 2015
tmp <- data.table(
  lsoa_clean = cohort2[year %in% 6:10]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2015[, c("lsoa_2011", "idaci_rank_1_most_deprived")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2011",
             all.x = T,
             sort = F)

cohort2[year %in% 6:10]$idaci_rank_clean <- tmp$idaci_rank_1_most_deprived
rm(tmp)

# year 11 - IDACI 2019
tmp <- data.table(
  lsoa_clean = cohort2[year == 11]$lsoa_clean
)

tmp <- merge(tmp,
             idaci_2019[, c("lsoa_2011", "idaci_rank_1_most_deprived")],
             by.x = "lsoa_clean",
             by.y = "lsoa_2011",
             all.x = T,
             sort = F)

cohort2[year == 11]$idaci_rank_clean <- tmp$idaci_rank_1_most_deprived
rm(tmp)

table(is.na(cohort2$idaci_rank_clean), cohort2$year)

cohort2$idaci_deciles <- as.integer(NA)

# years r to 5 - LSOA01
cohort2[!is.na(idaci_rank_clean) & year < 5]$idaci_deciles <- as.integer(cut(cohort2[!is.na(idaci_rank_clean) & year < 5]$idaci_rank_clean,
                                                                             breaks = quantile(1:32482, probs = seq(0, 1, 0.1)),
                                                                             include.lowest = T,
                                                                             right = T))

# years 6 to 11 - LSOA11
cohort2[!is.na(idaci_rank_clean) & year >= 5]$idaci_deciles <- as.integer(cut(cohort2[!is.na(idaci_rank_clean) & year >= 5]$idaci_rank_clean,
                                                                              breaks = quantile(1:32844, probs = seq(0, 1, 0.1)),
                                                                              include.lowest = T,
                                                                              right = T))

table(cohort2$idaci_deciles, cohort2$year)

# quintiles
cohort2$idaci_quintiles <- as.integer(NA)
cohort2[idaci_deciles %in% 1:2]$idaci_quintiles <- 1
cohort2[idaci_deciles %in% 3:4]$idaci_quintiles <- 2
cohort2[idaci_deciles %in% 5:6]$idaci_quintiles <- 3
cohort2[idaci_deciles %in% 7:8]$idaci_quintiles <- 4
cohort2[idaci_deciles %in% 9:10]$idaci_quintiles <- 5

table(cohort2$idaci_quintiles, cohort2$year)

round(prop.table(table(cohort1$idaci_deciles, cohort1$year), 2) * 100, 1)
round(prop.table(table(cohort1$idaci_quintiles, cohort1$year), 2) * 100, 1)
round(prop.table(table(cohort2$idaci_deciles, cohort2$year), 2) * 100, 1)
round(prop.table(table(cohort2$idaci_quintiles, cohort2$year), 2) * 100, 1)

rm(idaci_2007, idaci_2010, idaci_2015, idaci_2019)
gc()

# DfE ---------------------------------------------------------------------

# Imports EduBase variables.

dfe <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/DfE.csv", stringsAsFactors = F))
names(dfe) <- tolower(gsub("name", "", names(dfe)))

length(unique(dfe$urn))
dfe1 <- dfe[urn %in% cohort1$urn]
dfe2 <- dfe[urn %in% cohort2$urn]

length(unique(dfe1$urn))
length(unique(dfe2$urn))

table(cohort1$urn %in% dfe1$urn) # not all of the NPD schools are in EduBase
table(cohort1[!(urn %in% dfe1$urn)]$sourcetable) # because they are all AP

table(cohort2$urn %in% dfe2$urn)
table(cohort2[!(urn %in% dfe2$urn)]$sourcetable) # ditto

names(dfe1)[names(dfe1) == "closedate"] <- "schoolclosedate"
names(dfe1)[names(dfe1) == "gender"] <- "schoolgender"

names(dfe2)[names(dfe2) == "closedate"] <- "schoolclosedate"
names(dfe2)[names(dfe2) == "gender"] <- "schoolgender"

dfe1[schoolclosedate == ""]$schoolclosedate <- "01/01/1800"
dfe1$schoolclosedate <- as.Date(dfe1$schoolclosedate, format = "%d/%m/%Y")

dfe2[schoolclosedate == ""]$schoolclosedate <- "01/01/1800"
dfe2$schoolclosedate <- as.Date(dfe2$schoolclosedate, format = "%d/%m/%Y")

cohort1 <- merge(cohort1, dfe1[, c("urn", "typeofestablishment", "establishmenttypegroup", "phaseofeducation",
                                   "schoolgender", "boarders", "religiouscharacter", "admissionspolicy",
                                   "schoolcapacity", "specialclasses")],
                 all.x = T,
                 by = "urn")

cohort2 <- merge(cohort2, dfe2[, c("urn", "typeofestablishment", "establishmenttypegroup", "phaseofeducation",
                                   "schoolgender", "boarders", "religiouscharacter", "admissionspolicy",
                                   "schoolcapacity", "specialclasses")],
                 all.x = T,
                 by = "urn")

rm(dfe, dfe1, dfe2)

cohort1[sourcetable == "AP"]$establishmenttypegroup <- "AP" # is NA for AP
cohort1[sourcetable == "AP"]$typeofestablishment <- "AP"

cohort1[sourcetable == "PRU"]$establishmenttypegroup <- "PRU" # is mostly NA for PRU
cohort1[sourcetable == "PRU"]$typeofestablishment <- "PRU"

cohort2[sourcetable == "AP"]$establishmenttypegroup <- "AP" # is NA for AP
cohort2[sourcetable == "AP"]$typeofestablishment <- "AP"

cohort2[sourcetable == "PRU"]$establishmenttypegroup <- "PRU" # is mostly NA for PRU
cohort2[sourcetable == "PRU"]$typeofestablishment <- "PRU"

# MISC SCHOOL CLEANING ---------------------------------------------------

# * school type ----------------------------------------------------------

cohort1$school_type <- "Mainstream"
cohort1[establishmenttypegroup == "Special schools"]$school_type <- "Special"

ap <- c("Academy alternative provision converter", "Academy alternative provision sponsor led",
        "AP", "Free schools alternative provision")
cohort1[sourcetable == "AP" | typeofestablishment %in% ap]$school_type <- "AP"
rm(ap)

cohort1[sourcetable == "PRU" | typeofestablishment == "Pupil referral unit" | phase == "PR" |
          typeofestablishment == "PRU"]$school_type <- "PRU"

table(cohort1$school_type, cohort1$year, useNA = "always")

cohort2$school_type <- "Mainstream"
cohort2[establishmenttypegroup == "Special schools"]$school_type <- "Special"

ap <- c("Academy alternative provision converter", "Academy alternative provision sponsor led",
        "AP", "Free schools alternative provision")
cohort2[sourcetable == "AP" | typeofestablishment %in% ap]$school_type <- "AP"
rm(ap)

cohort2[sourcetable == "PRU" | typeofestablishment == "Pupil referral unit" | phase == "PR" |
          typeofestablishment == "PRU"]$school_type <- "PRU"

table(cohort2$school_type, cohort2$year, useNA = "always")

# * school legal status ---------------------------------------------------

cohort1$school_legal_status <- factor("LA maintained schools",
                                      levels = c("LA maintained schools",
                                                 "Academy/Free School/Independent",
                                                 "Special",
                                                 "PRU",
                                                 "AP"))

cohort1[establishmenttypegroup %in% c("Academies", "Free Schools", "Independent schools")]$school_legal_status <- "Academy/Free School/Independent"
cohort1[school_type == "AP"]$school_legal_status <- "AP"
cohort1[school_type == "PRU"]$school_legal_status <- "PRU"
cohort1[school_type == "Special"]$school_legal_status <- "Special"

table(cohort1$school_legal_status, cohort1$year, useNA = "always")

cohort2$school_legal_status <- factor("LA maintained schools",
                                      levels = c("LA maintained schools",
                                                 "Academy/Free School/Independent",
                                                 "Special",
                                                 "PRU",
                                                 "AP"))

cohort2[establishmenttypegroup %in% c("Academies", "Free Schools", "Independent schools")]$school_legal_status <- "Academy/Free School/Independent"
cohort2[school_type == "AP"]$school_legal_status <- "AP"
cohort2[school_type == "PRU"]$school_legal_status <- "PRU"
cohort2[school_type == "Special"]$school_legal_status <- "Special"

table(cohort2$school_legal_status, cohort2$year, useNA = "always")

# * religious character -----------------------------------------------------

cohort1$religiouscharacter_5_groups <- "None/DNA"

chr <- c("Catholic", "Christian", "Greek Orthodox", "Methodist",
         "Quaker", "Roman Catholic", "Roman Catholic/Church of England",
         "Seventh Day Adventist", "United Reformed Church")
coe17 <- "Church of England"

cohort1[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter_5_groups <- "Christian"
cohort1[religiouscharacter == "Sikh"]$religiouscharacter_5_groups <- "Sikh"
cohort1[religiouscharacter == "Muslim"]$religiouscharacter_5_groups <- "Muslim"
cohort1[religiouscharacter == "Jewish"]$religiouscharacter_5_groups <- "Jewish"

oth <- c("Sikh", "Muslim", "Jewish")
cohort1$religiouscharacter_3_groups <- "None/DNA"
cohort1[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter_3_groups <- "Christian"
cohort1[religiouscharacter %in% oth]$religiouscharacter_3_groups <- "Other"
rm(chr, oth, coe17)

cohort1$religiouscharacter <- NULL

table(cohort1$religiouscharacter_5_groups, useNA = "always")
table(cohort1$religiouscharacter_3_groups, useNA = "always")


cohort2$religiouscharacter_5_groups <- "None/DNA"

chr <- c("Catholic", "Christian", "Greek Orthodox", "Methodist",
         "Quaker", "Roman Catholic", "Roman Catholic/Church of England",
         "Seventh Day Adventist", "United Reformed Church")
coe17 <- "Church of England"

cohort2[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter_5_groups <- "Christian"
cohort2[religiouscharacter == "Sikh"]$religiouscharacter_5_groups <- "Sikh"
cohort2[religiouscharacter == "Muslim"]$religiouscharacter_5_groups <- "Muslim"
cohort2[religiouscharacter == "Jewish"]$religiouscharacter_5_groups <- "Jewish"

oth <- c("Sikh", "Muslim", "Jewish")
cohort2$religiouscharacter_3_groups <- "None/DNA"
cohort2[religiouscharacter %in% chr |
          substr(religiouscharacter, 1, 17) %in% coe17]$religiouscharacter_3_groups <- "Christian"
cohort2[religiouscharacter %in% oth]$religiouscharacter_3_groups <- "Other"
rm(chr, oth, coe17)

cohort2$religiouscharacter <- NULL

table(cohort2$religiouscharacter_5_groups, useNA = "always")
table(cohort2$religiouscharacter_3_groups, useNA = "always")

# * admissions policy -------------------------------------------------------

cohort1[admissionspolicy %in% c("", "Not collected") | is.na(admissionspolicy)]$admissionspolicy <- "Not applicable"
cohort1[admissionspolicy %in% c("Comprehensive (secondary)",
                                "Modern (secondary)")]$admissionspolicy <- "Non-selective"
cohort1$admissionspolicy <- factor(cohort1$admissionspolicy,
                                   levels = c("Non-selective", "Selective (grammar)", "Not applicable"))
table(cohort1$admissionspolicy, useNA = "always")

cohort2[admissionspolicy %in% c("", "Not collected") | is.na(admissionspolicy)]$admissionspolicy <- "Not applicable"
cohort2[admissionspolicy %in% c("Comprehensive (secondary)",
                                "Modern (secondary)")]$admissionspolicy <- "Non-selective"
cohort2$admissionspolicy <- factor(cohort2$admissionspolicy,
                                   levels = c("Non-selective", "Selective (grammar)", "Not applicable"))
table(cohort2$admissionspolicy, useNA = "always")


# * boarders & special classes ---------------------------------------------------

cohort1[boarders == ""]$boarders <- "Not applicable"
cohort1[specialclasses == ""]$specialclasses <- "Not applicable"

cohort2[boarders == ""]$boarders <- "Not applicable"
cohort2[specialclasses == ""]$specialclasses <- "Not applicable"

# * phase of education ------------------------------------------------------

cohort1[is.na(phaseofeducation)]$phaseofeducation <- "Not applicable"
cohort1$phaseofeducation <- factor(cohort1$phaseofeducation,
                                   levels = c("Nursery", "Primary", "Middle deemed primary",
                                              "Secondary", "Middle deemed secondary",
                                              "All through", "16 plus", "Not applicable"))
table(cohort1$phaseofeducation)

cohort2[is.na(phaseofeducation)]$phaseofeducation <- "Not applicable"
cohort2$phaseofeducation <- factor(cohort2$phaseofeducation,
                                   levels = c("Nursery", "Primary", "Middle deemed primary",
                                              "Secondary", "Middle deemed secondary",
                                              "All through", "16 plus", "Not applicable"))
table(cohort2$phaseofeducation)

# * school gender -----------------------------------------------------------

cohort1[schoolgender == ""]$schoolgender <- NA
cohort1$schoolgender <- factor(cohort1$schoolgender,
                               levels = c("Mixed", "Girls", "Boys"))
table(cohort1$schoolgender, useNA = "always")

cohort2[schoolgender == ""]$schoolgender <- NA
cohort2$schoolgender <- factor(cohort2$schoolgender,
                               levels = c("Mixed", "Girls", "Boys"))
table(cohort2$schoolgender, useNA = "always")

# * ncyearactual -------------------------------------------------------------

cohort1$ncyearactual <- factor(cohort1$ncyearactual,
                               levels = c(as.character(1:11), "X"))

cohort2$ncyearactual <- factor(cohort2$ncyearactual,
                               levels = c(as.character(1:11), "X"))

# REGION ------------------------------------------------------------------

# Imports region from a region lookup file.

lea_region <- data.table(read.csv("lea_region.csv",
                                  header = T,
                                  stringsAsFactors = F))

cohort1 <- merge(cohort1,
                 lea_region[, c("lea", "region", "region_major")],
                 all.x = T,
                 by.x = "la",
                 by.y = "lea")

cohort2 <- merge(cohort2,
                 lea_region[, c("lea", "region", "region_major")],
                 all.x = T,
                 by.x = "la",
                 by.y = "lea")

rm(lea_region)

# REMOVE UNNEEDED VARIABLES -----------------------------------------------

remove_vars <- c("censusdate", "censusterm", "datacollection", "sourcetable",
                 "gender", "yearofbirth", "monthofbirth", "ethnicgroupminor",
                 "connexions", "incare", "careauthority", "incareatcurrentschool",
                 "firstlanguage", "enrolstatus", "senprovision", "primarysentype",
                 "specialprovisionindicator", "postadvanced", "alevel", "gcse", "gnvq",
                 "pregnvq", "nvq", "other", "idaciscore", "idacirank", "entry.calyear",
                 "entry.acayear", "languagegroupminor", "modeoftravel", "onroll",
                 "everfsm_3", "everfsm_6", "hoursatsetting", "yssa", "phase", "llsoa", "lsoa01", "lsoa11",
                 "ethnicgroupmajor", "languagegroupmajor", "idaciscore_10", "idacirank_10", "idaciscore_15",
                 "idacirank_15", "lang", "eth.2", "female", "idaci_rank_clean", "typeofestablishment",
                 "establishmenttypegroup")

cohort1 <- cohort1[, !remove_vars, with = F]
cohort2 <- cohort2[, !remove_vars, with = F]

rm(remove_vars)

# SAVE --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1_npd_clean_sch.rda")
save(cohort2, file = "PROCESSED DATA/cohort2_npd_clean_sch.rda")
