# load -------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

load("PROCESSED DATA/SEN CI/cohort2.rda")

# Misc --------------------------------------------------------------------

cohort2 <- cohort2[, !(c("schoollunchtaken"))]

cohort2[is.na(sourcetable)]$sourcetable <- "SpringCensus"
cohort2[sourcetable == "PupilsOnRoll"]$sourcetable <- "SpringCensus"

cohort2$eth.2 <- cohort2$ethnicgroupminor
cohort2[ethnicgroupminor %in% c("N/A ", "NOBT", "REFU", "MISS")]$eth.2 <- "unknown"

cohort2[languagegroupminor == ""]$languagegroupminor <- NA
cohort2$lang <- factor("unknown", levels = c("eng", "oth", "unknown"))
cohort2[languagegroupminor %in% c("ENB", "ENG") | languagegroupmajor == "1_ENG"]$lang <- "eng"
cohort2[languagegroupminor %in% c("OTB", "OTH") | languagegroupmajor == "2_OTH"]$lang <- "oth"
cohort2[year == 0 & firstlanguage %in% c("ENB", "ENG")]$lang <- "eng"
cohort2[year == 0 & firstlanguage %in% c("OTB", "OTH")]$lang <- "oth"

cohort2[gender %in% c(0, 9)]$gender <- NA
cohort2$female <- NA
cohort2[gender == "M" | gender == "1"]$female <- 0
cohort2[gender == "F" | gender == "2"]$female <- 1

cohort2[, row_per_child := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
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
set.seed(100)
cohort2[, female.clean := mode.fun(female), by = "pupilmatchingrefanonymous"]

# ethnicity
set.seed(1)
cohort2[, eth.clean := mode.fun(eth.2), by = "pupilmatchingrefanonymous"]
cohort2[is.na(eth.clean)]$eth.clean <- "unknown"

# language
set.seed(753)
cohort2[, lang.clean := mode.fun(lang), by = "pupilmatchingrefanonymous"]
cohort2[is.na(lang.clean)]$lang.clean <- "unknown"

rm(mode.fun)

# SEN ---------------------------------------------------------------------

cohort2$sen2 <- factor("None", levels = c("None", "AAPS", "SEHCP"))
cohort2[senprovision %in% c("A", "P", "K")]$sen2 <- "AAPS"
cohort2[senprovision %in% c("S", "E")]$sen2 <- "SEHCP"
cohort2$sen2.integer <- as.integer(cohort2$sen2)

# Pupil eth, gender, idaci, fsm -------------------------------------------

cohort2$eth.clean.integer <- as.integer(as.factor(cohort2$eth.clean))
table(cohort2$eth.clean); table(cohort2$eth.clean.integer)

cohort2$lang.clean.integer <- as.integer(cohort2$lang.clean)
table(cohort2$lang.clean); table(cohort2$lang.clean.integer)

cohort2[, fsm_y0 := fsmeligible[1], by = "pupilmatchingrefanonymous"]
cohort2[, sen_y0 := sen2[1], by = "pupilmatchingrefanonymous"]
cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

# Complete record ---------------------------------------------------------

cohort2$complete.record <-
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

round(prop.table(table(cohort2$idaci_deciles, cohort2$year), 2) * 100, 1)
round(prop.table(table(cohort2$idaci_quintiles, cohort2$year), 2) * 100, 1)

rm(idaci_2007, idaci_2010, idaci_2015, idaci_2019)
gc()

cohort2[, idaci_y0 := idaci_quintiles[1], by = "pupilmatchingrefanonymous"]
cohort2[is.na(idaci_y0)]$idaci_y0 <- 6

# SAVE --------------------------------------------------------------------

save(cohort2, file = "PROCESSED DATA/SEN CI/cohort2_npd_clean.rda")
