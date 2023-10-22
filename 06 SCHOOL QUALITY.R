
# LOAD -----------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

# install.packages("R:/Software Add-Ons/R-library/NEW 3.6 R-Library/haven_2.3.1.zip",
#                  repos = NULL,
#                  lib = "r-library",
#                  type = "win.binary")

library("data.table")
library("lme4")

load("PROCESSED DATA/cohort1_npd_clean_sch_ad.rda")
load("PROCESSED DATA/cohort2_npd_clean_sch_ad.rda")

# * OFSTED -----------------------------------------------------------------

ofsted <- list()
ofsted[[1]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2006.csv", stringsAsFactors = F))
ofsted[[2]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2007.csv", stringsAsFactors = F))
ofsted[[3]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2008.csv", stringsAsFactors = F))
ofsted[[4]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2009.csv", stringsAsFactors = F))
ofsted[[5]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2010.csv", stringsAsFactors = F))
ofsted[[6]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2011.csv", stringsAsFactors = F))
ofsted[[7]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2012.csv", stringsAsFactors = F))
ofsted[[8]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2014.csv", stringsAsFactors = F))
ofsted[[9]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2015.csv", stringsAsFactors = F))
ofsted[[10]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2016.csv", stringsAsFactors = F))
ofsted[[11]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2017.csv", stringsAsFactors = F))
ofsted[[12]] <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/OFSTED/clean/OfstedMarch2018.csv", stringsAsFactors = F))
names(ofsted) <- c("mar2006", "mar2007", "mar2008", "mar2009", "mar2010", "mar2011",
                   "mar2012", "mar2014", "mar2015", "mar2016", "mar2017", "mar2018")

ofsted_all <- data.table(rbind(ofsted[[1]], ofsted[[2]], ofsted[[3]], ofsted[[4]], ofsted[[5]], ofsted[[6]], ofsted[[7]],
                               ofsted[[8]], ofsted[[9]], ofsted[[10]], ofsted[[11]], fill = T))
ofsted_all <- ofsted_all[inspect_start != "" & inspect_start != "NULL"]

ofsted_all$inspect_start <- as.Date(ofsted_all$inspect_start, format = "%d/%m/%Y")
ofsted_all$cyear <- format(ofsted_all$inspect_start, format = "%Y")
lt <- as.POSIXlt(ofsted_all$inspect_start)
ofsted_all$ayear <- lt$year + (lt$mo > 7) + 1900
rm(lt)

ofsted_all <- ofsted_all[order(urn, inspect_start)]
ofsted_all <- ofsted_all[!duplicated(ofsted_all[, c("urn", "inspect_start")])]
ofsted_all$overall_effectiveness <- as.integer(ofsted_all$overall_effectiveness)

ofsted_all[, record_n := seq_len(.N), by = rleid(urn)]
length(unique(ofsted_all$urn))
table(ofsted_all$record_n)
ofsted_all[, max.record_n := max(record_n), by = "urn"]
table(ofsted_all[record_n == 1]$max.record_n)

rm(ofsted)

get_nearest <- function(targetcol, targetidcol, targetdatecol, sourcedate, sourceid) {
  abs.Date <- function(x) {x}
  target <- targetcol[targetidcol == sourceid]
  targetdatevec <- targetdatecol[targetidcol == sourceid]
  return(target[which(abs(sourcedate - targetdatevec) == min(abs(sourcedate - targetdatevec)))][1])
}

# ** cohort 1 --------------------------------------------------------------

cohort1$ayear_startdate <- as.Date(paste0(substr(cohort1$academicyear, 1, 4), "-09-01"))
table(is.na(cohort1$urn))

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child)]

cohort1[year == 7 & row_per_child_year == 1,
        ofsted_overall_effectiveness := get_nearest(ofsted_all$overall_effectiveness,
                                                    ofsted_all$urn,
                                                    ofsted_all$inspect_start,
                                                    ayear_startdate,
                                                    urn),
        by = seq_len(nrow(cohort1[year == 7 & row_per_child_year == 1]))]

cohort1[year == 7 & row_per_child_year == 1,
        ofsted_date := get_nearest(ofsted_all$inspect_start,
                                   ofsted_all$urn,
                                   ofsted_all$inspect_start,
                                   ayear_startdate,
                                   urn),
        by = seq_len(nrow(cohort1[year == 7 & row_per_child_year == 1]))]

# ** cohort 2 --------------------------------------------------------------

cohort2$ayear_startdate <- as.Date(paste0(substr(cohort2$academicyear, 1, 4), "-09-01"))
table(is.na(cohort2$urn))

cohort2[year == 7 & row_per_child_year == 1,
        ofsted_overall_effectiveness := get_nearest(ofsted_all$overall_effectiveness,
                                                    ofsted_all$urn,
                                                    ofsted_all$inspect_start,
                                                    ayear_startdate,
                                                    urn),
        by = seq_len(nrow(cohort2[year == 7 & row_per_child_year == 1]))]

cohort2[year == 7 & row_per_child_year == 1,
        ofsted_date := get_nearest(ofsted_all$inspect_start,
                                   ofsted_all$urn,
                                   ofsted_all$inspect_start,
                                   ayear_startdate,
                                   urn),
        by = seq_len(nrow(cohort2[year == 7 & row_per_child_year == 1]))]

rm(get_nearest, ofsted_all)

# * GCSE --------------------------------------------------------------------

sdb <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/school_panel/school_panel_2017.csv",
                           stringsAsFactors = F,
                           header = T))

get_prev_urn <- function(source_urn, target_urn_id, target_urn) {
  # there are some schools that merge from year 6 to year 7.
  # we will have to pick at random what the previous school was.
  urn_v <- target_urn[target_urn_id == source_urn]
  if (length(urn_v) > 0) {
    if (length(urn_v) == 1) {
      return(urn_v)
    } else {
      return(urn_v[which(rmultinom(1, 1, rep(1/length(urn_v), length(urn_v))) == 1)])
    }
  } else {
    # these seem to be schools who changed identity during the school year.
    # in the panel database, we just need to look to the year before to get the correct ID,
    # which is the source_urn
    return(source_urn)
  }
}

# ** cohort 1 -------------------------------------------------------------

sdb_c1 <- sdb[, c("urn_2011", "urn_2012")]
sdb_c1 <- sdb_c1[!is.na(urn_2012)]
sdb_c1 <- sdb_c1[!duplicated(sdb_c1)]

cohort1[year == 7 & row_per_child_year == 1, urn_prev_yr := get_prev_urn(urn,
                                                                         sdb_c1$urn_2012,
                                                                         sdb_c1$urn_2011),
        by = seq_len(nrow(cohort1[year == 7 & row_per_child_year == 1]))]

cohort1$urn_for_ks4 <- cohort1$urn
cohort1[year != 7 | row_per_child_year != 1]$urn_for_ks4 <- NA
cohort1[!is.na(urn_prev_yr) & urn != urn_prev_yr & year == 7 & row_per_child_year == 1]$urn_for_ks4 <-
  cohort1[!is.na(urn_prev_yr) & urn != urn_prev_yr & year == 7 & row_per_child_year == 1]$urn_prev_yr

# 2010/11 ks4 data (i.e. the year before y7 for cohort1)
ks4_c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4CandInd_2011_Census.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

names(ks4_c1) <- tolower(gsub("KS4_", "", names(ks4_c1)))

ks4_c1 <- ks4_c1[urn %in% cohort1[year == 7 & row_per_child_year == 1]$urn_for_ks4]

length(unique(ks4_c1$urn))
length(unique(cohort1[year == 7 & row_per_child_year == 1]$urn))

# deduplicate
length(unique(ks4_c1$pupilmatchingrefanonymous))
ks4_c1 <- ks4_c1[order(pupilmatchingrefanonymous)]
ks4_c1[, n_pmr := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
table(ks4_c1$n_pmr)
ks4_c1 <- ks4_c1[n_pmr == 1]
ks4_c1$n_pmr <- NULL
length(unique(ks4_c1$pupilmatchingrefanonymous))

# assign counter per school
ks4_c1 <- ks4_c1[order(urn)]
ks4_c1[, n_urn := seq_len(.N), by = rleid(urn)]

# get measures per school
# prop 5 A* - C
ks4_c1[, prop_level2 := mean(level2), by = "urn"]

# newer_type
# 1 Comprehensive
# 2 Selective
# 3 Modern
# 4 Not ADMPOL (admissions policy) (???)
# 5 Maintained special
# 6 Hospitals and PRUs
# 7 Non-maintained/independent special schools
# 8 Independent (not in dataset)

# hist(ks4_c1[n_urn == 1 & newer_type %in% c(1:3)]$prop_level2, breaks = 100)

temp_df <- data.table(
  urn = ks4_c1[n_urn == 1]$urn,
  prop_level2_y6 = ks4_c1[n_urn == 1]$prop_level2
)

cohort1 <- merge(cohort1,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child_year)]

cohort1[, prop_level2_y6 := prop_level2_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort1[year == 7 & row_per_child_year == 1]$prop_level2_y6))

# prop 5 A* - C with English and Maths
ks4_c1[, prop_level2em := mean(level2_em), by = "urn"]

temp_df <- data.table(
  urn = ks4_c1[n_urn == 1]$urn,
  prop_level2em_y6 = ks4_c1[n_urn == 1]$prop_level2em
)

cohort1 <- merge(cohort1,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort1[, prop_level2em_y6 := prop_level2em_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort1[year == 7 & row_per_child_year == 1]$prop_level2em_y6))

# EBacc
table(ks4_c1$ebacc)

ks4_c1[, prop_ebacc := mean(ebacc), by = "urn"]

temp_df <- data.table(
  urn = ks4_c1[n_urn == 1]$urn,
  prop_ebacc_y6 = ks4_c1[n_urn == 1]$prop_ebacc
)

cohort1 <- merge(cohort1,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year)]
cohort1[, prop_ebacc_y6 := prop_ebacc_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort1[year == 7 & row_per_child_year == 1]$prop_ebacc_y6))

rm(temp_df, sdb_c1)

# ** cohort 2 -------------------------------------------------------------

sdb_c2 <- sdb[, c("urn_2012", "urn_2013")]
sdb_c2 <- sdb_c2[!is.na(urn_2013)]
sdb_c2 <- sdb_c2[!duplicated(sdb_c2)]

cohort2[year == 7 & row_per_child_year == 1, urn_prev_yr := get_prev_urn(urn,
                                                                         sdb_c2$urn_2013,
                                                                         sdb_c2$urn_2012),
        by = seq_len(nrow(cohort2[year == 7 & row_per_child_year == 1]))]

cohort2$urn_for_ks4 <- cohort2$urn
cohort2[year != 7 | row_per_child_year != 1]$urn_for_ks4 <- NA
cohort2[!is.na(urn_prev_yr) & urn != urn_prev_yr & year == 7 & row_per_child_year == 1]$urn_for_ks4 <-
  cohort2[!is.na(urn_prev_yr) & urn != urn_prev_yr & year == 7 & row_per_child_year == 1]$urn_prev_yr

# 2011/12 ks4 data (i.e. the year before y7 for cohort2)
ks4_c2 <- data.table(read.table(
  "P:/Working/WORKING DATA/KS4/KS4CandInd_2012_Census.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F,
  skipNul = T
))

names(ks4_c2) <- tolower(gsub("KS4_", "", names(ks4_c2)))

ks4_c2 <- ks4_c2[urn %in% cohort2[year == 7 & row_per_child_year == 1]$urn_for_ks4]

length(unique(ks4_c2$urn))
length(unique(cohort2[year == 7 & row_per_child_year == 1]$urn))

# deduplicate
length(unique(ks4_c2$pupilmatchingrefanonymous))
ks4_c2 <- ks4_c2[order(pupilmatchingrefanonymous)]
ks4_c2[, n_pmr := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
table(ks4_c2$n_pmr)
ks4_c2 <- ks4_c2[n_pmr == 1]
ks4_c2$n_pmr <- NULL
length(unique(ks4_c2$pupilmatchingrefanonymous))

# assign counter per school
ks4_c2 <- ks4_c2[order(urn)]
ks4_c2[, n_urn := seq_len(.N), by = rleid(urn)]

# get measures per school
# prop 5 A* - C
ks4_c2[, prop_level2 := mean(level2), by = "urn"]

# newer_type
# 1 Comprehensive
# 2 Selective
# 3 Modern
# 4 Not ADMPOL (admissions policy) (???)
# 5 Maintained special
# 6 Hospitals and PRUs
# 7 Non-maintained/independent special schools
# 8 Independent (not in dataset)

temp_df <- data.table(
  urn = ks4_c2[n_urn == 1]$urn,
  prop_level2_y6 = ks4_c2[n_urn == 1]$prop_level2
)

cohort2 <- merge(cohort2,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, row_per_child_year)]

cohort2[, prop_level2_y6 := prop_level2_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort2[year == 7 & row_per_child_year == 1]$prop_level2_y6))

# prop 5 A* - C with English and Maths
ks4_c2[, prop_level2em := mean(level2_em), by = "urn"]

temp_df <- data.table(
  urn = ks4_c2[n_urn == 1]$urn,
  prop_level2em_y6 = ks4_c2[n_urn == 1]$prop_level2em
)

cohort2 <- merge(cohort2,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort2[, prop_level2em_y6 := prop_level2em_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort2[year == 7 & row_per_child_year == 1]$prop_level2em_y6))

# EBacc
table(ks4_c2$ebacc)

ks4_c2[, prop_ebacc := mean(ebacc), by = "urn"]

temp_df <- data.table(
  urn = ks4_c2[n_urn == 1]$urn,
  prop_ebacc_y6 = ks4_c2[n_urn == 1]$prop_ebacc
)

cohort2 <- merge(cohort2,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year)]
cohort2[, prop_ebacc_y6 := prop_ebacc_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort2[year == 7 & row_per_child_year == 1]$prop_ebacc_y6))

rm(temp_df, sdb, sdb_c2, get_prev_urn)

# * BEST 8 VA --------------------------------------------------------

# can already get value added score
# ptscnewe - capped points score GCSE and eq
# b8scrplusbonus - final capped best 8 to be used in VA (double counts maths and eng)
# b8vascr - pupil's value added score - prior attainment controls

# ** cohort 1 -------------------------------------------------------------

ks4_c1[, mean_b8vascr := mean(b8vascr), by = "urn"]

temp_df <- data.table(
  urn = ks4_c1[n_urn == 1]$urn,
  mean_b8vascr_y6 = ks4_c1[n_urn == 1]$mean_b8vascr
)

cohort1 <- merge(cohort1,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort1[, mean_b8vascr_y6 := mean_b8vascr_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort1[year == 7 & row_per_child_year == 1]$mean_b8vascr_y6))

rm(temp_df)

# ** cohort 2 -------------------------------------------------------------

ks4_c2[, mean_b8vascr := mean(b8vascr), by = "urn"]

temp_df <- data.table(
  urn = ks4_c2[n_urn == 1]$urn,
  mean_b8vascr_y6 = ks4_c2[n_urn == 1]$mean_b8vascr
)

cohort2 <- merge(cohort2,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort2[, mean_b8vascr_y6 := mean_b8vascr_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort2[year == 7 & row_per_child_year == 1]$mean_b8vascr_y6))

rm(temp_df)

# * CVA ---------------------------------------------------------------------

# need care and age
source("SCRIPTS/06a CLA CLEANING.R")
ks4_c1$care_before_gcse <- ifelse(ks4_c1$pupilmatchingrefanonymous %in%
                                    cla_episodes[dateepisodestarted1 < as.Date("2011-06-01")]$pmr2,
                               T, F)
ks4_c2$care_before_gcse <- ifelse(ks4_c2$pupilmatchingrefanonymous %in%
                                    cla_episodes[dateepisodestarted1 < as.Date("2012-06-01")]$pmr2,
                                  T, F)
rm(cla_episodes)

ks4_c1$ageatstartofacademicyear <- 2010 - ks4_c1$yearofbirth - (ks4_c1$monthofbirth >= 9)
ks4_c2$ageatstartofacademicyear <- 2011 - ks4_c2$yearofbirth - (ks4_c2$monthofbirth >= 9)

# ** cohort 1 -------------------------------------------------------------

# covars
full1011 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2011.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
names(full1011) <- tolower(gsub("_S.*", "", names(full1011)))
full1011 <- full1011[pupilmatchingrefanonymous %in% ks4_c1$pupilmatchingrefanonymous]
full1011 <- full1011[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]
full1011 <- full1011[!duplicated(pupilmatchingrefanonymous)]

# ks2
summary(ks4_c1$vap2taaps)
summary(ks4_c1$vap2taeng)
summary(ks4_c1$vap2tamat)

# gender
table(ks4_c1$gender, useNA = "always")

# sen
table(ks4_c1$senprovision_spr11, useNA = "always")
ks4_c1[senprovision_spr11 == ""]$senprovision_spr11 <- "N"

# ethnicity
table(ks4_c1$ethnicgroupminor_spr11, useNA = "always")

temp_df <- data.table(
  pupilmatchingrefanonymous = full1011$pupilmatchingrefanonymous,
  eth_census = full1011$ethnicgroupminor
)

ks4_c1 <- merge(ks4_c1, temp_df, all.x = T)

table(ks4_c1[ethnicgroupminor_spr11 %in% c("", "NOBT", "REFU")]$eth_census)

ks4_c1[ethnicgroupminor_spr11 %in% c("", "NOBT", "REFU")]$ethnicgroupminor_spr11 <-
  ks4_c1[ethnicgroupminor_spr11 %in% c("", "NOBT", "REFU")]$eth_census
ks4_c1[ethnicgroupminor_spr11 %in% c("", "NOBT", "REFU") |
      is.na(ethnicgroupminor_spr11)]$ethnicgroupminor_spr11 <- "UNCLA"

rm(temp_df)
ks4_c1$eth_census <- NULL

# fsm
table(ks4_c1$fsmeligible_spr11, useNA = "always")
ks4_c1[is.na(ks4_c1$fsmeligible_spr11)]$fsmeligible_spr11 <- 0

# first lang
table(ks4_c1$languagegroupmajor_spr11, useNA = "always")
ks4_c1[languagegroupmajor_spr11 == ""]$languagegroupmajor_spr11 <- "1_ENG"
ks4_c1$firstlang <- ks4_c1$languagegroupmajor_spr11
ks4_c1[languagegroupmajor_spr11 == "1_ENG"]$firstlang <- "eng"
ks4_c1[languagegroupmajor_spr11 == "2_OTH"]$firstlang <- "oth"
ks4_c1[languagegroupmajor_spr11 == "3_UNCL"]$firstlang <- "eng"
table(ks4_c1$firstlang, useNA = "always")

# joined in year 10/11
table(ks4_c1$mob1, useNA = "always")

# joined in month other than jul aug sep
table(ks4_c1$mob2, useNA = "always")

# age
table(is.na(ks4_c1$ageatstartofacademicyear))

# in care
table(ks4_c1$care_before_gcse, useNA = "always")

# idaci
table(is.na(ks4_c1$idacirank_spr11))
ks4_c1[, mean_idaci_urn := mean(idacirank_spr11, na.rm = T), by = "urn"]
ks4_c1$idaci_rank_imputed <- as.numeric(ks4_c1$idacirank_spr11)
ks4_c1[is.na(idaci_rank_imputed)]$idaci_rank_imputed <- ks4_c1[is.na(idaci_rank_imputed)]$mean_idaci_urn
ks4_c1[is.na(idaci_rank_imputed)]$idaci_rank_imputed <- 0.14 # per DfE guide

rm(full1011)

# get whether special school
dfe <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/DfE.csv", stringsAsFactors = F))
names(dfe) <- tolower(gsub("name", "", names(dfe)))

dfe <- dfe[urn %in% ks4_c1$urn]
table(ks4_c1$urn %in% dfe$urn)

names(dfe)[names(dfe) == "closedate"] <- "schoolclosedate"
names(dfe)[names(dfe) == "gender"] <- "schoolgender"

dfe[schoolclosedate == ""]$schoolclosedate <- "01/01/1800"
dfe$schoolclosedate <- as.Date(dfe$schoolclosedate, format = "%d/%m/%Y")

ks4_c1 <- merge(ks4_c1, dfe[, c("urn", "typeofestablishment", "establishmenttypegroup", "phaseofeducation",
                          "schoolgender", "boarders", "nurseryprovision", "officialsixthform",
                          "religiouscharacter", "religiousethos", "admissionspolicy",
                          "schoolcapacity", "specialclasses", "schoolclosedate", "reasonestablishmentclosed")],
             all.x = T,
             by = "urn")
rm(dfe)

# get school level prior attainment
ks4_c1[, mean_urn_ks2 := mean(vap2taaps), by = "urn"]

ks4_c1$sqd_diffs_urn_ks2 <- (ks4_c1$mean_urn_ks2 - ks4_c1$vap2taaps)^2
ks4_c1[, sd_urn_ks2 := sqrt(mean(sqd_diffs_urn_ks2)), by = "urn"]

#ks2 average - vap2taaps
# non-special schools
reg_data_non_spec <- ks4_c1[establishmenttypegroup != "Special schools", c("pupilmatchingrefanonymous",
                                                                        "b8scrplusbonus",
                                                                        "vap2taaps",
                                                                        "vap2taeng",
                                                                        "vap2tamat",
                                                                        "gender",
                                                                        "senprovision_spr11",
                                                                        "ethnicgroupminor_spr11",
                                                                        "fsmeligible_spr11",
                                                                        "firstlang",
                                                                        "mob1",
                                                                        "mob2",
                                                                        "ageatstartofacademicyear",
                                                                        "care_before_gcse",
                                                                        "idaci_rank_imputed",
                                                                        "mean_urn_ks2",
                                                                        "sd_urn_ks2",
                                                                        "urn")]

mod_cohort1 <- lmer(b8scrplusbonus ~ vap2taaps + I(vap2taaps^2) +
                      I(vap2taeng - vap2taaps) + I(vap2tamat - vap2taaps) +
                      gender + senprovision_spr11 + ethnicgroupminor_spr11 +
                      fsmeligible_spr11 + firstlang + as.factor(mob1) + as.factor(mob2) +
                      ageatstartofacademicyear + as.factor(care_before_gcse) +
                      I(idaci_rank_imputed / 10000) +
                      fsmeligible_spr11 * ethnicgroupminor_spr11 +
                      firstlang * vap2taaps +
                      firstlang * I(vap2taaps^2) +
                      + mean_urn_ks2 + sd_urn_ks2 +
                      (1 | urn),
                    data = reg_data_non_spec)

reg_data_non_spec$pred_score_non_spec <- predict(mod_cohort1)

ks4_c1 <- merge(ks4_c1,
             reg_data_non_spec[, c("pupilmatchingrefanonymous", "pred_score_non_spec")],
             by = "pupilmatchingrefanonymous",
             all.x = T)

rm(mod_cohort1, reg_data_non_spec)

# special schools
reg_data_spec <- ks4_c1[establishmenttypegroup == "Special schools", c("pupilmatchingrefanonymous",
                                                                       "b8scrplusbonus",
                                                                       "vap2taaps",
                                                                       "vap2taeng",
                                                                       "vap2tamat",
                                                                       "gender",
                                                                       "senprovision_spr11",
                                                                       "ethnicgroupminor_spr11",
                                                                       "fsmeligible_spr11",
                                                                       "firstlang",
                                                                       "mob1",
                                                                       "mob2",
                                                                       "ageatstartofacademicyear",
                                                                       "care_before_gcse",
                                                                       "idaci_rank_imputed",
                                                                       "urn")]


mod_cohort1 <- lmer(b8scrplusbonus ~ vap2taaps + I(vap2taaps^2) +
                      I(vap2taeng - vap2taaps) + I(vap2tamat - vap2taaps) +
                      gender + senprovision_spr11 + ethnicgroupminor_spr11 +
                      fsmeligible_spr11 + firstlang + as.factor(mob1) + as.factor(mob2) +
                      ageatstartofacademicyear + as.factor(care_before_gcse) +
                      I(idaci_rank_imputed / 10000) +
                      (1 | urn),
                    data = reg_data_spec)

reg_data_spec$pred_score_spec <- predict(mod_cohort1)

ks4_c1 <- merge(ks4_c1,
             reg_data_spec[, c("pupilmatchingrefanonymous", "pred_score_spec")],
             by = "pupilmatchingrefanonymous",
             all.x = T)

rm(mod_cohort1, reg_data_spec)

ks4_c1$pred_score <- ks4_c1$pred_score_non_spec
ks4_c1[is.na(ks4_c1$pred_score)]$pred_score <- ks4_c1[is.na(ks4_c1$pred_score)]$pred_score_spec
table(is.na(ks4_c1$pred_score))

ks4_c1$cva <- ks4_c1$b8scrplusbonus - ks4_c1$pred_score

View(ks4_c1[, c("pupilmatchingrefanonymous", "pred_score", "b8scrplusbonus", "cva")])

# need to add shrinkage factor to convert to school level measure - see spreadsheet
ks4_c1[, urn_mean_cva := mean(cva), by = "urn"]
ks4_c1[, row_per_urn := seq_len(.N), by = "urn"]
ks4_c1[, n_pupils := max(seq_len(.N)), by = "urn"]

sf <- read.csv("P:/Working/WORKING DATA/SCHOOLS/cva_shrinkage_factors.csv", header = T)
ks4_c1 <- merge(ks4_c1,
             sf,
             by = "n_pupils",
             all.x = T)

summary(ks4_c1[is.na(s_factor)]$n_pupils)
ks4_c1[is.na(s_factor)]$s_factor <- max(sf$s_factor)

ks4_c1$urn_mean_cva_shrunk <- as.numeric(NA)
ks4_c1[establishmenttypegroup != "Special schools"]$urn_mean_cva_shrunk <-
  ks4_c1[establishmenttypegroup != "Special schools"]$urn_mean_cva * ks4_c1[establishmenttypegroup != "Special schools"]$s_factor

# need to do separately for special schools
sf <- read.csv("P:/Working/WORKING DATA/SCHOOLS/cva_shrinkage_factors_special_schools.csv", header = T)
ks4_c1 <- merge(ks4_c1,
                sf,
                by = "n_pupils",
                all.x = T)

summary(ks4_c1[is.na(s_factor_spec)]$n_pupils)
ks4_c1[is.na(s_factor_spec)]$s_factor_spec <- max(sf$s_factor_spec)

ks4_c1[establishmenttypegroup == "Special schools"]$urn_mean_cva_shrunk <-
  ks4_c1[establishmenttypegroup == "Special schools"]$urn_mean_cva * ks4_c1[establishmenttypegroup == "Special schools"]$s_factor_spec

table(is.na(ks4_c1$urn_mean_cva_shrunk))
summary(ks4_c1$urn_mean_cva_shrunk)

# merge with cohort1 dataset
temp_df <- data.table(
  urn = ks4_c1[n_urn == 1]$urn,
  urn_mean_cva_shrunk_y6 = ks4_c1[n_urn == 1]$urn_mean_cva_shrunk
)

cohort1 <- merge(cohort1,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort1[, urn_mean_cva_shrunk_y6 := urn_mean_cva_shrunk_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort1[year == 7 & row_per_child_year == 1]$urn_mean_cva_shrunk_y6))
summary(cohort1[year == 7 & row_per_child_year == 1]$urn_mean_cva_shrunk_y6)
hist(cohort1[year == 7 & row_per_child_year == 1]$urn_mean_cva_shrunk_y6, breaks = 100)
hist(cohort1[year == 7 & row_per_child_year == 1 &
               urn_mean_cva_shrunk_y6 > -10 & urn_mean_cva_shrunk_y6 < 10]$urn_mean_cva_shrunk_y6, breaks = 100)

rm(temp_df, sf, ks4_c1); gc()

# ** cohort 2 -------------------------------------------------------------

# covars
full1112 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
names(full1112) <- tolower(gsub("_S.*", "", names(full1112)))
full1112 <- full1112[pupilmatchingrefanonymous %in% ks4_c2$pupilmatchingrefanonymous]
full1112 <- full1112[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]
full1112 <- full1112[!duplicated(pupilmatchingrefanonymous)]

# gender
table(ks4_c2$gender, useNA = "always")

# sen
table(ks4_c2$senprovision_spr12, useNA = "always")
ks4_c2[senprovision_spr12 == ""]$senprovision_spr12 <- "N"

# ethnicity
table(ks4_c2$ethnicgroupminor_spr12, useNA = "always")

temp_df <- data.table(
  pupilmatchingrefanonymous = full1112$pupilmatchingrefanonymous,
  eth_census = full1112$ethnicgroupminor
)

ks4_c2 <- merge(ks4_c2, temp_df, all.x = T)

table(ks4_c2[ethnicgroupminor_spr12 %in% c("", "NOBT", "REFU")]$eth_census)

ks4_c2[ethnicgroupminor_spr12 %in% c("", "NOBT", "REFU")]$ethnicgroupminor_spr12 <-
  ks4_c2[ethnicgroupminor_spr12 %in% c("", "NOBT", "REFU")]$eth_census
ks4_c2[ethnicgroupminor_spr12 %in% c("", "NOBT", "REFU") |
         is.na(ethnicgroupminor_spr12)]$ethnicgroupminor_spr12 <- "UNCLA"

rm(temp_df)
ks4_c2$eth_census <- NULL

# fsm
table(ks4_c2$fsmeligible_spr12, useNA = "always")
ks4_c2[is.na(ks4_c2$fsmeligible_spr12)]$fsmeligible_spr12 <- 0

# first lang
table(ks4_c2$languagegroupmajor_spr12, useNA = "always")
ks4_c2[languagegroupmajor_spr12 == ""]$languagegroupmajor_spr12 <- "1_ENG"
ks4_c2$firstlang <- ks4_c2$languagegroupmajor_spr12
ks4_c2[languagegroupmajor_spr12 == "1_ENG"]$firstlang <- "eng"
ks4_c2[languagegroupmajor_spr12 == "2_OTH"]$firstlang <- "oth"
ks4_c2[languagegroupmajor_spr12 == "3_UNCL"]$firstlang <- "eng"
table(ks4_c2$firstlang, useNA = "always")

# mob
table(ks4_c2$mob2, useNA = "always")

# age
table(is.na(ks4_c2$ageatstartofacademicyear))

# in care
table(ks4_c2$care_before_gcse, useNA = "always")

# idaci
table(is.na(ks4_c2$idacirank_spr12))
ks4_c2[, mean_idaci_urn := mean(idacirank_spr12, na.rm = T), by = "urn"]
ks4_c2$idaci_rank_imputed <- as.numeric(ks4_c2$idacirank_spr12)
ks4_c2[is.na(idaci_rank_imputed)]$idaci_rank_imputed <- ks4_c2[is.na(idaci_rank_imputed)]$mean_idaci_urn
ks4_c2[is.na(idaci_rank_imputed)]$idaci_rank_imputed <- 0.14 # per DfE guide

rm(full1112)

# get whether special school
dfe <- data.table(read.csv("P:/Working/WORKING DATA/SCHOOLS/DfE.csv", stringsAsFactors = F))
names(dfe) <- tolower(gsub("name", "", names(dfe)))

dfe <- dfe[urn %in% ks4_c2$urn]
table(ks4_c2$urn %in% dfe$urn) # all are

names(dfe)[names(dfe) == "closedate"] <- "schoolclosedate"
names(dfe)[names(dfe) == "gender"] <- "schoolgender"

dfe[schoolclosedate == ""]$schoolclosedate <- "01/01/1800"
dfe$schoolclosedate <- as.Date(dfe$schoolclosedate, format = "%d/%m/%Y")

ks4_c2 <- merge(ks4_c2, dfe[, c("urn", "typeofestablishment", "establishmenttypegroup", "phaseofeducation",
                                "schoolgender", "boarders", "nurseryprovision", "officialsixthform",
                                "religiouscharacter", "religiousethos", "admissionspolicy",
                                "schoolcapacity", "specialclasses", "schoolclosedate", "reasonestablishmentclosed")],
                all.x = T,
                by = "urn")
rm(dfe)

# get school level prior attainment
ks4_c2[, mean_urn_ks2 := mean(vap2taaps), by = "urn"]

ks4_c2$sqd_diffs_urn_ks2 <- (ks4_c2$mean_urn_ks2 - ks4_c2$vap2taaps)^2
ks4_c2[, sd_urn_ks2 := sqrt(mean(sqd_diffs_urn_ks2)), by = "urn"]

#ks2 average - vap2taaps
# non-special schools
reg_data_non_spec <- ks4_c2[establishmenttypegroup != "Special schools", c("pupilmatchingrefanonymous",
                                                                           "b8scrplusbonus",
                                                                           "vap2taaps",
                                                                           "vap2taeng",
                                                                           "vap2tamat",
                                                                           "gender",
                                                                           "senprovision_spr12",
                                                                           "ethnicgroupminor_spr12",
                                                                           "fsmeligible_spr12",
                                                                           "firstlang",
                                                                           "mob1",
                                                                           "mob2",
                                                                           "ageatstartofacademicyear",
                                                                           "care_before_gcse",
                                                                           "idaci_rank_imputed",
                                                                           "mean_urn_ks2",
                                                                           "sd_urn_ks2",
                                                                           "urn")]

mod_cohort2 <- lmer(b8scrplusbonus ~ vap2taaps + I(vap2taaps^2) +
                      I(vap2taeng - vap2taaps) + I(vap2tamat - vap2taaps) +
                      gender + senprovision_spr12 + ethnicgroupminor_spr12 +
                      fsmeligible_spr12 + firstlang + as.factor(mob1) + as.factor(mob2) +
                      ageatstartofacademicyear + as.factor(care_before_gcse) +
                      I(idaci_rank_imputed / 10000) +
                      fsmeligible_spr12 * ethnicgroupminor_spr12 +
                      firstlang * vap2taaps +
                      firstlang * I(vap2taaps^2) +
                      + mean_urn_ks2 + sd_urn_ks2 +
                      (1 | urn),
                    data = reg_data_non_spec)

reg_data_non_spec$pred_score_non_spec <- predict(mod_cohort2)

ks4_c2 <- merge(ks4_c2,
                reg_data_non_spec[, c("pupilmatchingrefanonymous", "pred_score_non_spec")],
                by = "pupilmatchingrefanonymous",
                all.x = T)

rm(mod_cohort2, reg_data_non_spec)

# special schools
reg_data_spec <- ks4_c2[establishmenttypegroup == "Special schools", c("pupilmatchingrefanonymous",
                                                                       "b8scrplusbonus",
                                                                       "vap2taaps",
                                                                       "vap2taeng",
                                                                       "vap2tamat",
                                                                       "gender",
                                                                       "senprovision_spr12",
                                                                       "ethnicgroupminor_spr12",
                                                                       "fsmeligible_spr12",
                                                                       "firstlang",
                                                                       "mob1",
                                                                       "mob2",
                                                                       "ageatstartofacademicyear",
                                                                       "care_before_gcse",
                                                                       "idaci_rank_imputed",
                                                                       "urn")]


mod_cohort2 <- lmer(b8scrplusbonus ~ vap2taaps + I(vap2taaps^2) +
                      I(vap2taeng - vap2taaps) + I(vap2tamat - vap2taaps) +
                      gender + senprovision_spr12 + ethnicgroupminor_spr12 +
                      fsmeligible_spr12 + firstlang + as.factor(mob1) + as.factor(mob2) +
                      ageatstartofacademicyear + as.factor(care_before_gcse) +
                      I(idaci_rank_imputed / 10000) +
                      (1 | urn),
                    data = reg_data_spec)

reg_data_spec$pred_score_spec <- predict(mod_cohort2)

ks4_c2 <- merge(ks4_c2,
                reg_data_spec[, c("pupilmatchingrefanonymous", "pred_score_spec")],
                by = "pupilmatchingrefanonymous",
                all.x = T)

rm(mod_cohort2, reg_data_spec)

ks4_c2$pred_score <- ks4_c2$pred_score_non_spec
ks4_c2[is.na(ks4_c2$pred_score)]$pred_score <- ks4_c2[is.na(ks4_c2$pred_score)]$pred_score_spec
table(is.na(ks4_c2$pred_score))

ks4_c2$cva <- ks4_c2$b8scrplusbonus - ks4_c2$pred_score

# need to add shrinkage factor to convert to school level measure - see spreadsheet
ks4_c2[, urn_mean_cva := mean(cva), by = "urn"]
ks4_c2[, row_per_urn := seq_len(.N), by = "urn"]
ks4_c2[, n_pupils := max(seq_len(.N)), by = "urn"]

sf <- read.csv("P:/Working/WORKING DATA/SCHOOLS/cva_shrinkage_factors.csv", header = T)
ks4_c2 <- merge(ks4_c2,
                sf,
                by = "n_pupils",
                all.x = T)

summary(ks4_c2[is.na(s_factor)]$n_pupils)
ks4_c2[is.na(s_factor)]$s_factor <- max(sf$s_factor)

ks4_c2$urn_mean_cva_shrunk <- as.numeric(NA)
ks4_c2[establishmenttypegroup != "Special schools"]$urn_mean_cva_shrunk <-
  ks4_c2[establishmenttypegroup != "Special schools"]$urn_mean_cva * ks4_c2[establishmenttypegroup != "Special schools"]$s_factor

# need to do separately for special schools
sf <- read.csv("P:/Working/WORKING DATA/SCHOOLS/cva_shrinkage_factors_special_schools.csv", header = T)
ks4_c2 <- merge(ks4_c2,
                sf,
                by = "n_pupils",
                all.x = T)

summary(ks4_c2[is.na(s_factor_spec)]$n_pupils)
ks4_c2[is.na(s_factor_spec)]$s_factor_spec <- max(sf$s_factor_spec)

ks4_c2[establishmenttypegroup == "Special schools"]$urn_mean_cva_shrunk <-
  ks4_c2[establishmenttypegroup == "Special schools"]$urn_mean_cva * ks4_c2[establishmenttypegroup == "Special schools"]$s_factor_spec

table(is.na(ks4_c2$urn_mean_cva_shrunk))
summary(ks4_c2$urn_mean_cva_shrunk)

# merge with cohort2 dataset
temp_df <- data.table(
  urn = ks4_c2[n_urn == 1]$urn,
  urn_mean_cva_shrunk_y6 = ks4_c2[n_urn == 1]$urn_mean_cva_shrunk
)

cohort2 <- merge(cohort2,
                 temp_df,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)

cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, row_per_child_year)]
cohort2[, urn_mean_cva_shrunk_y6 := urn_mean_cva_shrunk_y6[year == 7 & row_per_child_year == 1], by = "pupilmatchingrefanonymous"]

mean(!is.na(cohort2[year == 7 & row_per_child_year == 1]$urn_mean_cva_shrunk_y6))

rm(temp_df, sf, ks4_c2)

# * SCHOOL COMPOSITION ----------------------------------------------------

# c1 get 2010/11 spring census
c1 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2011.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(c1) <- tolower(gsub("_S.*", "", names(c1)))
c1 <- c1[urn %in% cohort1$urn_for_ks4]
c1 <- c1[, c("urn", "fsmeligible")]
table(is.na(c1$fsmeligible)) 
#c1[is.na(fsmeligible), fsmeligible := 0]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))
pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2011-01-20")]
pru <- pru[urn %in% cohort1$urn_for_ks4]
pru <- pru[, c("urn", "fsmeligible")]
table(is.na(pru$fsmeligible)) 
c1 <- rbind(c1, pru)
rm(pru)

c1[, prop_fsm_year_before := mean(fsmeligible), by = .(urn)]
c1 <- c1[, c("urn", "prop_fsm_year_before")]
c1 <- c1[!duplicated(c1)]

cohort1 <- merge(cohort1,
                 c1,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)
cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, row_per_child_year)]
rm(c1)
table(is.na(cohort1[year == 7 & sch_n_in_year == 1]$prop_fsm_year_before),
      cohort1[year == 7 & sch_n_in_year == 1]$school_legal_status)

# c2 get 2011/12 spring census
c2 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(c2) <- tolower(gsub("_S.*", "", names(c2)))
c2 <- c2[urn %in% cohort2$urn_for_ks4]
c2 <- c2[, c("urn", "fsmeligible")]
table(is.na(c2$fsmeligible))
#c2[is.na(fsmeligible), fsmeligible := 0]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))
pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2012-01-19")]
pru <- pru[urn %in% cohort2$urn_for_ks4]
pru <- pru[, c("urn", "fsmeligible")]
table(is.na(pru$fsmeligible))
c2 <- rbind(c2, pru)
rm(pru)

c2[, prop_fsm_year_before := mean(fsmeligible), by = .(urn)]
c2 <- c2[, c("urn", "prop_fsm_year_before")]
c2 <- c2[!duplicated(c2)]

cohort2 <- merge(cohort2,
                 c2,
                 by.x = "urn_for_ks4",
                 by.y = "urn",
                 all.x = T)
cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, row_per_child_year)]
rm(c2)
table(is.na(cohort2[year == 7 & sch_n_in_year == 1]$prop_fsm_year_before),
      cohort2[year == 7 & sch_n_in_year == 1]$school_legal_status)

# SAVE --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1_npd_clean_sch_ad_qual.rda")
save(cohort2, file = "PROCESSED DATA/cohort2_npd_clean_sch_ad_qual.rda")
rm(list = ls()); gc()
