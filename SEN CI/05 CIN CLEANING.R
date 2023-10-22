
setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

# Step 1: Load data --------------------------------------------------------------------

cin <- data.table(read.table("P:/Working/WORKING DATA/CIN_2009_2017.txt",
                             header = T,
                             sep = "\t",
                             skipNul = T,
                             stringsAsFactors = F,
                             na.strings = ""
))

cin[is.na(CIN_LA), CIN_LA := CIN_CIN_LA]
cin[, CIN_CIN_LA := NULL]

names(cin) <- tolower(gsub("CIN_", "", names(cin)))

length(unique(cin$pupilmatchingrefanonymous))

mode.fun <- function(v) {
  v <- v[!is.na(v)]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  return(md[length(md)]) # is either modal or last if multimodal
}

# Step 2: Subset to NPD group -----------------------------------------------------

load("PROCESSED DATA/SEN CI/cohort2_npd_clean_sch.rda")

cin <- cin[pupilmatchingrefanonymous %in% cohort2$pupilmatchingrefanonymous]

cin$cohort <- 2 # hangover from previous verion of code - easier to retain this as constant than edit subsequent code

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

rm(cohort2)

cin <- cin[order(pupilmatchingrefanonymous, referraldate)]

# Misc --------------------------------------------------------------------

cin$female <- cin$gender - 1
cin[female %in% c(-1, 8), female := NA]
cin[, gender := NULL]

white <- c("WBRI", "WIRI", "WOTH")
mixed <- c("MWBC", "MWBA", "MWAS", "MOTH")
asian <- c("AIND", "APKN", "ABAN", "AOTH", "CHNE")
black <- c("BCRB", "BAFR", "BOTH")
trave <- c("WIRT", "WROM")

cin$eth_grp <- factor(NA, levels = c("White", "Mixed", "Asian (inc Cn)",
                                     "Black", "Traveller/Roma", "Other"))

cin[ethnicgroupminor %in% white, eth_grp := "White"]
cin[ethnicgroupminor %in% mixed, eth_grp := "Mixed"]
cin[ethnicgroupminor %in% asian, eth_grp := "Asian (inc Cn)"]
cin[ethnicgroupminor %in% black, eth_grp := "Black"]
cin[ethnicgroupminor %in% trave, eth_grp := "Traveller/Roma"]
cin[ethnicgroupminor == "OOTH", eth_grp := "Other"]

rm(white, mixed, asian, black, trave)
cin$eth_grp <- factor(cin$eth_grp)

cin[, ethnicgroupminor := NULL]
cin[, ethnicgroupmajor := NULL]
cin[, ethnicity := NULL]

cin$referraldate <- as.Date(cin$referraldate, format = "%Y-%m-%d")
cin[referraldate %in% c(as.Date("1800-01-01"), as.Date("1900-01-01"))]$referraldate <- NA

cin$cinclosuredate <- as.Date(cin$cinclosuredate, format = "%Y-%m-%d")
cin$cppstartdate <- as.Date(cin$cppstartdate, format = "%Y-%m-%d")
cin$dob <- as.Date(cin$dob, format = "%Y-%m-%d")
cin$aystart <- as.Date(paste0(as.integer(substr(cin$acadyr, 1, 4)), "-09-01"))

# Step 3: drop pre-2008 ------------------------------------------------------------

table(cin$cinclosuredate < as.Date("2008-04-01") & !is.na(cin$cinclosuredate))
cin <- cin[cinclosuredate >= as.Date("2008-04-01") | is.na(cinclosuredate)]

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 4: Check duplicated IDs ------------------------------------------------------

cin <- cin[order(pupilmatchingrefanonymous, aystart, referraldate, cinclosuredate)]

cin$child_id_int <- as.integer(as.factor(cin$lachildid_anon))

cin[!is.na(pupilmatchingrefanonymous), pmr.min := min(child_id_int), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous), pmr.max := max(child_id_int), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous), pmr.check := pmr.min != pmr.max]
table(cin$pmr.check,
      cin$cohort)

length(unique(cin[pmr.check == TRUE]$child_id_int))
length(unique(cin[pmr.check == TRUE]$pupilmatchingrefanonymous))

cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, dob.min := min(dob), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, dob.max := max(dob), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, dob.check := dob.min != dob.max]
table(cin$dob.check)

cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, fem.min := min(female), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, fem.max := max(female), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, fem.check := fem.min != fem.max]
table(cin$fem.check)

cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, eth.min := min(as.integer(as.factor(eth_grp))), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, eth.max := max(as.integer(as.factor(eth_grp))), by = "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous) & pmr.check == TRUE, eth.check := eth.min != eth.max]
table(cin$eth.check)

# if any two differ, assume really a different child
cin$dob_fem <- ifelse(cin$dob.check == T & cin$fem.check == T, 1, 0)
cin$dob_eth <- ifelse(cin$dob.check == T & cin$eth.check == T, 1, 0)
cin$fem_eth <- ifelse(cin$fem.check == T & cin$eth.check == T, 1, 0)
cin$all_con <- ifelse(cin$fem.check == T & cin$eth.check == T & cin$dob.check == T, 1, 0)

table(cin$dob_fem)
table(cin$dob_eth) 
table(cin$fem_eth)
table(cin$all_con)

cin[, pmr_flag_dob_fem := dob_fem == 1, by = pupilmatchingrefanonymous]
cin[, pmr_flag_dob_eth := dob_eth == 1, by = pupilmatchingrefanonymous]
cin[, pmr_flag_fem_eth := fem_eth == 1, by = pupilmatchingrefanonymous]
cin[, pmr_flag_all_con := all_con == 1, by = pupilmatchingrefanonymous]
cin[, any_flag := dob_fem == 1 | dob_eth == 1 | fem_eth == 1 | all_con == 1, by = pupilmatchingrefanonymous]

cin <- cin[any_flag == F | is.na(any_flag)]

# re-move check vars
cin[, pmr.min := NULL]
cin[, pmr.max := NULL]
cin[, pmr.check := NULL]

cin[, dob.min := NULL]
cin[, dob.max := NULL]
cin[, dob.check := NULL]

cin[, fem.min := NULL]
cin[, fem.max := NULL]
cin[, fem.check := NULL]

cin[, eth.min := NULL]
cin[, eth.max := NULL]
cin[, eth.check := NULL]

cin[, dob_fem := NULL]
cin[, dob_eth := NULL]
cin[, fem_eth := NULL]
cin[, all_con := NULL]

cin[, pmr_flag_dob_fem := NULL]
cin[, pmr_flag_dob_eth := NULL]
cin[, pmr_flag_fem_eth := NULL]
cin[, pmr_flag_all_con := NULL]
cin[, any_flag := NULL]

# remove redundant id vars
cin[, child_id_int := NULL]
cin[, lachildid_anon := NULL]

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 5: Deduplicate -------------------------------------------------------------

dups <- duplicated(cin[, c("pupilmatchingrefanonymous", "acadyr", "dob", "female", "disability",
                           "la", "referraldate", "cinclosuredate", "primaryneedcode",
                           "reasonforclosure", "referralsource", "eth_grp", "categoryofabuse",
                           "initialcategoryofabuse", "latestcategoryofabuse", "cppstartdate", "cppenddate")])
table(dups)
cin <- cin[!dups]
rm(dups)

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 6: Referral, closure and CPP start date missing ----------------------------

cin[cppenddate == as.Date("1900-01-01")]$cppenddate <- NA

# drop those with no dates at all
# nrow(cin[is.na(latestreferraldate) & is.na(latestclosuredate) & is.na(cppstartdate) & cohort == 2])
# 
# cin <- cin[!(is.na(latestreferraldate) & is.na(latestclosuredate) & is.na(cppstartdate))]
# 
# table(cin$cohort)
# aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })
# 
# # we can fill in some blank end dates easily where start date is not NA
cin$closuredate <- cin$cinclosuredate
cin[!is.na(cinclosuredate),
    closuredate := closuredate[which.max(closuredate)],
    by = .(pupilmatchingrefanonymous, referraldate)]
cin[, cinclosuredate := NULL]

# Step 7: Referral date alone is missing ----------------------------------

table(is.na(cin$referraldate))
cin <- cin[!is.na(referraldate)]

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 8: Validity of end dates --------------------------

# inconsitent end dates
cin[, check := any(length(unique(closuredate[which(!is.na(closuredate))])) > 1), by = .(pupilmatchingrefanonymous, referraldate)]
table(cin$check, cin$cohort)
cin[, check := NULL]

# fill in missing end dates where known from other episodes
cin[, closuredate_tmp := closuredate[which(!is.na(closuredate))][1], by = .(pupilmatchingrefanonymous, referraldate)]
cin[, closuredate := NULL]
names(cin)[names(cin) == "closuredate_tmp"] <- "closuredate"

# Step 9: Referral data > closure date --------------------------------------

table(cin$referraldate > cin$closuredate)

cin[referraldate > closuredate]$closuredate <- cin[referraldate > closuredate]$referraldate
table(cin$referraldate > cin$closuredate)

# Step 10: Dedulicate -------------------------------------------------------

dups <- duplicated(cin[, c("pupilmatchingrefanonymous", "acadyr", "dob", "female", "disability",
                           "la", "referraldate", "closuredate", "primaryneedcode",
                           "reasonforclosure", "referralsource", "eth_grp", "categoryofabuse",
                           "initialcategoryofabuse", "latestcategoryofabuse", "cppstartdate", "cppenddate")])
table(dups)
cin <- cin[!dups]
rm(dups)

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 11: remove enders pre Sept 2008 ---------------------------------------------

cin[, max_ref_end := max(closuredate), by = pupilmatchingrefanonymous]
table(cin$max_ref_end < as.Date("2008-09-01"))
cin <- cin[max_ref_end >= as.Date("2008-09-01") | is.na(max_ref_end)]

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# assign indices
cin <- cin[order(pupilmatchingrefanonymous, referraldate, closuredate, acadyr)]
cin[, row_per_child := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
cin[, row_per_child_ref := seq_len(.N), by = rleid(pupilmatchingrefanonymous, referraldate)]

# Step 12: Referral source inconsistent --------------------------------------

cin[referralsource %in% c("9", "10")]$referralsource <- NA
cin[, referralsource := mode.fun(referralsource), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 13: Referral NFA inconsistent --------------------------------------

cin[, referralnfa := mode.fun(referralnfa), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 14: NFA missing ----------------------------------------------------

cin[is.na(referralnfa)]$referralnfa <- 0

# Step 15: Primary need code inconsistent ---------------------------------

cin[primaryneedcode == "n1"]$primaryneedcode <- "N1"
cin[primaryneedcode %in% c("A2", "n2")]$primaryneedcode <- "N2" # the A codes are not actually now among this cohort
cin[primaryneedcode %in% c("A3", "n3")]$primaryneedcode <- "N3"
cin[primaryneedcode %in% c("A4", "n4")]$primaryneedcode <- "N4"
cin[primaryneedcode %in% c("A5", "n5")]$primaryneedcode <- "N5"
cin[primaryneedcode == "n6"]$primaryneedcode <- "N6"
cin[primaryneedcode == "n7"]$primaryneedcode <- "N7"
cin[primaryneedcode == "n8"]$primaryneedcode <- "N8"
cin[primaryneedcode == "n9"]$primaryneedcode <- "N9"
cin[primaryneedcode %in% c("n0", "N0", "NA")]$primaryneedcode <- NA

cin[, primaryneedcode := mode.fun(primaryneedcode), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 16: Closure reason inconsistent ------------------------------------

cin[reasonforclosure == "rc1"]$reasonforclosure <- "RC1"
cin[reasonforclosure == "rc2"]$reasonforclosure <- "RC2"
cin[reasonforclosure == "rc3"]$reasonforclosure <- "RC3"
cin[reasonforclosure == "rc4"]$reasonforclosure <- "RC4"
cin[reasonforclosure == "rc5"]$reasonforclosure <- "RC5"
cin[reasonforclosure == "rc6"]$reasonforclosure <- "RC6"
cin[reasonforclosure == "rc7"]$reasonforclosure <- "RC7"
cin[reasonforclosure == "rc8"]$reasonforclosure <- "RC8"
cin[!(substr(reasonforclosure, 1, 1) %in% c("R", "r"))]$reasonforclosure <- NA

cin[, reasonforclosure := mode.fun(reasonforclosure), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 17: Disability inconsistent ----------------------------------------

cin[, disability := mode.fun(disability), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 18: Ethnicity inconsistent -----------------------------------------

cin[, eth_grp := mode.fun(eth_grp), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 19: Gender inconsitent ---------------------------------------------

cin[, female := mode.fun(female), by = .(pupilmatchingrefanonymous, referraldate)]

# Step 20: End dates where referral NFA -----------------------------------

cin[is.na(closuredate) & referralnfa == 1]$closuredate <- cin[is.na(closuredate) & referralnfa == 1]$referraldate

# Step 21: fix DOB --------------------------------------------------------

cin[, expecteddob := NULL]
cin <- cin[!(dob > referraldate) | is.na(dob) | is.na(referraldate)]

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Step 22: Clean CPP ------------------------------------------------------

cin[, cppstartdate := mode.fun(cppstartdate), by = .(pupilmatchingrefanonymous, referraldate)]

# fill in missing end dates where known from other episodes
cin[, cppstartdate_tmp := cppstartdate[which(!is.na(cppstartdate))][1], by = .(pupilmatchingrefanonymous, referraldate)]

table(is.na(cin$cppstartdate),
      cin$cohort)

table(is.na(cin$cppstartdate_tmp),
      cin$cohort)

cin[, cppstartdate_tmp := NULL]

# Step 24: Final deduplication --------------------------------------------

dups <- duplicated(cin[, c("pupilmatchingrefanonymous", "acadyr", "dob", "female", "disability",
                           "la", "referraldate", "closuredate", "primaryneedcode",
                           "reasonforclosure", "referralsource", "eth_grp", "categoryofabuse",
                           "initialcategoryofabuse", "latestcategoryofabuse", "cppstartdate", "cppenddate")])

table(dups, cin$cohort)
cin <- cin[!dups]
rm(dups)

# Step 25: Final dataset --------------------------------------------------

table(cin$cohort)
aggregate(pupilmatchingrefanonymous ~ cohort, cin, function(x) { length(unique(x)) })

# Miscellaneous -------------------------------------------------------------------

cin$ref_cy <- format(cin$referraldate, "%Y")

lt <- as.POSIXlt(cin$referraldate)
cin$ref_ay <- lt$year + (lt$mo >= 8) + 1900
cin$ref_fy <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

cin$ageatrefdays <- as.numeric(difftime(cin$referraldate, cin$dob, units = "days"))
cin$ageatrefyrs <- cin$ageatrefdays / 365.25

cin <- cin[order(pupilmatchingrefanonymous, referraldate, closuredate, acadyr)]
cin[, row_per_child := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
cin[, row_per_child_ref := seq_len(.N), by = rleid(pupilmatchingrefanonymous, referraldate)]

# tidy up
cin[, la_09 := NULL]
cin[, la_lgr := NULL]

nord <- c("pupilmatchingrefanonymous", "dob", "female", "eth_grp", "cohort", "la", "la_9code", "disability",
          "row_per_child", "row_per_child_ref",
          "acadyr", "aystart", "ref_cy", "ref_ay", "ref_fy", "ageatrefdays", "ageatrefyrs",
          "referraldate", "closuredate", "primaryneedcode", "referralsource", "cinat31march",
          "seensocialworker", "referralnfa", "reasonforclosure",
          "cppstartdate", "cppenddate", "categoryofabuse", "initialcategoryofabuse", "latestcategoryofabuse",
          "numberofpreviouscpp", "dateofinitialcpc")

cin <- cin[, nord, with = F]

rm(nord, mode.fun)

# Save --------------------------------------------------------------------

save(cin, file = "PROCESSED DATA/SEN CI/cin.rda")
rm(list = ls()); gc()
