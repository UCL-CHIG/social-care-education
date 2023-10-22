
# Load data --------------------------------------------------------

cla_episodes <- data.table(read.table("P:/Working/WORKING DATA/CLA_Episode_Data.txt",
                                      header = T,
                                      sep = "\t",
                                      skipNul = T,
                                      stringsAsFactors = F))

names(cla_episodes) <- tolower(names(cla_episodes))

cla_children <- data.table(read.table("P:/Working/WORKING DATA/CLA_2006_to_2017.txt",
                                      header = T,
                                      sep = "\t",
                                      stringsAsFactors = F))

names(cla_children) <- tolower(names(cla_children))

cla_children <- cla_children[order(cla_child_la_code_anon, cla_acadyr)]
cla_episodes <- cla_episodes[order(cla_child_la_code_anon, dateepisodestarted1)]

cla_episodes$dateofbirth <- as.Date(cla_episodes$dateofbirth)
cla_episodes$poc_start <- as.Date(cla_episodes$poc_start)
cla_episodes$dateepisodestarted1 <- as.Date(cla_episodes$dateepisodestarted1)

# Misc --------------------------------------------------------------------

# GENDER
cla_episodes$female <- cla_episodes$sex - 1

# ETH
white <- c("WBRI", "WIRI", "WOTH")
mixed <- c("MWBC", "MWBA", "MWAS", "MOTH")
asian <- c("AIND", "APKN", "ABAN", "AOTH", "CHNE")
black <- c("BCRB", "BAFR", "BOTH")
trave <- c("WIRT", "WROM")

cla_episodes$eth_grp <- factor(NA, levels = c("White", "Mixed", "Asian (inc Cn)",
                                              "Black", "Traveller/Roma", "Other",
                                              "Refused", "NotObtained"))

cla_episodes[ethnic_or %in% white]$eth_grp <- "White"
cla_episodes[ethnic_or %in% mixed]$eth_grp <- "Mixed"
cla_episodes[ethnic_or %in% asian]$eth_grp <- "Asian (inc Cn)"
cla_episodes[ethnic_or %in% black]$eth_grp <- "Black"
cla_episodes[ethnic_or %in% trave]$eth_grp <- "Traveller/Roma"
cla_episodes[ethnic_or == "OOTH"]$eth_grp <- "Other"
cla_episodes[ethnic_or == "REFU"]$eth_grp <- "Refused"
cla_episodes[ethnic_or == "NOBT"]$eth_grp <- "NotObtained"

rm(white, mixed, asian, black, trave)
cla_episodes$eth_grp <- factor(cla_episodes$eth_grp)

# PMRs --------------------------------------------------------------------

cla_children[cla_children$cla_pupilmatchingrefanonymous == "", ]$cla_pupilmatchingrefanonymous <- NA
cla_children <- cla_children[order(cla_child_id_anon, cla_acadyr)]
cla_children[, pmr2 := cla_pupilmatchingrefanonymous[which(!is.na(cla_pupilmatchingrefanonymous))][1], by = cla_child_id_anon]

# import into cla_ep
tmp <- data.table(
  cla_child_id_anon = cla_children$cla_child_id_anon,
  pmr2 = cla_children$pmr2
)

tmp <- tmp[!duplicated(tmp)]
tmp <- tmp[!is.na(pmr2)]
tmp <- tmp[!is.na(cla_child_id_anon)]

cla_episodes <- merge(cla_episodes,
                      tmp,
                      by = "cla_child_id_anon",
                      all.x = T,
                      all.y = F,
                      sort = F)

cla_episodes <- cla_episodes[order(pmr2, dateepisodestarted1)]
rm(tmp, cla_children)

# Drop not in cohort ------------------------------------------------------

cla_episodes <- cla_episodes[pmr2 %in% ks4_c1$pupilmatchingrefanonymous | pmr2 %in% ks4_c2$pupilmatchingrefanonymous]

# Check PMRs given to different children --------------------------

# # first need to ensure that PMRs not shared across different child IDs
cla_episodes$child_id_int <- as.integer(as.factor(cla_episodes$cla_child_id_anon))

cla_episodes[!is.na(pmr2), pmr.min := min(child_id_int), by = "pmr2"]
cla_episodes[!is.na(pmr2), pmr.max := max(child_id_int), by = "pmr2"]
cla_episodes[!is.na(pmr2), pmr.check := pmr.min != pmr.max]

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.min := min(dateofbirth), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.max := max(dateofbirth), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.check := dob.min != dob.max]

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.min := min(female), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.max := max(female), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.check := fem.min != fem.max]

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.min := min(as.integer(as.factor(eth_grp))), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.max := max(as.integer(as.factor(eth_grp))), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.check := eth.min != eth.max]

# if any two differ, assume really a different child
cla_episodes$dob_fem <- ifelse(cla_episodes$dob.check == T & cla_episodes$fem.check == T, 1, 0)
cla_episodes$dob_eth <- ifelse(cla_episodes$dob.check == T & cla_episodes$eth.check == T, 1, 0)
cla_episodes$fem_eth <- ifelse(cla_episodes$fem.check == T & cla_episodes$eth.check == T, 1, 0)
cla_episodes$all_con <- ifelse(cla_episodes$fem.check == T & cla_episodes$eth.check == T & cla_episodes$dob.check == T, 1, 0)

cla_episodes[, pmr_flag_dob_fem := dob_fem == 1, by = pmr2]
cla_episodes[, pmr_flag_dob_eth := dob_eth == 1, by = pmr2]
cla_episodes[, pmr_flag_fem_eth := fem_eth == 1, by = pmr2]
cla_episodes[, pmr_flag_all_con := all_con == 1, by = pmr2]
cla_episodes[, any_flag := dob_fem == 1 | dob_eth == 1 | fem_eth == 1 | all_con == 1, by = pmr2]

cla_episodes <- cla_episodes[any_flag == F | is.na(any_flag)]

# re-move check vars
cla_episodes[, pmr.min := NULL]
cla_episodes[, pmr.max := NULL]
cla_episodes[, pmr.check := NULL]

cla_episodes[, dob.min := NULL]
cla_episodes[, dob.max := NULL]
cla_episodes[, dob.check := NULL]

cla_episodes[, fem.min := NULL]
cla_episodes[, fem.max := NULL]
cla_episodes[, fem.check := NULL]

cla_episodes[, eth.min := NULL]
cla_episodes[, eth.max := NULL]
cla_episodes[, eth.check := NULL]

cla_episodes[, dob_fem := NULL]
cla_episodes[, dob_eth := NULL]
cla_episodes[, fem_eth := NULL]
cla_episodes[, all_con := NULL]

cla_episodes[, pmr_flag_dob_fem := NULL]
cla_episodes[, pmr_flag_dob_eth := NULL]
cla_episodes[, pmr_flag_fem_eth := NULL]
cla_episodes[, pmr_flag_all_con := NULL]
cla_episodes[, any_flag := NULL]

# remove redundant id vars
cla_episodes[, child_id_int := NULL]
cla_episodes[, cla_child_la_code_anon := NULL]
cla_episodes[, ss_idx := NULL]
cla_episodes[, epi_idx := NULL]

# de-duplicate ---------------------------------------------------

cla_episodes <- cla_episodes[!duplicated(cla_episodes[, c("pmr2",
                                                          "la",
                                                          "female",
                                                          "eth_grp",
                                                          "dateofbirth",
                                                          "poc_start",
                                                          "dateepisodestarted1",
                                                          "cin",
                                                          "placement",
                                                          "legal_status",
                                                          "uasc_status1")])]

# drop short-term breaks ------------------------------------------

cla_episodes <- cla_episodes[!(legal_status %in% c("V3", "V4"))]

# first epi start == poc start ------------------------------

cla_episodes <- cla_episodes[order(pmr2, poc_start, dateepisodestarted1)]
cla_episodes[, poc_n := frank(poc_start, ties.method = "dense"), by = pmr2]
cla_episodes[, epi_n_per_poc := seq_len(.N), by = rleid(pmr2, poc_n)]
cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$poc_start <-
  cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$dateepisodestarted1

cla_episodes <- cla_episodes[order(pmr2, poc_start, dateepisodestarted1)]
