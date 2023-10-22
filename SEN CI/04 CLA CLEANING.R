
setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")

# Step 1: Load data --------------------------------------------------------

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

cla_children <- cla_children[order(cla_child_la_code_anon, cla_acadyr)] # is one per year
cla_episodes <- cla_episodes[order(cla_child_la_code_anon, dateepisodestarted1)] # multiple episodes

cla_episodes$dateofbirth <- as.Date(cla_episodes$dateofbirth)
cla_episodes$poc_start <- as.Date(cla_episodes$poc_start)
cla_episodes$dateepisodestarted1 <- as.Date(cla_episodes$dateepisodestarted1)
cla_episodes$dateepisodeceased1 <- as.Date(cla_episodes$dateepisodeceased1)

cla_children$cla_dob <- as.Date(cla_children$cla_dob)
cla_children$cla_poc_start <- as.Date(cla_children$cla_poc_start)
cla_children$cla_date_epi_comm <- as.Date(cla_children$cla_date_epi_comm)
cla_children$cla_date_epi_ceased <- as.Date(cla_children$cla_date_epi_ceased)

length(unique(cla_episodes$cla_child_la_code_anon))
length(unique(cla_children$cla_child_la_code_anon))

# Misc --------------------------------------------------------------------

# GENDER
cla_episodes$female <- cla_episodes$sex - 1
cla_children$female <- cla_children$cla_sex - 1

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

cla_children$eth_grp <- factor(NA, levels = c("White", "Mixed", "Asian (inc Cn)",
                                              "Black", "Traveller/Roma", "Other",
                                              "Refused", "NotObtained"))

cla_children[cla_ethnic %in% white]$eth_grp <- "White"
cla_children[cla_ethnic %in% mixed]$eth_grp <- "Mixed"
cla_children[cla_ethnic %in% asian]$eth_grp <- "Asian (inc Cn)"
cla_children[cla_ethnic %in% black]$eth_grp <- "Black"
cla_children[cla_ethnic %in% trave]$eth_grp <- "Traveller/Roma"
cla_children[cla_ethnic == "OOTH"]$eth_grp <- "Other"
cla_children[cla_ethnic == "REFU"]$eth_grp <- "Refused"
cla_children[cla_ethnic == "NOBT"]$eth_grp <- "NotObtained"

rm(white, mixed, asian, black, trave)
cla_episodes$eth_grp <- factor(cla_episodes$eth_grp)
cla_children$eth_grp <- factor(cla_children$eth_grp)

# Step 2: in cla_children: child IDs duplicated across LAs ---------------------------

ids <- unique(cla_children[, c("cla_child_id_anon", "cla_cla_la")])
any(duplicated(ids$cla_child_id_anon)) #
rm(ids)

# Step 3: in cla_children: Fill in back PMRs -------------------------------

cla_children[cla_pupilmatchingrefanonymous == ""]$cla_pupilmatchingrefanonymous <- NA
cla_children <- cla_children[order(cla_child_id_anon, cla_acadyr)]
cla_children[, pmr2 := cla_pupilmatchingrefanonymous[which(!is.na(cla_pupilmatchingrefanonymous))][1], by = cla_child_id_anon]

table(is.na(cla_children$cla_pupilmatchingrefanonymous))
table(is.na(cla_children$pmr2))

# Step 4: import PMR into cla_episodes ------------------------------------------

# import into cla_ep
tmp <- data.table(
  cla_child_id_anon = cla_children$cla_child_id_anon,
  pmr2 = cla_children$pmr2
)

tmp <- tmp[!duplicated(tmp)]
tmp <- tmp[!is.na(pmr2)]
tmp <- tmp[!is.na(cla_child_id_anon)]

length(unique(tmp$cla_child_id_anon))
length(unique(tmp$pmr2))

cla_episodes <- merge(cla_episodes,
                      tmp,
                      by = "cla_child_id_anon",
                      all.x = T,
                      all.y = F,
                      sort = F)

cla_episodes <- cla_episodes[order(pmr2, dateepisodestarted1)]

# how many children now
length(unique(cla_episodes$pmr2))

rm(tmp, cla_children)

# Step 5: drop not in cohort ------------------------------------------------------

load("PROCESSED DATA/SEN CI/cohort2_npd_clean_sch.rda")

cla_episodes <- cla_episodes[pmr2 %in% cohort2$pupilmatchingrefanonymous]

cla_episodes$cohort <- 2 # hangover from previous version of code - easier to do this than edit all subsequent

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

rm(cohort2)

# Step 6: check LA child IDs given to different children --------------------------

# # first need to ensure that PMRs not shared across different child IDs
cla_episodes$child_id_int <- as.integer(as.factor(cla_episodes$cla_child_id_anon))

cla_episodes[!is.na(pmr2), pmr.min := min(child_id_int), by = "pmr2"]
cla_episodes[!is.na(pmr2), pmr.max := max(child_id_int), by = "pmr2"]
cla_episodes[!is.na(pmr2), pmr.check := pmr.min != pmr.max]
table(cla_episodes$pmr.check,
      cla_episodes$cohort) # 

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.min := min(dateofbirth), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.max := max(dateofbirth), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, dob.check := dob.min != dob.max]
table(cla_episodes$dob.check) # 

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.min := min(female), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.max := max(female), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, fem.check := fem.min != fem.max]
table(cla_episodes$fem.check) # 

cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.min := min(as.integer(as.factor(eth_grp))), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.max := max(as.integer(as.factor(eth_grp))), by = "pmr2"]
cla_episodes[!is.na(pmr2) & pmr.check == TRUE, eth.check := eth.min != eth.max]
table(cla_episodes$eth.check) # 

# if any two differ, assume really a different child
cla_episodes$dob_fem <- ifelse(cla_episodes$dob.check == T & cla_episodes$fem.check == T, 1, 0)
cla_episodes$dob_eth <- ifelse(cla_episodes$dob.check == T & cla_episodes$eth.check == T, 1, 0)
cla_episodes$fem_eth <- ifelse(cla_episodes$fem.check == T & cla_episodes$eth.check == T, 1, 0)
cla_episodes$all_con <- ifelse(cla_episodes$fem.check == T & cla_episodes$eth.check == T & cla_episodes$dob.check == T, 1, 0)

table(cla_episodes$dob_fem)
table(cla_episodes$dob_eth)
table(cla_episodes$fem_eth)
table(cla_episodes$all_con)

cla_episodes[, pmr_flag_dob_fem := dob_fem == 1, by = pmr2]
cla_episodes[, pmr_flag_dob_eth := dob_eth == 1, by = pmr2]
cla_episodes[, pmr_flag_fem_eth := fem_eth == 1, by = pmr2]
cla_episodes[, pmr_flag_all_con := all_con == 1, by = pmr2]
cla_episodes[, any_flag := dob_fem == 1 | dob_eth == 1 | fem_eth == 1 | all_con == 1, by = pmr2]

# extract a separate table in order to count
table(cla_episodes$any_flag) # 
length(unique(cla_episodes[any_flag == T]$pmr2)) #

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

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

# Step 7: missing episode start and end dates -------------------------------------------------------------

table(is.na(cla_episodes$dateepisodestarted1)) #
table(is.na(cla_episodes$dateepisodeceased1)) #
table(is.na(cla_episodes$poc_start)) #

# Step 8: episode end date < start date -----------------------------------

# table(cla_episodes$dateepisodeceased1 < cla_episodes$dateepisodestarted1) #
# # View(cla_episodes[pmr2 %in% cla_episodes[cla_episodes$dateepisodeceased1 < cla_episodes$dateepisodestarted1]$pmr2])
# # drop it
# cla_episodes <- cla_episodes[-which(cla_episodes$dateepisodeceased1 < cla_episodes$dateepisodestarted1), ]
# table(cla_episodes$dateepisodeceased1 < cla_episodes$dateepisodestarted1) # 

# Step 9: episode start date < period of care start ----------------------

any(cla_episodes$dateepisodestarted1 < cla_episodes$poc_start) #

# Step 10: poc start < dob ------------------------------------------------

any(cla_episodes$poc_start < cla_episodes$dateofbirth) 

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

# Step 11: de-duplicate ---------------------------------------------------

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
                                                          "uasc_status1",
                                                          "cohort")])]

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

# Step 12: drop short-term breaks ------------------------------------------

stb <- cla_episodes[legal_status %in% c("V3", "V4")]
cla_episodes <- cla_episodes[!(legal_status %in% c("V3", "V4"))]

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

save(stb, file = "PROCESSED DATA/SEN CI/stb.rda")

table(stb$cohort)
aggregate(pmr2 ~ cohort, stb, function(x) { length(unique(x)) })

rm(stb)

# Step 13: clean episode end dates ----------------------------------------

cla_episodes <- cla_episodes[order(pmr2, poc_start, dateepisodestarted1)]

cla_episodes[reasonepisodeceased1 == "-10"]$dateepisodeceased1 <- NA
table(is.na(cla_episodes$dateepisodeceased1)) 

table(is.na(cla_episodes$dateepisodeceased1),
      format(cla_episodes$dateepisodestarted1, format = "%Y"))

table(cla_episodes$dateepisodestarted1 < cla_episodes$poc_start) #
table(cla_episodes$dateepisodestarted1 > cla_episodes$dateepisodeceased1) #
table(cla_episodes$dateepisodestarted1 == cla_episodes$dateepisodeceased1) 

cla_episodes$epi_cy <- format(cla_episodes$dateepisodestarted1, "%Y")

# assign indices
cla_episodes <- cla_episodes[order(pmr2, poc_start, dateepisodestarted1)]
cla_episodes[, poc_n := frank(poc_start, ties.method = "dense"), by = pmr2]
cla_episodes[, epi_n_per_poc := seq_len(.N), by = rleid(pmr2, poc_n)]
cla_episodes[, epi_n_per_child := seq_len(.N), by = rleid(pmr2)]

# Step 14: drop epi end < 2005/09/01 --------------------------------------------------------------------

table(cla_episodes$dateepisodeceased1 < as.Date("2005-09-01"),
      cla_episodes$cohort)
cla_episodes <- cla_episodes[dateepisodeceased1 >= as.Date("2005-09-01") | is.na(dateepisodeceased1)]

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

# Step 15: first epi start == poc start ------------------------------

table(cla_episodes[epi_n_per_poc == 1]$dateepisodestarted1 != cla_episodes[epi_n_per_poc == 1]$poc_start,
      cla_episodes[epi_n_per_poc == 1]$cohort)

table(cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$epi_cy)
all(cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$poc_start <
      cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$dateepisodestarted1) #

cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$poc_start <-
  cla_episodes[epi_n_per_poc == 1 & dateepisodestarted1 != poc_start]$dateepisodestarted1
table(cla_episodes[epi_n_per_poc == 1]$dateepisodestarted1 != cla_episodes[epi_n_per_poc == 1]$poc_start) 

# Step 16: clean gender and eth ----------------------------------------------------

mode.fun <- function(v) {
  v <- v[!(v %in% c("Refused", "NotObtained"))]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  return(md[length(md)]) # is either modal or last if multimodal
}

# ethnicity
cla_episodes[, eth.min := min(as.integer(eth_grp)), by = "pmr2"]
cla_episodes[, eth.max := max(as.integer(eth_grp)), by = "pmr2"]
cla_episodes[, eth.check := eth.min != eth.max]

table(cla_episodes$eth.check,
      cla_episodes$cohort) # 
table(cla_episodes[epi_n_per_child == 1]$eth.check) # 

cla_episodes[, eth_grp_2 := mode.fun(eth_grp), by = "pmr2"]

cla_episodes[, eth.min := min(as.integer(eth_grp_2)), by = "pmr2"]
cla_episodes[, eth.max := max(as.integer(eth_grp_2)), by = "pmr2"]
cla_episodes[, eth.check := eth.min != eth.max]
table(cla_episodes$eth.check) # 

# female
cla_episodes[, fem.min := min(female), by = "pmr2"]
cla_episodes[, fem.max := max(female), by = "pmr2"]
cla_episodes[, fem.check := fem.min != fem.max]
table(cla_episodes$fem.check) # 
table(cla_episodes[epi_n_per_child == 1]$fem.check) # 

cla_episodes[, female_2 := mode.fun(female), by = "pmr2"]

cla_episodes[, fem.min := min(female_2), by = "pmr2"]
cla_episodes[, fem.max := max(female_2), by = "pmr2"]
cla_episodes[, fem.check := fem.min != fem.max]
table(cla_episodes$fem.check)

# clean up
cla_episodes[, eth.min := NULL]
cla_episodes[, eth.max := NULL]
cla_episodes[, eth.check := NULL]
cla_episodes[, fem.min := NULL]
cla_episodes[, fem.max := NULL]
cla_episodes[, fem.check := NULL]

# Step 17: Re-calc age and duration ---------------------------------------------------

# AGE
cla_episodes$age_poc_start <- as.numeric(round(difftime(cla_episodes$poc_start,
                                                        cla_episodes$dateofbirth, units = "days") /
                                                 365.25, 2))

# DURATION
# where any -10 by poc - flag incomplete
cla_episodes[, poc.flag := any(reasonepisodeceased1 == "-10"), by = c("pmr2", "poc_start")]

# only do where complete
cla_episodes$poc_end <- as.Date(NA)
cla_episodes[!(poc.flag), poc_end := max(dateepisodeceased1), by = c("pmr2", "poc_start")]

table(cla_episodes$poc_end < cla_episodes$poc_start)

cla_episodes$poc_dur <- as.integer(NA)
cla_episodes[!(poc.flag)]$poc_dur <- as.integer(difftime(cla_episodes[!(poc.flag)]$poc_end,
                                                                cla_episodes[!(poc.flag)]$poc_start,
                                                                units = "days"))
cla_episodes[, poc.flag := NULL]

cla_episodes$epi_cy <- format(cla_episodes$dateepisodestarted1, "%Y")
cla_episodes$poc_cy <- format(cla_episodes$poc_start, "%Y")

lt <- as.POSIXlt(cla_episodes$poc_start)
cla_episodes$poc_fy <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

lt <- as.POSIXlt(cla_episodes$dateepisodestarted1)
cla_episodes$epi_fy <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

lt <- as.POSIXlt(cla_episodes$poc_start)
cla_episodes$poc_ay <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

lt <- as.POSIXlt(cla_episodes$dateepisodestarted1)
cla_episodes$epi_ay <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

# More misc ----------------------------------------------------------

cla_episodes$placement_2 <- factor(NA, levels = c("Placed for adoption",
                                                  "Foster placement (relative or friend)",
                                                  "Foster placement (other)",
                                                  "Secure unit",
                                                  "Children's home",
                                                  "s 51 refuge",
                                                  "Missing from placement",
                                                  "Own parents or person w PR",
                                                  "Independent living",
                                                  "Residential employment",
                                                  "Care home or NHS Trust",
                                                  "Family centre or MBU",
                                                  "YOI or prison",
                                                  "Residential school",
                                                  "Other",
                                                  "M2 (whereabouts known - not in refuge)"))

placed <- c("A1", "A2", "A3", "A4", "A5", "A6")
fosterrel <- c("F1", "F4", "Q1", "U1", "U2", "U3")
fosteroth <- c("F2", "F3", "F5", "F6", "Q2", "U4", "U5", "U6")
secure <- c("H1", "H2", "K1")
home <- c("H3", "H4", "H5", "K2")
refuge <- "M1"
missing <- "M3"
ownparents <- "P1"
independent <- "P2"
resideemp <- "P3"
carehome <- c("R1", "R2")
mbu <- "R3"
yoi <- "R5"
sch <- "S1"
oth <- "Z1"
m2 <- "M2"

cla_episodes[placement %in% placed]$placement_2 <- "Placed for adoption"
cla_episodes[placement %in% fosterrel]$placement_2 <- "Foster placement (relative or friend)"
cla_episodes[placement %in% fosteroth]$placement_2 <- "Foster placement (other)"
cla_episodes[placement %in% secure]$placement_2 <- "Secure unit"
cla_episodes[placement %in% home]$placement_2 <- "Children's home"
cla_episodes[placement %in% refuge]$placement_2 <- "s 51 refuge"
cla_episodes[placement %in% missing]$placement_2 <- "Missing from placement"
cla_episodes[placement %in% ownparents]$placement_2 <- "Own parents or person w PR"
cla_episodes[placement %in% independent]$placement_2 <- "Independent living"
cla_episodes[placement %in% resideemp]$placement_2 <- "Residential employment"
cla_episodes[placement %in% carehome]$placement_2 <- "Care home or NHS Trust"
cla_episodes[placement %in% mbu]$placement_2 <- "Family centre or MBU"
cla_episodes[placement %in% yoi]$placement_2 <- "YOI or prison"
cla_episodes[placement %in% sch]$placement_2 <- "Residential school"
cla_episodes[placement %in% oth]$placement_2 <- "Other"
cla_episodes[placement %in% m2]$placement_2 <- "M2 (whereabouts known - not in refuge)"

rm(placed, fosterrel, fosteroth, secure, home, refuge, missing,
   ownparents, independent, resideemp, carehome, mbu, yoi, sch,
   oth, m2)

table(cla_episodes$legal_status, useNA = "always")
cla_episodes$legal_status_2 <- factor(NA, levels = c("Interim care order",
                                                     "Full care order",
                                                     "Freeing/placement order",
                                                     "s 20",
                                                     "Short-term breaks",
                                                     "Police protection",
                                                     "EPO",
                                                     "Child assessment order",
                                                     "Remand/committed for trial",
                                                     "Detained under PACE",
                                                     "Sentenced to CYPA 69 sup"))

cla_episodes[cla_episodes$legal_status == "C1"]$legal_status_2 <- "Interim care order"
cla_episodes[cla_episodes$legal_status == "C2"]$legal_status_2 <- "Full care order"
cla_episodes[cla_episodes$legal_status %in% c("D1", "E1")]$legal_status_2 <- "Freeing/placement order"
cla_episodes[cla_episodes$legal_status == "V2"]$legal_status_2 <- "s 20"
cla_episodes[cla_episodes$legal_status %in% c("V3", "V4")]$legal_status_2 <- "Short-term breaks"
cla_episodes[cla_episodes$legal_status == "L1"]$legal_status_2 <- "Police protection"
cla_episodes[cla_episodes$legal_status == "L2"]$legal_status_2 <- "EPO"
cla_episodes[cla_episodes$legal_status == "L3"]$legal_status_2 <- "Child assessment order"
cla_episodes[cla_episodes$legal_status == "J1"]$legal_status_2 <- "Remand/committed for trial"
cla_episodes[cla_episodes$legal_status == "J2"]$legal_status_2 <- "Detained under PACE"
cla_episodes[cla_episodes$legal_status == "J3"]$legal_status_2 <- "Sentenced to CYPA 69 sup"

cla_episodes$age_grp <- factor("<1", levels = c("<1",
                                                "1 to 4",
                                                "5 to 9",
                                                "10 to 15",
                                                "16+"))

cla_episodes[age_poc_start >= 1 & age_poc_start < 5]$age_grp <- "1 to 4"
cla_episodes[age_poc_start >= 5 & age_poc_start < 10]$age_grp <- "5 to 9"
cla_episodes[age_poc_start >= 10 & age_poc_start < 16]$age_grp <- "10 to 15"
cla_episodes[age_poc_start >= 16]$age_grp <- "16+"

new_order <- c("pmr2", "dateofbirth", "female_2", "eth_grp_2", "uasc_status1", "la", "age_poc_start", "age_grp", "cohort",
               "poc_n", "epi_n_per_poc", "epi_n_per_child",
               "poc_start", "poc_end", "poc_dur", "dateepisodestarted1", "dateepisodeceased1",
               "poc_cy", "poc_ay", "poc_fy", "epi_cy", "epi_ay", "epi_fy",
               "reasonepisodeceased1",
               "placement", "placement_2", "legal_status", "legal_status_2", "cin")

cla_episodes <- cla_episodes[, new_order, with = F]

rm(new_order)

names(cla_episodes) <- c("pmr2",
                         "dateofbirth",
                         "female",
                         "eth_grp",
                         "uasc",
                         "la",
                         "age_poc_start",
                         "age_grp",
                         "cohort",
                         "poc_n",
                         "epi_n_per_poc",
                         "epi_n_per_child",
                         "poc_start",
                         "poc_end",
                         "poc_dur",
                         "dateepisodestarted",
                         "dateepisodeceased",
                         "poc_cy",
                         "poc_ay",
                         "poc_fy",
                         "epi_cy",
                         "epi_ay",
                         "epi_fy",
                         "reasonepisodeceased",
                         "placement",
                         "placement_2",
                         "legal_status",
                         "legal_status_2",
                         "cin")

# Step 18: count episodes per children ------------------------------------

cla_episodes <- cla_episodes[order(pmr2, dateepisodestarted)]
cla_episodes[, n_epi_total := max(epi_n_per_child), by = "pmr2"]

cla_episodes$count_episode_for_placement <- T
flag <- cla_episodes$placement == shift(cla_episodes$placement, type = "lead") &
  cla_episodes$poc_start == shift(cla_episodes$poc_start, type = "lead") &
  cla_episodes$legal_status != shift(cla_episodes$legal_status, type = "lead") &
  cla_episodes$pmr2 == shift(cla_episodes$pmr2, type = "lead")

cla_episodes[flag == T]$count_episode_for_placement <- F
rm(flag)
cla_episodes[is.na(count_episode_for_placement)]$count_episode_for_placement <- T # last one is NA because of shift fx

cla_episodes[, n_placements_total :=  sum(count_episode_for_placement), by = "pmr2"]

table(cla_episodes$n_placements_total)

length(unique(cla_episodes[n_placements_total > 100]$pmr2))

cla_episodes <- cla_episodes[n_placements_total < 100]

# Step 19: final dataset --------------------------------------------------

table(cla_episodes$cohort)
aggregate(pmr2 ~ cohort, cla_episodes, function(x) { length(unique(x)) })

# Save --------------------------------------------------------------------

save(cla_episodes, file = "PROCESSED DATA/SEN CI/cla_episodes.rda")
rm(list = ls()); gc()
