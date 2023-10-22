
# setwd -------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")
library("plyr")

# BOTH COHORTS ----------------------------------------------------------------

# Create vectors of integers in order to store lengths of files

raw_length_c1 <- as.integer(rep(NA, 11))
subset_length_c1 <- as.integer(rep(NA, 11))
final_length_c1 <- as.integer(rep(NA, 11))

raw_length_c2 <- as.integer(rep(NA, 12))
subset_length_c2 <- as.integer(rep(NA, 12))
final_length_c2 <- as.integer(rep(NA, 12))

# ANCHOR (year 7) -------------------------------------------------------------------

# In this section, the index census years are loaded (i.e. year 7 for the two cohorts).
# Included throughout are, where available, the Alternative Provision and Pupil Referral Unit censuses.

# * cohort 1 (2011/12) --------------------------------------------------------------

c1y7 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[7] <- nrow(c1y7)
names(c1y7) <- tolower(gsub("_S.*", "", names(c1y7)))

c1y7 <- c1y7[ncyearactual == "7" |
               (ncyearactual == "X" & ageatstartofacademicyear == 11)]

subset_length_c1[7] <- nrow(c1y7)

c1y7$entrydate <- as.Date(c1y7$entrydate, format = "%Y-%m-%d")
c1y7$entry.calyear <- format(c1y7$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y7$entrydate)
c1y7$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c1y7 <- c1y7[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2012-01-19")]

#pru <- pru[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
keep <- pru$pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous | pru$ageatstartofacademicyear == 11
pru <- pru[keep == T]
rm(keep)
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c1y7 <- rbind(c1y7, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2011/2012"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
#ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
keep <- ap$pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous | ap$ageatstartofacademicyear == 11
ap <- ap[keep == T]
rm(keep)
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c1y7 <- rbind(c1y7, ap, fill = T)

rm(ap)

c1y7 <- c1y7[order(pupilmatchingrefanonymous, entrydate)]
c1y7[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y7$pupilmatchingrefanonymous))
final_length_c1[7] <- nrow(c1y7)

# * cohort 2 (2012/13) ----------------------------------------------------------------

c2y7 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_CENSUS_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[8] <- nrow(c2y7)
names(c2y7) <- tolower(gsub("_S.*", "", names(c2y7)))

c2y7 <- c2y7[ncyearactual == "7" |
               (ncyearactual == "X" & ageatstartofacademicyear == 11)]

subset_length_c2[8] <- nrow(c2y7)

c2y7$entrydate <- as.Date(c2y7$entrydate, format = "%Y-%m-%d")
c2y7$entry.calyear <- format(c2y7$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y7$entrydate)
c2y7$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y7 <- c2y7[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2013-01-17")]

#pru <- pru[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
keep <- pru$pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous | pru$ageatstartofacademicyear == 11
pru <- pru[keep == T]
rm(keep)
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c2y7 <- rbind(c2y7, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2012/2013"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
#ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
keep <- ap$pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous | ap$ageatstartofacademicyear == 11
ap <- ap[keep == T]
rm(keep)
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y7 <- rbind(c2y7, ap, fill = T)

rm(ap)

c2y7 <- c2y7[order(pupilmatchingrefanonymous, entrydate)]
c2y7[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y7$pupilmatchingrefanonymous))
final_length_c2[8] <- nrow(c2y7)

# * check for duplicates across cohorts -------------------------------------

# There should be no duplicated PMRs across year: these would be erroneous and so dropped.

table(c1y7$pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous)
table(c2y7$pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous)

c2y7 <- c2y7[!(pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous)]

table(c1y7$pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous)
table(c2y7$pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous)

final_length_c1[7] <- nrow(c1y7)
final_length_c2[8] <- nrow(c2y7)

gc()

# RECEPTION YEAR ----------------------------------------------------------

# In all that follows, the census files are loaded and subset to the index census files created above.

# * cohort 2 only (2005/06) -----------------------------------------------

c2y0 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_PLASC_2006.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c2[1] <- nrow(c2y0)

names(c2y0) <- tolower(gsub("_S.*", "", names(c2y0)))

c2y0 <- c2y0[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[1] <- nrow(c2y0)

c2y0 <- c2y0[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c2y0$pupilmatchingrefanonymous))

c2y0$entrydate <- as.Date(c2y0$entrydate, format = "%Y-%m-%d")
c2y0$entry.calyear <- format(c2y0$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y0$entrydate)
c2y0$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c2y0)[names(c2y0) == "languagegroup"] <- "languagegroupminor"

c2y0[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length_c2[1] <- nrow(c2y0)

gc()

# YEAR 1 --------------------------------------------------------------------

# * cohort 1 (2005/06) ----------------------------------------------------

c1y1 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_PLASC_2006.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c1[1] <- nrow(c1y1)

names(c1y1) <- tolower(gsub("_S.*", "", names(c1y1)))

c1y1 <- c1y1[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[1] <- nrow(c1y1)

c1y1 <- c1y1[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c1y1$pupilmatchingrefanonymous))

c1y1$entrydate <- as.Date(c1y1$entrydate, format = "%Y-%m-%d")
c1y1$entry.calyear <- format(c1y1$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y1$entrydate)
c1y1$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c1y1)[names(c1y1) == "languagegroup"] <- "languagegroupminor"

c1y1[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length_c1[1] <- nrow(c1y1)

# * cohort 2 (2006/07) ----------------------------------------------------

c2y1 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2007.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c2[2] <- nrow(c2y1)

names(c2y1) <- tolower(gsub("_S.*", "", names(c2y1)))

c2y1 <- c2y1[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[2] <- nrow(c2y1)

c2y1 <- c2y1[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c2y1$pupilmatchingrefanonymous))

c2y1$entrydate <- as.Date(c2y1$entrydate, format = "%Y-%m-%d")
c2y1$entry.calyear <- format(c2y1$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y1$entrydate)
c2y1$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c2y1)[names(c2y1) == "languagegroup"] <- "languagegroupminor"

c2y1[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length_c2[2] <- nrow(c2y1)

gc()

# YEAR 2 --------------------------------------------------------------------

# * cohort 1 (2006/07) ----------------------------------------------------

c1y2 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2007.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c1[2] <- nrow(c1y2)

names(c1y2) <- tolower(gsub("_S.*", "", names(c1y2)))

c1y2 <- c1y2[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[2] <- nrow(c1y2)

c1y2 <- c1y2[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c1y2$pupilmatchingrefanonymous))

c1y2$entrydate <- as.Date(c1y2$entrydate, format = "%Y-%m-%d")
c1y2$entry.calyear <- format(c1y2$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y2$entrydate)
c1y2$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c1y2)[names(c1y2) == "languagegroup"] <- "languagegroupminor"

c1y2[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length_c1[2] <- nrow(c1y2)

# * cohort 2 (2007/08) ----------------------------------------------------

c2y2 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2008.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c2[3] <- nrow(c2y2)

names(c2y2) <- tolower(gsub("_S.*", "", names(c2y2)))

c2y2 <- c2y2[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[3] <- nrow(c2y2)

c2y2$entrydate <- as.Date(c2y2$entrydate, format = "%Y-%m-%d")
c2y2$entry.calyear <- format(c2y2$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y2$entrydate)
c2y2$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y2 <- c2y2[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c2y2$pupilmatchingrefanonymous))

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2007/2008"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c2y2 <- rbind(c2y2, ap, fill = T)

rm(ap)

c2y2 <- c2y2[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c2y2[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y2$pupilmatchingrefanonymous))

names(c2y2)[names(c2y2) == "languagegroup"] <- "languagegroupminor"
final_length_c2[3] <- nrow(c2y2)

gc()

# YEAR 3 --------------------------------------------------------------------

# * cohort 1 (2007/08) ----------------------------------------------------

c1y3 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2008.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length_c1[3] <- nrow(c1y3)

names(c1y3) <- tolower(gsub("_S.*", "", names(c1y3)))

c1y3 <- c1y3[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[3] <- nrow(c1y3)

c1y3$entrydate <- as.Date(c1y3$entrydate, format = "%Y-%m-%d")
c1y3$entry.calyear <- format(c1y3$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y3$entrydate)
c1y3$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c1y3 <- c1y3[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
length(unique(c1y3$pupilmatchingrefanonymous))

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2007/2008"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c1y3 <- rbind(c1y3, ap, fill = T)

rm(ap)

c1y3 <- c1y3[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c1y3[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y3$pupilmatchingrefanonymous))

names(c1y3)[names(c1y3) == "languagegroup"] <- "languagegroupminor"
final_length_c1[3] <- nrow(c1y3)

# * cohort 2 (2008/09) ----------------------------------------------------

c2y3 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2009.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[4] <- nrow(c2y3)
names(c2y3) <- tolower(gsub("_S.*", "", names(c2y3)))

c2y3 <- c2y3[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[4] <- nrow(c2y3)

c2y3$entrydate <- as.Date(c2y3$entrydate, format = "%Y-%m-%d")
c2y3$entry.calyear <- format(c2y3$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y3$entrydate)
c2y3$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y3 <- c2y3[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2008/2009"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]

ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap$censusdate <- as.character(ap$censusdate)

c2y3 <- rbind(c2y3, ap, fill = T)

rm(ap)

c2y3 <- c2y3[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c2y3[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y3$pupilmatchingrefanonymous))
final_length_c2[4] <- nrow(c2y3)

gc()

# YEAR 4 --------------------------------------------------------------------

# * cohort 1 (2008/09) ----------------------------------------------------

c1y4 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2009.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[4] <- nrow(c1y4)
names(c1y4) <- tolower(gsub("_S.*", "", names(c1y4)))

c1y4 <- c1y4[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[4] <- nrow(c1y4)

c1y4$entrydate <- as.Date(c1y4$entrydate, format = "%Y-%m-%d")
c1y4$entry.calyear <- format(c1y4$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y4$entrydate)
c1y4$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c1y4 <- c1y4[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2008/2009"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]

ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap$censusdate <- as.character(ap$censusdate)

c1y4 <- rbind(c1y4, ap, fill = T)

rm(ap)

c1y4 <- c1y4[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c1y4[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y4$pupilmatchingrefanonymous))
final_length_c1[4] <- nrow(c1y4)

# * cohort 2 (2009/10) ----------------------------------------------------

c2y4 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2010.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[5] <- nrow(c2y4)
names(c2y4) <- tolower(gsub("_S.*", "", names(c2y4)))

c2y4 <- c2y4[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[5] <- nrow(c2y4)

c2y4$entrydate <- as.Date(c2y4$entrydate, format = "%Y-%m-%d")
c2y4$entry.calyear <- format(c2y4$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y4$entrydate)
c2y4$entry.acayear <- lt$year + (lt$mo >= 9) + 1900
rm(lt)

c2y4 <- c2y4[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2010-01-21")]
pru <- pru[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c2y4 <- rbind(c2y4, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2009/2010"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y4 <- rbind(c2y4, ap, fill = T)

rm(ap)

c2y4 <- c2y4[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c2y4[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y4$pupilmatchingrefanonymous))
final_length_c2[5] <- nrow(c2y4)

gc()

# YEAR 5 --------------------------------------------------------------------

# * cohort 1 (2009/10) ----------------------------------------------------

c1y5 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2010.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[5] <- nrow(c1y5)
names(c1y5) <- tolower(gsub("_S.*", "", names(c1y5)))

c1y5 <- c1y5[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[5] <- nrow(c1y5)

c1y5$entrydate <- as.Date(c1y5$entrydate, format = "%Y-%m-%d")
c1y5$entry.calyear <- format(c1y5$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y5$entrydate)
c1y5$entry.acayear <- lt$year + (lt$mo >= 9) + 1900
rm(lt)

c1y5 <- c1y5[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2010-01-21")]
pru <- pru[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c1y5 <- rbind(c1y5, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2009/2010"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c1y5 <- rbind(c1y5, ap, fill = T)

rm(ap)

c1y5 <- c1y5[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c1y5[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y5$pupilmatchingrefanonymous))
final_length_c1[5] <- nrow(c1y5)

# * cohort 2 (2010/11) ----------------------------------------------------

c2y5 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2011.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[6] <- nrow(c2y5)
names(c2y5) <- tolower(gsub("_S.*", "", names(c2y5)))

c2y5 <- c2y5[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[6] <- nrow(c2y5)

c2y5$entrydate <- as.Date(c2y5$entrydate, format = "%Y-%m-%d")
c2y5$entry.calyear <- format(c2y5$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y5$entrydate)
c2y5$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y5 <- c2y5[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2011-01-20")]

pru <- pru[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c2y5 <- rbind(c2y5, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2010/2011"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c2y5 <- rbind(c2y5, ap, fill = T)

rm(ap)

c2y5 <- c2y5[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c2y5[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y5$pupilmatchingrefanonymous))
final_length_c2[6] <- nrow(c2y5)

gc()

# YEAR 6 --------------------------------------------------------------------

# * cohort 1 (2010/11) ----------------------------------------------------

c1y6 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2011.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[6] <- nrow(c1y6)
names(c1y6) <- tolower(gsub("_S.*", "", names(c1y6)))

c1y6 <- c1y6[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[6] <- nrow(c1y6)

c1y6$entrydate <- as.Date(c1y6$entrydate, format = "%Y-%m-%d")
c1y6$entry.calyear <- format(c1y6$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y6$entrydate)
c1y6$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c1y6 <- c1y6[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2011-01-20")]

pru <- pru[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c1y6 <- rbind(c1y6, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2010/2011"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c1y6 <- rbind(c1y6, ap, fill = T)

rm(ap)

c1y6 <- c1y6[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c1y6[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y6$pupilmatchingrefanonymous))
final_length_c1[6] <- nrow(c1y6)

# * cohort 2 (2011/12) ----------------------------------------------------

c2y6 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[7] <- nrow(c2y6)
names(c2y6) <- tolower(gsub("_S.*", "", names(c2y6)))

c2y6 <- c2y6[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[7] <- nrow(c2y6)

c2y6$entrydate <- as.Date(c2y6$entrydate, format = "%Y-%m-%d")
c2y6$entry.calyear <- format(c2y6$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y6$entrydate)
c2y6$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y6 <- c2y6[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2012-01-19")]

pru <- pru[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c2y6 <- rbind(c2y6, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2011/2012"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y6 <- rbind(c2y6, ap, fill = T)

rm(ap)

c2y6 <- c2y6[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c2y6[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y6$pupilmatchingrefanonymous))
final_length_c2[7] <- nrow(c2y6)

gc()

# YEAR 8 --------------------------------------------------------------------

# * cohort 1 (2012/13) ----------------------------------------------------

c1y8 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[8] <- nrow(c1y8)
names(c1y8) <- tolower(gsub("_S.*", "", names(c1y8)))

c1y8 <- c1y8[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[8] <- nrow(c1y8)

c1y8$entrydate <- as.Date(c1y8$entrydate, format = "%Y-%m-%d")
c1y8$entry.calyear <- format(c1y8$entrydate, format = "%Y")

lt <- as.POSIXlt(c1y8$entrydate)
c1y8$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c1y8 <- c1y8[order(pupilmatchingrefanonymous, recordstatus, entrydate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2013-01-17")]

pru <- pru[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
pru <- pru[!duplicated(pupilmatchingrefanonymous)]

pru$entrydate <- as.Date(pru$entrydate, format = "%Y-%m-%d")
pru$entry.calyear <- format(pru$entrydate, format = "%Y")

lt <- as.POSIXlt(pru$entrydate)
pru$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

pru$sourcetable <- "PRU"
pru$censusdate <- as.character(pru$censusdate)
c1y8 <- rbind(c1y8, pru, fill = T)
rm(pru)

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2008_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2012/2013"]
ap <- ap[, -1]

names(ap)[c(1, 5:6, 11)] <- c("academicyear", "ageatstartofacademicyear", "monthpartofageatstartofacademicyear",
                              "fsmeligible")

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c1y8 <- rbind(c1y8, ap, fill = T)

rm(ap)

c1y8 <- c1y8[order(pupilmatchingrefanonymous, recordstatus, entrydate)]
c1y8[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y8$pupilmatchingrefanonymous))
final_length_c1[8] <- nrow(c1y8)

# * cohort 2 (2013/14) ----------------------------------------------------

c2y8 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2014.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[9] <- nrow(c2y8)
names(c2y8) <- tolower(gsub("_S.*", "", names(c2y8)))

c2y8$ageatstartofacademicyear <- ifelse(c2y8$monthofbirth < 9, 2013 - c2y8$yearofbirth, 2013 - c2y8$yearofbirth - 1)

c2y8 <- c2y8[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[9] <- nrow(c2y8)

c2y8 <- c2y8[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2013/2014"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2013 - ap$yearofbirth,
                                      2013 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap$censusdate <- as.character(ap$censusdate)
c2y8 <- rbind(c2y8, ap, fill = T)
rm(ap)

c2y8 <- c2y8[order(pupilmatchingrefanonymous, recordstatus)]
c2y8[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y8$pupilmatchingrefanonymous))
final_length_c2[9] <- nrow(c2y8)

gc()

# YEAR 9 --------------------------------------------------------------------

# * cohort 1 (2013/14) ----------------------------------------------------

c1y9 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2014.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[9] <- nrow(c1y9)
names(c1y9) <- tolower(gsub("_S.*", "", names(c1y9)))

c1y9$ageatstartofacademicyear <- ifelse(c1y9$monthofbirth < 9, 2013 - c1y9$yearofbirth, 2013 - c1y9$yearofbirth - 1)

c1y9 <- c1y9[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[9] <- nrow(c1y9)

c1y9 <- c1y9[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2013/2014"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2013 - ap$yearofbirth,
                                      2013 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c1y9 <- rbind(c1y9, ap, fill = T)

rm(ap)

c1y9 <- c1y9[order(pupilmatchingrefanonymous, recordstatus)]
c1y9[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y9$pupilmatchingrefanonymous))
final_length_c1[9] <- nrow(c1y9)

# * cohort 2 (2014/15) ----------------------------------------------------

c2y9 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2015.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[10] <- nrow(c2y9)
names(c2y9) <- tolower(gsub("_S.*", "", names(c2y9)))

c2y9$ageatstartofacademicyear <- ifelse(c2y9$monthofbirth < 9, 2014 - c2y9$yearofbirth, 2014 - c2y9$yearofbirth - 1)

c2y9 <- c2y9[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[10] <- nrow(c2y9)

c2y9 <- c2y9[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2014/2015"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2014 - ap$yearofbirth,
                                      20134 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y9 <- rbind(c2y9, ap, fill = T)

rm(ap)

c2y9 <- c2y9[order(pupilmatchingrefanonymous, recordstatus)]
c2y9[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y9$pupilmatchingrefanonymous))
final_length_c2[10] <- nrow(c2y9)

gc()

# YEAR 10 --------------------------------------------------------------------

# * cohort 1 (2014/15) ----------------------------------------------------

c1y10 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2015.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[10] <- nrow(c1y10)
names(c1y10) <- tolower(gsub("_S.*", "", names(c1y10)))

c1y10$ageatstartofacademicyear <- ifelse(c1y10$monthofbirth < 9,
                                         2014 - c1y10$yearofbirth,
                                         2014 - c1y10$yearofbirth - 1)

c1y10 <- c1y10[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[10] <- nrow(c1y10)

c1y10 <- c1y10[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2014/2015"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2014 - ap$yearofbirth,
                                      2014 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c1y10 <- rbind(c1y10, ap, fill = T)

rm(ap)

c1y10 <- c1y10[order(pupilmatchingrefanonymous, recordstatus)]
c1y10[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y10$pupilmatchingrefanonymous))
final_length_c1[10] <- nrow(c1y10)

# * cohort 2 (2015/16) ----------------------------------------------------

c2y10 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2016.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[11] <- nrow(c2y10)
names(c2y10) <- tolower(gsub("_S.*", "", names(c2y10)))

c2y10$ageatstartofacademicyear <- ifelse(c2y10$monthofbirth < 9,
                                         2015 - c2y10$yearofbirth,
                                         2015 - c2y10$yearofbirth - 1)

c2y10 <- c2y10[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[11] <- nrow(c2y10)

c2y10 <- c2y10[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2015/2016"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2015 - ap$yearofbirth,
                                      2015 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y10 <- rbind(c2y10, ap, fill = T)

rm(ap)

c2y10 <- c2y10[order(pupilmatchingrefanonymous, recordstatus)]
c2y10[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y10$pupilmatchingrefanonymous))
final_length_c2[11] <- nrow(c2y10)

gc()

# YEAR 11 --------------------------------------------------------------------

# * cohort 1 (2015/16) ----------------------------------------------------

c1y11 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2016.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c1[11] <- nrow(c1y11)
names(c1y11) <- tolower(gsub("_S.*", "", names(c1y11)))

c1y11$ageatstartofacademicyear <- ifelse(c1y11$monthofbirth < 9,
                                         2015 - c1y11$yearofbirth,
                                         2015 - c1y11$yearofbirth - 1)

c1y11 <- c1y11[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
subset_length_c1[11] <- nrow(c1y11)

c1y11 <- c1y11[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2015/2016"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2015 - ap$yearofbirth,
                                      2015 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c1y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c1y11 <- rbind(c1y11, ap, fill = T)

rm(ap)

c1y11 <- c1y11[order(pupilmatchingrefanonymous, recordstatus)]
c1y11[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c1y11$pupilmatchingrefanonymous))
final_length_c1[11] <- nrow(c1y11)

# * cohort 2 (2016/17) ----------------------------------------------------

c2y11 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length_c2[12] <- nrow(c2y11)
names(c2y11) <- tolower(gsub("_S.*", "", names(c2y11)))

c2y11$ageatstartofacademicyear <- ifelse(c2y11$monthofbirth < 9,
                                         2016 - c2y11$yearofbirth,
                                         2016 - c2y11$yearofbirth - 1)

c2y11 <- c2y11[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
subset_length_c2[12] <- nrow(c2y11)

c2y11 <- c2y11[order(pupilmatchingrefanonymous, recordstatus)]

ap <- data.table(read.table(
  "P:/Working/WORKING DATA/AP_Census_2014_to_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(ap) <- tolower(gsub("AP_", "", names(ap)))

ap <- ap[ap$acadyr == "2016/2017"]
ap <- ap[, -2]

ap$ageatstartofacademicyear <- ifelse(ap$monthofbirth < 9,
                                      2016 - ap$yearofbirth,
                                      2016 - ap$yearofbirth - 1)

ap$monthpartofageatstartofacademicyear <- as.integer(NA)
ap[monthofbirth <= 8]$monthpartofageatstartofacademicyear <- as.integer(8 - ap[monthofbirth <= 8]$monthofbirth)
ap[monthofbirth == 9]$monthpartofageatstartofacademicyear <- 11
ap[monthofbirth == 10]$monthpartofageatstartofacademicyear <- 10
ap[monthofbirth == 11]$monthpartofageatstartofacademicyear <- 9
ap[monthofbirth == 12]$monthpartofageatstartofacademicyear <- 8

names(ap)[1] <- "academicyear"

ap$sourcetable <- "AP"
ap <- ap[pupilmatchingrefanonymous %in% c2y7$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y11 <- rbind(c2y11, ap, fill = T)

rm(ap)

c2y11 <- c2y11[order(pupilmatchingrefanonymous, recordstatus)]
c2y11[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y11$pupilmatchingrefanonymous))
final_length_c2[12] <- nrow(c2y11)

c2y11$entrydate <- as.Date(c2y11$entrydate)

gc()

# MERGE ----------------------------------------------------------

# Now the lenght vectors are saved, the separate years' censuses are merged and index variables generated.

write.csv(raw_length_c1, file = "OUTPUTS/CHAPTER 4/raw_length_c1.csv")
write.csv(subset_length_c1, file = "OUTPUTS/CHAPTER 4/subset_length_c1.csv")
write.csv(final_length_c1, file = "OUTPUTS/CHAPTER 4/final_length_c1.csv")

write.csv(raw_length_c2, file = "OUTPUTS/CHAPTER 4/raw_length_c2.csv")
write.csv(subset_length_c2, file = "OUTPUTS/CHAPTER 4/subset_length_c2.csv")
write.csv(final_length_c2, file = "OUTPUTS/CHAPTER 4/final_length_c2.csv")

rm(raw_length_c1, subset_length_c1, final_length_c1,
   raw_length_c2, subset_length_c2, final_length_c2)

c1y1$year <- 1
c1y2$year <- 2
c1y3$year <- 3
c1y4$year <- 4
c1y5$year <- 5
c1y6$year <- 6
c1y7$year <- 7
c1y8$year <- 8
c1y9$year <- 9
c1y10$year <- 10
c1y11$year <- 11

cohort1 <- rbind(c1y1, c1y2, c1y3, c1y4, c1y5, c1y6, c1y7, c1y8, c1y9, c1y10, c1y11, fill = T)

rm(c1y1, c1y2, c1y3, c1y4, c1y5, c1y6, c1y7, c1y8, c1y9, c1y10, c1y11)

c2y0$year <- 0
c2y1$year <- 1
c2y2$year <- 2
c2y3$year <- 3
c2y4$year <- 4
c2y5$year <- 5
c2y6$year <- 6
c2y7$year <- 7
c2y8$year <- 8
c2y9$year <- 9
c2y10$year <- 10
c2y11$year <- 11

cohort2 <- rbind(c2y0, c2y1, c2y2, c2y3, c2y4, c2y5, c2y6, c2y7, c2y8, c2y9, c2y10, c2y11, fill = T)

rm(c2y0, c2y1, c2y2, c2y3, c2y4, c2y5, c2y6, c2y7, c2y8, c2y9, c2y10, c2y11)

gc()

length(unique(cohort1$pupilmatchingrefanonymous))
length(unique(cohort2$pupilmatchingrefanonymous))

aggregate(pupilmatchingrefanonymous ~ year, data = cohort1, function(x) length(x))
aggregate(pupilmatchingrefanonymous ~ year, data = cohort1, function(x) length(unique(x)))

aggregate(pupilmatchingrefanonymous ~ year, data = cohort2, function(x) length(x))
aggregate(pupilmatchingrefanonymous ~ year, data = cohort2, function(x) length(unique(x)))

cohort1[, sch_n_in_year := seq_len(.N), by = .(pupilmatchingrefanonymous, year)]
cohort2[, sch_n_in_year := seq_len(.N), by = .(pupilmatchingrefanonymous, year)]

cohort1 <- cohort1[order(pupilmatchingrefanonymous, year, sch_n_in_year)]
cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

nrow(cohort1[is.na(pupilmatchingrefanonymous) | cohort1$pupilmatchingrefanonymous == ""])
nrow(cohort2[is.na(pupilmatchingrefanonymous) | cohort2$pupilmatchingrefanonymous == ""])

# any children in both cohorts?
any(cohort1$pupilmatchingrefanonymous %in% cohort2$pupilmatchingrefanonymous)
any(cohort2$pupilmatchingrefanonymous %in% cohort1$pupilmatchingrefanonymous)

# SAVE --------------------------------------------------------------------

save(cohort1, file = "PROCESSED DATA/cohort1.rda")
save(cohort2, file = "PROCESSED DATA/cohort2.rda")
rm(list = ls()); gc()
