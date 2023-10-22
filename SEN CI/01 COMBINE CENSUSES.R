# setwd -------------------------------------------------------------------

setwd("P:/Working/Matt")

lib_base = "P:/Working/Matt/r-library/"
assign(".lib.loc", c(.libPaths(), lib_base), envir = environment(.libPaths))
rm(lib_base)

library("data.table")
library("plyr")

raw_length <- as.integer(rep(NA, 12))
subset_length <- as.integer(rep(NA, 12))
final_length <- as.integer(rep(NA, 12))

# ANCHOR (Recetption) -------------------------------------------------------------------

c2y0 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_PLASC_2006.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length[1] <- nrow(c2y0)

names(c2y0) <- tolower(gsub("_S.*", "", names(c2y0)))

c2y0 <- c2y0[ncyearactual == "R" |
               (ncyearactual == "X" & ageatstartofacademicyear == 4)]
subset_length[1] <- nrow(c2y0)

c2y0 <- c2y0[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]
length(unique(c2y0$pupilmatchingrefanonymous))

c2y0$entrydate <- as.Date(c2y0$entrydate, format = "%Y-%m-%d")
c2y0$entry.calyear <- format(c2y0$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y0$entrydate)
c2y0$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c2y0)[names(c2y0) == "languagegroup"] <- "languagegroupminor"

c2y0[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length[1] <- nrow(c2y0)

gc()

# YEAR 1 --------------------------------------------------------------------

c2y1 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2007.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length[2] <- nrow(c2y1)

names(c2y1) <- tolower(gsub("_S.*", "", names(c2y1)))

c2y1 <- c2y1[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[2] <- nrow(c2y1)

c2y1 <- c2y1[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]
length(unique(c2y1$pupilmatchingrefanonymous))

c2y1$entrydate <- as.Date(c2y1$entrydate, format = "%Y-%m-%d")
c2y1$entry.calyear <- format(c2y1$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y1$entrydate)
c2y1$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

names(c2y1)[names(c2y1) == "languagegroup"] <- "languagegroupminor"

c2y1[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

final_length[2] <- nrow(c2y1)

gc()

# YEAR 2 --------------------------------------------------------------------

c2y2 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2008.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

raw_length[3] <- nrow(c2y2)

names(c2y2) <- tolower(gsub("_S.*", "", names(c2y2)))

c2y2 <- c2y2[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[3] <- nrow(c2y2)

c2y2$entrydate <- as.Date(c2y2$entrydate, format = "%Y-%m-%d")
c2y2$entry.calyear <- format(c2y2$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y2$entrydate)
c2y2$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y2 <- c2y2[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]
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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c2y2 <- rbind(c2y2, ap, fill = T)

rm(ap)

c2y2 <- c2y2[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y2[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y2$pupilmatchingrefanonymous))

names(c2y2)[names(c2y2) == "languagegroup"] <- "languagegroupminor"
final_length[3] <- nrow(c2y2)

gc()

# YEAR 3 --------------------------------------------------------------------

c2y3 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2009.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[4] <- nrow(c2y3)
names(c2y3) <- tolower(gsub("_S.*", "", names(c2y3)))

c2y3 <- c2y3[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[4] <- nrow(c2y3)

c2y3$entrydate <- as.Date(c2y3$entrydate, format = "%Y-%m-%d")
c2y3$entry.calyear <- format(c2y3$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y3$entrydate)
c2y3$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y3 <- c2y3[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]

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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]

ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap$censusdate <- as.character(ap$censusdate)

c2y3 <- rbind(c2y3, ap, fill = T)

rm(ap)

c2y3 <- c2y3[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y3[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y3$pupilmatchingrefanonymous))
final_length[4] <- nrow(c2y3)

gc()

# YEAR 4 --------------------------------------------------------------------

c2y4 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2010.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[5] <- nrow(c2y4)
names(c2y4) <- tolower(gsub("_S.*", "", names(c2y4)))

c2y4 <- c2y4[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[5] <- nrow(c2y4)

c2y4$entrydate <- as.Date(c2y4$entrydate, format = "%Y-%m-%d")
c2y4$entry.calyear <- format(c2y4$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y4$entrydate)
c2y4$entry.acayear <- lt$year + (lt$mo >= 9) + 1900
rm(lt)

c2y4 <- c2y4[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2010-01-21")]
pru <- pru[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y4 <- rbind(c2y4, ap, fill = T)

rm(ap)

c2y4 <- c2y4[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y4[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y4$pupilmatchingrefanonymous))
final_length[5] <- nrow(c2y4)

gc()

# YEAR 5 --------------------------------------------------------------------

c2y5 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2011.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[6] <- nrow(c2y5)
names(c2y5) <- tolower(gsub("_S.*", "", names(c2y5)))

c2y5 <- c2y5[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[6] <- nrow(c2y5)

c2y5$entrydate <- as.Date(c2y5$entrydate, format = "%Y-%m-%d")
c2y5$entry.calyear <- format(c2y5$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y5$entrydate)
c2y5$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y5 <- c2y5[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2011-01-20")]

pru <- pru[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)

c2y5 <- rbind(c2y5, ap, fill = T)

rm(ap)

c2y5 <- c2y5[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y5[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y5$pupilmatchingrefanonymous))
final_length[6] <- nrow(c2y5)

gc()

# YEAR 6 --------------------------------------------------------------------

c2y6 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2012.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[7] <- nrow(c2y6)
names(c2y6) <- tolower(gsub("_S.*", "", names(c2y6)))

c2y6 <- c2y6[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[7] <- nrow(c2y6)

c2y6$entrydate <- as.Date(c2y6$entrydate, format = "%Y-%m-%d")
c2y6$entry.calyear <- format(c2y6$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y6$entrydate)
c2y6$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y6 <- c2y6[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2012-01-19")]

pru <- pru[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y6 <- rbind(c2y6, ap, fill = T)

rm(ap)

c2y6 <- c2y6[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y6[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y6$pupilmatchingrefanonymous))
final_length[7] <- nrow(c2y6)

gc()

# YEAR 7 ------------------------------------------------------------------

c2y7 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_CENSUS_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[8] <- nrow(c2y7)
names(c2y7) <- tolower(gsub("_S.*", "", names(c2y7)))

c2y7 <- c2y7[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[8] <- nrow(c2y7)

c2y7$entrydate <- as.Date(c2y7$entrydate, format = "%Y-%m-%d")
c2y7$entry.calyear <- format(c2y7$entrydate, format = "%Y")

lt <- as.POSIXlt(c2y7$entrydate)
c2y7$entry.acayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

c2y7 <- c2y7[order(pupilmatchingrefanonymous, recordstatus, entrydate, leavingdate)]

pru <- data.table(read.table(
  "P:/Working/WORKING DATA/PRU_Census_2010_to_2013.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))

names(pru) <- tolower(gsub("PRU_", "", names(pru)))

pru$censusdate <- as.Date(pru$censusdate, format = "%Y-%m-%d")
pru <- pru[censusdate == as.Date("2013-01-17")]

pru <- pru[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
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
ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

ap$censusdate <- as.character(ap$censusdate)
c2y7 <- rbind(c2y7, ap, fill = T)

rm(ap)

c2y7 <- c2y7[order(pupilmatchingrefanonymous, entrydate, leavingdate)]
c2y7[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y7$pupilmatchingrefanonymous))
final_length[8] <- nrow(c2y7)

# YEAR 8 --------------------------------------------------------------------

c2y8 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2014.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[9] <- nrow(c2y8)
names(c2y8) <- tolower(gsub("_S.*", "", names(c2y8)))

c2y8$ageatstartofacademicyear <- ifelse(c2y8$monthofbirth < 9, 2013 - c2y8$yearofbirth, 2013 - c2y8$yearofbirth - 1)

c2y8 <- c2y8[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[9] <- nrow(c2y8)

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
ap <- ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]
ap$censusdate <- as.character(ap$censusdate)
c2y8 <- rbind(c2y8, ap, fill = T)
rm(ap)

c2y8[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y8$pupilmatchingrefanonymous))
final_length[9] <- nrow(c2y8)

gc()

# YEAR 9 --------------------------------------------------------------------

c2y9 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2015.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[10] <- nrow(c2y9)
names(c2y9) <- tolower(gsub("_S.*", "", names(c2y9)))

c2y9$ageatstartofacademicyear <- ifelse(c2y9$monthofbirth < 9, 2014 - c2y9$yearofbirth, 2014 - c2y9$yearofbirth - 1)

c2y9 <- c2y9[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[10] <- nrow(c2y9)

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
ap <- ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y9 <- rbind(c2y9, ap, fill = T)

rm(ap)

c2y9 <- c2y9[order(pupilmatchingrefanonymous)]

c2y9[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y9$pupilmatchingrefanonymous))
final_length[10] <- nrow(c2y9)

gc()

# YEAR 10 --------------------------------------------------------------------

c2y10 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2016.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[11] <- nrow(c2y10)
names(c2y10) <- tolower(gsub("_S.*", "", names(c2y10)))

c2y10$ageatstartofacademicyear <- ifelse(c2y10$monthofbirth < 9,
                                         2015 - c2y10$yearofbirth,
                                         2015 - c2y10$yearofbirth - 1)

c2y10 <- c2y10[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[11] <- nrow(c2y10)

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
ap <- ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y10 <- rbind(c2y10, ap, fill = T)

rm(ap)

c2y10 <- c2y10[order(pupilmatchingrefanonymous)]

c2y10[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y10$pupilmatchingrefanonymous))
final_length[11] <- nrow(c2y10)

gc()

# YEAR 11 --------------------------------------------------------------------

c2y11 <- data.table(read.table(
  "P:/Working/WORKING DATA/Spring_Census_2017.txt",
  header = T,
  sep = "\t",
  stringsAsFactors = F
))
raw_length[12] <- nrow(c2y11)
names(c2y11) <- tolower(gsub("_S.*", "", names(c2y11)))

c2y11$ageatstartofacademicyear <- ifelse(c2y11$monthofbirth < 9,
                                         2016 - c2y11$yearofbirth,
                                         2016 - c2y11$yearofbirth - 1)

c2y11 <- c2y11[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
subset_length[12] <- nrow(c2y11)

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
ap <- ap <- ap[pupilmatchingrefanonymous %in% c2y0$pupilmatchingrefanonymous]
ap <- ap[!duplicated(pupilmatchingrefanonymous)]

c2y11 <- rbind(c2y11, ap, fill = T)

rm(ap)

c2y11 <- c2y11[order(pupilmatchingrefanonymous)]

c2y11[, sch_n_in_year := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]
length(unique(c2y11$pupilmatchingrefanonymous))
final_length[12] <- nrow(c2y11)

c2y11$entrydate <- as.Date(c2y11$entrydate)

gc()

# MERGE ----------------------------------------------------------

write.csv(raw_length, file = "OUTPUTS/SEN CI/raw_length.csv")
write.csv(subset_length, file = "OUTPUTS/SEN CI/subset_length.csv")
write.csv(final_length, file = "OUTPUTS/SEN CI/final_length.csv")

rm(raw_length, subset_length, final_length)

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

length(unique(cohort2$pupilmatchingrefanonymous))

aggregate(pupilmatchingrefanonymous ~ year, data = cohort2, function(x) length(x))
aggregate(pupilmatchingrefanonymous ~ year, data = cohort2, function(x) length(unique(x)))

cohort2[, sch_n_in_year := seq_len(.N), by = .(pupilmatchingrefanonymous, year)]
cohort2 <- cohort2[order(pupilmatchingrefanonymous, year, sch_n_in_year)]

nrow(cohort2[is.na(pupilmatchingrefanonymous) | cohort2$pupilmatchingrefanonymous == ""]) 

# SAVE --------------------------------------------------------------------

save(cohort2, file = "PROCESSED DATA/SEN CI/cohort2.rda")
