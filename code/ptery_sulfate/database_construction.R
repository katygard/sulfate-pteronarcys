#build RSQlite database

#load packages
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(lessR)
library(tibble)


### Creating the database

#establish a database connection
ptery_sulf_db <- dbConnect(drv = RSQLite::SQLite(), 
                        "../../raw_data/ptery_sulf.db")

#load in USUAL .csv files
USUAL_start <- read.csv("../../raw_data/2020-07_USUAL_results.csv")
USUAL_end <- read.csv("../../raw_data/2021-01_USUAL_results.csv")
#original USUAL docs had "<" when measurement values were below detection limits (typically <0.001)
#I replaced "<" with empty cells for R purposes. Not sure if empty/blank cells are more appropriate than 0's?


#combine USUAL files for database and clean data
USUAL_start <- USUAL_start %>%
  rename(USU_ID = ï..USU_ID,
         Sulfate = Sulfate.Sulfur,
         arsenic = As) #renaming columns that imported into R poorly

USUAL_start <- USUAL_start[1:37, ] #deleting empty rows that were for some reason included in the csv import

USUAL_start <- add_column(USUAL_start, date = rep(as.Date("2020-07-27"),37), .after = 1) #add in date of water sample to differentiate between start and end

USUAL_end <- USUAL_end %>%
  rename(USU_ID = ï..USU_ID,
         Sulfate = Sulfate.Sulfur,
         arsenic = As, #for some reason RSQLite won't create table with this column name as As
         carbonate = Carbonate_mmolc.L,
         bicarbonate = Bicarbonate_mmolc.L) #the "." in the original name not SQL friendly

USUAL_end <- USUAL_end[1:36, ] #USUAL_end has one fewer rows because we did not test the well water a second time

USUAL_end <- add_column(USUAL_end, date = rep(as.Date("2021-01-07"),36), .after = 1)

empty_cols <- c("carbonate", "bicarbonate") #adding empty columns before vertically merging
USUAL_start[, empty_cols] <- NA

#merge files vertically
usual <- Merge(USUAL_start, USUAL_end, by="rows")

# add the USUAL dataframe into our database.
# write table in database
dbExecute(ptery_sulf_db, "CREATE TABLE usual (
          USU_ID float NOT NULL PRIMARY KEY,
          date text,
          Trough integer,
          Tank integer,
          Chloride float, 
          Al float,
          arsenic float,
          B float,
          Ba float,
          Ca float,
          Cd float,
          Co float,
          Cr float,
          Cu float,
          Fe float,
          K float,
          Mg float,
          Mn float,
          Mo float,
          Na float,
          Ni float,
          P float,
          Pb float,
          S float,
          Se float,
          Si float,
          Sr float,
          Zn float,
          Sulfate float,
          carbonate float,
          bicarbonate float);")

dbWriteTable(ptery_sulf_db, "usual", usual, append = TRUE)

# check to see that it worked before moving on to the other 3 tables.
dbListTables(ptery_sulf_db) 
dbGetQuery(ptery_sulf_db, "SELECT * FROM usual LIMIT 10;")

#add other .csv files
surv_wide <- read.csv("../../raw_data/weekly_survival.csv")

#recode to r-friendly format
survival <- gather(surv_wide, sdate, survival, na.rm = F, X28.Jul:X28.Dec)
survival$Sulfate.mg.L <- as.character(survival$Sulfate.mg.L)
survival <- survival %>%
  mutate(sdate2 = dplyr::recode(sdate,                     # note use of dplyr::
                                X28.Jul  = "2020-07-28",
                                X3.Aug = "2020-08-03",
                                X10.Aug = "2020-08-10",
                                X17.Aug = "2020-08-17",
                                X24.Aug = "2020-08-24",
                                X31.Aug = "2020-08-31",
                                X8.Sep = "2020-09-08",
                                X14.Sep = "2020-09-14",
                                X21.Sep = "2020-09-21",
                                X28.Sep = "2020-09-28",
                                X5.Oct = "2020-10-05",
                                X12.Oct = "2020-10-12",
                                X19.Oct = "2020-10-19",
                                X26.Oct = "2020-10-26",
                                X2.Nov = "2020-11-02",
                                X9.Nov = "2020-11-09",
                                X16.Nov = "2020-11-16",
                                X23.Nov = "2020-11-23",
                                X30.Nov = "2020-11-30",
                                X7.Dec = "2020-12-07",
                                X14.Dec = "2020-12-14",
                                X21.Dec = "2020-12-21",
                                X28.Dec = "2020-12-28"))


with(survival, table(sdate2, sdate)) #check to make sure therer are 36 values at each old date and recoded date

survival <- survival %>%
  rename(sulfate_trtment = Sulfate.mg.L,
         date = sdate2,
         set_temp = Temperature,
         tank = Tank) #renaming columns that imported into R poorly

survival <- survival[,c(2:4,6:7)]
survival$survival_ID <- 1:nrow(survival) #add a serial no. column to act as unique primary key
survival <- survival[, c("survival_ID", "date", "tank", "set_temp", "sulfate_trtment", "survival")] #reordering columns

# Write the survival table into database.
dbExecute(ptery_sulf_db, "CREATE TABLE survival (
          survival_ID integer NOT NULL PRIMARY KEY,
          date text,
          tank integer,
          set_temp integer,
          sulfate_trtment integer, 
          survival integer,
          FOREIGN KEY(tank) REFERENCES usual(tank));")

dbWriteTable(ptery_sulf_db, "survival", survival, append = TRUE)
dbListTables(ptery_sulf_db) 
dbGetQuery(ptery_sulf_db, "SELECT * FROM survival LIMIT 10;")


#add weekly water measurement data. 
## NOTE: This data has not all been entered yet, so this data file is really incomplete. 
## Will need to go back and re-add to database when all data has been entered

#water_meas <- read.csv("../../../../Research/experiments/sulfate/pteronarcys/raw_data/weekly_water_measurements.csv")

#rename some columns
#water_meas <- water_meas %>%
#  rename(date = ï..DATE,
#         measured_temp = TEMP..C.,
#         DO_percent = DO.,
#         DO_mgL = DO..mg.L.,
#         conductivity = CONDUCTIVITY,
#         tank = TANK)

#add a serial no. column to act as unique primary key, put it as first column
#water_meas <- add_column(water_meas, water_ID = 1:nrow(water_meas), .after = 0)

# create water_measures table in SQLite.
# dbExecute(ptery_sulf_db, "CREATE TABLE water_measures (
#          water_ID integer NOT NULL PRIMARY KEY,
#          date text,
#          tank integer,
#          measured_temp float,
#         DO_percent float,
# DO_mgL float,
# conductivity integer,
# FOREIGN KEY(tank) REFERENCES survival(tank));")

# dbWriteTable(ptery_sulf_db, "water_measures", water_meas, append = TRUE)
# dbListTables(ptery_sulf_db) 
# dbGetQuery(ptery_sulf_db, "SELECT * FROM water_measures LIMIT 10;")

# import head width data.
head_width <- read.csv("../../raw_data/head-width_measurements.csv")

#rename some columns
head_width <- head_width %>%
  rename(meas_date = DATE,
         week = WEEK,
         observer = MEASURED_BY,
         bug_ID = ID,
         tank = TANK,
         head_width = HEAD_WIDTH)

#add a serial no. column to act as unique primary key, put it as first column
head_width <- add_column(head_width, meas_ID = 1:nrow(head_width), .after = 0)

#eliminate the "ZOOM" and "MICROMETER UNITS" columns (not needed for db)
head_width <- head_width[,c(1:4, 6:7,10)]

# create head_width table in SQLite.
dbExecute(ptery_sulf_db, "CREATE TABLE head_width (
          meas_ID integer NOT NULL PRIMARY KEY,
          week integer,
          meas_date text,
          observer text,
          tank integer,
          bug_ID varchar(4),
          head_width float,
          FOREIGN KEY(tank) REFERENCES survival(tank));")

dbWriteTable(ptery_sulf_db, "head_width", head_width, append = TRUE)
dbListTables(ptery_sulf_db) 
dbGetQuery(ptery_sulf_db, "SELECT * FROM head_width LIMIT 10;")
