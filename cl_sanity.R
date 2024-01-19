#=========================================
#Core Logic and Parcel Data 
# Sanity checks corelogic & parcel data
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr, sqldf, stringr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")


# data files
cl_file <- "./ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.xlsx"
cl_file2 <- "ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.txt" # above saved to text with "|" sep

# Monterey County Open Data portal - Parcels layer, published April 19, 2022 and last updated 10/18/2022
moparcel_file <- "parcel_data/moco/moco_parcels.geojson" #"moco_parcels.shp"
# Santa Cruz County ODP - Assessor Parcels layer, published April 20, 2021 and last updated October 19, 2023
scparcel_file <- "parcel_data/santacruz/santa_cruz_Assessor_Parcels_63933764656152178/Assessor_Parcels.shp"  #Assessor_Parcels / APN
# San Benito County ODP - Parcels Open Data Layer, published July 24, 2019 and last updated February 22, 2023
sbparcel_file <- "parcel_data/sanbenito/san_benito_Parcels_Open_data/Parcels_Open_Data.shp" #Parcels_Open_Data / APN

# Prelim: take a look at a subset of the data (should be 254,837 properties)

# Read in the full dataset
df0 <- read.xlsx(cl_file, sheet = 1, startRow=1)

#check dimensions to see how many properties
dim(df0) # 254837 rows   232 cols
#[1] 254837    232

#check column names
colnames(df0)

# How many rows total
clrows <- nrow(df0)
clrows #254837

# how many unique APNs?
unique_apns <- length(unique(df0$"APN.(PARCEL.NUMBER.UNFORMATTED)"))
unique_apns #254796
cl_apn_dups <- clrows - unique_apns
cl_apn_dups # 41
unique_apns2 <- length(unique(df0$ORIGINAL.APN))
unique_apns2 #254788

# How many rows do not have geo coords: Lat/Lons
nrow(df0[df0$PARCEL.LEVEL.LATITUDE=='',]) #14076
nrow(df0[df0$PARCEL.LEVEL.LONGITUDE=='',]) #14076


# keep only a subset to free up memory
# Subset the data to only keep parcels with corporate owner
df <- df0[!is.na(df$"OWNER.1.CORPORATE.INDICATOR") & df0$"OWNER.1.CORPORATE.INDICATOR"=="Y",]
dim(df)

table(df0$"OWNER.1.CORPORATE.INDICATOR", useNA = "always")
table(df$"OWNER.1.CORPORATE.INDICATOR", useNA = "always")

# free memory usage
#rm(df0) 
# check that the number of rows in the parcel data match

# Read in parcels
scparcels = read_sf(scparcel_file)
dim(scparcels) # [1] 97021    59
scparcel_rows <- nrow(scparcels)
scparcel_rows

sbparcels = read_sf(sbparcel_file)
dim(sbparcels) # [1]  24175    15
sbparcel_rows <- nrow(sbparcels)
sbparcel_rows

moparcels = read_sf(moparcel_file)
dim(moparcels) # [1]  123588     90
moparcel_rows <- nrow(moparcels)
moparcel_rows

totparcels <- moparcel_rows + sbparcel_rows + scparcel_rows
totparcels #244784

# difference in parcels
clrows - totparcels #10053

# by county
table(df0$SITUS.COUNTY, useNA = "always")
# MONTEREY SAN BENITO SANTA CRUZ       <NA> 
# 132201      22674      99962 

# by county - corporate ownnership
table(df$SITUS.COUNTY, useNA = "always")
#MONTEREY SAN BENITO SANTA CRUZ       <NA> 
#  21324       2965      12030          0 

# Moco Diff
nrow(df0[df0$SITUS.COUNTY=='MONTEREY',]) - moparcel_rows
# 8613 more rows in CL data

# SC Diff
nrow(df0[df0$SITUS.COUNTY=='SANTA CRUZ',]) - scparcel_rows
# 2941 more rows in CL data

# SB Diff
nrow(df0[df0$SITUS.COUNTY=='SAN BENITO',]) - sbparcel_rows
# 1501 FEWER rows in CL data

#So does it add up to 10053?
8613 + 2941 - 1501 #yes

# colnames
colnames(df0)

# what rows are not in Parcel data - can we figure this out
mo_cl <- df0[df0$SITUS.COUNTY=='MONTEREY',]
mo_cl_not_in_pdata <- mo_cl[!(mo_cl$`APN.(PARCEL.NUMBER.UNFORMATTED)` %in% moparcels$APN),]
table(mo_cl_not_in_pdata$RECORD.ACTION.INDICATOR, useNA="always")

# APNs are too tricky for this
#How many can I match or should i do it spatially?
nrow(mo_cl[(mo_cl$`APN.(PARCEL.NUMBER.UNFORMATTED)` %in% moparcels$APN),])
#[1] 122892

# Santa cruz
sc_cl <- df0[df0$SITUS.COUNTY=='SANTA CRUZ',]
nrow(sc_cl[(sc_cl$ORIGINAL.APN %in% scparcels$APNNODASH),])
#96964

# SB not matching directly so...maybe just join all spatially
sb_cl <- df0[df0$SITUS.COUNTY=='SAN BENITO',]
nrow(sb_cl[(sb_cl$`APN.(PARCEL.NUMBER.UNFORMATTED)` %in% sbparcels$APN),])
summary(nchar(sbparcels$APN))
summary(nchar(sb_cl$`APN.(PARCEL.NUMBER.UNFORMATTED)`))


# COORDs
nrow(mo_cl[mo_cl$PARCEL.LEVEL.LATITUDE=='',])
nrow(mo_cl)

# Spatiall Select the Parcel data that
