#=========================================
# Sample Code
# for exploring corelogic & parcel data
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")

# take a look at a subset of the data
df0 <- read.xlsx("./ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.xlsx", 
                sheet = 1, startRow=1, rows=c(1:100000))

# take a look at the column names
colnames(df)

# keep these columns for now
keepcols <- c("CLIP", "ORIGINAL.APN", "ONLINE.FORMATTED.PARCEL.ID","MUNICIPALITY.NAME", "ZONING.CODE.DESCRIPTION",
              "NUMBER.OF.BUILDINGS" , "LIVING.SQUARE.FEET.-.ALL.BUILDINGS", "LAND.SQUARE.FOOTAGE",  
              "ZONING.CODE", "ZONING.CODE.DESCRIPTION", "LAND.USE.CODE",  
              "COUNTY.USE.DESCRIPTION" ,"STATE.USE.DESCRIPTION", 
              "MUNICIPALITY.NAME", "COUNTY.USE.DESCRIPTION", 
              "SITUS.COUNTY", "SITUS.CITY", "SITUS.ZIP.CODE",
              "OWNER.1.CORPORATE.INDICATOR","OWNER.1.FULL.NAME",
              "PARCEL.LEVEL.LONGITUDE","PARCEL.LEVEL.LATITUDE" )

# subset the data    
df <- df0[keepcols]

# NOTES
# Can grep city or state or county from owner full name
# but otherwise hard to get "public" designation so maybe join to parcel data which may have that info

# df1 <- df %>%
#   mutate(public_owner = dplyr::case_when(FID_BBG2012_Clip %in% c(1:4455, 5736, 10220:10330) ~ "Rotterdam", 
#                                  FID_BBG2012_Clip %in% c(4744:5655, 10335) ~ "The_Hague",
#                                  FID_BBG2012_Clip %in% c(5861:7015, 10381:10385) ~ "Utrecht",
#                                  FID_BBG2012_Clip %in% c(7110:10190, 10447:10488) ~ "Amsterdam",
#                                  TRUE ~ "no"))

#
# Parcel Data
#

# read in the Monterey County Parcel data (with geometry removed in QGIS)
p1 <- read.csv("./mocoparcels.csv", nrows = 100000, sep="\t")

# Take a look
View(p1)

colnames(p1)


#
# Merge Parcel Data to CoreLogic data
#
df2 <- base::merge(df, p1, by.x="ONLINE.FORMATTED.PARCEL.ID", by.y="APN_FORMAT")
summary(df2$PARCEL.LEVEL.LONGITUDE)
summary(df2$PARCEL.LEVEL.LATITUDE)


# make it a spatial points data frame
df2 <- df2[!is.na(df2$PARCEL.LEVEL.LATITUDE),] # remove rows where no coords
spatial_df <- st_as_sf(df2, coords = c("PARCEL.LEVEL.LONGITUDE", "PARCEL.LEVEL.LATITUDE"), crs=4326)

# Map a subset it to check
corpown <- spatial_df[spatial_df$OWNER.1.CORPORATE.INDICATOR=="Y",]
tmap_mode('view')

qtm(corpown)
