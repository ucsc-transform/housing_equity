#=========================================
# Joining Corelogic Data to Parcel Data
# to map publicly owned parcels
## Note
# This requires a lot of mem to run
# so make sure to run this first 
# library(usethis) 
# usethis::edit_r_environ()
# R_MAX_VSIZE=100Gb
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr, sqldf, stringr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")

# Corelogic data file with parcel info - public parcels only
clfile_pub <- "mobay_cl_and_parcel_fp_polys_data.geojson"

# all corelogic parcel data
cl_file <- "./cl_indata/ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.xlsx"


# Read in the data
clpub_parcels <- st_read(clfile_pub)
cldata <- read.xlsx(cl_file, sheet = 1, startRow=1)

# Mostly nonempty and potentially useful cols:
cl_keepcols <- c("CLIP",
                 "APN.(PARCEL.NUMBER.UNFORMATTED)", "ORIGINAL.APN","ONLINE.FORMATTED.PARCEL.ID", 
                 "FIPS.CODE", "CENSUS.ID",
                 "NEIGHBORHOOD.DESCRIPTION", "PROPERTY.INDICATOR.CODE",  
                 "LAND.USE.CODE", "COUNTY.USE.DESCRIPTION" ,"STATE.USE.DESCRIPTION", 
                 "ZONING.CODE", "ZONING.CODE.DESCRIPTION", 
                 "LAND.SQUARE.FOOTAGE", "ACRES", 
                 "MUNICIPALITY.NAME", 
                 "SITUS.COUNTY", "SITUS.CITY",  "TAX.RATE.AREA.CODE", "SITUS.STREET.ADDRESS",
                 "OWNER.1.FULL.NAME","OWNER.1.CORPORATE.INDICATOR",
                 "MAILING.STREET.ADDRESS", "MAILING.CITY", "MAILING.STATE", "MAILING.ZIP.CODE",
                 "TOTAL.VALUE.CALCULATED", "LAND.VALUE.CALCULATED", "IMPROVEMENT.VALUE.CALCULATED",
                 "PARCEL.LEVEL.LONGITUDE","PARCEL.LEVEL.LATITUDE" )

cl_keepcols <- c("CLIP",
                  "PROPERTY.INDICATOR.CODE",  
                 "LAND.USE.CODE",
                 "PARCEL.LEVEL.LONGITUDE", "PARCEL.LEVEL.LATITUDE")


cldata <- cldata[cl_keepcols]

#Subtract the cl_pub_parcels via CLIP id
dim(cldata)
cldata <- cldata[!cldata$CLIP %in% clpub_parcels$CLIP,]
dim(cldata)
dim(clpub_parcels)

# read in Look up tables
luse_df <- read_csv("./CL_META/LUSE-Table 1.csv")
prind_df <- read_csv("./CL_META/PRIND-Table 1.csv")

# get rid of unused cols
luse_df <- luse_df[c(2,3)]
prind_df <- prind_df[c(2,3)]

# rename description col for simplicity and for join
colnames(luse_df) <- c("LAND.USE.CODE", "luse_desc")
colnames(prind_df) <- c("PROPERTY.INDICATOR.CODE","prop_desc")

# Join to get text desc instead of just codes
dim(cldata)
cldata <- base::merge(cldata, luse_df, by="LAND.USE.CODE", all.x=TRUE)
cldata <- base::merge(cldata, prind_df, by="PROPERTY.INDICATOR.CODE", all.x=TRUE)
dim(cldata) # make sure no new rows, only two new cols

# Take a look - we don't want unexplaniend NAs or '' values
doh <-as.data.frame(table(cldata$luse_desc, useNA="always"))
doh2 <- as.data.frame(table(cldata$prop_desc, useNA="always"))

###############################################
# SUBSET OUT TO KEEP RESIDENTIAL PARCELS ONLY
###############################################

# keep CLdata rows with the following land use (luse_desc) vales
luse_resvals <- c(
  "SFR", "CONDOMINIUM", "RURAL HOMESITE", "TIME SHARE", "MOBILE HOME",
  "DUPLEX"," TOWNHOUSE/ROWHOUSE", "MULTI FAMILY DWELLING", "RESIDENTIAL (NEC)",
  "RESIDENTIAL LOT", "RESIDENTIAL ACREAGE", "APARTMENT, MOBILE HOME LOT",
  "QUADRUPLEX", "TRIPLEX", "MOBILE HOME PARK", "MULTI FAMILY LOT",
  "STORES & RESIDENTIAL", "FRAT/SORORITY HOUSE", "MULTI FAMILY ACREAGE"
)

# keep CLdata rows with the following property use (prop_desc) vales
propdesc_resvals <- c("APARTMENT", "CONDOMINIUM", 
                      "DUPLEX", "SINGLE FAMILY RESIDENCE")

cldata_res <- cldata[cldata$prop_desc %in% propdesc_resvals | 
                       cldata$luse_desc %in% luse_resvals,]
table(cldata_res$luse_desc, useNA="always")
table(cldata_res$prop_desc, useNA="always")
dim(cldata_res)

###############################################
# Make the residential data spatial points
###############################################
# drop rows with no coords
summary(cldata_res$PARCEL.LEVEL.LATITUDE) # NAs = 13233 out of 206103 (6.4%)
cldata_res_withlatlon <- cldata_res[!is.na(cldata_res$PARCEL.LEVEL.LATITUDE),]
dim(cldata_res_withlatlon)

cldata_res_spdf <- st_as_sf(cldata_res_withlatlon, 
                     coords = c("PARCEL.LEVEL.LONGITUDE", "PARCEL.LEVEL.LATITUDE"), 
                     crs=4326)
dim(cldata_res_spdf)
plot(cldata_res_spdf$geometry)

# remove unneeded dfs taking memory
rm(cldata, cldata_res, cldata_res_withlatlon)
###########################################################################
# Count the residential parcel points within 1/10, 1/4 mile, 1/5 mile of 
# all of our publicly owned parcels
########################################################################

# define constants -for distances in meters
tenth_mile_in_meters = 161
qrt_mile_in_meters = 402
half_mile_in_meters = 805

# crs transformations to 3310 (units meters)
cldata_res_spdf_3310 <- st_transform(cldata_res_spdf, 3310)
clpub_parcels_3310 <- st_transform(clpub_parcels, 3310)

# buffer parcel polys
clpub_parcels_3310_buftenth <- st_buffer(clpub_parcels_3310, tenth_mile_in_meters)
clpub_parcels_3310_bufqtr <- st_buffer(clpub_parcels_3310, qrt_mile_in_meters)
clpub_parcels_3310_bufhalf <- st_buffer(clpub_parcels_3310, half_mile_in_meters)

#checks - make sure all have same number of rows
dim(clpub_parcels)
dim(clpub_parcels_3310)
dim(clpub_parcels_3310_buftenth)
dim(clpub_parcels_3310_bufqtr)
dim(clpub_parcels_3310_bufhalf)

#qtm(clpub_parcels_3310_bufhalf) + qtm(clpub_parcels_3310) # looks good

# count residential parcel POINTS within the public owned parcel buffer POLYS
# fpcounts <- st_within(fprint_ctrs, fbuffers_clipX, sparse = FALSE)
clpub_parcels_3310_buftenth <-clpub_parcels_3310_buftenth %>% st_cast("POLYGON")
class(clpub_parcels_3310_buftenth$geometry)
clpub_parcels_3310_buftenth <- st_make_valid(clpub_parcels_3310_buftenth)

#take a look at buffers
tmap_mode('view')
qtm(clpub_parcels_3310_buftenth)
nrow(clpub_parcels_3310_buftenth)
nrow(clpub_parcels)

# this next operation returns a matrix with 
# nrow(cldata_res_spdf_3310) * nrow(clpub_parcels_3310_buftenth) elements
res10_counts <- st_within(cldata_res_spdf_3310, clpub_parcels_3310_buftenth, sparse = FALSE)

# add the counts to the publicly owned parcel POLYGON (not buf) data
clpub_parcels_3310 <- clpub_parcels_3310 %>%
  mutate(res10th_counts = apply(res10_counts, 2, sum))

dim(clpub_parcels_3310)

# check this one should have 4 res parcels within buffer
# pdata_APN = 07611610
qtm(clpub_parcels_3310_buftenth[clpub_parcels_3310_buftenth$pdata_APN=='07611610',]) + 
  qtm(clpub_parcels_3310[clpub_parcels_3310$pdata_APN=='07611610',]) +
  qtm(cldata_res_spdf_3310)


# ok it worked!

# clean up
rm(res10_counts)

# Do the other buf counts

# Quarter mile buffer
clpub_parcels_3310_bufqtr <-clpub_parcels_3310_bufqtr %>% st_cast("POLYGON")
class(clpub_parcels_3310_bufqtr$geometry)
clpub_parcels_3310_bufqtr <- st_make_valid(clpub_parcels_3310_bufqtr)

# this next operation returns a matrix with 
# nrow(cldata_res_spdf_3310) * nrow(clpub_parcels_3310_bufqtr) elements
resqtr_counts <- st_within(cldata_res_spdf_3310, clpub_parcels_3310_bufqtr, sparse = FALSE)

# add the counts to the publicly owned parcel data
clpub_parcels_3310 <- clpub_parcels_3310 %>%
  mutate(resqtr_counts = apply(resqtr_counts, 2, sum))

# checks
colnames(clpub_parcels_3310)
clpub_parcels_3310[clpub_parcels_3310$pdata_APN=='07611610',]$resqtr_counts

qtm(clpub_parcels_3310_bufqtr[clpub_parcels_3310_bufqtr$pdata_APN=='07611610',]) + 
  qtm(clpub_parcels_3310[clpub_parcels_3310$pdata_APN=='07611610',]) +
  qtm(cldata_res_spdf_3310)

# cleanup
rm(resqtr_counts)

# Half mile buffer
clpub_parcels_3310_bufhalf <-clpub_parcels_3310_bufhalf %>% st_cast("POLYGON")
class(clpub_parcels_3310_bufhalf$geometry)
clpub_parcels_3310_bufhalf <- st_make_valid(clpub_parcels_3310_bufhalf)

# this next operation returns a matrix with 
# nrow(cldata_res_spdf_3310) * nrow(clpub_parcels_3310_bufqtr) elements
reshalf_counts <- st_within(cldata_res_spdf_3310, clpub_parcels_3310_bufhalf, sparse = FALSE)

# add the counts to the publicly owned parcel data
clpub_parcels_3310 <- clpub_parcels_3310 %>%
  mutate(reshalf_counts = apply(reshalf_counts, 2, sum))

#checks
dim(clpub_parcels_3310)
# check how many to expect
clpub_parcels_3310[clpub_parcels_3310$pdata_APN=='07611610',]$reshalf_counts

# plot and count
qtm(clpub_parcels_3310_bufhalf[clpub_parcels_3310_bufhalf$pdata_APN=='07611610',]) + 
  qtm(clpub_parcels_3310[clpub_parcels_3310$pdata_APN=='07611610',]) +
  qtm(cldata_res_spdf_3310)

# deletee huge matrix
rm(reshalf_counts)

# Add binary vars for filtering

clpub_parcels_3310$hasResTenth <- if_else(clpub_parcels_3310$res10th_counts==0, "NO", "YES")
clpub_parcels_3310$hasResQtr <- if_else(clpub_parcels_3310$resqtr_counts==0, "NO", "YES")
clpub_parcels_3310$hasResHalf <- if_else(clpub_parcels_3310$reshalf_counts==0, "NO", "YES")

# Final checks
dim(clpub_parcels_3310)
colnames(clpub_parcels_3310)

summary(clpub_parcels_3310$res10th_counts)
summary(clpub_parcels_3310$resqtr_counts)
summary(clpub_parcels_3310$reshalf_counts)

####################
# SAVE TO FILE
####################

clpub_parcels <- st_transform(clpub_parcels_3310, 4326)
write_sf(obj = clpub_parcels, dsn = "mobay_cl_pubparcels_w_fpcounts_and_rescounts.geojson")
