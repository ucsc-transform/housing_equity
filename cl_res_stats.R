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
cldata <- read.xlsx(cl_file, sheet = 1, startRow=1)

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

#-------------------------------------
# COUNT MISSING VALUES in each column
#-------------------------------------

num_rows <- nrow(cldata_res)
cl_na_counts <- as.data.frame(sapply(cldata_res, function(x) sum(is.na(x))))
colnames(cl_na_counts) <- c("na_count")
cl_na_counts$num_rows <- num_rows
cl_na_counts$pct_missing <- round(100 * (cl_na_counts$na_count / cl_na_counts$num_rows),2)
cl_na_counts$pct_w_data <- round(100-cl_na_counts$pct_missing,2)
cl_na_counts$columns <- rownames(cl_na_counts)
colnames(cl_na_counts)
rownames(cl_na_counts) <- seq(length=nrow(cl_na_counts))
cl_na_counts<-cl_na_counts[c("columns", "na_count", "num_rows", "pct_missing", "pct_w_data")]
head(cl_na_counts)

# Save MISSING VALUES metadata to file
cl_na_counts <- cl_na_counts[order(cl_na_counts$pct_w_data, decreasing = TRUE),]
write_csv(cl_na_counts, "CL_missing_data_in_residential_subset.csv")
