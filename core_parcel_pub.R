#=========================================
# Joining Corelogic Data to Parcel Data
# to map publicly owned parcles
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr, sqldf, stringr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")

###############################################################################
# DATA FILES
###############################################################################
# Corelogic data files
cl_file <- "./cl_indata/ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.xlsx"

# Monterey County Open Data portal - Parcels layer, published April 19, 2022 and last updated 10/18/2022
moparcel_file <- "parcel_data/moco/moco_parcels.geojson" #"moco_parcels.shp"
# Santa Cruz County ODP - Assessor Parcels layer, published April 20, 2021 and last updated October 19, 2023
scparcel_file <- "parcel_data/santacruz/santa_cruz_Assessor_Parcels_63933764656152178/Assessor_Parcels.shp"  #Assessor_Parcels / APN
# San Benito County ODP - Parcels Open Data Layer, published July 24, 2019 and last updated February 22, 2023
sbparcel_file <- "parcel_data/sanbenito/san_benito_Parcels_Open_data/Parcels_Open_Data.shp" #Parcels_Open_Data / APN


################################################################################
# Read in the full Corelogic (CL) dataset
## Prelim: take a look at a subset of the data (should be 254,837 properties)
################################################################################
df0 <- read.xlsx(cl_file, sheet = 1, startRow=1)


#check dimensions to see how many properties
dim(df0) # 254837 rows   232 cols
# NOTE: there are 244784 parcels in the 3 county parcel files
#254837 - 244784 = 10053 more rows in CL data

# create a metadata dataframe to keep track of key info
clparcels_metadata <- data.frame(
  "dataset" = character(),
  "var" = character(),
  "subset" = character(),
  "val" = integer(),
  stringsAsFactors = FALSE
)
clparcels_metadata
# add meta function to make easier
add_meta <- function(dataset, var, subset, val, add=FALSE){
  mrow <- list(dataset, var, subset, val)
  # use double arrow assignment to change global
  if (add==TRUE) {
    print("Adding row to metadata df...")
    clparcels_metadata[nrow(clparcels_metadata)+1,] <<- mrow 
  } else {
    print("NOT adding row to metadata df...")
    print(mrow)
  }
}
add_meta("cl full", "num rows", "total", nrow(df0), add=T)
# check 
clparcels_metadata
add_meta("cl full", "num cols", "total", ncol(df0), add=T)
clparcels_metadata
################################################################################
# Take a look at the dataset
################################################################################

# take a look at the column names
colnames(df0)
#-------------------------------------
# COUNT MISSING VALUES in each column
#-------------------------------------
num_rows <- nrow(df0)
cl_na_counts <- as.data.frame(sapply(df0, function(x) sum(is.na(x))))
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
write_csv(cl_na_counts, "CL_missing_data_in_full_dataset.csv")

# add some more metadata to our metadata df
add_meta("cl full", "num rows", "Monterey County", nrow(df0[df0$SITUS.COUNTY=='MONTEREY',]), add=T)
add_meta("cl full", "num rows", "Santa Cruz County", nrow(df0[df0$SITUS.COUNTY=='SANTA CRUZ',]), add=T)
add_meta("cl full", "num rows", "San Benito County", nrow(df0[df0$SITUS.COUNTY=='SAN BENITO',]), add=T)
clparcels_metadata

add_meta("cl full", "num rows - unique APNs", "total", length(unique(df0$`APN.(PARCEL.NUMBER.UNFORMATTED)`)), add=T)
add_meta("cl full", "num rows - no lat/lon", "total", nrow(df0[is.na(df0$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl full", "num rows - corp owner", "total", nrow(subset(df0,OWNER.1.CORPORATE.INDICATOR=='Y' )), add=T)
clparcels_metadata

# HERE!!!
add_meta("cl full", "num rows - unique APNs", "Monterey County", length(unique(df0[df0$SITUS.COUNTY=='MONTEREY',]$`APN.(PARCEL.NUMBER.UNFORMATTED)`)), add=T)
add_meta("cl full", "num rows - unique APNs", "Santa Cruz County", length(unique(df0[df0$SITUS.COUNTY=='SANTA CRUZ',]$`APN.(PARCEL.NUMBER.UNFORMATTED)`)), add=T)
add_meta("cl full", "num rows - unique APNs", "San Benito County", length(unique(df0[df0$SITUS.COUNTY=='SAN BENITO',]$`APN.(PARCEL.NUMBER.UNFORMATTED)`)), add=T)
#
add_meta("cl full", "num rows - no lat/lon", "Monterey County", nrow(df0[df0$SITUS.COUNTY=='MONTEREY' & is.na(df0$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl full", "num rows - no lat/lon", "Santa Cruz County", nrow(df0[df0$SITUS.COUNTY=='SANTA CRUZ' & is.na(df0$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl full", "num rows - no lat/lon", "San Benito County", nrow(df0[df0$SITUS.COUNTY=='SAN BENITO' & is.na(df0$PARCEL.LEVEL.LATITUDE),]), add=T)
clparcels_metadata

# IF MAKE MISTAKE - fix like this
#clparcels_metadata <- clparcels_metadata[-which(rownames(clparcels_metadata) %in% c(12:13)), ]
#rownames(clean_data) <- NULL # reset row numbers - IMPORTANT

add_meta("cl full", "num rows - corp owner", "Monterey County", nrow(subset(df0[df0$SITUS.COUNTY=='MONTEREY',],OWNER.1.CORPORATE.INDICATOR=='Y' )), add=T)
add_meta("cl full", "num rows - corp owner", "Santa Cruz County", nrow(subset(df0[df0$SITUS.COUNTY=='SANTA CRUZ',],OWNER.1.CORPORATE.INDICATOR=='Y' )), add=T)
add_meta("cl full", "num rows - corp owner", "San Benito County", nrow(subset(df0[df0$SITUS.COUNTY=='SAN BENITO',],OWNER.1.CORPORATE.INDICATOR=='Y' )), add=T)
clparcels_metadata

################################################################################
# Subset the data to only keep parcels with corporate owner
#
# After previously exploring the data it looks like any parcel that is publicly 
# owned has a *corporate ownership* designation
# Where publicly owned parcels are a subset of that designation
# So, filter data to keep those parcels where owner is corporate
################################################################################
# hold onto full dataset unless we run out of memory
df_fulldataset <- df0
df0 <- df0[!is.na(df0$"OWNER.1.CORPORATE.INDICATOR") & df0$"OWNER.1.CORPORATE.INDICATOR"=="Y",]
dim(df0) # 36319 because of the  filter - but in total there are 254,837 properties in CL dataset

#-------------------------------------------------------------------
# REPEAT Check of missing values for the CORPORATE only SUBGROUP
#-------------------------------------------------------------------
corp_num_rows <- nrow(df0)
clcorp_na_counts <- as.data.frame(sapply(df0, function(x) sum(is.na(x))))
colnames(clcorp_na_counts) <- c("corp_na_count")
clcorp_na_counts$corp_num_rows <- corp_num_rows
clcorp_na_counts$pct_corp_missing <- round(100 * (clcorp_na_counts$corp_na_count / clcorp_na_counts$corp_num_rows),2)
clcorp_na_counts$pct_corp_w_data <- round(100-clcorp_na_counts$pct_corp_missing,2)
clcorp_na_counts$column <- rownames(clcorp_na_counts)
rownames(clcorp_na_counts) <- seq(length=nrow(clcorp_na_counts))
colnames(clcorp_na_counts)

clcorp_na_counts<-clcorp_na_counts[c("column", "corp_na_count", "corp_num_rows", "pct_corp_missing", "pct_corp_w_data")]
head(clcorp_na_counts)

# Save MISSING VALUES metadata
clcorp_na_counts <- clcorp_na_counts[order(clcorp_na_counts$pct_corp_w_data, decreasing = TRUE),]
write_csv(clcorp_na_counts, "CL_missing_data_in_corporate_owner_subset.csv")

# Review clcorp_na_counts to see which of the cols have useful data for us
# What percent of cols have data vals?
# Which of these might be usefule?
#View(clcorp_na_counts)

# Of the attributes in CLcorp data we will keep
# [1] "CLIP : 1". # keep unique id
# [3] "APN..PARCEL.NUMBER.UNFORMATTED. : 1"
# [4] "ORIGINAL.APN : 1"
# [5] "ONLINE.FORMATTED.PARCEL.ID : 1"
# [2] "FIPS.CODE : 1"
# [1] "CENSUS.ID : 0.91"

# [1] "NEIGHBORHOOD.DESCRIPTION : 0.53"
# [1] "LAND.USE.CODE : 1"
# [1] "COUNTY.USE.DESCRIPTION : 1"
# [1] "PROPERTY.INDICATOR.CODE : 0.92"
#     ZONING.CODE: .25, 
#     ZONING.CODE.DESCRIPTION: 18
# [1] "PARCEL.LEVEL.LATITUDE : 0.91"
# [1] "PARCEL.LEVEL.LONGITUDE : 0.91"

# [1] "SITUS.CITY : 0.62"
# [1] "SITUS.COUNTY : 1"
# [1] "TAX.RATE.AREA.CODE : 1"
# [1] "SITUS.STREET.ADDRESS : 0.62"
#      "MUNICIPALITY.NAME: .33"
# [1] "OWNER.1.FULL.NAME : 1"
# [1] "OWNER1CORPORATEINDICATOR : 1"

# [1] "TOTAL.VALUE.CALCULATED : 0.71"
# [1] "LAND.VALUE.CALCULATED : 0.7"
# [1] "IMPROVEMENT.VALUE.CALCULATED : 0.48"

# [1] "ACRES : 0.93"
# [1] "LAND.SQUARE.FOOTAGE : 0.93"
#------------------------------------------------

# Be sure to maintain
# CLIP = corelogic unique id
# LAND.USE.CODE - A CoreLogic established Land Use code converted from various county Land Use codes to aid in search and extract functions.
# PROPERTY INDICATOR CODE - A CoreLogic general code used to easily recognize specific property types (e.g., Single Family Residence, Condominium, Commercial).
# We will want to join the. LUSE table and the PRIND table to the data

# Mostly nonempty and potentially useful cols:
cl_keepcols <- c("CLIP",
                 "APN.(PARCEL.NUMBER.UNFORMATTED)", "ORIGINAL.APN","ONLINE.FORMATTED.PARCEL.ID", 
                 "FIPS.CODE", "CENSUS.ID",
                 "NEIGHBORHOOD.DESCRIPTION", "PROPERTY.INDICATOR.CODE",  
                 "LAND.USE.CODE", "COUNTY.USE.DESCRIPTION", 
                 "ZONING.CODE", "ZONING.CODE.DESCRIPTION", 
                 "LAND.SQUARE.FOOTAGE", "ACRES", 
                 "MUNICIPALITY.NAME", 
                 "SITUS.COUNTY", "SITUS.CITY",  "TAX.RATE.AREA.CODE", "SITUS.STREET.ADDRESS",
                 "OWNER.1.FULL.NAME","OWNER.1.CORPORATE.INDICATOR",
                 "MAILING.STREET.ADDRESS", "MAILING.CITY", "MAILING.STATE", "MAILING.ZIP.CODE",
                 "TOTAL.VALUE.CALCULATED", "LAND.VALUE.CALCULATED", "IMPROVEMENT.VALUE.CALCULATED",
                 "PARCEL.LEVEL.LONGITUDE","PARCEL.LEVEL.LATITUDE" )


# subset the data    
df <- df0[cl_keepcols]

#rename a few cols with non-consistant formatting
names(df)[names(df) == "APN.(PARCEL.NUMBER.UNFORMATTED)"] <-'APN_PARCEL_NUM_UNFORMATTED'

#Remove corp subset of corelogic dataframe
rm(df0)

# Check out the types of corp owners
owner_df <- as.data.frame(table(df$OWNER.1.FULL.NAME))
owner_df
# By looking through the names of the big owners
# we get a sense of the name patterns for public owners
bigowner_df <- owner_df[owner_df$Freq > 10,]
head(bigowner_df)
# Create a new column called public_owner
# that is 1 for true and 0 for false
# that is an estimate of whether or not parcel is publicly owned
# based on data in the OWNER.1.FULL.NAME and COUNTY.USE.DESCRIPTION cols
#
# NOTES
# The Column "COUNTY.USE.DESCRIPTION" has a value of "public" but it's not comprehensive, as far as I can tell
# Can grep "city of" or "state of" or "county of" or "public " or "public works" or "school dist" from OWNER.1.FULL.NAME
# or "fire dist" or "fire dep" of "fire protection district" or "fire brigade" or "fire team" or "fire fighters"
# or "farm bureau" or 
# school dist, college dist, open space district, open space dist, airport dist, park dist, community services dist
# "state university", "regents", 
# also: begins with "U S " *u s space
# or "SANTA CRUZ CO", "Monterey CO" or "San Be Co"
# add - HIGH SCHOOL, ELEMENtary school, grammar school, preschool, unified school, , city school
# add harbor dist
# but otherwise hard to get "public" designation from one column
# so we will add one that will be approximate
df1 <- df %>%
  mutate(public_owner = dplyr::case_when(
    grepl('city of', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('state of', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('county of', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('public', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('school dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire dep', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire prot', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire brigade', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire team', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('fire fighters', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('farm bureau', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('college dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('open space dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('airport dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('park dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('community services dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('state univers', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('regents', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('^U S ', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('SANTA CRUZ CO', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('monterey CO', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('san benito CO', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('harbor dist', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('high school', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('elementary school', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('grammar school', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('preschool', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('unified school', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('city school', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('RECREATION DIST', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('parks dept', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('UNIFIED SCH', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('SCH DI', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('SCHOOL DIST', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1,
    grepl('^UNITED STATES', df$OWNER.1.FULL.NAME, ignore.case = T) ~ 1, 
    TRUE ~ 0))

table(df1$public_owner)
pubown_df <- as.data.frame(table(df1[df1$public_owner==1,]$OWNER.1.FULL.NAME))
pubown_df
unknown_df <- as.data.frame(table(df1[df1$public_owner==0,]$OWNER.1.FULL.NAME))
unknown_df
# checks
unknown_df[grepl('harbor dist', unknown_df$Var1, ignore.case=T),]
unknown_df[grepl('school', unknown_df$Var1, ignore.case=T),] # private/charter

# check if all parcels with county.use.description = publicly owned have public_owner==1)
# check if any with public_owner = 0 have county.use.description = pub own
countyuse_df1 <- as.data.frame(table(df1[df1$public_owner==1,]$COUNTY.USE.DESCRIPTION))
countyuse_df1
stateuse_df1 <- as.data.frame(table(df1[df1$public_owner==1,]$STATE.USE.DESCRIPTION))

countyuse_df0 <- as.data.frame(table(df1[df1$public_owner==0,]$COUNTY.USE.DESCRIPTION))
stateuse_df0 <- as.data.frame(table(df1[df1$public_owner==0,]$STATE.USE.DESCRIPTION))

# Additional adjustments
df1 <- df1 %>%
  mutate(public_owner = dplyr::case_when(
    ((public_owner==0) & grepl('city',COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 1,
    ((public_owner==0) & grepl('county',COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 1,
    ((public_owner==0) & grepl('cnty',COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 1,
    ((public_owner==0) & grepl('federal',COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 1,
    ((public_owner==0) & grepl('public',COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 1,
    ((public_owner==0) & grepl('water dist',OWNER.1.FULL.NAME, ignore.case = T)) ~ 1,
    TRUE ~ public_owner))

# Now check our COUNTY.USE Descriptions and OWNER.Fullname again
# to identify false positives
table(df1$public_owner)
pubown_df <- as.data.frame(table(df1[df1$public_owner==1,]$OWNER.1.FULL.NAME))
unknown_df <- as.data.frame(table(df1[df1$public_owner==0,]$OWNER.1.FULL.NAME))

# looks ok SO
# Save public_owned parcels to its own df
pub_df <- df1[df1$public_owner==1,]

# Check usetypes from the data in two cols
pubtab_df <- as.data.frame(table(pub_df$COUNTY.USE.DESCRIPTION))
pubtab2_df <- as.data.frame(table(pub_df$OWNER.1.FULL.NAME))

# Now let's categorize all the parcel use types
# as indicated in the OWNER.1.FULL.NAME and COUNTY.USE.DESCRIPTION cols

# Create new variable use_type from CL data
# This is a reclass/reduction/summary of data in
# the OWNER.1.FULL.NAME and COUNTY.USE.DESCRIPTION cols
pub_df <- pub_df %>%
  mutate(use_type = dplyr::case_when(
    grepl('fire', OWNER.1.FULL.NAME, ignore.case = T) ~ "FIRE",
    grepl('school|unified|education|univ|college|regents|trustee', OWNER.1.FULL.NAME, ignore.case = T) ~ "EDU",
    grepl('FARM|GRAZ|agric', OWNER.1.FULL.NAME, ignore.case = T) ~ "AGRICULTURE",
    grepl('park|recr|pks|public rec|REGIONAL PAR', OWNER.1.FULL.NAME, ignore.case = T) ~ "PARK/REC",
    grepl('open space|land trust|land cons|conservation land', OWNER.1.FULL.NAME, ignore.case = T) ~ "OPEN SPACE",
    grepl('airport|railroad|trans', OWNER.1.FULL.NAME, ignore.case = T) ~ "TRANSPORTATION",
    grepl('parking', OWNER.1.FULL.NAME, ignore.case = T) ~ "PARKING",
    grepl('community serv|community found|human serv|shelter|food bank|hospice|crisis|youth|hospital|health', OWNER.1.FULL.NAME, ignore.case = T) ~ "COMMUNITY SERVICES",
    grepl('housing|hsing|communityd dev|redevel| reuse|building auth|homeowners', OWNER.1.FULL.NAME, ignore.case = T) ~ "HOUSING",
    grepl('water|flood', OWNER.1.FULL.NAME, ignore.case = T) ~ "WATER",
    grepl('harbor|port', OWNER.1.FULL.NAME, ignore.case = T) ~ "HARBOR/PORT",
    grepl('public work|DPW|sanita|waste|garbage|ABATEMENT DIST|hazard', OWNER.1.FULL.NAME, ignore.case = T) ~ "PUBLIC WORKS",
    grepl('utility', OWNER.1.FULL.NAME, ignore.case = T) ~ "UTILITY",
    TRUE ~ "OTHER"))

# check
table(pub_df$use_type)

# NOW CHECK ALL OF ABOVE AND THOSE BELOW IN COUNTY.USE.DESC column
pub_df <- pub_df %>%
  mutate(use_type = dplyr::case_when(
    ((use_type=='OTHER') & grepl('vac|vacant', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'VACANT',
    ((use_type=='OTHER') & grepl('building|BLDG|storage', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'BUILDING',
    ((use_type=='OTHER') & grepl('water', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'WATER',
    ((use_type=='OTHER') & grepl('utility', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'UTILITY',
    ((use_type=='OTHER') & grepl('city', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'CITY ',
    ((use_type=='OTHER') & grepl('county', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'COUNTY',
    ((use_type=='OTHER') & grepl('state', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'STATE',
    ((use_type=='OTHER') & grepl('^U S|USA|United States|federal', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'FEDERAL',
    TRUE ~ use_type))

# check
table(pub_df$use_type)

countyuse2usetype_table <- as.data.frame(table(pub_df[pub_df$use_type=="OTHER",]$COUNTY.USE.DESCRIPTION))
ownername2usetype_table <- as.data.frame(table(pub_df[pub_df$use_type=="OTHER",]$OWNER.1.FULL.NAME))

# note we still have a lot of "OTHER"
# but more info can be had if one looks at values 
# in the COUNTY.USE.DESCRIPTION or OWNER.1.FULL.NAME cols

## Add a column for Owner Category (owner_cat)
# This is a reclass/reduction of data in 
# the OWNER.1.FULL.NAME and COUNTY.USE.DESCRIPTION cols
#
pub_df <- pub_df %>%
  mutate(owner_cat = dplyr::case_when(
    grepl('city|municipal', OWNER.1.FULL.NAME, ignore.case = T) ~ 'CITY',
    grepl('state|california', OWNER.1.FULL.NAME, ignore.case = T) ~ 'STATE',
    grepl('county|cnty', OWNER.1.FULL.NAME, ignore.case = T) ~ 'COUNTY',
    grepl('SANTA CRUZ CO|monterey co|san benito co', OWNER.1.FULL.NAME, ignore.case = T) ~ 'COUNTY',
    grepl('regents', OWNER.1.FULL.NAME, ignore.case = T) ~ 'STATE',
    grepl('^U S|USA|United States|federal', OWNER.1.FULL.NAME, ignore.case = T) ~ 'FEDERAL',
    TRUE ~ "OTHER"))

pub_df <- pub_df %>%
  mutate(owner_cat = dplyr::case_when(
    ((owner_cat=='OTHER') & grepl('city|municipal', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'CITY',
    ((owner_cat=='OTHER') & grepl('county|cnty', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'COUNTY',
    ((owner_cat=='OTHER') & grepl('state|california', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'STATE',
    ((owner_cat=='OTHER') & grepl('^U S|USA|United States|federal', COUNTY.USE.DESCRIPTION, ignore.case = T)) ~ 'FEDERAL',
    TRUE ~ owner_cat))

table(pub_df$owner_cat)

# take a look at those others again to see if we can get ideas to further categorize
countyuse2owncat_table <- as.data.frame(table(pub_df[pub_df$owner_cat=="OTHER",]$COUNTY.USE.DESCRIPTION))
ownername2owncat_table <- as.data.frame(table(pub_df[pub_df$owner_cat=="OTHER",]$OWNER.1.FULL.NAME))

# WHats our county breakdown?
table(pub_df$SITUS.COUNTY, useNA = "always")

#MONTEREY SAN BENITO SANTA CRUZ       <NA> 
#  3906         32       3012          0

####################################
# CORELOGIC derived data columns
# - We already reviewed these above
# - we just added 
#-- "public_owner", "use_type", "owner_cat"
#############################################################
colnames(pub_df)

# added cls
new_cl_cols <- c("public_owner", "use_type", "owner_cat")

# So lets update the cl_keepcols list to include our new cols
cl_keepcols <- c(cl_keepcols, new_cl_cols)

# check
cl_keepcols
colnames(pub_df)

# keep an eye on the parcels without CL coords
i <- "PARCEL.LEVEL.LONGITUDE"
n <- nrow(pub_df[pub_df[[i]]=="" ,]) + nrow(pub_df[pub_df[[i]]==0,])
p <- n/num_rows
print(p)
print(n) # 324 rows with no lat/lon
nogeom_df <- pub_df[pub_df$PARCEL.LEVEL.LATITUDE=="" | pub_df$PARCEL.LEVEL.LATITUDE==0,]


#################################
## Join in CL lookup tables for
#################################
## - LUSE: A CoreLogic established Land Use code converted from various county 
## Land Use codes to aid in search and extract functions.
## LAND.USE.CODE
#
## - PRIND: A CoreLogic general code used to easily recognize specific property 
## types (e.g., Single Family Residence, Condominium, Commercial).
## PROPERTY.INDICATOR.CODE
#

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
dim(pub_df)
pub_df <- base::merge(pub_df, luse_df, by="LAND.USE.CODE", all.x=TRUE)
pub_df <- base::merge(pub_df, prind_df, by="PROPERTY.INDICATOR.CODE", all.x=TRUE)
dim(pub_df) # make sure no new rows, only two new cols

# Take a look - we don't want unexplaniend NAs or '' values
doh <-as.data.frame(table(pub_df$luse_desc, useNA="always"))
doh <- as.data.frame(table(pub_df$prop_desc, useNA="always"))

#
# Standardize Municipality.Name since need for filter
#
table(pub_df$MUNICIPALITY.NAME, useNA="always")
table(pub_df$SITUS.CITY, useNA="always")

# we have 3938 NAs in MUNI.NAME
# we have 3610 NAs in SITUS.CITY
# SO lets combine into one for CITY var
pub_df$city <- pub_df$SITUS.CITY

pub_df <- pub_df %>%
  mutate(city = dplyr::case_when(
    (is.na(SITUS.CITY) & !is.na(MUNICIPALITY.NAME)) ~ MUNICIPALITY.NAME,
    TRUE ~ SITUS.CITY
  )) 
table(pub_df$city, useNA="always")

# set value for NAs in case need to for filtering
pub_df <- pub_df %>%
  mutate(city = dplyr::case_when(
    is.na(city) ~ "NONE OR UNDETERMINED",
    TRUE ~ city
  )) 
table(pub_df$city, useNA="always")

# Add text to zoning col so we can use as a filter
table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always")
pub_df <- pub_df %>%
  mutate(ZONING.CODE.DESCRIPTION = dplyr::case_when(
    ZONING.CODE.DESCRIPTION == '' ~ "unknown",
    TRUE ~ pub_df$ZONING.CODE.DESCRIPTION
  )) 
table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always")

# tons of NAs in ZONING.CODE.DESC
pub_df <- pub_df %>%
  mutate(ZONING.CODE.DESCRIPTION = dplyr::case_when(
    is.na(ZONING.CODE.DESCRIPTION) ~ "no data",
    TRUE ~ ZONING.CODE.DESCRIPTION
  )) 
table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always")

dim(pub_df)

#---------------------------------------------------------------
# Add metadata for public cl subset
#---------------------------------------------------------------
add_meta("cl pub", "num rows", "total", nrow(pub_df), add=T)
add_meta("cl pub", "num rows", "Monterey County", nrow(pub_df[pub_df$SITUS.COUNTY=='MONTEREY',]), add=T)
add_meta("cl pub", "num rows", "Santa Cruz County", nrow(pub_df[pub_df$SITUS.COUNTY=='SANTA CRUZ',]), add=T)
add_meta("cl pub", "num rows", "San Benito County", nrow(pub_df[pub_df$SITUS.COUNTY=='SAN BENITO',]), add=T)
clparcels_metadata

add_meta("cl pub", "num rows - unique APNs", "total", length(unique(pub_df$APN_PARCEL_NUM_UNFORMATTED)), add=T) 
# all APNs are unique so do not need breakdown

add_meta("cl pub", "num rows - no lat/lon", "total", nrow(pub_df[is.na(pub_df$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl pub", "num rows - no lat/lon", "Monterey County", nrow(pub_df[pub_df$SITUS.COUNTY=='MONTEREY' & is.na(pub_df$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl pub", "num rows - no lat/lon", "Santa Cruz County", nrow(pub_df[pub_df$SITUS.COUNTY=='SANTA CRUZ' & is.na(pub_df$PARCEL.LEVEL.LATITUDE),]), add=T)
add_meta("cl pub", "num rows - no lat/lon", "San Benito County", nrow(pub_df[pub_df$SITUS.COUNTY=='SAN BENITO' & is.na(pub_df$PARCEL.LEVEL.LATITUDE),]), add=T)

clparcels_metadata

##############################################################################
# Make CL Data Spatial
##############################################################################

# drop rows with no coords
summary(pub_df$PARCEL.LEVEL.LATITUDE)
pub_geom_df <- pub_df[!is.na(pub_df$PARCEL.LEVEL.LATITUDE),]

dim(pub_df) #[1] 6950   36
dim(pub_geom_df) # 6626   36
table(pub_df$SITUS.COUNTY)
#MONTEREY SAN BENITO SANTA CRUZ 
#3906         32       3012



table(pub_geom_df$SITUS.COUNTY)
pub_spdf <- st_as_sf(pub_geom_df, 
                     coords = c("PARCEL.LEVEL.LONGITUDE", "PARCEL.LEVEL.LATITUDE"), 
                     crs=4326)

table(pub_spdf$SITUS.COUNTY, useNA="always")

#MONTEREY SAN BENITO SANTA CRUZ
#3608         31       2987 

# add metadata
add_meta("cl pub geom", "num rows", "total", nrow(pub_spdf), add=T)
add_meta("cl pub geom", "num rows", "Monterey County", nrow(pub_spdf[pub_spdf$SITUS.COUNTY=='MONTEREY',]), add=T)
add_meta("cl pub geom", "num rows", "Santa Cruz County", nrow(pub_spdf[pub_spdf$SITUS.COUNTY=='SANTA CRUZ',]), add=T)
add_meta("cl pub geom", "num rows", "San Benito County", nrow(pub_spdf[pub_spdf$SITUS.COUNTY=='SAN BENITO',]), add=T)
clparcels_metadata

##############################################################################
# PARCEL DATA 
##############################################################################

########################## 
# San Benito Parcels
##########################

sbparcels = read_sf(sbparcel_file)

#check dims
dim(sbparcels) # [1] 24175    15

#id cols with APNs
colnames(sbparcels)
# take a look at format
head(sbparcels$APN)
summary(nchar(sbparcels$APN))

table(pub_spdf$SITUS.COUNTY, useNA = "ifany")
#MONTEREY SAN BENITO SANTA CRUZ 
# 3608         31       2987

# SPATIALLY SUBSET TO KEEP ONLY SB PARCels that spatially intersect the CL data
st_crs(pub_spdf) == st_crs(sbparcels)

# Note: there are more sb parcels than rows in CL data likely due to multipart parcels
sbparcels <- st_make_valid(sbparcels)
sb_cl_parcels <- sbparcels[pub_spdf,]

# SB Parcels that spatially intersect the CL data
dim(sb_cl_parcels) # should be 31 parcels
#[1] 32 15 # one more poly

# let's check the inverse spatial intersection
# select only pub_spdf pts that spatially intersect sc_parcels
# these should lead to the same num of parcels
sb_cl_parcel_points <- pub_spdf[sbparcels,]
dim(sb_cl_parcel_points)
#[1] 32 35 # its the same # of parcels!

# Save SB metadata
add_meta("sbparcels", "num rows (parcels)", "total", nrow(sbparcels), add=T)
add_meta("sbparcels", "num rows - unique APNs", "total", length(unique(sbparcels$APN)), add=T)

add_meta("sb_cl_parcels", "num rows", "total", nrow(sb_cl_parcels), add=T)
add_meta("sb_cl_parcels", "num rows - unique APNs", "total", length(unique(sb_cl_parcels$APN)), add=T)
clparcels_metadata

# remove sbparcels if we need the memory - only keeping for troubleshooting/stats
#rm(sbparcels)

################################################################################
# Santa Cruz Parcel data
################################################################################

scparcels = read_sf(scparcel_file)
dim(scparcels) # [1] 97021    59

colnames(scparcels)

table(pub_spdf$SITUS.COUNTY, useNA = "ifany")
#MONTEREY SAN BENITO SANTA CRUZ 
#3608         31       2987 

# SPATIALLY SUBSET TO KEEP ONLY parcels that spatially intersect the CL data
st_crs(pub_spdf) == st_crs(scparcels)
scparcels <- st_transform(scparcels, st_crs(pub_spdf))
st_crs(pub_spdf) == st_crs(scparcels)
dim(scparcels) #[1] 97021    59

# Make valid and then subset
scparcels <- st_make_valid(scparcels)
dim(scparcels) #97021
sc_cl_parcels <- scparcels[pub_spdf,]

# SC Parcels that spatially intersectthe CL data
dim(sc_cl_parcels) # should be 2987 parcels
#[1] 2989   59

# let's check the inverse spatial intersection
# select only pub_spdf pts that spatially intersect sc_parcels
# these should lead to the same num of parcels
sc_cl_parcel_points <- pub_spdf[scparcels,]
dim(sc_cl_parcel_points)
#[1] 2976   35 # 13 fewer parcels identified in this way

# Save Metadata
add_meta("scparcels", "num rows (parcels)", "total", nrow(scparcels), add=T)
add_meta("scparcels", "num rows - unique APNs", "total", length(unique(scparcels$APN)), add=T)

add_meta("sc_cl_parcels", "num rows", "total", nrow(sc_cl_parcels), add=T)
add_meta("sc_cl_parcels", "num rows - unique APNs", "total", length(unique(sc_cl_parcels$APN)), add=T)
clparcels_metadata
#
# Remove files from memory
#
#rm(scparcels)

###################################################
# MONTEREY COUNTY PARCEL DATA
###################################################
moparcels = read_sf(moparcel_file) # big file

dim(moparcels) # [1] 123588     90

colnames(moparcels)

table(pub_spdf$SITUS.COUNTY, useNA = "ifany")
#MONTEREY SAN BENITO SANTA CRUZ 
#3608         31       2987 

# SPATIALLY SUBSET TO KEEP ONLY parcels that spatially intersect the CL data
st_crs(pub_spdf) == st_crs(moparcels)

# Make valid and then subset
moparcels <- st_make_valid(moparcels)
# select only moparcels that spatially intersect pub_spdf points
mo_cl_parcel_polys <- moparcels[pub_spdf,] # spatial subset

# Just because, let's check the inverse spatial intersection
# select only pub_spdf pts that spatially intersect mo_parcels
# these should lead to the same num of parcels
mo_cl_parcel_points <- pub_spdf[moparcels,]

# MO Parcels that intersect the CL data
dim(mo_cl_parcel_polys) # should be 3608 parcels
#[1] 3566   90

# CL points that intersect Mo parcel data
dim(mo_cl_parcel_points) # should be 3608 parcels
#[1] 3608   35 # 42 more parcels identified this way

# THESE NUMBERS ARE NOT THE SAME (off by 42)
# They differ spatially bc more than one point is intersecting some polygons
# and some points are not within any polygons
#
# Remove files from memory if needed
#
#rm(moparcels)

# Keep the parcel data that intersect CL points
# because that is our current approach
mo_cl_parcels <- mo_cl_parcel_polys

# check
plot(mo_cl_parcels$geometry, col="green")
plot(sb_cl_parcels$geometry, col="blue")
plot(sc_cl_parcels$geometry, col="red")

# 
# ADD Metadata
#
clparcels_metadata
add_meta("moparcels", "num rows (parcels)", "total", nrow(moparcels), add=T)
add_meta("moparcels", "num rows - unique APNs", "total", length(unique(moparcels$APN)), add=T)

add_meta("mo_cl_parcels", "num rows", "total", nrow(mo_cl_parcels), add=T)
add_meta("mo_cl_parcels", "num rows - unique APNs", "total", length(unique(mo_cl_parcels$APN)), add=T)
clparcels_metadata

add_meta("cl_points_in_sbparcels", "num rows", "total", nrow(sb_cl_parcel_points), add=T)
add_meta("cl_points_in_scparcel_points", "num rows", "total", nrow(sc_cl_parcel_points), add=T)
add_meta("cl_points_in_moparcel_points", "num rows", "total", nrow(mo_cl_parcel_points), add=T)
clparcels_metadata

##########################################################
#Reshape THE parcel Data BEFORE JOINING CL data
##########################################################
# We have parcel data from 3 counties
# but they have different columns
# They need to have the same columns
# before we can join by attribute
# though we can spatially merge with
# different columns but would need to
#add a suffix to colnames to differentiate
##############################################

#-------------
# SAN BENITO
#-------------
colnames(sb_cl_parcels)
sb_keepcols <- c("APN", "CountyGP", "CountyZone", "UseCode")

sbparcels_pub <- sb_cl_parcels[sb_keepcols]

# rename cols
colnames(sbparcels_pub) <- colnames(sbparcels_pub) <- paste("SB", colnames(sbparcels_pub), sep = "_")
colnames(sbparcels_pub)
# keep geom name as geometry
colnames(sbparcels_pub)[colnames(sbparcels_pub) == 'SB_geometry'] <- 'geometry'
plot(sbparcels_pub$geometry)
#keep apn name
colnames(sbparcels_pub)[colnames(sbparcels_pub) == 'SB_APN'] <- 'pdata_APN'
colnames(sbparcels_pub)

#------------
# SANTA CRUZ
#------------
colnames(sc_cl_parcels)

# Lots of cols so lets subset and keep most useful
# Keeping APNNODASH bc makes best match with CL APN
sc_keepcols <- c("APNNODASH","USECDDESC", "ACRES", "SQUAREFT", "ZONING", 
                 "GPLANDUSE", "COASTALZN", "FLOODCDIST", "FAULTZONE", 
                 "FLOODWAYS",  "SRESPAREA", "GEOHAZSCRN", "COASTHAZRD")

scparcels_pub <- sc_cl_parcels[sc_keepcols]

colnames(scparcels_pub) <- colnames(scparcels_pub) <- paste("SC", colnames(scparcels_pub), sep = "_")
colnames(scparcels_pub)

# keep geom name as geometry
colnames(scparcels_pub)[colnames(scparcels_pub) == 'SC_geometry'] <- 'geometry'
plot(scparcels_pub$geometry)

#keep apn col name apn
colnames(scparcels_pub)[colnames(scparcels_pub) == 'SC_APNNODASH'] <- 'pdata_APN'
colnames(scparcels_pub)

#-------------------
# Monterey County
#-------------------
colnames(mo_cl_parcels)

mo_keepcols <- c("APN","GIS_ACRES", "GIS_SQFT" ,"Urbnzd_Area",
              "Land_Value", "Improved_Value",  
              "Land_Use_Code","Land_Use",
              "Liquefact_Susc", "SRA_Fire_Haz_Zones",
              "Costl_Zone","Slope_GT25", "Seismic_Haz_Zone","Landslide_Susc",
              "Willmsn_Act",  "Actv_Fault_Buffer", 
              "Hist_Site", "Floodway", "Flood_Zone", "Erosion_Hazard")

moparcels_pub <- mo_cl_parcels[mo_keepcols]
colnames(moparcels_pub)

colnames(moparcels_pub) <- paste("MO", colnames(moparcels_pub), sep = "_")
colnames(moparcels_pub)

# keep geom name as geometry
colnames(moparcels_pub)[colnames(moparcels_pub) == 'MO_geometry'] <- 'geometry'
plot(moparcels_pub$geometry)
#keep apn15 col name apn15
colnames(moparcels_pub)[colnames(moparcels_pub) == 'MO_APN'] <- 'pdata_APN'
colnames(moparcels_pub)


################################################################################
#  RESHAPE Parcel Data attributes before joining to CL data
## we want to keep a few things to filter on
## None of this data is in the SB parcels but not many SB parcels anyway
################################################################################

#---------------------
# Fire hazard areas
#---------------------
# SC Fire: SC_SRESPAREA
# Mo FIre: MO_SRA_Fire_Haz_Zones
#
# Fire - take a look
table(moparcels_pub$MO_SRA_Fire_Haz_Zones, useNA = "always")
table(scparcels_pub$SC_SRESPAREA, useNA = "always")

# Fire - reclass MoCo values
moparcels_pub <- moparcels_pub %>%
  mutate(infirezone = dplyr::case_when(
    grepl('high|moderate', MO_SRA_Fire_Haz_Zones, ignore.case=T) ~ "YES",
    grepl('None', MO_SRA_Fire_Haz_Zones, ignore.case=T) ~ "No",
    TRUE ~ "Not Applicable"
  ))

# Fire - reclass SCCo values
scparcels_pub <- scparcels_pub %>%
  mutate(infirezone = dplyr::case_when(
    grepl('high|moderate', SC_SRESPAREA, ignore.case=T) ~ "YES",
    SC_SRESPAREA == 'LRA' ~ "LRA",
    SC_SRESPAREA == 'n/a' ~ "Not Applicable",
    #is.na(SC_SRESPAREA) ~ "Not Applicable" #,  # don't do this bc for MoCo all NA!
    TRUE ~ "Not Applicable"
  ))

sbparcels_pub$infirezone <- "no data"

# check
table(moparcels_pub$infirezone, useNA = "always")
table(scparcels_pub$infirezone, useNA = "always")
table(sbparcels_pub$infirezone, useNA = "always")

#---------------------
# Earthquake hazards
#---------------------
# SC EARTHQUAKE: SC_FAULTZONE
# MO EARTHQUAKE: MO_Actv_Fault_Buffer
# Earthquake faultzone
table(moparcels_pub$MO_Actv_Fault_Buffer, useNA = "always")
table(scparcels_pub$SC_FAULTZONE, useNA = "always")

# Earthquake - reclass MoCo
moparcels_pub <- moparcels_pub %>%
  mutate(infaultzone = dplyr::case_when(
    grepl('None', MO_Actv_Fault_Buffer, ignore.case=T) ~ "NO",
    grepl('YES', MO_Actv_Fault_Buffer, ignore.case=T) ~ "YES",
    TRUE ~ "Not Applicable"
  ))

table(moparcels_pub$infaultzone, useNA = "always")

# Earthquake - reclass SCCo
scparcels_pub <- scparcels_pub %>%
  mutate(infaultzone = dplyr::case_when(
    grepl('FZ', SC_FAULTZONE, ignore.case=F) ~ "YES",
    #grepl('n/a',  SC_FAULTZONE, ignore.case=T) ~ "Not Applicable",
    TRUE ~  "NO"
  ))

#check
table(scparcels_pub$infaultzone, useNA="always")

#sb
sbparcels_pub$infaultzone <- "no data"

#---------------------
#steep slopes
#---------------------
# MO Slope GT 25 = MO_Slope_GT25
# SC Slope gt25: no data (i.e. pending calc)
# sb no data
# only avail in MoCo parcel data
table(moparcels_pub$MO_Slope_GT25, useNA="always")

moparcels_pub <- moparcels_pub %>%
  mutate(slopeGT25 = dplyr::case_when(
    MO_Slope_GT25=="Yes" ~ "YES",
    MO_Slope_GT25=="None" ~ "NO",
    TRUE ~ "no data"
  ))

# Check
table(moparcels_pub$slopeGT25, useNA="always")

scparcels_pub$slopeGT25 <-'no data'
sbparcels_pub$slopeGT25 <-'no data'

# Coastal Zone
# Mo Coastal zone: MO_Costl_Zone
# SC coastal Zone: SC_COASTALZN
# sb no data but not on coast
table(moparcels_pub$MO_Costl_Zone, useNA="always")
table(scparcels_pub$SC_COASTALZN, useNA="always")

moparcels_pub <- moparcels_pub %>%
  mutate(incoastzone = dplyr::case_when(
    MO_Costl_Zone=="Yes" ~ "YES",
    #MO_Costl_Zone=="None" ~ "NO",
    TRUE ~ "NO"
  ))

scparcels_pub <- scparcels_pub %>%
  mutate(incoastzone = dplyr::case_when(
    grepl('Yes', SC_COASTALZN, ignore.case=T) ~ "YES",
    #grepl('No',  SC_COASTALZN, ignore.case=T) ~ "NO",
    TRUE ~  "NO"
  ))

sbparcels_pub$incoastzone =  "NO"

#check
table(moparcels_pub$incoastzone, useNA="always")
table(scparcels_pub$incoastzone, useNA="always")
table(sbparcels_pub$incoastzone, useNA="always")

# Flood zoon
# SC Flood: downloaded - need to intersect
# MO FLOOD: MO_Flood_Zone
# sb no data

# First make a spatial df with all polys and pdata_APN as uid
mo2 <- moparcels_pub[c('pdata_APN')]
sc2 <- scparcels_pub[c('pdata_APN')]
sb2 <-sbparcels_pub[c('pdata_APN')]
allp <- rbind(mo2, sc2, sb2)
dim(allp) #6587    2
# SO we should have 6587 parcels after we intersect allpolys w.flood data
# we could also just add the row totals
total_num_parcels <- nrow(scparcels_pub) + nrow(moparcels_pub) + nrow(sbparcels_pub)
total_num_parcels
#add that to metadata
clparcels_metadata
add_meta("total_num_parcels", "num rows in parcel data", "total", (total_num_parcels), add=T)
clparcels_metadata

# SC Flood: downloaded - need to intersect
# Flood hazards downloaded from
# https://www.santacruzcountyca.gov/Departments/GeographicInformationSystems(GIS).aspx
sc_flood <- st_read("constraints/SC_FEMA_Flood_Hazard_Areas/FEMA_Flood_Hazard_Areas.shp")
sc_flood <- st_transform(sc_flood, st_crs(scparcels_pub))
st_crs(scparcels_pub) == st_crs(sc_flood)

# join the count back to the Alameda County census tract data frame `tracts_acs_sf_ac2`
sc_flood <- st_make_valid(sc_flood)

# spatial join
scparcels_pub_with_flood <- st_join(scparcels_pub, sc_flood['FLD_ZONE'])

# take a look at the output
#View(scparcels_pub_with_flood)
dim(scparcels_pub_with_flood)
dim(scparcels_pub)
# The two scparcels_pub and pub_with_flood need to have the same num of rows
# ok we have picked up a bunch of parcels bc
# parcel likely intersected +1 flood zones
# so want to flatten, eg APN 00226103

#For example of problem 
scparcels_pub_with_flood[scparcels_pub_with_flood$pdata_APN == '00226103',]
#compared to
scparcels_pub[scparcels_pub$pdata_APN == '00226103',]

# SO - flatten flood vars into one-to-one 
# sample code to do this
paste(unlist(doh$FLD_ZONE),collapse = "+")
#try test on entire data set
tempflood_spdf <- scparcels_pub_with_flood %>% group_by(., pdata_APN) %>% 
  mutate(
   floodstring = paste(unlist(unique(FLD_ZONE)),collapse = "+")
  )

# above works but FIRST want to 
## - convert flood zone codes to descriptive strings
## - remove FLD_ZONE and then de-dupe

table (scparcels_pub_with_flood$FLD_ZONE, useNA="always")

# FEMA Flood Hazard Areas - downloaded for SC
# 100 Year Flood Zone - A
# 100 Year Flood Zone - A99
# 100 Year Flood Zone - AE
# 100 Year Flood Zone - AH
# 100 Year Flood Zone - AO
# 100 Year Flood Zone - VE
# 500 Year Flood Zone - X

scparcels_pub_with_flood <- scparcels_pub_with_flood %>%
  mutate(floodzone_desc = dplyr::case_when(
    grepl('X',FLD_ZONE, ignore.case=T) ~ "YES 500 year",
    grepl('A|V', FLD_ZONE, ignore.case=T) ~ "YES 100 year",
    TRUE ~ "NO"
  )) 

scparcels_pub_with_flood <- scparcels_pub_with_flood %>% group_by(., pdata_APN) %>% 
  mutate(
    floodstring = paste(unlist(unique(floodzone_desc)),collapse = "+")
  )

table(scparcels_pub_with_flood$floodstring, useNA="always")

# standardize
scparcels_pub_with_flood <- scparcels_pub_with_flood %>%
  mutate(infloodzone = dplyr::case_when(
    floodstring == 'YES 100 year' ~ "YES 100 year",
    floodstring == 'YES 500 year' ~ "YES 500 year",
    floodstring == 'YES 100 year+YES 500 year' ~ "YES 100 year and 500 year",
    floodstring == 'YES 500 year+YES 100 year' ~ "YES 100 year and 500 year",
    TRUE ~ "NO"
  ))

# Drop temp cols
scparcels_pub_with_flood2 <- scparcels_pub_with_flood %>%
    select (-c(FLD_ZONE, floodstring, floodzone_desc))
# drop dup cols
scparcels_pub_with_flood3 <- scparcels_pub_with_flood2 %>% distinct()

# check make sure we didnt split parcels
dim(scparcels_pub_with_flood2)
dim(scparcels_pub)
dim(scparcels_pub_with_flood3)

# Ok so we end up number of rows we started with (2989)
# rename now that we are sure it worked!
# this will add the flood column to thescparcels_pub spdf  
scparcels_pub <- scparcels_pub_with_flood3

dim(scparcels_pub)
rm(scparcels_pub_with_flood, scparcels_pub_with_flood2, scparcels_pub_with_flood3)
#-----

moparcels_pub <- moparcels_pub %>%
  mutate(infloodzone = dplyr::case_when(
    grepl('X',MO_Flood_Zone, ignore.case=T) ~ "YES 500 year",
    grepl('A|V', MO_Flood_Zone, ignore.case=T) ~ "YES 100 year",
    TRUE ~ "NO"
  ))

# I do not have any floodzone data for SAN BENITO!
# But could get it and join - just not enough
# parcels to make it worth effort right now
sbparcels_pub$infloodzone <- "no data"

# check
table(moparcels_pub$infloodzone, useNA="always")
table(scparcels_pub$infloodzone, useNA="always")
table(sbparcels_pub$infloodzone, useNA="always")

colnames(moparcels_pub)
colnames(scparcels_pub)
colnames(sbparcels_pub)
# COUNTY Land Use values
#"MO_Land_Use", "SC_USECDDESC"

# check
doh <- as.data.frame(table(moparcels_pub$"MO_Land_Use", useNA="always"))
doh2 <- as.data.frame(table(scparcels_pub$"SC_USECDDESC", useNA="always"))

moparcels_pub$pdata_luse <- moparcels_pub$MO_Land_Use
scparcels_pub$pdata_luse <- scparcels_pub$SC_USECDDESC
sbparcels_pub$pdata_luse <- "no data"

# Add parcel data county to each spdf
moparcels_pub$pdata_county <- "MONTEREY"
sbparcels_pub$pdata_county <- "SAN BENITO"
scparcels_pub$pdata_county <- "SANTA CRUZ"

#####################################################
# Subset parcel data to keep only 
# the cols we want to explore
## we may add others later
# NOTE
## Need same cols in all 3 spdfs to merge into one
#####################################################

#parcel cols
parcel_cols <- c("pdata_APN", "infirezone", "infaultzone", "slopeGT25",
                 "incoastzone", "infloodzone", "pdata_luse", "pdata_county")

head(sbparcels_pub[parcel_cols])
head(scparcels_pub[parcel_cols])
head(moparcels_pub[parcel_cols])

#subset to keep only apn15, geom (automatic), and our parcel cols
sc2 <- scparcels_pub[parcel_cols]
sb2 <- sbparcels_pub[parcel_cols]
mo2 <- moparcels_pub[parcel_cols]
# check match
colnames(sc2) == colnames(sb2)
colnames(sc2) == colnames(mo2)
#take a look
colnames(sc2)

#transform data to CRS 4326 (WGS84)
st_crs(sb2)
sc2 <- st_transform(sc2, 4326)
sb2 <- st_transform(sb2, 4326)
mo2 <- st_transform(mo2, 4326)
st_crs(mo2) == st_crs(sc2)
st_crs(sc2) == st_crs(sb2)


# COMBINE parcel data
mobay_spdf <- rbind(mo2, sc2, sb2)
dim(mobay_spdf)
dim(pub_spdf)

#
dim(sc2)
dim(sb2)
dim(mo2)
table(pub_df$SITUS.COUNTY, useNA = "always")
#MONTEREY SAN BENITO SANTA CRUZ       <NA> 
#  3906         32       3012          0 
table(mobay_spdf$pdata_county, useNA = "always")
#MONTEREY SAN BENITO SANTA CRUZ       <NA> 
#  3566         32       2989          0 

dim(pub_df) #[1] 6950   36
dim(mobay_spdf)# [1] 6587    9
# NOTE:
# at this point we have 363 more
# rows in the parcel mobay_spdf than in the
# core logic pub_df
# this could be duplicates or it could be
# multipart polys or my error in processing 
class(mobay_spdf)
class(mobay_spdf$geometry)

length(unique(mobay_spdf$pdata_APN))
# 6587
length(unique(mobay_spdf$pdata_APN)) == total_num_parcels
# TRUE so we have not lost any parcels in our reshaping

# lets ad a UID to all parcel rows (likely don't need this)
mobay_spdf$pdata_uid <-seq.int(nrow(mobay_spdf))
nrow(mobay_spdf) == length(unique(mobay_spdf$pdata_uid))

# Make sure all data looks like its in correct county
plot(mobay_spdf[mobay_spdf$pdata_county=="MONTEREY",]$geometry)
plot(mobay_spdf[mobay_spdf$pdata_county=="SAN BENITO",]$geometry)
plot(mobay_spdf[mobay_spdf$pdata_county=="SANTA CRUZ",]$geometry)

#######################################
# Check filter vals - don't want NAs
#######################################
doh <- as.data.frame(table(pub_df$city, useNA="always"))
doh #<NA> 0
doh <- as.data.frame(table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always"))
doh# NA is 0, "no data" is 5039 so not super useful as a descriptor
as.data.frame(table(pub_df$SITUS.COUNTY, useNA="always"))
# no changes 
doh <- as.data.frame(table(pub_df$owner_cat, useNA="always"))
doh # NA is 0
doh <- as.data.frame(table(pub_df$use_type, useNA="always"))
doh # NA count is 0
# check these vals too
as.data.frame(table(mobay_spdf$infirezone, useNA="always"))
as.data.frame(table(mobay_spdf$infloodzone, useNA="always"))
as.data.frame(table(mobay_spdf$infaultzone, useNA="always"))

#fixes - likely not necessary bc not likely to use as filters in interactive map
pub_df <- pub_df %>% replace_na(list(MUNICIPALITY.NAME = 'none or no data', ZONING.CODE.DESCRIPTION = "none or no data"))

##################
# geom fixes
##########################

parcel_polys <- st_make_valid(mobay_spdf)
dim(mobay_spdf)
dim(parcel_polys) # yeh - keeps all polys

#remove Z dimension (ie reduce to 2d)
parcel_polys <- st_zm(parcel_polys)
dim(parcel_polys)
plot(parcel_polys$geometry)

# Make sure all data looks like its in correct county
plot(parcel_polys[parcel_polys$pdata_county=="MONTEREY",]$geometry)
plot(parcel_polys[parcel_polys$pdata_county=="SAN BENITO",]$geometry)
plot(parcel_polys[parcel_polys$pdata_county=="SANTA CRUZ",]$geometry)

# add parcel calculated area in acres
parcel_polys$parcel_acres <- units::set_units(st_area(parcel_polys), acres)

# reduce area_acres from units object to reg numbers
parcel_polys$parcel_acres <- as.vector(parcel_polys$parcel_acres)
#check
str(parcel_polys)

summary(parcel_polys$parcel_acres) # make sure no NAs
# recass acres from unit to category
# because the spread is too wide for a good filter slider in the interactive map
parcel_polys <- parcel_polys %>%
  mutate(acres_cat = dplyr::case_when(
    parcel_acres <= .25 ~ .25,
    parcel_acres <= .5 ~ .5,
    parcel_acres <= .75 ~ .75,
    parcel_acres <= 1 ~ 1,
    parcel_acres >= 100 ~ 100,
    TRUE ~ as.numeric(round(parcel_acres,1))
  ))

# Any duplicate APNs?
length(unique(parcel_polys$pdata_APN)) #6587
dim(parcel_polys) # 6587 - same so no dupes

#parcel_acres are too hard to filter bc of range so make a simple binary for filtering
parcel_polys$acresGTquarter <- if_else(parcel_polys$parcel_acres >=0.25, "YES", "NO")

# add parcel metadata before join
clparcels_metadata
add_meta("all parcel polys", "num rows (parcels)", "total", nrow(parcel_polys), add=T)
add_meta("all parcel polys", "unique APNS (parcels)", "total", length(unique(parcel_polys$pdata_APN)), add=T)
add_meta("all parcel polys", "SB County (parcels)", "total", nrow(parcel_polys[parcel_polys$pdata_county=='SAN BENITO',]), add=T)
add_meta("all parcel polys", "SC County (parcels)", "total", nrow(parcel_polys[parcel_polys$pdata_county=='SANTA CRUZ',]) , add=T)
add_meta("all parcel polys", "MO County (parcels)", "total", nrow(parcel_polys[parcel_polys$pdata_county=='MONTEREY',]), add=T)
clparcels_metadata


############################################
# Join corelogic data to the parcel data
############################################

# FIRST RENAME CL  COLS BEFORE SAVING
colnames(pub_spdf)
pub_spdf <- pub_spdf %>% 
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))
colnames(pub_spdf)

# Add Metadata - 
add_meta("CL public parcels", "num rows", "total", nrow(pub_spdf), add=T)
add_meta("CL public parcels", "unique APNS (cl)", "total", length(unique(pub_spdf$APN_PARCEL_NUM_UNFORMATTED)), add=T)
add_meta("CL public parcels", "num rows", "San Benito County", nrow(pub_spdf[pub_spdf$SITUS_COUNTY=='SAN BENITO',]), add=T)
add_meta("CL public parcels", "num rows", "Santa Cruz County", nrow(pub_spdf[pub_spdf$SITUS_COUNTY=='SANTA CRUZ',]) , add=T)
add_meta("CL public parcels", "num rows", "Monterey County", nrow(pub_spdf[pub_spdf$SITUS_COUNTY=='MONTEREY',]), add=T)
clparcels_metadata

#
# Now Join SPATIALLY
#

cl_parcel_join <- st_join(parcel_polys, pub_spdf)

dim(cl_parcel_join)
table(cl_parcel_join$SITUS_COUNTY)
table(cl_parcel_join$pdata_county)

# CHECKS
length(unique(cl_parcel_join$CLIP))

# Add Metadata - 
add_meta("CL + Parcels joined", "num rows", "total", nrow(cl_parcel_join), add=T)
add_meta("CL + Parcels joined", "unique CLIPS (cl)", "total", length(unique(cl_parcel_join$CLIP)), add=T)
add_meta("CL + Parcels joined", "unique APNS (cl)", "total", length(unique(cl_parcel_join$APN_PARCEL_NUM_UNFORMATTED)), add=T)
add_meta("CL + Parcels joined", "unique APNS (pdata)", "total", length(unique(cl_parcel_join$pdata_APN)), add=T)
#
add_meta("CL + Parcels joined", "num rows (cl)", "San Benito County", nrow(cl_parcel_join[cl_parcel_join$SITUS_COUNTY=='SAN BENITO',]), add=T)
add_meta("CL + Parcels joined", "num rows(cl)", "Santa Cruz County", nrow(cl_parcel_join[cl_parcel_join$SITUS_COUNTY=='SANTA CRUZ',]) , add=T)
add_meta("CL + Parcels joined", "num rows(cl)", "Monterey County", nrow(cl_parcel_join[cl_parcel_join$SITUS_COUNTY=='MONTEREY',]), add=T)
#
add_meta("CL + Parcels joined", "num rows (pdata)", "San Benito County", nrow(cl_parcel_join[cl_parcel_join$pdata_county=='SAN BENITO',]), add=T)
add_meta("CL + Parcels joined", "num rows(pdata)", "Santa Cruz County", nrow(cl_parcel_join[cl_parcel_join$pdata_county=='SANTA CRUZ',]) , add=T)
add_meta("CL + Parcels joined", "num rows(pdata)", "Monterey County", nrow(cl_parcel_join[cl_parcel_join$pdata_county=='MONTEREY',]), add=T)

clparcels_metadata

# which parcels are duplicated
## dup CLIPs == dup CL rows introduced by spatial join
dupClips <- as.data.frame(table(cl_parcel_join$CLIP))
dupClips <- dupClips[dupClips$Freq > 1,]$Var1
length(dupClips) #115 duplicate clips

## dup Parcel APNS == dup parcel data rows intro'd by spatial join
dupParcelApns <- as.data.frame(table(cl_parcel_join$pdata_APN))
dupParcelApns <- dupParcelApns[dupParcelApns$Freq > 1,]$Var1
length(dupParcelApns) # 86

## Missing CLIPs == CL rows that did not spatially join (13)
missingClips_spdf <- pub_spdf[!(pub_spdf$CLIP %in% cl_parcel_join$CLIP),]
length(unique(missingClips_spdf$CLIP)) #13

## missing parcel APNS == parcel rows that didnt join (0)
missingParcelAPNs_spdf <- parcel_polys[!(parcel_polys$pdata_APN %in% cl_parcel_join$pdata_APN),]
length(unique(missingParcelAPNs_spdf$pdata_APN)) #0

## SAVE problem rows so we can check later in more detail
dupClip_spdf <- cl_parcel_join[cl_parcel_join$CLIP %in% dupClips,]
dupApns_spdf <- cl_parcel_join[cl_parcel_join$pdata_APN %in% dupParcelApns,]

nrow(dupClip_spdf) #324
nrow(dupApns_spdf) #321

# Add Metadata - 
add_meta("CL + Parcels joined", "num dup CLIPS", "total", length(dupClips), add=T)
add_meta("CL + Parcels joined", "num dup Parcel APNs", "total", length(dupParcelApns), add=T)
add_meta("CL + Parcels joined", "num missing CLIPs", "total", length(unique(missingClips_spdf$CLIP)), add=T)
add_meta("CL + Parcels joined", "num missing Parcel APNs", "total", length(unique(missingParcelAPNs_spdf$pdata_APN)), add=T)
#
add_meta("CL + Parcels joined", "num dup CLIPS", "San Benito County", nrow(dupClip_spdf[dupClip_spdf$SITUS_COUNTY=='SAN BENITO',]), add=T)
add_meta("CL + Parcels joined", "num dup CLIPS", "Santa Cruz County", nrow(dupClip_spdf[dupClip_spdf$SITUS_COUNTY=='SANTA CRUZ',]) , add=T)
add_meta("CL + Parcels joined", "num dup CLIPS", "Monterey County", nrow(dupClip_spdf[dupClip_spdf$SITUS_COUNTY=='MONTEREY',]), add=T)
#
add_meta("CL + Parcels joined", "num dup Parcel APNs", "San Benito County", nrow(dupApns_spdf[dupApns_spdf$SITUS_COUNTY=='SAN BENITO',]), add=T)
add_meta("CL + Parcels joined", "num dup Parcel APNs", "Santa Cruz County", nrow(dupApns_spdf[dupApns_spdf$SITUS_COUNTY=='SANTA CRUZ',]) , add=T)
add_meta("CL + Parcels joined", "num dup Parcel APNs", "Monterey County", nrow(dupApns_spdf[dupApns_spdf$SITUS_COUNTY=='MONTEREY',]), add=T)
#
add_meta("CL + Parcels joined", "num missing CLIPS", "San Benito County", nrow(missingClips_spdf[missingClips_spdf$SITUS_COUNTY=='SAN BENITO',]), add=T)
add_meta("CL + Parcels joined", "num missing CLIPS", "Santa Cruz County", nrow(missingClips_spdf[missingClips_spdf$SITUS_COUNTY=='SANTA CRUZ',]) , add=T)
add_meta("CL + Parcels joined", "num missing CLIPS", "Monterey County", nrow(missingClips_spdf[missingClips_spdf$SITUS_COUNTY=='MONTEREY',]), add=T)

## plot missing clips
#tmap_mode('view')
#qtm(missingClips_spdf) + qtm(parcel_polys) 
# Missing CLIPs are all in santa cruz
# It looks like the points all fell between parcels - eg in streets


#########################################################################
## Save  to files
#########################################################################

write_sf(obj = cl_parcel_join, dsn = "mobay_cl_and_parcel_data.geojson")
write_sf(obj = dupClip_spdf, dsn = "mobay_cl_and_parcel_data_dupClips.geojson")
write_sf(obj = dupApns_spdf, dsn = "mobay_cl_and_parcel_data_dupParcelApns.geojson")
write_sf(obj = missingClips_spdf, dsn = "mobay_cl_and_parcel_data_missingClips.geojson")

write_csv(clparcels_metadata, "CL_parcels_pub_metadata.csv")
