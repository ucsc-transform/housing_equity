#=========================================
# Joining Corelogic Data to Parcel Data
# to map publicly owned parcles
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr, sqldf, stringr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")

# Corelogic data files
cl_file <- "./ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.xlsx"
#cl_file2 <- "ucsc_institute_for_social_transformation_property_basic2_dpc_01466432_20230804_091703_data.txt" # above saved to text with "|" sep

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
# NOTE: there are 244784 parcels in the 3 county parcel files
#254837 - 244784 = 10053

# After previously exploring the data it looks like any parcel that is publicly owned has a *corporate ownership* designation
# Where publicly owned parcels are a subset of that designation
# So, first read in the data - filtered by those parcels where owner is corporate

# Subset the data to only keep parcels with corporate owner
df0 <- df0[!is.na(df0$"OWNER.1.CORPORATE.INDICATOR") & df0$"OWNER.1.CORPORATE.INDICATOR"=="Y",]
dim(df0) # 36319 because of the  filter - but there are 254,837 properties in CL dataset

# take a look at the column names
colnames(df0)

# Check to see what cols have lots of values so that they are useful
num_rows <- nrow(df0)
print("Columns where 40% or more cells have values")
for (i in colnames(df0)) {
  #n <- sum( is.na(df0[[i]]) | grepl("^ *$", df[[i]]) ) # doesnt work as no NAs
  # for the most part
  # na string VALs are encoded as "" and empty num col vals are zeros
  n <- nrow(df0[df0[[i]]=="" ,]) + nrow(df0[df0[[i]]==0,])
  p <- n/num_rows
  if (p < .6) {
    #print(paste0("Number of empty cells in col ", i, " : ", n ) )
    print(paste0( i, " : ", 1 - round(p,2) ) )
  }
}

#manually double check a few
i <- "MUNICIPALITY.NAME"
i <- "SITUS.CITY"
i < "ZONING.CODE"
n <- nrow(df0[df0[[i]]=="" ,]) + nrow(df0[df0[[i]]==0,])
p <- n/num_rows
print(p)

# These cols have proportion of cells with values > .4
# [1] "CLIP : 1"
# [1] "FIPS.CODE : 1"
# [1] "APN..PARCEL.NUMBER.UNFORMATTED. : 1"
# [1] "APN.SEQUENCE.NUMBER : 1"
# [1] "COMPOSITE.PROPERTY.LINKAGE.KEY : 1"
# [1] "ORIGINAL.APN : 1"
# [1] "ONLINE.FORMATTED.PARCEL.ID : 1"
# [1] "CENSUS.ID : 0.91"
# [1] "TAX.RATE.AREA.CODE : 1"
# [1] "SCHOOL.DISTRICT : 0.95"
# [1] "FIRE.DISTRICT : 0.42"
# [1] "ELEMENTARY.SCHOOL.DISTRICT.COUNTY.DESCRIPTION : 0.44"
# [1] "HIGH.SCHOOL.DISTRICT.COUNTY.DESCRIPTION : 0.6"
# [1] "NEIGHBORHOOD.DESCRIPTION : 0.53"
# [1] "SITUS.CORE.BASED.STATISTICAL.AREA..CBSA. : 1"
# [1] "LAND.USE.CODE : 1"
# [1] "COUNTY.USE.DESCRIPTION : 1"
# [1] "PROPERTY.INDICATOR.CODE : 0.92"
# [1] "BLOCK.LEVEL.LATITUDE : 0.61"
# [1] "BLOCK.LEVEL.LONGITUDE : 0.61"
# [1] "PARCEL.LEVEL.LATITUDE : 0.91"
# [1] "PARCEL.LEVEL.LONGITUDE : 0.91"
# [1] "SITUS.HOUSE.NUMBER : 0.48"
# [1] "SITUS.STREET.NAME : 0.62"
# [1] "SITUS.MODE : 0.57"
# [1] "SITUS.CITY : 0.62"
# [1] "SITUS.STATE : 1"
# [1] "SITUS.ZIP.CODE : 0.62"
# [1] "SITUS.COUNTY : 1"
# [1] "SITUS.CARRIER.ROUTE : 0.44"
# [1] "SITUS.STREET.ADDRESS : 0.62"
# [1] "SITUS.CITY.STATE.ZIP.SOURCE : 0.62"
# [1] "SITUS.DELIVERY.POINT.VALIDATION.CODE : 0.44"
# [1] "LEGAL.LOT.NUMBER : 0.46"
# [1] "LEGAL.DESCRIPTION : 0.69"
# [1] "OWNER.1.FULL.NAME : 1"
# [1] "OWNER1CORPORATEINDICATOR : 1"
# [1] "MAILING.HOUSE.NUMBER : 0.77"
# [1] "MAILING.STREET.NAME : 0.99"
# [1] "MAILING.MODE : 0.72"
# [1] "MAILING.CITY : 1"
# [1] "MAILING.STATE : 1"
# [1] "MAILING.ZIP.CODE : 1"
# [1] "MAILING.CARRIER.ROUTE : 0.96"
# [1] "MAILING.STREET.ADDRESS : 0.99"
# [1] "MAILING.DELIVERY.POINT.VALIDATION.CODE : 0.96"
# [1] "TOTAL.VALUE.CALCULATED : 0.71"
# [1] "LAND.VALUE.CALCULATED : 0.7"
# [1] "IMPROVEMENT.VALUE.CALCULATED : 0.48"
# [1] "CALCULATED.VALUE.SOURCE.INDICATOR : 1"
# [1] "ASSESSED.TOTAL.VALUE : 0.71"
# [1] "ASSESSED.LAND.VALUE : 0.7"
# [1] "ASSESSED.IMPROVEMENT.VALUE : 0.48"
# [1] "TAX.AMOUNT : 0.76"
# [1] "TAX.YEAR : 0.76"
# [1] "ASSESSED.YEAR : 1"
# [1] "TAX.AREA.CODE : 1"
# [1] "TAXABLE.LAND.VALUE : 0.59"
# [1] "NET.TAXABLE.AMOUNT : 0.66"
# [1] "TRANSACTION.BATCH.DATE : 0.57"
# [1] "TRANSACTION.BATCH.SEQUENCE.NUMBER : 0.57"
# [1] "SALE.RECORDED.DOCUMENT.NUMBER : 0.43"
# [1] "SALE.DOCUMENT.TYPE.CODE : 0.57"
# [1] "SALE.RECORDING.DATE : 0.57"
# [1] "SALE.AMOUNT : 0.41"
# [1] "SALE.CODE : 0.41"
# [1] "TRANSACTION.TYPE.CODE : 0.57"
# [1] "SELLER.NAME : 0.5"
# [1] "ACRES : 0.93"
# [1] "LAND.SQUARE.FOOTAGE : 0.93"
# [1] "LAST.ASSESSOR.UPDATE.DATE : 1"
# [1] "TAXROLL.CERTIFICATION.DATE : 1"
# [1] "RECORD.ACTION.INDICATOR : 1"
#------------------------------------------------
# Of the above we note which ones we will keep
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

# [1] "PARCEL.LEVEL.LATITUDE : 0.91"
# [1] "PARCEL.LEVEL.LONGITUDE : 0.91"

# [1] "SITUS.CITY : 0.62"
# [1] "SITUS.COUNTY : 1"
# [1] "TAX.RATE.AREA.CODE : 1"
# [1] "SITUS.STREET.ADDRESS : 0.62"

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
                 "LAND.USE.CODE", "COUNTY.USE.DESCRIPTION" ,"STATE.USE.DESCRIPTION", 
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
#Remove big corelogic dataframe
rm(df0)

# Check out the types of corp owners
owner_df <- as.data.frame(table(df$OWNER.1.FULL.NAME))

# By looking through the names of the big owners
# we get a sense of the name patterns for public owners
bigowner_df <- owner_df[owner_df$Freq > 10,]

# Create a new column called public_owner
# that is 1 for true and 0 for false
# that is an estimate of whether or not parcel is publically owned
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
unknown_df <- as.data.frame(table(df1[df1$public_owner==0,]$OWNER.1.FULL.NAME))
# checks
unknown_df[grepl('harbor dist', unknown_df$Var1, ignore.case=T),]
unknown_df[grepl('school', unknown_df$Var1, ignore.case=T),]

# check if all parcels with county.use.description = publicly owned have public_owner==1)
# check if any with public_owner = 0 have county.use.description = pub own
countyuse_df1 <- as.data.frame(table(df1[df1$public_owner==1,]$COUNTY.USE.DESCRIPTION))
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

# Check usetypes
pubtab_df <- as.data.frame(table(pub_df$COUNTY.USE.DESCRIPTION))
pubtab2_df <- as.data.frame(table(pub_df$OWNER.1.FULL.NAME))
# Now let's categorize all the use_types

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
pub_df <- pub_df %>%
  mutate(MUNICIPALITY.NAME = dplyr::case_when(
    MUNICIPALITY.NAME == '' ~ "NONE OR UNDETERMINED",
    TRUE ~ pub_df$MUNICIPALITY.NAME
  )) 

table(pub_df$MUNICIPALITY.NAME, useNA="always")

# Add text to zonning col so we can use as a filter
table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always")
pub_df <- pub_df %>%
  mutate(ZONING.CODE.DESCRIPTION = dplyr::case_when(
    ZONING.CODE.DESCRIPTION == '' ~ "unknown",
    TRUE ~ pub_df$ZONING.CODE.DESCRIPTION
  )) 
table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always")

dim(pub_df)
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

pub_spdf <- st_as_sf(pub_geom_df, 
                     coords = c("PARCEL.LEVEL.LONGITUDE", "PARCEL.LEVEL.LATITUDE"), 
                     crs=4326)
table(pub_spdf$SITUS.COUNTY)

#MONTEREY SAN BENITO SANTA CRUZ
#3608         31       2987 

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

# Note: there are more sbparcels than rows in CL data likely due to multipart parcels
sbparcels <- st_make_valid(sbparcels)
sb_cl_parcels <- sbparcels[pub_spdf,]

# SB Parcels that have points/data in the CL data
dim(sb_cl_parcels) # should be 31 parcels

rm(sbparcels)
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
scparcels0 <- st_make_valid(scparcels)
dim(scparcels0) #97021
sc_cl_parcels <- scparcels0[pub_spdf,]

# SC Parcels that have points/data in the CL data
dim(sc_cl_parcels) # should be 2987 parcels
#[1] 2989   59

#
# Remove files from memory
#
rm(scparcels)

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
# select only pub_spdf pts that spatially intersect mo_parcels
mo_cl_parcel_points <- pub_spdf[moparcels,]

# MO Parcels that have points/data in the CL data
dim(mo_cl_parcel_polys) # should be 3608 parcels
#[1] 3566   90
dim(mo_cl_parcel_points) # should be 3608 parcels
#[1] 3608   35
# THESE NUMBERS ARE NOT THE SAME (off by 42)
# They differ spatially bc more than one point is intersecting some polygons
# and some points are not within any polygons
#
# Remove files from memory
#
#rm(moparcels)
mo_cl_parcels <- mo_cl_parcel_polys
# check
plot(mo_cl_parcels$geometry, col="green")
plot(sb_cl_parcels$geometry, col="blue")
plot(sc_cl_parcels$geometry, col="red")

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

sbparcels_pub <- sbparcels_pub
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
mo2 <- moparcels_pub[c('pdata_APN')]
sc2 <- scparcels_pub[c('pdata_APN')]
sb2 <-sbparcels_pub[c('pdata_APN')]
allp <- rbind(mo2, sc2, sb2)
dim(allp) #6587    2

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
View(scparcels_pub_with_flood)
dim(scparcels_pub_with_flood)
dim(scparcels_pub)
#
# ok we have picked up a bunch of parcels bc
# parcel likely intersected +1 flood zones
# so want to flatten, eg 00226103

#test - flatten flood vars into one-to-one 
doh <- scparcels_pub_with_flood[scparcels_pub_with_flood$pdata_APN == '00226103',]
paste(unlist(doh$FLD_ZONE),collapse = "+")
#try test on entire data set
doh <- scparcels_pub_with_flood %>% group_by(., pdata_APN) %>% 
  mutate(
   floodstring = paste(unlist(unique(FLD_ZONE)),collapse = "+")
  )
# above works but still need to 
## - convert code to descriptive strings
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
doh <- scparcels_pub_with_flood2 %>% distinct()

# check
dim(scparcels_pub_with_flood2)
dim(scparcels_pub)
dim(doh)

#rename
scparcels_pub <- doh
#-----

moparcels_pub <- moparcels_pub %>%
  mutate(infloodzone = dplyr::case_when(
    grepl('X',MO_Flood_Zone, ignore.case=T) ~ "YES 500 year",
    grepl('A|V', MO_Flood_Zone, ignore.case=T) ~ "YES 100 year",
    TRUE ~ "NO"
  ))


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
# initial cols we want to explore
## we may add others later
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

# lets ad a UID to all parcel rows
mobay_spdf$uid <-seq.int(nrow(mobay_spdf))
nrow(mobay_spdf) == length(unique(mobay_spdf$uid))

# Make sure all data looks like its in correct county
plot(mobay_spdf[mobay_spdf$pdata_county=="MONTEREY",]$geometry)
plot(mobay_spdf[mobay_spdf$pdata_county=="SAN BENITO",]$geometry)
plot(mobay_spdf[mobay_spdf$pdata_county=="SANTA CRUZ",]$geometry)

#######################################
# Check filter vals - don't want NAs
#######################################
doh <- as.data.frame(table(pub_df$MUNICIPALITY.NAME, useNA="always"))
doh #<NA> 3938
doh <- as.data.frame(table(pub_df$ZONING.CODE.DESCRIPTION, useNA="always"))
doh# 5039
doh <- as.data.frame(table(pub_df$SITUS.COUNTY, useNA="always"))
doh # 0
doh <- as.data.frame(table(pub_df$owner_cat, useNA="always"))
doh # 0
doh <- as.data.frame(table(pub_df$use_type, useNA="always"))
doh #0
# check these vals too
as.data.frame(table(mobay_spdf$infirezone, useNA="always"))
as.data.frame(table(mobay_spdf$infloodzone, useNA="always"))
as.data.frame(table(mobay_spdf$infaultzone, useNA="always"))
#fixes
pub_df <- pub_df %>% replace_na(list(MUNICIPALITY.NAME = 'none or no data', ZONING.CODE.DESCRIPTION = "none or no data"))
#############################
#Check coord values
#############################
nrow(is.na(mobay_spdf$geometry))
nrow(mobay_spdf[is.na(mobay_spdf$geometry),])

##################
# geom fixes
##########################

parcel_polys1 <- mobay_spdf[st_is_valid(mobay_spdf),]
parcel_polys2 <- st_make_valid(mobay_spdf)
dim(mobay_spdf)
dim(parcel_polys1) # we have lost 8 invalid polygons
dim(parcel_polys2) # winner - keeps all polys
#remove Z dimensions
parcel_polys <- st_zm(parcel_polys2)
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
dim(parcel_polys) # 6587 - so yes no dupes

#parcel_acres are too hard to filter bc of range so make a simple binary for filtering
parcel_polys$acresGTquarter <- if_else(parcel_polys$parcel_acres >=0.25, "YES", "NO")
############################################
# Join corelogic data to the parcel data
############################################

# FIRST RENAME CL  COLS BEFORE SAVING
colnames(pub_spdf)
pub_spdf <- pub_spdf %>% 
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))
colnames(pub_spdf)

#
# Now Join SPATIALLY
#

joined_sf <- st_join(parcel_polys, pub_spdf)

dim(joined_sf)
table(joined_sf$SITUS_COUNTY)
table(joined_sf$pdata_county)

# CHECKS
length(unique(joined_sf$CLIP))
# which parcels are duplicated
doh <- as.data.frame(table(joined_sf$CLIP))
doh2 <- as.data.frame(table(joined_sf$uid))

dupClips <- doh[doh$Freq > 1,]$Var1
dupUids <- doh2[doh2$Freq > 1,]$Var1

dupClip_spdf <- joined_sf[joined_sf$CLIP %in% dupClips,]
dupUids_spdf <- joined_sf[joined_sf$uid %in% dupUids,]

# Note there are dups bc in many cases condos in one building have
# multiple rows in CL data (not sure if same is true in parcel data)

#########################################################################
## Save  to files
#########################################################################

write_sf(obj = joined_sf, dsn = "mobay_cl_and_parcel_data.geojson")

#=============================================================================
# Sanity checks
#=============================================================================
# df is full CL dataset rows but with fewer cols
# df0 is CL dataset with corporate ownership
# df1/pub_df is CL dataset that we estimate is publically owned
# pub_geom_df is the CL dataship public owned with geom
# pub_spdf is spatial df of pub_geom_df

# how many CL rows are coded as corporate owner
dim(df) #[1] 36319    31

# how many CL rows have unique CLIP
length(unique(df$CLIP)) #36319

# how many CL rows have unique Parcel IDs
length(unique(df$ORIGINAL.APN)) #36319
length(unique(df$APN_PARCEL_NUM_UNFORMATTED)) #36319
length(unique(df$ONLINE.FORMATTED.PARCEL.ID)) #36319

# How many CL rows have lon/lats
nrow(df[df$PARCEL.LEVEL.LATITUDE != '',]) #0
nrow(df[!is.na(df$PARCEL.LEVEL.LATITUDE),]) #[1] 33095

# How many CL rows have we estimated to have public owner
nrow(df1[df1$public_owner==1,]) #6950
nrow(pub_df[pub_df$public_owner==1,]) #6950

# How many CL rows, pub owned have geom
nrow(pub_geom_df) #6626 (324 fewer)
nrow(pub_spdf) #6626 (same as above but spatial)
# why fewer - because we ddropped cl rows w/no geom
nrow(df1[df1$public_owner==1 & is.na(df$PARCEL.LEVEL.LATITUDE),]) #324

# Did we lose or gain any parcels along the way
# and if yes why?
dim(sb_cl_parcels) #32
nrow(pub_spdf[pub_spdf$SITUS_COUNTY=="SAN BENITO",]) #31
nrow(joined_sf[joined_sf$SITUS_COUNTY=="SAN BENITO",]) #32
nrow(joined_sf[joined_sf$pdata_county=='SAN BENITO',]) #32
# so l ooks like a county mismatch or dup parcel that crosses county line

# WHAT ROWS are in the CL data that are not in the combined cl-parcel data?
cldiff <- unlist(setdiff(pub_spdf[pub_spdf$SITUS_COUNTY=="SAN BENITO",]$CLIP, joined_sf[joined_sf$pdata_county=='SAN BENITO',]$CLIP ))
cldiff
#numeric(0)

# WHAT ROWs are in the Parcel+CL data that were not in the CL data
pdiff <- unlist(setdiff(joined_sf[joined_sf$pdata_county=='SAN BENITO',]$CLIP, pub_spdf[pub_spdf$SITUS_COUNTY=="SAN BENITO",]$CLIP ))
pdiff
#[1] 4209133238
sb_pdiff <- joined_sf[joined_sf$CLIP %in% pdiff,]
## This CLIP has one row in the CL data w/in Santa Cruz county and two rows
## in the parcel data - one in Santa Cruz (8.22 acres) and one in San Benito (1.7acres)
## the CL ORIGNINAL.APN is 11025108 (sc)
## the Parceldata APNs are pdata_APN == 11025108 (sc) and 0110400020 (sb)

## Santa cruz checks
dim(sc_cl_parcels) #2989
nrow(pub_spdf[pub_spdf$SITUS_COUNTY=="SANTA CRUZ",]) #2987
# check difs - what CL rows are not in the joined data?
cldiff <- unlist(setdiff(pub_spdf[pub_spdf$SITUS_COUNTY=="SANTA CRUZ",]$CLIP, joined_sf[joined_sf$pdata_county=='SANTA CRUZ',]$CLIP ))
cldiff
#[1] 8368801894 2344249575 8741225145 5824245049 2726159633 8551421127 6786356471 5077199464 2600303052 5396889904 8383529543 2721499372
#[13] 2338972869
sc_cldiff <- pub_spdf[pub_spdf$CLIP %in% cldiff,]

sc_cldiff2 <- joined_sf[joined_sf$pdata_APN %in% sc_cldiff$ORIGINAL_APN,] # the APNs are not in there either
sc_cldiff3 <- scparcels0[scparcels0$APNNODASH %in% sc_cldiff$ORIGINAL_APN,] # the APNs are not in there either

tmap_mode('view')
qtm(sc_cl_parcels ) + qtm(sc_cldiff)
# when I look at the cl points that are not in the SC data its because they don't intersect any sc_cl_parcels
qtm(scparcels0) + qtm(sc_cldiff)
qtm(sc_cldiff3) + qtm(sc_cldiff)

# FINDING
# the 13 CL points in cldiff are IN the sc parcel data but they did not get selected via
# spatial intersection due to geometry issues - the SC parcels are oddly shapped and the CL points don't intersect
# SO further upstream (RIGHT after the spatial intersection) we can manually add these

# Ok so what Parcel rows are in JOINED data but NOT the CL data
pdiff <- setdiff(joined_sf[joined_sf$pdata_county=='SANTA CRUZ',]$CLIP, pub_spdf[pub_spdf$SITUS_COUNTY=="SANTA CRUZ",]$CLIP )
#[1] 6380726482 7368375233

qtm(joined_sf[joined_sf$CLIP %in% pdiff,]) + qtm(pub_spdf[pub_spdf$CLIP %in% pdiff,])

# FINDING
# those two pdfiff polys ARE in the output data in 4 parcel polys, 2 in moco and 2 in sc
# but the CL points are only in moco. so no data loss but data gain - no action needed.

#####################################
# MONTEREY PARCEL Discrepancies
#####################################
dim(mo_cl_parcels) #3566
mo_pub_spdf <- pub_spdf[pub_spdf$SITUS_COUNTY=="MONTEREY",]
nrow(mo_pub_spdf) #3608 (+42 rows)
mo_joined <- joined_sf[joined_sf$SITUS_COUNTY=="MONTEREY",]
nrow(mo_joined) #3636 (+70 rows)
nrow(joined_sf[joined_sf$pdata_county=="MONTEREY",]) #3634
# so different county values (pdata_county vs SITUS_COUNTY) for two rows

doha<- as.data.frame(table(mo_pub_spdf$CLIP)) # any dups
nrow(doha[doha$Freq > 1,])
#[1] 0 # no dup clips
dohb<- as.data.frame(table(mo_pub_spdf$ORIGINAL_APN)) # any dups
#[1] 0 #nrow(dohb[dohb$Freq > 1,])
# no dup APNs

# ANY missing APNS
mo_cl_parcels[!(mo_cl_parcels$APN %in% mo_pub_spdf$ORIGINAL_APN),]
# so why mo_pub_spdf has 42 more rows than mo_cl_parcels?
(mo_missing = st_disjoint(mo_cl_parcels,mo_pub_spdf, sparse=FALSE))
#apply(mo_missing, 1, any)
nrow(mo_cl_parcels[!apply(mo_missing, 1, any),])
nrow(mo_pub_spdf)
nrow(mo_missing)
nrow(mo_pub_spdf[!(mo_pub_spdf$ORIGINAL_APN %in% mo_cl_parcels$APN),])
qtm(mo_cl_parcels) + qtm(mo_pub_spdf[!(mo_pub_spdf$ORIGINAL_APN %in% mo_cl_parcels$APN),])
#eg
#113092014000 in mo_pub ORIGINAL_APN
#113092013000 in mo_cl APN

# Finding
# in mo_pub_spdf (CL pub rows for MoCo there are no dupe APNs or CLIPs)
# but once we join with the moparcel data we get dupes
# eg

mo_joined <-joined_sf[joined_sf$SITUS_COUNTY=="MONTEREY",] 
nrow(mo_joined) #3636 (+70 rows)
doh1<- as.data.frame(table(mo_joined$CLIP)) # any dups
nrow(doh1[doh1$Freq > 1,])
# YES 28 dup clips
doh2 <- as.data.frame(table(mo_joined$ORIGINAL_APN)) # any dups
nrow(doh2[doh2$Freq > 1,])
# YES 28 dup APNs

# are they the same dupes?
mo_pub_clip <- mo_pub_spdf[(mo_pub_spdf$CLIP %in% doh1[doh1$Freq > 1,]$Var1),]
mo_dup_clip <- mo_joined[(mo_joined$CLIP %in% doh1[doh1$Freq > 1,]$Var1),]
nrow(mo_dup_clip) #56

mo_dup_clip2
qtm(mo_dup_clip[mo_dup_clip$CLIP== '1006852797',]) + 
  qtm(pub_spdf[pub_spdf$CLIP == 1006852797,])

qtm(mo_dup_clip) + 
  qtm(pub_spdf[pub_spdf$CLIP %in% mo_dup_clip$CLIP,])
# the dup clip ids in pub_spdf seem to make sense when 
# the pub_spdf points are mapped on the dup clip polys
# in these cases geom differences account for the dups
# and the dupes are valid and should stay in pub_spdf

qtm(mo_dup_clip) + 
  qtm(joined_sf[joined_sf$CLIP %in% mo_dup_clip$CLIP,])

# so same set in pub_spdf and joined_sf
nrow(joined_sf[joined_sf$CLIP %in% mo_dup_clip$CLIP,]) # 56
nrow(pub_spdf[pub_spdf$CLIP %in% mo_dup_clip$CLIP,]) # 28

# check difs - what MOCO CL rows are not in the joined data?
cldiff <- unlist(setdiff(pub_spdf[pub_spdf$SITUS_COUNTY=="MONTEREY",]$CLIP, 
                         joined_sf[joined_sf$pdata_county=='MONTEREY',]$CLIP ))


#numeric(0) YEH
mo_pub_spdf <- pub_spdf[pub_spdf$SITUS_COUNTY=="MONTEREY",]
cldiff2 <-  mo_pub_spdf[!(mo_pub_spdf$ORIGINAL_APN %in% mo_cl_parcels$APN),]
nrow(cldiff2) #57
length(unique(cldiff2$CLIP))
length(unique(cldiff2$ORIGINAL_APN))
cldiff2b <- subset(mo_pub_spdf, !(ORIGINAL_APN %in% mo_cl_parcels$APN))
nrow(cldiff2b)


qtm(mo_cl_parcels) + qtm(cldiff2b, dots.col="red")
#SOme of the diffs are a spatial mismatch so not in mo_cl_parcels should be in joined_sf
#Some are in both but have diff APNS, eg
####OAPN: 031111045000
####APN:  031111048000

print(cldiff2[1,"ORIGINAL_APN"]$ORIGINAL_APN)
print(pub_spdf[!(pub_spdf[pub_spdf$SITUS_COUNTY=="MONTEREY",]$ORIGINAL_APN %in% mo_cl_parcels$APN),]$ORIGINAL_APN)
print(mo_cl_parcels[!(mo_cl_parcels$APN %in% pub_spdf$ORIGINAL_APN),]$APN)
# Ok so what Parcel rows are in JOINED data but NOT the CL data
pdiff <- setdiff(joined_sf[joined_sf$pdata_county=='MONTEREY',]$CLIP, 
                 pub_spdf[pub_spdf$SITUS_COUNTY=="MONTEREY",]$CLIP )
pdiff
#numeric(0) HUH?

mo_cl_not_in_pdata <- mo_cl[!(mo_cl_parcels$`APN.(PARCEL.NUMBER.UNFORMATTED)` %in% moparcels$APN),]
table(mo_cl_not_in_pdata$RECORD.ACTION.INDICATOR, useNA="always")
qtm(joined_sf[joined_sf$CLIP %in% pdiff,]) + qtm(pub_spdf[pub_spdf$CLIP %in% pdiff,])
