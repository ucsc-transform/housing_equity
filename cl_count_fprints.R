#=========================================
# Joining Corelogic Data to Parcel Data
# to map publicly owned parcles
#=========================================

library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr, sqldf, stringr)
p_load(sf, tmap)

setwd("~/Documents/ucsc/corelogic")

# Corelogic data file with parcel info
clfile <- "mobay_cl_and_parcel_data.geojson"

# footprint data files
moprintsf <- "footprints/monterey_footprints.csv"
scprintsf <- "footprints/santacruz_footprints.csv"
sbprintsf <- "footprints/sanbenito_footprints.csv"

# Read in the data
clparcels <- st_read(clfile)

moprints <- st_read(moprintsf)
sbprints <- st_read(sbprintsf)
scprints <- st_read(scprintsf)

# Set the CRS on our footprint data
# interset to keep only the building footprints that intersect our public parcels
st_crs(moprints) <- 4326
st_crs(sbprints) <- 4326
st_crs(scprints) <- 4326

# clean the geom as needed
moprints <- st_make_valid(moprints)
sbprints <- st_make_valid(sbprints)
scprints <- st_make_valid(scprints)

# Keep only the prints that intersect our pub parcles
moprints_pub <- moprints[clparcels,]
sbprints_pub <- sbprints[clparcels,]
scprints_pub <- scprints[clparcels,]

# create point data
# mo_fprint_ctrs <- st_centroid(moprints_pub)
# sb_fprint_ctrs <- st_centroid(sbprints_pub)
# sc_fprint_ctrs <- st_centroid(scprints_pub)

# MERGE the point data into one file
#fprint_ctrs <- rbind(mo_fprint_ctrs, sb_fprint_ctrs, sc_fprint_ctrs)
fprint_polys <- rbind(moprints_pub, sbprints_pub, scprints_pub)
#plot(fprint_ctrs$geometry)

# count the number of footprints within each clparcel
#fprint_counts <- st_within(fprint_ctrs, clparcels, sparse = FALSE)

# LETS TRY THIS WITH POLYS
fprint_counts <- st_intersects(fprint_polys, clparcels, sparse = FALSE)
clparcels_wpts <- clparcels %>%
  mutate(bldg_counts = apply(fprint_counts, 2, sum))

table(clparcels_wpts$bldg_counts, useNA="always")

#tmap_mode('view')
#qtm(clparcels_wpts[clparcels_wpts$bldg_counts==0,])

# ok so works

# add binary
clparcels_wpts$hasBldg <- if_else(clparcels_wpts$bldg_counts==0, "NO", "YES")
table(clparcels_wpts$hasBldg)


# Clean up the footprints to save to file
## add a unique id
fprint_polys$uid <-seq.int(nrow(fprint_polys))
nrow(fprint_polys) == length(unique(fprint_polys$uid))

################################
# SAVE to files
################################
write_sf(obj = clparcels_wpts, dsn = "mobay_cl_and_parcel_fp_polys_data.geojson")
write_sf(obj = fprint_polys, dsn = "mobay_pub_fprints.geojson")

