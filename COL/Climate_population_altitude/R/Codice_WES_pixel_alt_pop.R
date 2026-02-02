# Municipalities altitude and population from WES 28Jan26 ----
library(terra)
library(data.table)

col = terra::vect("Z:/Shapefiles/GADM/4.1/countries/gadm41_COL_2.shp")
gpw = terra::rast("Z:/GPW/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
alt = terra::rast("Z:/WorldClim/Altitude/wc2.1_30s_elev.bil")

data_wt = terra::extract(
  gpw, col,
  method = "simple",
  exact = TRUE,
  xy = TRUE
)

colnames(data_wt) = c("id", "pop", "xcent", "ycent", "frac") # appena invertito
data_wt <- data_wt[!is.na(data_wt$pop), ]

pts <- terra::vect(
  data_wt,
  geom = c("xcent", "ycent"),
  crs = terra::crs(alt)
)

data_wt$altitude <- terra::extract(alt, pts)[, 2]
data.table::setDT(data_wt)
data_wt$gid_2 <- col$GID_2[data_wt$id]

# write.csv(data_wt, "D:/output.csv", row.names = FALSE)

data_wtWES = read.csv("C:/Users/efesce/Desktop/GitHub/DENVMods2/COL/Climate_population_altitude/Data/outputWES.csv.xz")

# da qui in poi mi scrivo le altitudini medie e sd per ogni municipalit ----
#> assumo che l'ID sia la municipalità quindi aggrego e mergio con il vecchio
#>  file  - NB CHECK NEEDED 28Jan
library(dplyr)
library(tidyr)
library(tidyverse)

tmp = data_wtWES %>%
  group_by(id, gid_2) %>%
  summarise(pop_mean = mean(pop),
            pop_sd = sd(pop),
            altitude_mean = mean(altitude),
            altitude_sd = sd(altitude))


