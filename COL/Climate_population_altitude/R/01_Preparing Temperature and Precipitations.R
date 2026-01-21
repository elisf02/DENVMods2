#> Gridded climate projections are then aggregated across regions defined by the chosen
#>  geographic boundaries using three approaches: 
#>  (1) population-unweighted,
#>  (2) static population-weighted using the baseline population to still isolate the 
#>      raw climate change signal, and 
#>  (3) dynamically population-weighted, updated every ten years based on the previous 
#>        population projections (e.g., 2035 values weighted by the 2030 population, 
#>        2048 values by the 2040 population). 
#>  For each combination of GCM, scenario, and variable, three corresponding columns 
#>  are hence produced: simple, static, and dynamic. 
#>  This procedure is repeated for the selected observational reference 
#>  datasets (using for the dynamic approach population estimates for 2000 for all years 
#>  prior to 2000 and SSP2 for estimates in 2010 and 2020), producing a comprehensive, 
#>  unified set of weather and climate data at the desired level of spatial granularity 
#>  for easy integration into subsequent impact models
#>    
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(ISOweek)
library(janitor)

library(dplyr)
library(stringr)

# install.packages("arrow") needed for sally's new files
library(arrow)

#> From Sally's climate files OLD (shared 19Jan26) - the new files are binary and i don't know what to do
#>  dates aggregated by the isodate, isoweek and isoyear to aggregate data weekly
#>  All files are from 2008 (ISOyear)

dpt_cod = readxl::read_excel("C:/Users/efesce/Desktop/GitHub/DENVMods2/COL/climate_data_dictionary.xlsx",
                             sheet = 'Foglio1')

df_clean = dpt_cod %>%
  mutate(
    dep_id = str_match(GID_1, "COL\\.(\\d+)_")[,2],
    mun_id = str_match(GID_2, "COL\\.\\d+\\.(\\d+)_")[,2]
  )

# dataset con i dipartimenti (senza duplicati)
departments <- df_clean %>%
  distinct(dep_id, NAME_1)

# dataset con le municipalità (senza duplicati)
municipalities <- df_clean %>%
  distinct(mun_id, NAME_2)

setwd_climate = "C:/Users/efesce/OneDrive - Imperial College London/Documents/Work Imperial/Dengue/Shared_ClimateColombia/COL/COL"

# nei nuovi dati sally ha tutte le variabili per municipalità in un unico file ----
#> Preparo un file perogni variabile climatica per ogni DPT, con come colonne le municipalità
#> e come righe ISOdays (e ISOyear)
#> 
#> TEMPERATURE ----
#> Near-surface (2m) air temperature: tas - per ora uso SIMPLE, check!!

for(i in seq(1:nrow(departments))) {
  prq_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  mncp_db = df_clean %>%
    filter(dep_id == dpt_cod) %>%
    distinct(mun_id, NAME_2)
  
  for (i in as.numeric(mncp_db$mun_id)) {
    print(paste(dpt_name,",", mncp_db$NAME_2[i]))
    mun_cod = mncp_db$mun_id[i]
    name_file = paste0('/Central_America_COL_v410_', dpt_cod, '_', mun_cod, 
                       '_2_CHIRPSv3_ERA5Land_1981_2024_observation.parquet.gzip')
    
    prq = read_parquet(paste0(setwd_climate, 
                              name_file))
    
    prq = prq %>%
      mutate(year = year(Date),
             day = yday(Date)) %>%
      select(day, year, tas_static) %>%
      rename(!!make_clean_names(mncp_db$NAME_2[i]) := tas_static)    
    
    # prq_dpt_mun = data.frame(prq) %>%
    #   mutate(NAME_1 = dpt_name,
    #          DPT_ID = dpt_cod,
    #          NAME_2 = mncp_db$NAME_2[i],
    #          MNC_ID = mun_cod)
    # 
    
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(prq_tot)) {
      prq_tot <- prq
    } else {
      prq_tot <- prq_tot %>%
        left_join(
          prq,
          by = c("year", "day"))
    }
    
  }
  write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level2_", dpt_name, "_temperature.csv"))
}

#> PRECIPITATION ----
#> Total precipitation: pr - per ora uso SIMPLE, check!!

for(i in seq(1:nrow(departments))) {
  prq_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  mncp_db = df_clean %>%
    filter(dep_id == dpt_cod) %>%
    distinct(mun_id, NAME_2)
  
  for (i in as.numeric(mncp_db$mun_id)) {
    print(paste(dpt_name,",", mncp_db$NAME_2[i]))
    mun_cod = mncp_db$mun_id[i]
    name_file = paste0('/Central_America_COL_v410_', dpt_cod, '_', mun_cod, 
                       '_2_CHIRPSv3_ERA5Land_1981_2024_observation.parquet.gzip')
    
    prq = read_parquet(paste0(setwd_climate, 
                              name_file))
    
    prq = prq %>%
      mutate(year = year(Date),
             day = yday(Date)) %>%
      select(day, year, pr_static) %>%
      rename(!!make_clean_names(mncp_db$NAME_2[i]) := pr_static)    
    
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(prq_tot)) {
      prq_tot <- prq
    } else {
      prq_tot <- prq_tot %>%
        left_join(
          prq,
          by = c("year", "day"))
    }
    
  }
  write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level2_", dpt_name, "_total_precipitation.csv"))
}

#> specific humidity ----
#> Near-surface specific humidity: huss - per ora uso SIMPLE, check!!

for(i in seq(1:nrow(departments))) {
  prq_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  mncp_db = df_clean %>%
    filter(dep_id == dpt_cod) %>%
    distinct(mun_id, NAME_2)
  
  for (i in as.numeric(mncp_db$mun_id)) {
    print(paste(dpt_name,",", mncp_db$NAME_2[i]))
    mun_cod = mncp_db$mun_id[i]
    name_file = paste0('/Central_America_COL_v410_', dpt_cod, '_', mun_cod, 
                       '_2_CHIRPSv3_ERA5Land_1981_2024_observation.parquet.gzip')
    
    prq = read_parquet(paste0(setwd_climate, 
                              name_file))
    
    prq = prq %>%
      mutate(year = year(Date),
             day = yday(Date)) %>%
      select(day, year, huss_static) %>%
      rename(!!make_clean_names(mncp_db$NAME_2[i]) := huss_static)    
    
    # prq_dpt_mun = data.frame(prq) %>%
    #   mutate(NAME_1 = dpt_name,
    #          DPT_ID = dpt_cod,
    #          NAME_2 = mncp_db$NAME_2[i],
    #          MNC_ID = mun_cod)
    # 
    
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(prq_tot)) {
      prq_tot <- prq
    } else {
      prq_tot <- prq_tot %>%
        left_join(
          prq,
          by = c("year", "day"))
    }
    
  }
  write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level2_", dpt_name, "_spec_hum.csv"))
}


#> relative humidity ----
#> Near-surface relative humidity: hurs - per ora uso SIMPLE, check!!

for(i in seq(1:nrow(departments))) {
  prq_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  mncp_db = df_clean %>%
    filter(dep_id == dpt_cod) %>%
    distinct(mun_id, NAME_2)
  
  for (i in as.numeric(mncp_db$mun_id)) {
    print(paste(dpt_name,",", mncp_db$NAME_2[i]))
    mun_cod = mncp_db$mun_id[i]
    name_file = paste0('/Central_America_COL_v410_', dpt_cod, '_', mun_cod, 
                       '_2_CHIRPSv3_ERA5Land_1981_2024_observation.parquet.gzip')
    
    prq = read_parquet(paste0(setwd_climate, 
                              name_file))
    
    prq = prq %>%
      mutate(year = year(Date),
             day = yday(Date)) %>%
      select(day, year, hurs_static) %>%
      rename(!!make_clean_names(mncp_db$NAME_2[i]) := hurs_static)    
    
    # prq_dpt_mun = data.frame(prq) %>%
    #   mutate(NAME_1 = dpt_name,
    #          DPT_ID = dpt_cod,
    #          NAME_2 = mncp_db$NAME_2[i],
    #          MNC_ID = mun_cod)
    # 
    
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(prq_tot)) {
      prq_tot <- prq
    } else {
      prq_tot <- prq_tot %>%
        left_join(
          prq,
          by = c("year", "day"))
    }
    
  }
  write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level2_", dpt_name, "_rel_hum.csv"))
}





