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
#> seleziono solo i dipartimenti con popolazione > 970k
# (as given here: https://en.wikipedia.org/wiki/Departments_of_Colombia).
dpts970 = c("antioquia", "valle",  "atlantico", "cundinamarca", "santander",
         "bolivar", "cordoba", "norte_santander", "narino", "cauca", 
         "magdalena", "tolima", "cesar", "boyaca", "huila", 
         "meta","caldas", "guajira", "risaralda", "sucre")

departments <- df_clean %>%
  distinct(dep_id, NAME_1)  %>%
  mutate(
    NAME_1 = make_clean_names(NAME_1),
    NAME_1 = case_when(
      NAME_1 == "valle_del_cauca"      ~ "valle",
      NAME_1 == "norte_de_santander"   ~ "norte_santander",
      NAME_1 == "la_guajira"           ~ "guajira",
      TRUE                             ~ NAME_1
    )
  ) %>%
  filter(NAME_1 %in% dpts970)
    
#> NB nella cartella COL manca il dpt 27, che però ha poche persone quindi me ne frego 
#> per ora ma devo fare check per il futuro e prossimi db       
 
setwd_climate = "C:/Users/efesce/Desktop/COL_Original"

#> TEMPERATURE ----
prq_tot = c()

for(i in seq(1:nrow(departments))) {
  df_clim_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  for(anni in c('1980_1989', '1990_1999', '2000_2009',
                '2010_2019', '2020_2029')){
    
    print(paste(dpt_name, anni))
    
    name_file = paste0('/COL_v410_', dpt_cod, 
                       '_2_ERA5_Land_2m_temperature_', anni, '.csv')
    
    prq = read.csv(paste0(setwd_climate, 
                          name_file))
    
    colnames(prq) = c("Date", 
                      "cos",
                      "pop_2000",
                      "pop_2005",
                      "pop_2010",
                      "pop_2015",
                      "pop_2020")
    
    df_clim = prq %>% 
      mutate(day = yday(Date),
             year = year(Date),
             clim = case_when(year  <=        2002      ~ pop_2000,
                              year %in% seq(2003, 2007) ~ pop_2005,
                              year %in% seq(2008, 2012) ~ pop_2010,
                              year %in% seq(2013, 2017) ~ pop_2015,
                              year >        2017        ~ pop_2020)) %>%
      select(day, year, clim) %>%
      rename(!!dpt_name := clim)  
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(df_clim_tot)) {
      df_clim_tot <- df_clim
    } else {
      df_clim_tot <- rbind(df_clim_tot, df_clim)
    }
    
  }
  #> Daily temperature time series 
  #> from two regions were aligned by year and ISO day and merged into a single dataset.
  if (is.null(prq_tot)) {
    prq_tot <- df_clim_tot
  } else {
    prq_tot <- prq_tot %>%
      left_join(
        df_clim_tot,
        by = c("year", "day"))
  }}

write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level1_temperature_Old_climate_file_dpt970k.csv"))

#> PRECIPITATION ----
prq_tot = c()

for(i in seq(1:nrow(departments))) {
  df_clim_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  for(anni in c('1980_1989', '1990_1999', '2000_2009',
                '2010_2019', '2020_2029')){
    
    print(paste(dpt_name, anni))
    
    name_file = paste0('/COL_v410_', dpt_cod, 
                       '_2_CHIRPS_total_precipitation_', anni, '.csv')
    
    prq = read.csv(paste0(setwd_climate, 
                          name_file))
    
    colnames(prq) = c("Date", 
                      "cos",
                      "pop_2000",
                      "pop_2005",
                      "pop_2010",
                      "pop_2015",
                      "pop_2020")
    
    df_clim = prq %>% 
      mutate(day = yday(Date),
             year = year(Date),
             clim = case_when(year  <=        2002      ~ pop_2000,
                              year %in% seq(2003, 2007) ~ pop_2005,
                              year %in% seq(2008, 2012) ~ pop_2010,
                              year %in% seq(2013, 2017) ~ pop_2015,
                              year >        2017        ~ pop_2020)) %>%
      select(day, year, clim) %>%
      rename(!!dpt_name := clim)  
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(df_clim_tot)) {
      df_clim_tot <- df_clim
    } else {
      df_clim_tot <- rbind(df_clim_tot, df_clim)
    }
    
  }
  #> Daily temperature time series 
  #> from two regions were aligned by year and ISO day and merged into a single dataset.
  if (is.null(prq_tot)) {
    prq_tot <- df_clim_tot
  } else {
    prq_tot <- prq_tot %>%
      left_join(
        df_clim_tot,
        by = c("year", "day"))
  }}

write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level1_total_precip_Old_climate_file_dpt970k.csv"))

#> Specific Humidity ----
prq_tot = c()

for(i in seq(1:nrow(departments))) {
  df_clim_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  for(anni in c('1980_1989', '1990_1999', '2000_2009',
                '2010_2019', '2020_2029')){
    
    print(paste(dpt_name, anni))
    
    name_file = paste0('/COL_v410_', dpt_cod, 
                       '_2_ERA5_Land_specific_humidity_', anni, '.csv')
    
    prq = read.csv(paste0(setwd_climate, 
                          name_file))
    
    colnames(prq) = c("Date", 
                      "cos",
                      "pop_2000",
                      "pop_2005",
                      "pop_2010",
                      "pop_2015",
                      "pop_2020")
    
    df_clim = prq %>% 
      mutate(day = yday(Date),
             year = year(Date),
             clim = case_when(year  <=        2002      ~ pop_2000,
                              year %in% seq(2003, 2007) ~ pop_2005,
                              year %in% seq(2008, 2012) ~ pop_2010,
                              year %in% seq(2013, 2017) ~ pop_2015,
                              year >        2017        ~ pop_2020)) %>%
      select(day, year, clim) %>%
      rename(!!dpt_name := clim)  
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(df_clim_tot)) {
      df_clim_tot <- df_clim
    } else {
      df_clim_tot <- rbind(df_clim_tot, df_clim)
    }
    
  }
  #> Daily temperature time series 
  #> from two regions were aligned by year and ISO day and merged into a single dataset.
  if (is.null(prq_tot)) {
    prq_tot <- df_clim_tot
  } else {
    prq_tot <- prq_tot %>%
      left_join(
        df_clim_tot,
        by = c("year", "day"))
  }}

write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level1_specific_humid_Old_climate_file_dpt970k.csv"))

#> Relative Humidity ----
prq_tot = c()

for(i in seq(1:nrow(departments))) {
  df_clim_tot = c()
  
  dpt_name = departments$NAME_1[i]
  dpt_cod = departments$dep_id[i]
  
  for(anni in c('1980_1989', '1990_1999', '2000_2009',
                '2010_2019', '2020_2029')){
    
    print(paste(dpt_name, anni))
    
    name_file = paste0('/COL_v410_', dpt_cod, 
                       '_2_ERA5_Land_relative_humidity_', anni, '.csv')
    
    prq = read.csv(paste0(setwd_climate, 
                          name_file))
    
    colnames(prq) = c("Date", 
                      "cos",
                      "pop_2000",
                      "pop_2005",
                      "pop_2010",
                      "pop_2015",
                      "pop_2020")
    
    df_clim = prq %>% 
      mutate(day = yday(Date),
             year = year(Date),
             clim = case_when(year  <=        2002      ~ pop_2000,
                              year %in% seq(2003, 2007) ~ pop_2005,
                              year %in% seq(2008, 2012) ~ pop_2010,
                              year %in% seq(2013, 2017) ~ pop_2015,
                              year >        2017        ~ pop_2020)) %>%
      select(day, year, clim) %>%
      rename(!!dpt_name := clim)  
    
    #> Daily temperature time series 
    #> from two regions were aligned by year and ISO day and merged into a single dataset.
    if (is.null(df_clim_tot)) {
      df_clim_tot <- df_clim
    } else {
      df_clim_tot <- rbind(df_clim_tot, df_clim)
    }
    
  }
  #> Daily temperature time series 
  #> from two regions were aligned by year and ISO day and merged into a single dataset.
  if (is.null(prq_tot)) {
    prq_tot <- df_clim_tot
  } else {
    prq_tot <- prq_tot %>%
      left_join(
        df_clim_tot,
        by = c("year", "day"))
  }}

write.csv(prq_tot, paste0("COL/Climate_population_altitude/Data/level1_relative_humid_Old_climate_file_dpt970k.csv"))

