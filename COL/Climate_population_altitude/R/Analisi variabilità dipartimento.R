# Analisi variabilità dipartimento (basandosi sulle municipalità però)
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
#> quello che faccio ora è importare tutto e poi guardare mean e dv standard per dpt

#> TEMPERATURE ----
#> Near-surface (2m) air temperature: tas - per ora uso SIMPLE, check!!
# importo tutte le variabili cilmatiche ----
# (JUST ONCE - poi ho il file saved) 
# prq_tot = c()
# for(i in seq(1:nrow(departments))) {
#   
#   dpt_name = departments$NAME_1[i]
#   dpt_cod = departments$dep_id[i]
#   
#   mncp_db = df_clean %>%
#     filter(dep_id == dpt_cod) %>%
#     distinct(mun_id, NAME_1, NAME_2)
#   
#   for (i in as.numeric(mncp_db$mun_id)) {
#     print(paste(dpt_name,",", mncp_db$NAME_2[i]))
#     mun_cod = mncp_db$mun_id[i]
#     name_file = paste0('/Central_America_COL_v410_', dpt_cod, '_', mun_cod, 
#                        '_2_CHIRPSv3_ERA5Land_1981_2024_observation.parquet.gzip')
#     
#     prq = read_parquet(paste0(setwd_climate, 
#                               name_file))
#     
#     prq = prq %>%
#       mutate(year = year(Date),
#              day = yday(Date),
#              NAME_1 = !!make_clean_names(mncp_db$NAME_1[i]),
#              NAME_2 = !!make_clean_names(mncp_db$NAME_2[i])) %>%
#       select(day, year, 
#              tas_static, pr_static, huss_static, hurs_static, 
#              NAME_1, NAME_2)
#     
#     #> Daily temperature time series 
#     #> from two regions were aligned by year and ISO day and merged into a single dataset.
#     if (is.null(prq_tot)) {
#       prq_tot <- prq
#     } else {
#       prq_tot <- rbind(prq_tot, prq)
#     }
#     
#   }
# }
# 
# 
# # guardo le variabilità e salvo quelle 
# var_db = prq_tot %>%
#   group_by(day, year, NAME_1) %>%
#   summarise(mean_T = mean(tas_static),
#             sd_T = sd(tas_static),
#             mean_pr = mean(pr_static),
#             sd_pr = sd(pr_static),
#             mean_hus = mean(huss_static),
#             sd_hus = sd(huss_static),
#             mean_hur = mean(hurs_static),
#             sd_hur = sd(hurs_static)
#   )
# 
# saveRDS(var_db, 'ClimateDPTVariability_by municipalities.RDS')

var_db = readRDS('ClimateDPTVariability_by municipalities.RDS')

# ELEVATION 
elev_pop_wes = read.csv("C:/Users/efesce/Desktop/GitHub/DENVMods2/COL/Climate_population_altitude/Data/COL_elev_pop_Wes.csv")
tmp = read.csv("C:/Users/efesce/Desktop/GitHub/DENVMods2/COL/Climate_population_altitude/Data/COL_legend_Wes.csv")

elev_pop_wes <- elev_pop_wes %>%
  left_join(
    tmp %>%
      select(NAME_1, NAME_2, id) %>%
      rename(unit = id),
    by = c("unit"))
# 
# elev_pop_wes_metrics = elev_pop_wes %>%
#   group_by(NAME_1) %>%
#   summarise(mean_pop = mean(unit_pop),
#             sd_pop = sd(unit_pop),
#             mean_h = mean(elev),
#             sd_h = sd(elev)
#   )
# 
# saveRDS(elev_pop_wes_metrics, 'AltPop_by municipalities.RDS')
# devo anche importare altitudine, popolazione e n casi dengue 

elev_pop_wes_metrics = readRDS('AltPop_by municipalities.RDS')

elev_pop_wes %>%
  ggplot(aes(x = NAME_1,  y = elev)) +
  geom_boxplot(outlier.size = 0.3, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.4) +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Department",
    y = "Altitude (m)",
    title = "Altitude distribution by department in Colombia"
  )

# plot ----
elev_pop_wes %>%
  filter(!is.finite(elev))

elev_pop_wes %>%
  filter(is.finite(elev),
         NAME_1 != "Bogotá D.C.") %>%
  group_by(NAME_1) %>%
  mutate(unit_pop = mean(unit_pop, na.rm = TRUE)) %>%  # calcola media dipartimento
  ungroup() %>%
  mutate(NAME_1 = factor(NAME_1, levels = unique(NAME_1))) %>%
  ggplot(aes(x = reorder(NAME_1, unit_pop),
             y = elev,
             fill = unit_pop)) +
  geom_boxplot(outlier.size = 0.3, width = 0.6, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.4, color = "black") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Department",
    y = "Altitude (m)",
    title = "Altitude distribution by department in Colombia",
    fill = "Mean population"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 7)
  )

elev_pop_wes %>%
  filter(NAME_1 %in% c("Atlántico",
                       "Córdoba",
                       "Bolívar",
                       "Arauca",
                       "Magdalena",
                       "Cesar",
                       "Meta",
                       "Sucre",
                       "Caquetá",
                       "Guaviare", 
                       "Casanare",
                       "San Andrés y Providencia",
                       "Vichada")) %>%
  group_by(NAME_1) %>%
  mutate(unit_pop = mean(unit_pop, na.rm = TRUE)) %>%  # calcola media dipartimento
  ungroup() %>%
  mutate(NAME_1 = factor(NAME_1, levels = unique(NAME_1))) %>%
  ggplot(aes(x = reorder(NAME_1, unit_pop),
             y = elev,
             fill = unit_pop)) +
  geom_boxplot(outlier.size = 0.3, width = 0.6, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.4, color = "black") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Department",
    y = "Altitude (m)",
    title = "Altitude distribution by department in Colombia",
    fill = "Mean population"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 7)
  )


elev_pop_wes %>%
  filter(NAME_1 %in% c("Atlántico",
                       "Córdoba",
                       "Bolívar",
                       "Arauca",
                       "Magdalena",
                       "Cesar",
                       "Meta",
                       "Sucre",
                       "Caquetá",
                       "Guaviare", 
                       "Casanare",
                       "San Andrés y Providencia",
                       "Vichada")) %>%
  group_by(NAME_1) %>%
  mutate(elev = mean(elev, na.rm = TRUE)) %>%  # calcola media dipartimento
  ungroup() %>%
  mutate(NAME_1 = factor(NAME_1, levels = unique(NAME_1))) %>%
  ggplot(aes(x = reorder(NAME_1, elev),
             y = unit_pop,
             fill = elev)) +
  geom_boxplot(outlier.size = 0.3, width = 0.6, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.4, color = "black") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Department",
    y = "Population",
    title = "Altitude distribution by department in Colombia",
    fill = "Mean altitude"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 7)
  ) +
  ylim(0, 150000) +
  geom_hline(yintercept = 90000, linetype = "dashed")

elev_pop_wes %>%
  filter(NAME_1 != "Bogotá D.C.") %>%
  group_by(NAME_1) %>%
  mutate(elev = mean(elev, na.rm = TRUE)) %>%  # calcola media dipartimento
  ungroup() %>%
  mutate(NAME_1 = factor(NAME_1, levels = unique(NAME_1))) %>%
  ggplot(aes(x = reorder(NAME_1, elev),
             y = unit_pop,
             fill = elev)) +
  geom_boxplot(outlier.size = 0.3, width = 0.6, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.4, color = "black") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Department",
    y = "Population",
    title = "Altitude distribution by department in Colombia",
    fill = "Mean altitude"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 7)
  )



# Dengue ----
weekly_counts_MNCP_2007_2023EF <- readRDS("C:/Users/efesce/Desktop/GitHub/DENVMods2/COL/Dengue/Data/weekly_counts_MNCP_2007_2023EF.RDS")
weekly_counts_MNCP_2007_2023EF 

denv_var = weekly_counts_MNCP_2007_2023EF %>%
  group_by(week_epi, year_epi, department) %>%
  summarise(sum_cases = sum(cases),
            mean_cases = mean(cases),
            sd_cases = sd(cases)
  ) %>%
  filter(department %in% c("atlantico",
                       "cordoba",
                       "bolivar",
                       "arauca",
                       "magdalena",
                       "cesar",
                       "meta",
                       "sucre",
                       "caqueta",
                       "guaviare", 
                       "casanare",
                       # "san_andres_y_providencia",
                       "vichada")) %>%
  mutate(
    date = ISOdate(year_epi, week_epi, 1)
  ) 
denv_var %>%
  ggplot() +
  # geom_ribbon(aes(x = date,
  #                 ymin = mean_T   - sd_T  ,
  #                 ymax = mean_T + sd_T,
  #                 fill = NAME_1),
  #             alpha = 0.3
  # ) +
  geom_line(aes(x = date, y = sum_cases,colour = department),
            linewidth = 0.7) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Daily mean temperature (°C)",
    title = "Daily mean temperature",
    subtitle = "Shaded area represents ± SD"
  ) +
  facet_wrap(vars(department), scales = "free_y")

# climate ----
tmp = var_db %>% 
  filter(NAME_1 %in% c("atlántico",
                       "córdoba",
                       "bolívar",
                       "arauca",
                       "magdalena",
                       "cesar",
                       "meta",
                       "sucre",
                       "caquetá",
                       "guaviare", 
                       "casanare",
                       # "san_andres_y_providencia",
                       "vichada")) %>%
  mutate(
    date = as.Date(day - 1, origin = paste0(year, "-01-01"))
  ) 
tmp %>%
  ggplot() +
  geom_ribbon(aes(x = date,
            ymin = mean_T   - sd_T  ,
            ymax = mean_T + sd_T,
        fill = NAME_1),
    alpha = 0.3
  ) +
  geom_line(aes(x = date, y = mean_T,colour = NAME_1),
            linewidth = 0.7) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Daily mean temperature (°C)",
    title = "Daily mean temperature",
    subtitle = "Shaded area represents ± SD"
  ) +
  facet_wrap(vars(NAME_1))


  


# 28Jan26 ----
#> provo a fare heatmap altitudine popolosità
library(forcats)

tmp = elev_pop_wes %>%
  filter(NAME_1 %in% c("Atlántico",
                       "Córdoba",
                       # "Bolívar",
                       # "Arauca",
                       "Magdalena",
                       # "Cesar",
                       "Meta",
                       "Sucre",
                       # "Caquetá",
                       "Guaviare")) %>%
                       #"Casanare",
                       #" San Andrés y Providencia",
                       #"Vichada")) %>%
  arrange(elev) %>%
  mutate(NAME_2 = fct_inorder(NAME_2))

ggplot(tmp, aes(x = NAME_2, y = NAME_1, fill = unit_pop)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "unit_pop") +
  labs(
    x = "NAME_2",
    y = "NAME_1",
    title = "Heatmap di unit_pop (NAME_2 ordinate per elev crescente)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

df_rel <- tmp %>%
  group_by(NAME_1) %>%
  mutate(
    pop_tot_NAME_1 = sum(unit_pop, na.rm = TRUE),
    pop_rel = unit_pop / pop_tot_NAME_1
  ) %>%
  ungroup()

# check 
df_rel %>%
  group_by(NAME_1) %>%
  summarise(check = sum(pop_rel))

tmp %>%
  group_by(NAME_1) %>%
  # filter(NAME_1 %in% c("Atlántico")) %>%
  ggplot(aes(x = unit_pop, y = elev)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    x = "Popolazione unità",
    y = "Elevazione"
  ) +
  theme_classic() + 
  facet_wrap(facets = vars(NAME_1))

ggplot(df_rel, aes(
  x = unit_pop, y = elev, color = pop_rel
)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent,
    name = "Quota pop.\n(NAME_1)"
  ) +
  labs(
    x = "Popolazione unità",
    y = "Elevazione",
    title = "Popolazione assoluta colorata per quota relativa nel dipartimento"
  ) +
  theme_classic() + 
  facet_wrap(facets = vars(NAME_1))
 

tmp %>%
  group_by(NAME_1) %>%
  # filter(NAME_1 %in% c("Atlántico")) %>%
  ggplot(aes(x = unit_pop, y = elev)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    x = "Popolazione unità",
    y = "Elevazione"
  ) +
  theme_classic() + 
  facet_wrap(facets = vars(NAME_1), scales = "free")

ggplot(df_rel, aes(
  x = unit_pop, y = elev, color = pop_rel
)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent,
    name = "Quota pop.\n(NAME_1)"
  ) +
  labs(
    x = "Popolazione unità",
    y = "Elevazione") +
  theme_classic() + 
  facet_wrap(facets = vars(NAME_1), scales = "free")


ggplot(tmp, aes(x = NAME_2, y = NAME_1, fill = unit_pop)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "unit_pop") +
  labs(
    x = "NAME_2",
    y = "NAME_1",
    title = "Heatmap di unit_pop (NAME_2 ordinate per elev crescente)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

tmp %>%
  group_by(NAME_1) %>%
  # filter(NAME_1 %in% c("Atlántico")) %>%
  ggplot(aes(x = elev, y = unit_pop)) +
  geom_point(size = 3, alpha = 0.8) +
  ctheme_classic() + 
  facet_wrap(facets = vars(NAME_1))

ggplot(df_rel, aes(
  x = elev, y = unit_pop, color = pop_rel
)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent,
    name = "Quota pop.\n(NAME_1)"
  ) +
  labs(
    x = "Elevazione",
    y = "Popolazione unità",
    title = "Popolazione assoluta colorata per quota relativa nel dipartimento"
  ) +
  theme_classic() + 
  facet_wrap(facets = vars(NAME_1), scales = "free_x")

# proporzione di popolazione che vive sotto una det soglia ----
library(forcats)

breaks <- seq(0, 4000, by = 100)
labels <- paste(head(breaks, -1), tail(breaks, -1), sep = "–")

df_bins <- tmp %>%
  mutate(
    elev_bin = cut(
      elev,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

df_bins <- tmp %>%
  mutate(
    elev_bin = cut(
      elev,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

df_pop_bin <- df_bins %>%
  group_by(NAME_1, elev_bin) %>%
  summarise(
    pop_bin = sum(unit_pop, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    pop_tot = sum(pop_bin, na.rm = TRUE),
    pop_prop = pop_bin / pop_tot
  ) %>%
  ungroup()

ggplot(df_pop_bin,
       aes(x = elev_bin, y = NAME_1, fill = pop_prop)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#fff7e6",      # crema chiaro
    high = "darkblue",
    trans = "sqrt",
    labels = scales::percent,
    name = "Quota popolazione"
  ) +
  labs(
    x = "Fascia altitudinale (m)",
    y = "Dipartimento",
    title = "Distribuzione altitudinale della popolazione per dipartimento"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
