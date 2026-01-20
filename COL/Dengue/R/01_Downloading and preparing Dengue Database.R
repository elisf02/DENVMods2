################################################################################
#> start from scartch downloading the files  
#################################################################################
rm(list=ls())

# (1. JUST ONCE) Import data from public repo ----
library(sivirep)

#Total number of cases per year 2007 to 2023
dat <- import_data_event(nombre_event = "DENGUE",
                         years =c(2007:2023),
                         cache = TRUE)

# EF> provo a vedere se c e un db diverso per dengue grave
datG <- import_data_event(nombre_event = "DENGUE GRAVE",
                          years =c(2007:2023),
                          cache = TRUE)

dat_limpia <- limpiar_data_sivigila(data_event = dat) #standardize age in years
dat_limpiaG <- limpiar_data_sivigila(data_event = datG) #standardize age in years

# (EF) salvo dei RDS miei nella mia cartella
saveRDS(dat_limpia, 
        "COL/Dengue/Data/dat_denguesivirep2007_2023.RDS")
saveRDS(dat_limpiaG, 
        "COL/Dengue/Data/dat_dengueGRAVEsivirep2007_2023.RDS")


# (2. Usually not necessary) Preparo i dati according with Zulma ----
rm(list=ls())

library(dplyr)
library(tidyverse)
library(gridExtra)
library(openxlsx)
library(readxl)
library(stringr)
library(lubridate)

#Data
#> (EF) same as Jennifer, 
#> just also Dengue Grave that I dont know why was not included (17Oct)
dat <- readRDS("COL/Dengue/Data/dat_denguesivirep2007_2023.RDS")
datG <- readRDS("COL/Dengue/Data/dat_dengueGRAVEsivirep2007_2023.RDS")

colnames(dat)
colnames(datG) # (EF)

# VARIABLE Age ----
summary(dat$edad)
summary(dat$uni_med)
edad <- dat |> 
  select(edad, uni_med) |> 
  group_by(edad, uni_med) |> 
  summarise(cantidad = n(), .groups = "drop")

table(dat$uni_med) 
# 0 = Not applicable, 1 = Years, 2 = Months, 3 = Days, 4 = Hours, 5 = Minutes (already standardized)

# 1.1. Review of age when it is NA to see if it can be calculated with date of birth
años_na <- dat |> 
  select(edad, uni_med, fecha_nto, fec_con, fec_not) |> 
  filter(uni_med == "0") |> 
  rename(
    ini = fecha_nto,
    fin = fec_con
  ) |> 
  mutate(
    edad_fec_con = as.numeric(interval(start = ini, end = fin) / years())
  )

# 1.2. Apply what was previously done (years_na) in the database
dat2 <- dat |> 
  mutate(
    ini = fecha_nto,
    fin = fec_con,
    edad_fec_con = ifelse(
      uni_med == "0", 
      as.numeric(interval(start = ini, end = fin) / years()), 
      NA
    ),
    edad = case_when(
      !is.na(edad_fec_con) ~ edad_fec_con,
      TRUE ~ edad
    )
  ) |> 
  select(-ini, -fin, -edad_fec_con) |> 
  filter(!is.na(edad))
# (EF)
dat2G <- datG |> 
  mutate(
    ini = fecha_nto,
    fin = fec_con,
    edad_fec_con = ifelse(
      uni_med == "0", 
      as.numeric(interval(start = ini, end = fin) / years()), 
      NA
    ),
    edad = case_when(
      !is.na(edad_fec_con) ~ edad_fec_con,
      TRUE ~ edad
    )
  ) |> 
  select(-ini, -fin, -edad_fec_con) |> 
  filter(!is.na(edad))

# 1.3. Remove the unit of measurement=0 from the "recovered" ages calculated with the date of birth
anios_na2 <- dat2 |> 
  select(edad, uni_med, fecha_nto, fec_con, fec_not) |> 
  filter(uni_med == "0")
# (EF)
anios_na2G <- dat2G |> 
  select(edad, uni_med, fecha_nto, fec_con, fec_not) |> 
  filter(uni_med == "0")

# 1.4. Apply what was previously done (years_na) in the database
bd2 <- dat2 |> 
  mutate(
    uni_med = ifelse(uni_med == "0", "1", uni_med)
  )
# (EF)
bd2G <- dat2G |> 
  mutate(
    uni_med = ifelse(uni_med == "0", "1", uni_med)
  )

table(bd2$uni_med) 
table(bd2G$uni_med) 

# VARIABLE. Place of occurrence ----
# 2. VARIABLE. Place of occurrence

# 2.1. Country (País)
table(bd2$cod_pais_o)

bd2 <- bd2 |> 
  filter(cod_pais_o == 170) 
bd2G <- bd2G |> 
  filter(cod_pais_o == 170) 
# From the initial 1'322'912, 1'320'150 remain
bd2

# 2.2. Department (Departamento)
table(bd2$cod_dpto_o)

bd2 <- bd2 |> 
  filter(
    cod_dpto_o != "01", 
    cod_dpto_o != "00"
  )
bd2G <- bd2G |> 
  filter(
    cod_dpto_o != "01", 
    cod_dpto_o != "00"
  )

table(bd2$cod_dpto_o)

# Municipality - Administration Level 3 (Municipio) ----
#2.3. Municipality - Administration Level 3 (Municipio)
dim(bd2)
bd2 <- bd2 |>
  filter(!(str_detect(municipio_ocurrencia,
                      "SIN MUNICIPIO|MUNICIPIO DESCONOCIDO")))
dim(bd2)
# (EF)
dim(bd2G)
bd2G <- bd2G |>
  filter(!(str_detect(municipio_ocurrencia,
                      "SIN MUNICIPIO|MUNICIPIO DESCONOCIDO")))
dim(bd2G)

bd2$municipio_ocurrencia <- epitrix::clean_labels(bd2$municipio_ocurrencia)
bd2$departamento_ocurrencia <- epitrix::clean_labels(bd2$departamento_ocurrencia)

#(EF)
bd2G$municipio_ocurrencia <- epitrix::clean_labels(bd2G$municipio_ocurrencia)
bd2G$departamento_ocurrencia <- epitrix::clean_labels(bd2G$departamento_ocurrencia)

##2.3.1 Municipality < 2000 msnm
msnm <- read_excel("COL/Dengue/Data/elevation_Zulma.xlsx")
msnm$municipality <- epitrix::clean_labels(msnm$municipality)
msnm$department <- epitrix::clean_labels(msnm$department)
msnm <- msnm |> select(-department, -municipality)
# these codes are different from Wes'!

##2.3.2. Mpio match con altura - unión
bd2$cod_mun_o <- as.numeric(bd2$cod_mun_o)
bd20 <- left_join(bd2, msnm, by = c("cod_mun_o"="cod_mun"))

#(EF)
bd2G$cod_mun_o <- as.numeric(bd2G$cod_mun_o)
bd20G <- left_join(bd2G, msnm, by = c("cod_mun_o"="cod_mun"))

### Dejar los nombres de los mpios. indicados según DANE
bd2 <- bd20 |>
  mutate(municipio_ocurrencia = case_when(
    cod_mun_o == 47170 & municipio_ocurrencia == "chivolo" ~ "chibolo",
    cod_mun_o == 27361 & municipio_ocurrencia == "itsmina" ~ "istmina",
    cod_mun_o == 47001 & municipio_ocurrencia == "santa_martha" ~ "santa_marta",
    cod_mun_o == 05148 & municipio_ocurrencia == "carmen_de_viboral" ~ "el_carmen_de_viboral",
    cod_mun_o == 19418 & municipio_ocurrencia == "lopez_micay" ~ "lopez_de_micay",
    cod_mun_o == 27250 & municipio_ocurrencia == "litoral_del_bajo_san_juan" ~ "el_litoral_del_san_juan",
    cod_mun_o == 27600 & municipio_ocurrencia == "rioquito" ~ "rio_quito",
    cod_mun_o == 94343 & municipio_ocurrencia == "barranco_minas_cd" ~ "barranco_minas",
    cod_mun_o == 47745 & municipio_ocurrencia == "sitio_nuevo" ~ "sitionuevo",
    cod_mun_o == 27099 & municipio_ocurrencia == "bojaya_bellavista" ~ "bojaya",
    cod_mun_o == 25599 & municipio_ocurrencia == "rafael_reyes_apulo" ~ "apulo",
    cod_mun_o == 52699 & municipio_ocurrencia == "santa_cruz_guachaves" ~ "santa_cruz",
    cod_mun_o == 52258 & municipio_ocurrencia == "el_tablon" ~ "el_tablon_de_gomez",
    cod_mun_o == 19845 & municipio_ocurrencia == "villarica" ~ "villa_rica",
    cod_mun_o == 73873 & municipio_ocurrencia == "villarica" ~ "villarrica",
    TRUE ~ municipio_ocurrencia
  ))
bd2G <- bd20G |>
  mutate(municipio_ocurrencia = case_when(
    cod_mun_o == 47170 & municipio_ocurrencia == "chivolo" ~ "chibolo",
    cod_mun_o == 27361 & municipio_ocurrencia == "itsmina" ~ "istmina",
    cod_mun_o == 47001 & municipio_ocurrencia == "santa_martha" ~ "santa_marta",
    cod_mun_o == 05148 & municipio_ocurrencia == "carmen_de_viboral" ~ "el_carmen_de_viboral",
    cod_mun_o == 19418 & municipio_ocurrencia == "lopez_micay" ~ "lopez_de_micay",
    cod_mun_o == 27250 & municipio_ocurrencia == "litoral_del_bajo_san_juan" ~ "el_litoral_del_san_juan",
    cod_mun_o == 27600 & municipio_ocurrencia == "rioquito" ~ "rio_quito",
    cod_mun_o == 94343 & municipio_ocurrencia == "barranco_minas_cd" ~ "barranco_minas",
    cod_mun_o == 47745 & municipio_ocurrencia == "sitio_nuevo" ~ "sitionuevo",
    cod_mun_o == 27099 & municipio_ocurrencia == "bojaya_bellavista" ~ "bojaya",
    cod_mun_o == 25599 & municipio_ocurrencia == "rafael_reyes_apulo" ~ "apulo",
    cod_mun_o == 52699 & municipio_ocurrencia == "santa_cruz_guachaves" ~ "santa_cruz",
    cod_mun_o == 52258 & municipio_ocurrencia == "el_tablon" ~ "el_tablon_de_gomez",
    cod_mun_o == 19845 & municipio_ocurrencia == "villarica" ~ "villa_rica",
    cod_mun_o == 73873 & municipio_ocurrencia == "villarica" ~ "villarrica",
    TRUE ~ municipio_ocurrencia
  ))

##2.3.3. Mpio match con altura - filtrado
dat_final <- bd2 |>
  arrange(elevation) |> 
  mutate(elevation = as.numeric(elevation)) |>
  filter(!(elevation >= 2300))
dat_finalG <- bd2G |>
  arrange(elevation) |> 
  mutate(elevation = as.numeric(elevation)) |>
  filter(!(elevation >= 2300))

## Revisar la cantidad de municipios finales
bd3 <- dat_final |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, elevation) |>
  summarize(cantidad = n(), .groups = "drop")
bd4 <- bd2 |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, elevation) |>
  summarize(cantidad = n(), .groups = "drop")

## Revisar los municipios eliminados por altura
bd55 <- bd2 |>
  arrange(elevation) |> 
  mutate(elevation = as.numeric(elevation)) |>
  filter(elevation >= 2300) |>
  select(departamento_ocurrencia, municipio_ocurrencia, elevation) |>
  group_by(elevation, municipio_ocurrencia) |>
  summarize(cantidad = n(), .groups = "drop")
bd55

# VARIABLES: death record by event code ----
# 3. VARIABLES: death record by event code

# 3.1. Elimination of cod_eve=580 when the final condition is alive (con_fin=1),
# since this record only reports mortality due to this event.
condicion_final <- dat_final |>
  select(cod_eve, con_fin) |>
  group_by(cod_eve, con_fin) |>
  summarize(cantidad = n())

# (EF) this is done on the data we save and use
dat_final <- dat_final |>
  filter((cod_eve %in% c("210", "220") & con_fin %in% c("1", "2")) |
           (cod_eve == "580" & con_fin == "2"))  # only 2 records are excluded
bd2 <- bd2 |>
  filter((cod_eve %in% c("210", "220") & con_fin %in% c("1", "2")) |
           (cod_eve == "580" & con_fin == "2"))  # (EF)check how many reecords are excluded

# 3.2. con_fin to confirm cod_evo 580 only deceased
bd <- dat_final |>
  select(cod_eve, con_fin)

bd |>
  (\(x) table(x$con_fin))() |>
  print()

bd |>
  (\(x) table(x$cod_eve))() |>
  print()

# 3.3. cod_eve deads table
cod_let <- bd |>
  filter((cod_eve == "210" & con_fin == "2") |
           (cod_eve == "220" & con_fin == "2") |
           (cod_eve == "580" & con_fin == "2") |
           (cod_eve == "210" & con_fin == "1") |
           (cod_eve == "220" & con_fin == "1") |
           (cod_eve == "580" & con_fin == "1")) |>
  group_by(cod_eve, con_fin) |>
  summarize(cantidad_let = n())


# OTHER VARIABLES (exploratory) ----

# Event code (código evento) 
table(dat_final$cod_eve)
table(dat_final$nombre_evento)

# Final condition (Condición final)
sum(is.na(dat_final$con_fin))
table(dat_final$con_fin)

# Notification date (Fecha notificación)
sum(is.na(dat_final$fec_not))
fec_not <- dat_final |> select(fec_not)
summary(fec_not)  # There are dates from 2006 to 2024

# Epidemiological week (Semana epidemiológica)
sum(is.na(dat_final$semana))
frecuencias <- table(dat_final$semana)
plot <- barplot(frecuencias)
valores <- c(15000, 20000, 25000)
for (valor in valores) {
  abline(h = valor, col = "blue")
}

# Cases per year (Casos por año)
sum(is.na(dat_final$ano))
frecuencias <- table(dat_final$ano)
plot <- barplot(frecuencias)

# Nationality (Nacionalidad)
table(dat_final$nacionalidad)
sum(is.na(dat_final$nacionalidad))
sum(is.na(dat_final$nombre_nacionalidad))
nacionalidad <- dat_final |>
  (\(x) table(x$nombre_nacionalidad))() |>
  as.data.frame()
sum(nacionalidad$Freq) + sum(is.na(dat_final$nacionalidad))

# Sex (Sexo)
sum(is.na(dat_final$sexo))
table(dat_final$sexo)

# Area of residence (Área)
sum(is.na(dat_final$area))
table(dat_final$area)

# Social security system (Régimen)
sum(is.na(dat_final$tip_ss))
table(dat_final$tip_ss)

# Notification source (Fuente)
sum(is.na(dat_final$fuente)) + sum(table(dat_final$fuente))
table(dat_final$fuente)

# Consultation date (Fecha consulta)
sum(is.na(dat_final$fec_con))
consulta <- dat_final |> select(fec_con)  # There are dates from 2006 to 2024
summary(consulta)

# Date symptoms started (Fecha inicio de síntomas)
sum(is.na(dat_final$ini_sin))
sintomas <- dat_final |> select(ini_sin)  # There are dates from 2006 to 2024
summary(sintomas)

# Initial case classification (Clasificación inicial del caso)
sum(is.na(dat_final$tip_cas))
table(dat_final$tip_cas)

# Hospitalized status (yes=1/no=2) (Paciente hospitalizado)
sum(is.na(dat_final$pac_hos))
table(dat_final$pac_hos)

# Hospitalization date
sum(!(is.na(dat_final$fec_hos)))
sum(is.na(dat_final$fec_hos))

# Adjusted final condition (Condición final ajustada)
table(dat_final$ajuste)

# Birthdate (Fecha de nacimiento)
sum(is.na(dat_final$fecha_nto))
nacimiento <- dat_final |> select(fecha_nto)
summary(nacimiento)  # There are dates from 1900 to 2023

# Final status of the case (Estado final del caso)
sum(is.na(dat_final$estado_final_de_caso))
table(dat_final$estado_final_de_caso)

# Save final db ----
# (EF) tutta la parte di dengue grave e inutile!! risalvo solo il db finale di Jenn
# Save verified data
saveRDS(dat_final, "COL/Dengue/Data/cleandat_2007_2023.RDS") 
saveRDS(bd2, "COL/Dengue/Data/cleandat_2007_2023_no_altitude_filter.RDS") 

##################################################################################
# (3) Import final db ----
rm(list=ls())

library(dplyr)
library(lubridate)

# from online source, filtered by municipality of ccurentia nad few other things a s above
dat_final <- readRDS("COL/Dengue/Data/cleandat_2007_2023.RDS") 
dat_final_all_altitude <- readRDS("COL/Dengue/Data/cleandat_2007_2023_no_altitude_filter.RDS") 

# Create age ranges
dengue_col <- dat_final |>
  mutate(
    grupo_edad = case_when(
      is.na(edad) ~ NA_character_,
      edad < 1 ~ "0",
      edad >= 1 & edad < 21 ~ as.character(floor(edad)),  # From 1 to 20 individual
      edad >= 21 & edad <= 25 ~ "21-25",
      edad > 25 & edad <= 30 ~ "26-30",
      edad > 30 & edad <= 35 ~ "31-35",
      edad > 35 & edad <= 40 ~ "36-40",
      edad > 40 & edad <= 45 ~ "41-45",
      edad > 45 & edad <= 50 ~ "46-50",
      edad > 50 & edad <= 55 ~ "51-55",
      edad > 55 & edad <= 60 ~ "56-60",
      edad > 60 & edad <= 65 ~ "61-65",
      edad > 65 & edad <= 70 ~ "66-70",
      edad > 70 & edad <= 75 ~ "71-75",
      edad > 75 & edad <= 80 ~ "76-80",
      edad > 80 ~ "81+",
    ),
    age_lower = case_when(
      is.na(edad) ~ NA_character_,
      edad < 1 ~ "0",
      edad >= 1 & edad < 15 ~ as.character(floor(edad)),  # From 1 to 20 individual
      edad >= 15 & edad < 20 ~ "15",
      edad >= 20 & edad < 30 ~ "20",
      edad >= 30 & edad < 40 ~ "30",
      edad >= 40 & edad < 50 ~ "40",
      edad >= 50 & edad < 60 ~ "50",
      edad >= 60 & edad < 70 ~ "60",
      edad >= 70 & edad < 80 ~ "70",
      edad >= 80 & edad < 90 ~ "80",
      edad >= 90 ~ "90"
    ),
    age_lower2 = case_when(
      is.na(edad) ~ NA_character_,
      edad < 5 ~ "0",
      edad >= 5 & edad < 10 ~ "5",  # From 1 to 20 individual
      edad >= 10 & edad < 15 ~ "10",
      edad >= 15 & edad < 20 ~ "15",
      edad >= 20 & edad < 30 ~ "20",
      edad >= 30 & edad < 40 ~ "30",
      edad >= 40 & edad < 50 ~ "40",
      edad >= 50 & edad < 60 ~ "50",
      edad >= 60 & edad < 70 ~ "60",
      edad >= 70 & edad < 80 ~ "70",
      edad >= 80 & edad < 90 ~ "80",
      edad >= 90 ~ "90"
    ))

dengue_col_all_altitude <- dat_final_all_altitude |>
  mutate(
    grupo_edad = case_when(
      is.na(edad) ~ NA_character_,
      edad < 1 ~ "0",
      edad >= 1 & edad < 21 ~ as.character(floor(edad)),  # From 1 to 20 individual
      edad >= 21 & edad <= 25 ~ "21-25",
      edad > 25 & edad <= 30 ~ "26-30",
      edad > 30 & edad <= 35 ~ "31-35",
      edad > 35 & edad <= 40 ~ "36-40",
      edad > 40 & edad <= 45 ~ "41-45",
      edad > 45 & edad <= 50 ~ "46-50",
      edad > 50 & edad <= 55 ~ "51-55",
      edad > 55 & edad <= 60 ~ "56-60",
      edad > 60 & edad <= 65 ~ "61-65",
      edad > 65 & edad <= 70 ~ "66-70",
      edad > 70 & edad <= 75 ~ "71-75",
      edad > 75 & edad <= 80 ~ "76-80",
      edad > 80 ~ "81+",
    ),
    age_lower = case_when(
      is.na(edad) ~ NA_character_,
      edad < 1 ~ "0",
      edad >= 1 & edad < 15 ~ as.character(floor(edad)),  # From 1 to 20 individual
      edad >= 15 & edad < 20 ~ "15",
      edad >= 20 & edad < 30 ~ "20",
      edad >= 30 & edad < 40 ~ "30",
      edad >= 40 & edad < 50 ~ "40",
      edad >= 50 & edad < 60 ~ "50",
      edad >= 60 & edad < 70 ~ "60",
      edad >= 70 & edad < 80 ~ "70",
      edad >= 80 & edad < 90 ~ "80",
      edad >= 90 ~ "90"
    ),
    age_lower2 = case_when(
      is.na(edad) ~ NA_character_,
      edad < 5 ~ "0",
      edad >= 5 & edad < 10 ~ "5",  # From 1 to 20 individual
      edad >= 10 & edad < 15 ~ "10",
      edad >= 15 & edad < 20 ~ "15",
      edad >= 20 & edad < 30 ~ "20",
      edad >= 30 & edad < 40 ~ "30",
      edad >= 40 & edad < 50 ~ "40",
      edad >= 50 & edad < 60 ~ "50",
      edad >= 60 & edad < 70 ~ "60",
      edad >= 70 & edad < 80 ~ "70",
      edad >= 80 & edad < 90 ~ "80",
      edad >= 90 ~ "90"
    ))

which(is.na(dengue_col$edad))
which(is.na(dengue_col$grupo_edad))
which(is.na(dengue_col$age_lower))
which(is.na(dengue_col_all_altitude$edad))
which(is.na(dengue_col_all_altitude$grupo_edad))
which(is.na(dengue_col_all_altitude$age_lower))

dengue_col2 <- dengue_col |>
  mutate(
    ini_sin = as.Date(ini_sin), 
    epi_semana = epiweek(ini_sin),
    epi_ano = epiyear(ini_sin), 
    onset_date = as.Date(ini_sin))
dengue_col3 = dengue_col2 |>
  filter(
    !is.na(grupo_edad),
    !is.na(age_lower),
    !is.na(ini_sin),
    !is.na(departamento_ocurrencia),
    !is.na(municipio_ocurrencia)
  ) |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, onset_date, age_lower) |>
  summarise(cases = n(), 
            .groups = "drop")
dengue_col3bis = dengue_col2 |>
  filter(
    !is.na(grupo_edad),
    !is.na(age_lower2),
    !is.na(ini_sin),
    !is.na(departamento_ocurrencia),
    !is.na(municipio_ocurrencia)
  ) |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, onset_date, age_lower2) |>
  summarise(cases = n(), 
            .groups = "drop")
sum(dengue_col3$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )
sum(dengue_col3bis$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )


# faccio lo stesso sempre anche per il db non filtrato per altitudine ----
dengue_col2_all_altitude <- dengue_col_all_altitude |>
  mutate(
    ini_sin = as.Date(ini_sin), 
    epi_semana = epiweek(ini_sin),
    epi_ano = epiyear(ini_sin), 
    onset_date = as.Date(ini_sin))
dengue_col3_all_altitude = dengue_col2_all_altitude |>
  filter(
    !is.na(grupo_edad),
    !is.na(age_lower),
    !is.na(ini_sin),
    !is.na(departamento_ocurrencia),
    !is.na(municipio_ocurrencia)
  ) |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, onset_date, age_lower) |>
  summarise(cases = n(), 
            .groups = "drop")
dengue_col3bis_all_altitude = dengue_col2_all_altitude |>
  filter(
    !is.na(grupo_edad),
    !is.na(age_lower2),
    !is.na(ini_sin),
    !is.na(departamento_ocurrencia),
    !is.na(municipio_ocurrencia)
  ) |>
  group_by(departamento_ocurrencia, municipio_ocurrencia, onset_date, age_lower2) |>
  summarise(cases = n(), 
            .groups = "drop")
sum(dengue_col3_all_altitude$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )
sum(dengue_col3bis_all_altitude$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )


# weekly counts ----
library(ISOweek)
weekly_counts <- dengue_col3 |>
  rename(
    department = departamento_ocurrencia,
    municipality = municipio_ocurrencia
    # age_group = grupo_edad,
  ) |>
  mutate(isoweek = isoweek(onset_date),
         isoyear = isoyear(onset_date),
         isodate = ISOweek2date(paste0(isoyear, "-W", sprintf("%02d", isoweek), "-1"))) |>
  group_by(department, municipality, isodate, isoyear, isoweek, age_lower) |>
  summarise(cases = sum(cases), 
            .groups = "drop") |>
  arrange(department, municipality,isodate, isoyear, isoweek, age_lower) %>%
  filter(isoyear > 2008) 

# nel bis cambiano solo le age classes, che osno più larghe
weekly_countsbis <- dengue_col3bis |>
  rename(
    department = departamento_ocurrencia,
    municipality = municipio_ocurrencia
    # age_group = grupo_edad,
  ) |>
  mutate(isoweek = isoweek(onset_date),
         isoyear = isoyear(onset_date),
         isodate = ISOweek2date(paste0(isoyear, "-W", sprintf("%02d", isoweek), "-1"))) |>
  group_by(department, municipality, isodate, isoyear, isoweek, age_lower2) |>
  summarise(cases = sum(cases), 
            .groups = "drop") |>
  arrange(department, municipality,isodate, isoyear, isoweek, age_lower2) %>%
  filter(isoyear > 2008) 
sum(weekly_counts$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )
sum(weekly_countsbis$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )

# all altitudes 
weekly_counts_all_altitude <- dengue_col3_all_altitude |>
  rename(
    department = departamento_ocurrencia,
    municipality = municipio_ocurrencia
    # age_group = grupo_edad,
  ) |>
  mutate(isoweek = isoweek(onset_date),
         isoyear = isoyear(onset_date),
         isodate = ISOweek2date(paste0(isoyear, "-W", sprintf("%02d", isoweek), "-1"))) |>
  group_by(department, municipality, isodate, isoyear, isoweek, age_lower) |>
  summarise(cases = sum(cases), 
            .groups = "drop") |>
  arrange(department, municipality,isodate, isoyear, isoweek, age_lower) %>%
  filter(isoyear > 2008) 

# nel bis cambiano solo le age classes, che osno più larghe
weekly_countsbis_all_altitude <- dengue_col3bis_all_altitude |>
  rename(
    department = departamento_ocurrencia,
    municipality = municipio_ocurrencia
    # age_group = grupo_edad,
  ) |>
  mutate(isoweek = isoweek(onset_date),
         isoyear = isoyear(onset_date),
         isodate = ISOweek2date(paste0(isoyear, "-W", sprintf("%02d", isoweek), "-1"))) |>
  group_by(department, municipality, isodate, isoyear, isoweek, age_lower2) |>
  summarise(cases = sum(cases), 
            .groups = "drop") |>
  arrange(department, municipality,isodate, isoyear, isoweek, age_lower2) %>%
  filter(isoyear > 2008) 

sum(weekly_counts_all_altitude$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )
sum(weekly_countsbis_all_altitude$cases) # 1'309.552 (151 reports excluded for not having a date of onset of symptoms )

str(weekly_counts)
levels(as.factor(weekly_counts$age_lower))
levels(as.factor(weekly_countsbis$age_lower2))

#Save data with new structure
saveRDS(weekly_counts, "COL/Dengue/Data/COL_Dengue_Zulma2008_EF_moreAges_low2300.RDS")
saveRDS(weekly_countsbis, "COL/Dengue/Data/COL_Dengue_Zulma2008_EF_lessAges_low2300.RDS")

saveRDS(weekly_counts_all_altitude,
        "COL/Dengue/Data/COL_Dengue_Zulma2008_EF_moreAges_all_altitude.RDS")
saveRDS(weekly_countsbis_all_altitude, 
        "COL/Dengue/Data/COL_Dengue_Zulma2008_EF_lessAges_all_altitude.RDS")
