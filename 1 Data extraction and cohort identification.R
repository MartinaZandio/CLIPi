library(lubridate)
library(readr) 

# demograficos <- read_delim("demograficos.csv", delim = "|")
# ldh <- read_delim("LDH_obx.csv", delim = "|")
# anatomia_patologica <- read_delim("anatomia_patologica.csv", delim = "|")
# anatomia_patologica_organo <- read_delim("anatomia_patologica_organo.csv", delim = "|")
# datos_Pablo <- read_delim("BBDD_Pablo.csv", delim = "|")

# 1. Filtrar SOLO registros M970* desde anatomia_patologica_organo
anatomia_filtrado1 <- anatomia_patologica_organo %>%
  mutate(
    morfologia = str_trim(as.character(morfologia))
  ) %>%
  filter(
    !is.na(morfologia)
  ) %>%
  filter(
    str_starts(morfologia, "M970")
  ) %>%
  select(
    CODPACI,
    es_especimen,
    morfologia,
    diagnostico_organo = diagnostico,
    fecha_prueba
  )

# 2. Textos anatomopatológicos
anatomia_filtrado2 <- anatomia_patologica %>% 
  select(
    CODPACI,
    es_especimen,
    fec_entrada,
    fec_validacion,
    diagnostico_ap = diagno_princ,
    t_macro,
    t_micro,
    t_diagnostico
  )

# 3. Merge principal
anatomia_final <- anatomia_filtrado1 %>%
  left_join(
    anatomia_filtrado2,
    by = c("CODPACI", "es_especimen")
  )

# 4. Fecha diagnóstico
tb_aux <- datos_Pablo %>% 
  select(CODPACI, FECHA_DIAGNOSTICO) %>% 
  distinct()

anatomia_final2 <- anatomia_final %>%
  left_join(tb_aux, by = "CODPACI") %>%
  select(
    CODPACI,
    FECHA_DIAGNOSTICO,
    fec_entrada,
    fec_validacion,
    fecha_prueba,
    morfologia,
    diagnostico_organo,
    diagnostico_ap,
    everything()
  )

# 5. Diferencias temporales
anatomia_final2 <- anatomia_final2 %>%
  mutate(
    dias_hasta_validacion = as.numeric(fec_validacion - FECHA_DIAGNOSTICO),
    dias_hasta_prueba = as.numeric(fecha_prueba - FECHA_DIAGNOSTICO),
    dias_prueba_validacion = as.numeric(fec_validacion - fecha_prueba)
  )

# 6. Eliminar fechas inválidas
anatomia_final3 <- anatomia_final2 %>%
  filter(
    !is.na(fecha_prueba)
  ) %>%
  filter(
    fecha_prueba != as.Date("1900-01-01")
  ) %>%
  filter(
    !year(fecha_prueba) %in% c(1991, 1992)
  )

# 7. Solo biopsias
anatomia_final4 <- anatomia_final3 %>%
  filter(
    str_starts(es_especimen, "B")
  )

# 8. Eliminar eventos anteriores al diagnóstico
anatomia_final5 <- anatomia_final4 %>%
  filter(
    is.na(fec_entrada) | fec_entrada >= FECHA_DIAGNOSTICO
  ) %>%
  filter(
    fecha_prueba >= FECHA_DIAGNOSTICO
  )

#### ----- EXITUS + EDAD

fecha_exitus_pablo <- datos_Pablo %>% select(CODPACI, FECHA_EXITUS_PABLO = FECHA_EXITUS, FECHA_ULTIMA_REVISION, FECHA_DIAGNOSTICO)
exitus_demograficos <- demograficos %>% select(CODPACI, FECHANAC, FECHA_EXITUS) 

exitus_final <- merge(fecha_exitus_pablo, exitus_demograficos)

exitus_final <- exitus_final %>%
  mutate(
    edad_diagnostico = floor(interval(FECHANAC, FECHA_DIAGNOSTICO) / years(1)),
    edad_exitus_pablo = floor(interval(FECHANAC, FECHA_EXITUS_PABLO) / years(1)),
    edad_exitus_demograficos = floor(interval(FECHANAC, FECHA_EXITUS) / years(1))
  )

exitus_final <- exitus_final %>%
  mutate(
    misma_fecha_exitus = case_when(
      
      # caso 1: ambas NA → NA
      is.na(FECHA_EXITUS_PABLO) & is.na(FECHA_EXITUS) ~ NA,
      
      # caso 2: ambas existen → comparar
      !is.na(FECHA_EXITUS_PABLO) & !is.na(FECHA_EXITUS) ~ 
        FECHA_EXITUS_PABLO == FECHA_EXITUS,
      
      # caso 3: una sí y otra no → FALSE
      TRUE ~ FALSE
    )
  )

#años de diagnostico a exitus
exitus_final <- exitus_final %>%
  mutate(
    anios_diagnostico_exitus_pablo = floor(interval(FECHA_DIAGNOSTICO, FECHA_EXITUS_PABLO) / years(1)),
    anios_diagnostico_exitus_demograficos = floor(interval(FECHA_DIAGNOSTICO, FECHA_EXITUS) / years(1))
  )

#años de última revisión a exitus
exitus_final <- exitus_final %>%
  mutate(
    anios_ultima_revision_exitus_pablo = floor(interval(FECHA_ULTIMA_REVISION, FECHA_EXITUS_PABLO) / years(1)),
    anios_ultima_revision_exitus_demograficos = floor(interval(FECHA_ULTIMA_REVISION, FECHA_EXITUS) / years(1))
  )

### ------ LDH

ldh_final <-  merge(ldh, tb_aux, all.x = TRUE) %>% select(CODPACI, FECHA_DIAGNOSTICO, FECHA, everything())

#n registros ldh por paciente
ldh2 <- ldh_final %>%
  group_by(CODPACI) %>%
  summarise(
    n_registros = n(),
    n_ldh_altos = sum(VALOR > 225, na.rm = TRUE)
  )

#ldh cada 3 meses
meses_corte <- seq(0, 60, by = 3)

ldh3 <- ldh_final %>%
  mutate(
    FECHA_DIAGNOSTICO = as.Date(FECHA_DIAGNOSTICO),
    FECHA = as.Date(FECHA)
  ) %>%
  filter(!is.na(FECHA_DIAGNOSTICO), !is.na(FECHA), !is.na(VALOR)) %>%
  
  crossing(mes_desde_dx = meses_corte) %>%
  
  mutate(
    fecha_teorica = FECHA_DIAGNOSTICO %m+% months(mes_desde_dx),
    diferencia_dias = abs(as.numeric(FECHA - fecha_teorica))
  ) %>%
  
  group_by(CODPACI, mes_desde_dx) %>%
  slice_min(diferencia_dias, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  mutate(
    columna = paste0("LDH_", mes_desde_dx, "m")
  ) %>%
  
  select(CODPACI, columna, VALOR) %>%
  
  pivot_wider(
    names_from = columna,
    values_from = VALOR
  )

edad_3m <- exitus_final %>%
  mutate(
    FECHA_DIAGNOSTICO = as.Date(FECHA_DIAGNOSTICO),
    FECHANAC = as.Date(FECHANAC)
  ) %>%
  select(CODPACI, FECHANAC, FECHA_DIAGNOSTICO) %>%
  distinct() %>%
  crossing(mes_desde_dx = meses_corte) %>%
  mutate(
    fecha_teorica = FECHA_DIAGNOSTICO %m+% months(mes_desde_dx),
    edad = floor(interval(FECHANAC, fecha_teorica) / years(1)),
    columna_edad = paste0("edad_", mes_desde_dx, "m")
  ) %>%
  select(CODPACI, columna_edad, edad) %>%
  pivot_wider(
    names_from = columna_edad,
    values_from = edad
  )



#número de diagnosticos nuevos al año
diag_anio <- anatomia_final5 %>%
  mutate(
    fecha_diag = as.Date(FECHA_DIAGNOSTICO),
    anio_diagnostico = format(fecha_diag, "%Y")
  ) %>%
  group_by(anio_diagnostico) %>%
  summarise(
    n_diagnosticos = n(),
    n_pacientes = n_distinct(CODPACI)
  )

#número de pacientes que están en seguimiento por año
anios <- seq(1977, 2025)

seguimiento_anual <- exitus_final %>%
  mutate(
    anio_dx = year(FECHA_DIAGNOSTICO),
    anio_exitus = year(FECHA_EXITUS),
    anio_ultima = year(FECHA_ULTIMA_REVISION)
  ) %>%
  
  # usamos exitus si existe, si no última revisión
  mutate(
    anio_fin = ifelse(!is.na(anio_exitus), anio_exitus, anio_ultima)
  ) %>%
  
  crossing(anio = anios) %>%
  
  filter(
    anio >= anio_dx &
      anio <= anio_fin
  ) %>%
  
  group_by(anio) %>%
  summarise(
    pacientes_en_seguimiento = n_distinct(CODPACI)
  )

#DURACIÓN SEGUIMIENTOS
duracion_seguimiento <- exitus_final %>%
  mutate(
    FECHA_DIAGNOSTICO = as.Date(FECHA_DIAGNOSTICO),
    FECHA_EXITUS = as.Date(FECHA_EXITUS),
    FECHA_ULTIMA_REVISION = as.Date(FECHA_ULTIMA_REVISION),
    
    fecha_fin_seguimiento = case_when(
      !is.na(FECHA_EXITUS) ~ FECHA_EXITUS,
      is.na(FECHA_EXITUS) & !is.na(FECHA_ULTIMA_REVISION) ~ FECHA_ULTIMA_REVISION,
      TRUE ~ as.Date(NA)
    ),
    
    duracion_dias = as.numeric(fecha_fin_seguimiento - FECHA_DIAGNOSTICO),
    duracion_meses = floor(duracion_dias / 30.44),
    duracion_anios = floor(duracion_dias / 365.25)
  ) %>%
  select(
    CODPACI,
    FECHA_DIAGNOSTICO,
    FECHA_EXITUS,
    FECHA_ULTIMA_REVISION,
    fecha_fin_seguimiento,
    duracion_dias,
    duracion_meses,
    duracion_anios
  )

