# RELLENAR VARIABLES CLIPI CON EL ÚLTIMO VALOR CONOCIDO

eventos_rellenados <- eventos_con_fechas %>%
  arrange(CODPACI, fecha_evento) %>%
  group_by(CODPACI) %>%
  fill(tcg, n3, .direction = "down") %>%
  fill(ldh_alta, valor_ldh, .direction = "down") %>%
  ungroup() %>%
  mutate(
    tcg = replace_na(tcg, FALSE),
    n3 = replace_na(n3, FALSE),
    ldh_alta = replace_na(ldh_alta, FALSE)
  )


# TABLA CLIPI 

clipi <- eventos_rellenados %>%
  mutate(
    tcg_flag = if_else(tcg == TRUE, 1, 0, missing = 0),
    n3_flag = if_else(n3 == TRUE, 1, 0, missing = 0),
    ldh_flag = if_else(ldh_alta == TRUE, 1, 0, missing = 0),
    edad_flag = if_else(edad_evento_mayor_60 == TRUE, 1, 0, missing = 0),
    
    CLIPI = tcg_flag + n3_flag + ldh_flag + edad_flag
  )

#CLIPi por paciente
clipi_paciente <- clipi %>%
  group_by(CODPACI) %>%
  summarise(
    CLIPI_min = min(CLIPI, na.rm = TRUE),
    CLIPI_max = max(CLIPI, na.rm = TRUE),
    CLIPI_last = last(CLIPI),
    n_eventos_CLIPI = n(),
    .groups = "drop"
  )

View(clipi_paciente)


# CLIPI ANUAL
# Cada columna = CLIPI máximo de ese año

clipi_anual <- clipi %>%
  filter(!is.na(CLIPI)) %>%
  group_by(CODPACI) %>%
  arrange(fecha_evento, .by_group = TRUE) %>%
  mutate(
    fecha_primer_clipi = first(fecha_evento),
    dias_desde_primer_clipi = as.numeric(fecha_evento - fecha_primer_clipi),
    anio_clipi = floor(dias_desde_primer_clipi / 365)
  ) %>%
  ungroup() %>%
  group_by(CODPACI, anio_clipi) %>%
  summarise(
    CLIPI = max(CLIPI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CODPACI) %>%
  complete(
    anio_clipi = 0:max(anio_clipi)
  ) %>%
  ungroup() %>%
  mutate(
    columna_anio = paste0("CLIPI_anio_", anio_clipi)
  ) %>%
  select(CODPACI, columna_anio, CLIPI) %>%
  pivot_wider(
    names_from = columna_anio,
    values_from = CLIPI
  )

View(clipi_anual)

# TABLA CLIPI SIMPLIFICADA PARA TODOS LOS PACIENTES

tabla_clipi_simplificada <- clipi %>%
  arrange(CODPACI, fecha_evento) %>%
  select(
    CODPACI,
    fecha_evento,
    tipo_evento,
    edad_flag,
    ldh_flag,
    tcg_flag,
    n3_flag,
    CLIPI
  ) %>%
  mutate(
    fecha_evento = as.Date(fecha_evento),
    tipo_evento = case_when(
      tipo_evento == "anat_pat" ~ "Biopsy",
      tipo_evento == "ldh" ~ "LDH",
      tipo_evento == "seguimiento" ~ "Follow-up",
      TRUE ~ tipo_evento
    )
  )

View(tabla_clipi_simplificada)


# ANÁLISIS DESCRIPTIVO DE RESULTADOS CLIPI

# número de evaluaciones CLIPi por paciente
resumen_n_clipi <- clipi_paciente %>%
  summarise(
    total_pacientes = n(),
    total_evaluaciones_CLIPI = sum(n_eventos_CLIPI),
    media_evaluaciones_paciente = mean(n_eventos_CLIPI),
    sd_evaluaciones_paciente = sd(n_eventos_CLIPI),
    mediana_evaluaciones_paciente = median(n_eventos_CLIPI),
    Q1_evaluaciones_paciente = quantile(n_eventos_CLIPI, 0.25),
    Q3_evaluaciones_paciente = quantile(n_eventos_CLIPI, 0.75),
    min_evaluaciones_paciente = min(n_eventos_CLIPI),
    max_evaluaciones_paciente = max(n_eventos_CLIPI)
  )

View(resumen_n_clipi)


#Distribución del último CLIPi obtenido por paciente
distribucion_clipi_last <- clipi_paciente %>%
  count(CLIPI_last, name = "n_pacientes") %>%
  mutate(
    porcentaje = round(100 * n_pacientes / sum(n_pacientes), 1)
  ) %>%
  arrange(CLIPI_last)

View(distribucion_clipi_last)


# Grupos de riesgo según último CLIPi
clipi_paciente <- clipi_paciente %>%
  mutate(
    risk_group_last = case_when(
      CLIPI_last %in% c(0, 1) ~ "Low risk (CLIPi 0-1)",
      CLIPI_last == 2 ~ "Intermediate risk (CLIPi 2)",
      CLIPI_last %in% c(3, 4) ~ "High risk (CLIPi 3-4)",
      TRUE ~ NA_character_
    )
  )

distribucion_risk_group_last <- clipi_paciente %>%
  count(risk_group_last, name = "n_pacientes") %>%
  mutate(
    porcentaje = round(100 * n_pacientes / sum(n_pacientes), 1)
  )

View(distribucion_risk_group_last)


#Evolución longitudinal del CLIPi por paciente
evolucion_clipi_paciente <- clipi_paciente %>%
  mutate(
    cambio_clipi = CLIPI_max - CLIPI_min,
    aumento_clipi = cambio_clipi > 0,
    alcanza_clipi_alto = CLIPI_max >= 3
  )

resumen_evolucion_clipi <- evolucion_clipi_paciente %>%
  summarise(
    pacientes_sin_cambio = sum(cambio_clipi == 0, na.rm = TRUE),
    porcentaje_sin_cambio = round(100 * pacientes_sin_cambio / n(), 1),
    
    pacientes_con_aumento = sum(aumento_clipi, na.rm = TRUE),
    porcentaje_con_aumento = round(100 * pacientes_con_aumento / n(), 1),
    
    pacientes_alcanzan_CLIPI_3_4 = sum(alcanza_clipi_alto, na.rm = TRUE),
    porcentaje_alcanzan_CLIPI_3_4 = round(100 * pacientes_alcanzan_CLIPI_3_4 / n(), 1)
  )

View(resumen_evolucion_clipi)


# GRÁFICOS

plot_clipi_last <- distribucion_clipi_last %>%
  ggplot(aes(x = factor(CLIPI_last), y = n_pacientes)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = n_pacientes),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Distribution of final CLIPi score across patients",
    x = "Final CLIPi score",
    y = "Number of patients"
  ) +
  theme_minimal(base_size = 13)

plot_clipi_last

plot_risk_group_last <- distribucion_risk_group_last %>%
  ggplot(aes(x = risk_group_last, y = n_pacientes)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = n_pacientes),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Distribution of final CLIPi risk groups",
    x = "Risk group",
    y = "Number of patients"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

plot_risk_group_last
