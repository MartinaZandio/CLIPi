# TABLA LONGITUDINAL DE EVENTOS ÚTILES PARA CLIPI

pacientes_finales <- unique(anatomia_final5$CODPACI)

eventos_anatomia <- df_resultado %>%
  filter(CODPACI %in% pacientes_finales) %>%
  transmute(
    CODPACI,
    tipo_evento = "biopsia_anatomia",
    fecha_evento = fecha_prueba,
    es_especimen,
    tcg = tcg_global,
    n3 = n3_global,
    valor_ldh = NA_real_,
    ldh_alta = NA,
    diagnostico_anatomia = diagnostico,
    diagnostico_micro = t_diagnostico,
    texto_micro = t_micro
  )

eventos_ldh <- ldh %>%
  filter(CODPACI %in% pacientes_finales) %>%
  transmute(
    CODPACI,
    tipo_evento = "LDH",
    fecha_evento = FECHA,
    es_especimen = NA_character_,
    tcg = NA,
    n3 = NA,
    valor_ldh = as.numeric(VALOR),
    ldh_alta = as.numeric(VALOR) > 225,
    diagnostico_anatomia = NA_character_,
    diagnostico_micro = NA_character_,
    texto_micro = NA_character_
  )

eventos_revision <- datos_Pablo %>%
  filter(CODPACI %in% pacientes_finales) %>%
  select(CODPACI, FECHA_ULTIMA_REVISION) %>%
  distinct() %>%
  filter(!is.na(FECHA_ULTIMA_REVISION)) %>%
  transmute(
    CODPACI,
    tipo_evento = "ultima_revision",
    fecha_evento = FECHA_ULTIMA_REVISION,
    es_especimen = NA_character_,
    tcg = NA,
    n3 = NA,
    valor_ldh = NA_real_,
    ldh_alta = NA,
    diagnostico_anatomia = NA_character_,
    diagnostico_micro = NA_character_,
    texto_micro = NA_character_
  )

eventos_con_fechas <- timeline_longitudinal %>%
  left_join(datos_paciente, by = "CODPACI") %>%
  mutate(
    dias_desde_diagnostico = as.numeric(fecha_evento - FECHA_DIAGNOSTICO),
    meses_desde_diagnostico = round(dias_desde_diagnostico / 30.44, 1),
    
    edad_evento = edad_diagnostico + floor(dias_desde_diagnostico / 365.25),
    edad_evento_mayor_60 = edad_evento > 60
  ) %>%
  filter(
    !is.na(fecha_evento),
    dias_desde_diagnostico >= 0
  ) %>%
  select(-edad_diagnostico, -dias_desde_diagnostico, -meses_desde_diagnostico)
