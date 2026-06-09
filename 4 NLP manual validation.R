# VALIDACIÓN MANUAL DEL NLP
# Muestra aleatoria de informes positivos y negativos para LCT y N3

library(dplyr)
library(writexl)

set.seed(123)

# Porcentaje de informes a revisar manualmente
porcentaje_validacion <- 0.05

# Tabla base para validación
validacion_base <- df_resultado %>%
  select(
    CODPACI,
    es_especimen,
    FECHA_DIAGNOSTICO,
    fecha_prueba,
    morfologia,
    diagnostico,
    t_micro,
    t_diagnostico,
    tcg_global,
    n3_global,
    ctx_tcg,
    ctx_n3
  )

# Validación LCT / TCG
validacion_tcg <- validacion_base %>%
  mutate(
    variable_validada = "LCT",
    resultado_nlp = if_else(tcg_global == TRUE, "Positive", "Negative"),
    contexto_detectado = ctx_tcg
  ) %>%
  select(
    CODPACI,
    fecha_prueba,
    es_especimen,
    morfologia,
    variable_validada,
    resultado_nlp,
    contexto_detectado,
    diagnostico,
    t_micro,
    t_diagnostico
  ) %>%
  group_by(resultado_nlp) %>%
  slice_sample(prop = porcentaje_validacion) %>%
  ungroup()

# Validación N3
validacion_n3 <- validacion_base %>%
  mutate(
    variable_validada = "N3",
    resultado_nlp = if_else(n3_global == TRUE, "Positive", "Negative"),
    contexto_detectado = ctx_n3
  ) %>%
  select(
    CODPACI,
    fecha_prueba,
    es_especimen,
    morfologia,
    variable_validada,
    resultado_nlp,
    contexto_detectado,
    diagnostico,
    t_micro,
    t_diagnostico
  ) %>%
  group_by(resultado_nlp) %>%
  slice_sample(prop = porcentaje_validacion) %>%
  ungroup()

# Tabla final para revisión manual
validacion_manual_nlp <- bind_rows(validacion_tcg, validacion_n3) %>%
  mutate(
    revision_manual = NA_character_,      # Correct / Incorrect
    tipo_error = NA_character_,           # False positive / False negative / Negation error / Other
    comentario_revision = NA_character_
  )

# Exportar tabla para revisión manual
write_xlsx(
  validacion_manual_nlp,
  "validacion_manual_nlp.xlsx"
)


#leer validacion revisada manualmente 
library(readxl)
validacion_manual_nlp_revisada <- read_excel("validacion_manual_nlp_revisada2.xlsx")

resumen_validacion_nlp <- validacion_manual_nlp_revisada %>%
  group_by(variable_validada, resultado_nlp, revision_manual) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable_validada, resultado_nlp) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

View(resumen_validacion_nlp)

# MATRIZ DE CONFUSIÓN

library(dplyr)
library(tidyr)
library(ggplot2)

validacion_confusion <- validacion_manual_nlp_revisada %>%
  mutate(
    
    # Predicción del NLP
    prediccion_nlp = resultado_nlp,
    
    # Verdad manual
    verdad_manual = case_when(
      
      # Si NLP dijo Positive y manual dice correct = realmente Positive
      resultado_nlp == "Positive" & revision_manual == "correct" ~ "Positive",
      
      # Si NLP dijo Positive y manual dice incorrect = realmente Negative
      resultado_nlp == "Positive" & revision_manual == "incorrect" ~ "Negative",
      
      # Si NLP dijo Negative y manual dice correct = realmente Negative
      resultado_nlp == "Negative" & revision_manual == "correct" ~ "Negative",
      
      # Si NLP dijo Negative y manual dice incorrect = realmente Positive
      resultado_nlp == "Negative" & revision_manual == "incorrect" ~ "Positive"
    )
  )

# MATRIZ DE CONFUSIÓN POR VARIABLE

matriz_confusion <- validacion_confusion %>%
  count(variable_validada, verdad_manual, prediccion_nlp) %>%
  complete(
    variable_validada,
    verdad_manual = c("Positive", "Negative"),
    prediccion_nlp = c("Positive", "Negative"),
    fill = list(n = 0)
  )

print(matriz_confusion)


metricas_nlp <- validacion_confusion %>%
  mutate(
    TP = verdad_manual == "Positive" & prediccion_nlp == "Positive",
    TN = verdad_manual == "Negative" & prediccion_nlp == "Negative",
    FP = verdad_manual == "Negative" & prediccion_nlp == "Positive",
    FN = verdad_manual == "Positive" & prediccion_nlp == "Negative"
  ) %>%
  group_by(variable_validada) %>%
  summarise(
    Accuracy = round(100 * (sum(TP) + sum(TN)) / n(), 1),
    Sensitivity = round(100 * sum(TP) / (sum(TP) + sum(FN)), 1),
    Specificity = round(100 * sum(TN) / (sum(TN) + sum(FP)), 1),
    .groups = "drop"
  )

print(metricas_nlp)
