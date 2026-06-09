library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)
library(readr)

# Crear tabla específica para NLP a partir de la tabla limpia
anatomia_descripciones <- anatomia_final5 %>%
  select(
    CODPACI,
    es_especimen,
    FECHA_DIAGNOSTICO,
    fecha_prueba,
    morfologia,
    t_micro,
    t_diagnostico,
    diagnostico = diagnostico_organo,
    diagnostico_ap
  )

# CONFIGURACIÓN

MIN_FREC <- 2     
N_EXPORT <- 500   

df <- anatomia_descripciones


# PREPROCESAMIENTO

stopwords_es <- c(
  "de","la","el","en","y","a","que","con","no","se","del","los","las",
  "un","una","por","al","es","su","lo","le","como","más","pero","o",
  "si","también","este","esta","hay","son","fue","ser","para","sin",
  "sobre","entre","algunos","algunas","dicho","caso","paciente","muestra",
  "material","presenta","presencia","ausencia","evidencia","compatible",
  "observa","aprecia","mismo","dicha","tras","piel","que","presenta",
  "aspecto","organico","satisfactoria","evaluacion","microscopica"
)

limpiar <- function(x) {
  x |>
    str_remove_all("^\\*+") |>                        # quita asteriscos iniciales
    str_to_lower() |>                # quita números
    str_replace_all("[^[:alnum:][:space:]]", " ") |>  #reemplaza la línea problemática
    str_replace_all("[[:space:]]+", " ") |>
    str_squish()
}


es_vacio <- function(x) {
  is.na(x) | str_trim(x) %in% c("", "NA", "--", ".")
}

df_prep <- df |>
  mutate(
    # Indicadores de contenido válido
    tiene_micro = !es_vacio(t_micro),
    tiene_diag  = !es_vacio(diagnostico),
    tiene_diagnostico_micro = !es_vacio(t_diagnostico),
    
    # Textos limpios
    micro_limpio = if_else(tiene_micro, limpiar(t_micro),      NA_character_),
    diag_limpio  = if_else(tiene_diag,  limpiar(diagnostico),  NA_character_),
    diagmicro_limpio  = if_else(tiene_diagnostico_micro,  limpiar(t_diagnostico),  NA_character_),
  )


# FUNCIÓN GENÉRICA DE N-GRAMAS

calcular_ngrams <- function(textos, n, stopwords, min_frec = 2) {
  
  df_tmp <- data.frame(texto = textos[!is.na(textos)])
  
  if (nrow(df_tmp) == 0) return(data.frame(combinacion = character(), n = integer()))
  
  palabras <- paste0("w", 1:n)
  
  df_tmp |>
    unnest_tokens(term, texto, token = "ngrams", n = n) |>
    separate(term, palabras, sep = " ") |>
    # Filtra stopwords solo en primera y última palabra
    filter(
      !.data[[palabras[1]]] %in% stopwords,
      !.data[[palabras[n]]] %in% stopwords,
      str_length(.data[[palabras[1]]]) > 2,
      str_length(.data[[palabras[n]]]) > 2
    ) |>
    unite(combinacion, all_of(palabras), sep = " ") |>
    count(combinacion, sort = TRUE) |>
    filter(n >= min_frec)
}


# CALCULAR N-GRAMAS PARA CADA COLUMNA (busca las combinaciones de palabras más frecuentes en los textos)

cat("n-gramas de t_micro...\n")
micro_uni  <- calcular_ngrams(df_prep$micro_limpio, 1, stopwords_es, MIN_FREC)
micro_bi   <- calcular_ngrams(df_prep$micro_limpio, 2, stopwords_es, MIN_FREC)
micro_tri  <- calcular_ngrams(df_prep$micro_limpio, 3, stopwords_es, MIN_FREC)
micro_4    <- calcular_ngrams(df_prep$micro_limpio, 4, stopwords_es, MIN_FREC)
micro_5    <- calcular_ngrams(df_prep$micro_limpio, 5, stopwords_es, MIN_FREC)

cat("n-gramas de diagnostico...\n")
diag_uni   <- calcular_ngrams(df_prep$diag_limpio, 1, stopwords_es, MIN_FREC)
diag_bi    <- calcular_ngrams(df_prep$diag_limpio, 2, stopwords_es, MIN_FREC)
diag_tri   <- calcular_ngrams(df_prep$diag_limpio, 3, stopwords_es, MIN_FREC)
diag_4     <- calcular_ngrams(df_prep$diag_limpio, 4, stopwords_es, MIN_FREC)
diag_5     <- calcular_ngrams(df_prep$diag_limpio, 5, stopwords_es, MIN_FREC)

cat("n-gramas de diagnostico micro...\n")
diagmicro_uni   <- calcular_ngrams(df_prep$diagmicro_limpio, 1, stopwords_es, MIN_FREC)
diagmicro_bi    <- calcular_ngrams(df_prep$diagmicro_limpio, 2, stopwords_es, MIN_FREC)
diagmicro_tri   <- calcular_ngrams(df_prep$diagmicro_limpio, 3, stopwords_es, MIN_FREC)
diagmicro_4     <- calcular_ngrams(df_prep$diagmicro_limpio, 4, stopwords_es, MIN_FREC)
diagmicro_5     <- calcular_ngrams(df_prep$diagmicro_limpio, 5, stopwords_es, MIN_FREC)

# RESUMEN EN CONSOLA

cat("\n── Top 15 bigramas en t_micro ──\n")
print(micro_bi |> head(15))

cat("\n── Top 15 bigramas en diagnostico ──\n")
print(diag_bi |> head(15))

cat("\n── Top 15 bigramas en t_diagnostico ──\n")
print(diagmicro_bi |> head(15))


# GRÁFICOS

grafico_ngrams <- function(datos, color) {
  datos |>
    head(20) |>
    mutate(combinacion = reorder(combinacion, n)) |>
    ggplot(aes(x = n, y = combinacion)) +
    geom_col(fill = color, alpha = 0.85, width = 0.7) +
    geom_text(
      aes(label = n),
      hjust = -0.2,
      size = 5,
      color = "gray30"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = NULL,
      x = "Frequency",
      y = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold")
    )
}

dir.create("plots_nlp", showWarnings = FALSE)

#plots
p_micro_bi <- grafico_ngrams(micro_bi, "#534AB7")
p_diag_bi <- grafico_ngrams(diag_bi, "#1D9E75")
p_diagmicro_bi <- grafico_ngrams(diagmicro_bi, "#76EEC6")

p_micro_tri <- grafico_ngrams(micro_tri, "#E07B3A")
p_diag_tri <- grafico_ngrams(diag_tri, "#E24B4A")
p_diagmicro_tri <- grafico_ngrams(diagmicro_tri, "#66CD00")

#guardar en PNG
ggsave("plots_nlp/top20_bigramas_t_micro.png", p_micro_bi, width = 10, height = 7, dpi = 300)
ggsave("plots_nlp/top20_bigramas_diagnostico.png", p_diag_bi, width = 10, height = 7, dpi = 300)
ggsave("plots_nlp/top20_bigramas_t_diagnostico.png", p_diagmicro_bi, width = 10, height = 7, dpi = 300)

ggsave("plots_nlp/top20_trigramas_t_micro.png", p_micro_tri, width = 10, height = 7, dpi = 300)
ggsave("plots_nlp/top20_trigramas_diagnostico.png", p_diag_tri, width = 10, height = 7, dpi = 300)
ggsave("plots_nlp/top20_trigramas_t_diagnostico.png", p_diagmicro_tri, width = 10, height = 7, dpi = 300)


# EXPORTAR A EXCEL

write_xlsx(
  list(
    # t_micro
    "micro_1_palabras"  = micro_uni  |> head(N_EXPORT),
    "micro_2_pares"     = micro_bi   |> head(N_EXPORT),
    "micro_3_trios"     = micro_tri  |> head(N_EXPORT),
    "micro_4_cuartetos" = micro_4    |> head(N_EXPORT),
    
    # diagnostico
    "diag_1_palabras"   = diag_uni   |> head(N_EXPORT),
    "diag_2_pares"      = diag_bi    |> head(N_EXPORT),
    "diag_3_trios"      = diag_tri   |> head(N_EXPORT),
    "diag_4_cuartetos"  = diag_4     |> head(N_EXPORT),
    
    # diagnostico micro
    "diagmicro_1_palabras"   = diagmicro_uni   |> head(N_EXPORT),
    "diagmicro_2_pares"      = diagmicro_bi    |> head(N_EXPORT),
    "diagmicro_3_trios"      = diagmicro_tri   |> head(N_EXPORT),
    "diagmicro_4_cuartetos"  = diagmicro_4     |> head(N_EXPORT)
  ),
  "ngrams_revision.xlsx"
)


# DETECTAR PATRONES

#FUNCIÓN DE NEGACIÓN

tiene_patron <- function(texto, patron, ventana = 60) {
  
  negaciones <- paste(
    "no\\s+", "sin\\s+", "sin\\s+evidencia", "sin\\s+signos",
    "se\\s+descarta", "descarta", "ausencia\\s+de",
    "no\\s+se\\s+observa", "no\\s+se\\s+aprecia",
    "no\\s+se\\s+detecta", "no\\s+hay",
    "no\\s+compatible", "negativo\\s+para",
    sep = "|"
  )
  
  texto_norm <- str_to_lower(texto) |> str_squish()
  
  # Encuentra todas las posiciones donde aparece el patrón
  matches <- gregexpr(patron, texto_norm, perl = TRUE, ignore.case = TRUE)[[1]]
  
  if (matches[1] == -1) return(FALSE)  # no aparece
  
  for (pos in matches) {
    # Extrae contexto antes del match
    inicio   <- max(1, pos - ventana)
    contexto <- substr(texto_norm, inicio, pos - 1)
    
    # Si NO hay negación antes es positivo
    if (!str_detect(contexto, regex(negaciones, ignore_case = TRUE))) {
      return(TRUE)
    }
  }
  
  return(FALSE)  # solo aparece negado
}

# en vector
detectar <- function(textos, patron, ventana = 60) {
  sapply(textos, function(x) {
    if (is.na(x) || str_trim(x) == "") return(NA)
    tiene_patron(x, patron, ventana)
  }, USE.NAMES = FALSE)
}


#PATRONES BASADOS EN TRIGRAMAS Y CONOCIMIENTO CLÍNICO

# TRANSFORMACIÓN A CÉLULAS GRANDES
PATRON_TCG <- paste(
  "transformaci[oó]n\\s+a\\s+c[eé]lulas?\\s+grandes?",
  "transformaci[oó]n\\s+a\\s+c[eé]lulas?",
  "transformad[ao]s?\\s+a\\s+c[eé]lulas?\\s+grandes?",
  "micosis\\s+fungoide[s]?.{0,40}c[eé]lulas?\\s+grandes?",
  "c[eé]lulas?\\s+grandes?.{0,40}micosis\\s+fungoide[s]?",
  "c[eé]lulas?\\s+grandes?.{0,40}estadio\\s+tumoral",
  "estadio\\s+tumoral.{0,40}c[eé]lulas?\\s+grandes?",
  "c[eé]lulas?\\s+grandes?.{0,40}cd\\s*30\\s*positiv[ao]s?",
  "cd\\s*30\\s*positiv[ao]s?.{0,40}c[eé]lulas?\\s+grandes?",
  "c[eé]lulas?\\s+grandes?\\s+transformadas?",
  "transformaci[oó]n\\s+blastoide",
  "tumoral\\s+en\\s+transformaci[oó]n",
  "transformaci[oó]n\\s+tumoral",
  "nidos?\\s+de\\s+c[eé]lulas?\\s+grandes?",
  "nidos?\\s+de\\s+linfocitos?.{0,20}tama[nñ]o",
  "veces\\s+el\\s+tama[nñ]o",
  "large\\s+cell\\s+transformation",
  sep = "|"
)

expresiones_lct_resultados <- c(
  "transformacion a celulas grandes" =
    "transformaci[oó]n\\s+a\\s+c[eé]lulas?\\s+grandes?",
  
  "transformacion a celulas" =
    "transformaci[oó]n\\s+a\\s+c[eé]lulas?",
  
  "transformados a celulas grandes" =
    "transformad[ao]s?\\s+a\\s+c[eé]lulas?\\s+grandes?",
  
  "micosis fungoide + celulas grandes" =
    "micosis\\s+fungoide[s]?.{0,40}c[eé]lulas?\\s+grandes?",
  
  "celulas grandes + micosis fungoide" =
    "c[eé]lulas?\\s+grandes?.{0,40}micosis\\s+fungoide[s]?",
  
  "celulas grandes + estadio tumoral" =
    "c[eé]lulas?\\s+grandes?.{0,40}estadio\\s+tumoral",
  
  "estadio tumoral + celulas grandes" =
    "estadio\\s+tumoral.{0,40}c[eé]lulas?\\s+grandes?",
  
  "celulas grandes + CD30 positivo" =
    "c[eé]lulas?\\s+grandes?.{0,40}cd\\s*30\\s*positiv[ao]s?",
  
  "CD30 positivo + celulas grandes" =
    "cd\\s*30\\s*positiv[ao]s?.{0,40}c[eé]lulas?\\s+grandes?",
  
  "celulas grandes transformadas" =
    "c[eé]lulas?\\s+grandes?\\s+transformadas?",
  
  "transformacion blastoide" =
    "transformaci[oó]n\\s+blastoide",
  
  "tumoral en transformacion" =
    "tumoral\\s+en\\s+transformaci[oó]n",
  
  "transformacion tumoral" =
    "transformaci[oó]n\\s+tumoral",
  
  "nidos de celulas grandes" =
    "nidos?\\s+de\\s+c[eé]lulas?\\s+grandes?",
  
  "nidos de linfocitos + tamaño" =
    "nidos?\\s+de\\s+linfocitos?.{0,20}tama[nñ]o",
  
  "veces el tamaño" =
    "veces\\s+el\\s+tama[nñ]o",
  
  "large cell transformation" =
    "large\\s+cell\\s+transformation"
)

frecuencias_lct <- sapply(expresiones_lct_resultados, function(patron) {
  sum(
    str_detect(
      df_resultado$texto_completo,
      regex(patron, ignore_case = TRUE)
    ),
    na.rm = TRUE
  )
})

frecuencias_lct <- data.frame(
  Expression = names(frecuencias_lct),
  Frequency = as.numeric(frecuencias_lct)
) %>%
  arrange(desc(Frequency))

print(frecuencias_lct)

# N3
PATRON_N3 <- paste(
  "\\bpN3\\b",
  "\\bN3\\b",
  "categor[ií]a\\s+N3",
  "estadio\\s+N3",
  "N3\\s+(de\\s+)?EORTC",
  "distorsi[oó]n.{0,30}ganglio",                  
  "afectaci[oó]n.{0,30}ganglio",
  "ganglio.{0,30}distorsion",
  "cilindros\\s+de\\s+ganglio.{0,40}afect",      
  "m[aá]s\\s+de\\s+(6|7|8|9|10).{0,10}ganglios?",
  sep = "|"
)

expresiones_n3_resultados <- c(
  "pN3" = "\\bpN3\\b",
  "N3" = "\\bN3\\b",
  "categoria N3" = "categor[ií]a\\s+N3",
  "estadio N3" = "estadio\\s+N3",
  "N3 EORTC" = "N3\\s+(de\\s+)?EORTC",
  "distorsion ganglio" = "distorsi[oó]n.{0,30}ganglio",
  "afectacion ganglio" = "afectaci[oó]n.{0,30}ganglio",
  "ganglio distorsion" = "ganglio.{0,30}distorsion",
  "cilindros de ganglio afectado" = "cilindros\\s+de\\s+ganglio.{0,40}afect",
  "mas de 6-10 ganglios" = "m[aá]s\\s+de\\s+(6|7|8|9|10).{0,10}ganglios?"
)

frecuencias_n3 <- sapply(expresiones_n3_resultados, function(patron) {
  sum(
    str_detect(
      df_resultado$texto_completo,
      regex(patron, ignore_case = TRUE)
    ),
    na.rm = TRUE
  )
})

frecuencias_n3 <- data.frame(
  Expression = names(frecuencias_n3),
  Frequency = as.numeric(frecuencias_n3)
) %>%
  arrange(desc(Frequency))

print(frecuencias_n3)


# EN MI TABLA
df_resultado <- df_prep |>
  mutate(
    # Concatena ambas columnas para buscar en las dos a la vez
    texto_completo = paste(
      if_else(is.na(micro_limpio),    "", micro_limpio),
      if_else(is.na(diag_limpio),"", diag_limpio),
      if_else(is.na(diagmicro_limpio),"", diagmicro_limpio),
      sep = " "
    ),
    
    # Detección con negación
    tcg_micro  = detectar(micro_limpio,        PATRON_TCG),
    tcg_diag   = detectar(diag_limpio,    PATRON_TCG),
    tcg_diagmicro   = detectar(diagmicro_limpio,    PATRON_TCG),
    tcg_global = detectar(texto_completo, PATRON_TCG),  # positivo si aparece en cualquiera
    
    n3_micro   = detectar(micro_limpio,        PATRON_N3),
    n3_diag    = detectar(diag_limpio,    PATRON_N3),
    n3_diagmicro    = detectar(diagmicro_limpio,    PATRON_N3),
    n3_global  = detectar(texto_completo, PATRON_N3),
    
    # Contexto para validación manual
    ctx_tcg = str_extract(
      str_to_lower(texto_completo),
      paste0(".{0,70}(?:", PATRON_TCG, ").{0,70}")
    ),
    ctx_n3  = str_extract(
      str_to_lower(texto_completo),
      paste0(".{0,70}(?:", PATRON_N3, ").{0,70}")
    )
  )

#GRÁFICO PARA VER DONDE SE DETECTAN

resumen_deteccion <- data.frame(
  patron = c("LCT", "LCT", "LCT", "N3", "N3", "N3"),
  columna = c(
    "t_micro", "diagnostico", "t_diagnostico",
    "t_micro", "diagnostico", "t_diagnostico"
  ),
  n_detectados = c(
    sum(df_resultado$tcg_micro, na.rm = TRUE),
    sum(df_resultado$tcg_diag, na.rm = TRUE),
    sum(df_resultado$tcg_diagmicro, na.rm = TRUE),
    sum(df_resultado$n3_micro, na.rm = TRUE),
    sum(df_resultado$n3_diag, na.rm = TRUE),
    sum(df_resultado$n3_diagmicro, na.rm = TRUE)
  )
)
p_deteccion <- ggplot(resumen_deteccion, aes(x = n_detectados, y = patron, fill = columna)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
  geom_text(
    aes(label = n_detectados),
    position = position_dodge(width = 0.8),
    hjust = -0.2,
    size = 3.5,
    color = "gray30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribution of LCT and N3 detections by text field",
    x = "Number of detected reports",
    y = "Detected concept",
    fill = "Text field"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(p_deteccion)

ggsave(
  "plots_nlp/detecciones_LCT_N3_por_campo.png",
  p_deteccion,
  width = 8,
  height = 5,
  dpi = 300
)


# RESULTADOS A NIVEL PACIENTE
pacientes_nlp <- df_resultado %>%
  group_by(CODPACI) %>%
  summarise(
    tcg_patient = any(tcg_global == TRUE, na.rm = TRUE),
    n3_patient  = any(n3_global  == TRUE, na.rm = TRUE)
  )

cat("Pacientes totales:              ", nrow(pacientes_nlp), "\n")
cat("Pacientes con LCT:              ", sum(pacientes_nlp$tcg_patient), "\n")
cat("Pacientes con N3:               ", sum(pacientes_nlp$n3_patient), "\n")
cat("Pacientes con ambos:            ",
    sum(pacientes_nlp$tcg_patient & pacientes_nlp$n3_patient), "\n")


#EXPORTAR
library(writexl)

write_xlsx(list(
  
  # Todos los casos con flags
  "todos" = df_resultado |>
    select(CODPACI, es_especimen, tcg_global, n3_global,
           tcg_micro, tcg_diag, n3_micro, n3_diag),
  
  # Solo positivos TCG — para validación manual
  "validar_TCG" = df_resultado |>
    filter(tcg_global == TRUE) |>
    select(CODPACI, es_especimen, diagnostico, ctx_tcg),
  
  # Solo positivos N3 — para validación manual
  "validar_N3" = df_resultado |>
    filter(n3_global == TRUE) |>
    select(CODPACI, es_especimen, diagnostico, ctx_n3)
  
), "deteccion_tcg_n3.xlsx")



# RESULTS

library(readxl)
df_nlp <- read_excel("deteccion_tcg_n3.xlsx", sheet = "todos")

# Convertir a lógico por si vienen como 0/1 o TRUE/FALSE
df_nlp <- df_nlp %>%
  mutate(
    tcg_global = as.logical(tcg_global),
    n3_global = as.logical(n3_global)
  )

# Tabla de co-ocurrencia por informe
tabla_coocurrencia <- df_nlp %>%
  mutate(
    LCT = if_else(tcg_global, "LCT positive", "LCT negative"),
    N3 = if_else(n3_global, "N3 positive", "N3 negative")
  ) %>%
  count(LCT, N3, name = "n_reports")

print(tabla_coocurrencia)

# Heatmap
p_heatmap <- ggplot(tabla_coocurrencia, aes(x = N3, y = LCT, fill = n_reports)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = n_reports), size = 6, color = "black", fontface = "bold") +
  scale_fill_gradient(
    low = "#EAF3FF",
    high = "#6BAED6"
  ) +
  labs(
    title = "Co-occurrence of LCT and N3 detections",
    x = "N3 detection",
    y = "LCT detection",
    fill = "Reports"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

print(p_heatmap)

ggsave(
  "plots_nlp/heatmap_cooccurrence_LCT_N3.png",
  p_heatmap,
  width = 7,
  height = 5,
  dpi = 300
)

#TOP EXPRESIONES
library(gridExtra)
library(grid)

top_expressions <- data.frame(
  Concept = c(
    "Large-cell transformation",
    "Large-cell transformation",
    "Large-cell transformation",
    "Large-cell transformation",
    "N3 nodal involvement",
    "N3 nodal involvement",
    "N3 nodal involvement"
  ),
  Expression = c(
    "transformacion a celulas",
    "celulas grandes",
    "tumoral en transformacion",
    "celulas grandes cd30",
    "N3 / pN3",
    "categoria N3",
    "ganglio linfatico / cilindros de ganglio"
  ),
  Interpretation = c(
    "Direct mention of transformation",
    "Reference to large cells",
    "Alternative wording for transformation",
    "Large cells associated with CD30 expression",
    "Explicit nodal stage",
    "Explicit staging category",
    "Contextual reference to lymph node involvement"
  )
)

p_top_expressions <- tableGrob(
  top_expressions,
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 10),
      bg_params = list(fill = c("#F7FBFF", "#EAF3FF"))
    ),
    colhead = list(
      fg_params = list(fontsize = 11, fontface = "bold", col = "white"),
      bg_params = list(fill = "#2B6CB0")
    )
  )
)

png(
  "plots_nlp/top_expressions_LCT_N3.png",
  width = 3000,
  height = 1300,
  res = 300
)

