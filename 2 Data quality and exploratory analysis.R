# ══════════════════════════════════════════════════════════════
# 4.2 DATA PREPROCESSING AND EXPLORATORY ANALYSIS
# Organized script for final cohort EDA and longitudinal reconstruction
# TFG - Automated CLIPi computation in Mycosis Fungoides
# ══════════════════════════════════════════════════════════════

# PURPOSE OF THIS SCRIPT
# This script organizes the analyses corresponding to:
# - Methods section 3.2.2: Data preprocessing and exploratory analysis
# - Results section 4.2: Data preprocessing and exploratory analysis results
#
# It assumes that the final cohort has already been created in Section 3.2.1
# and is stored as anatomia_final5.


library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(gt)

# 1. OPTIONAL: LOAD ORIGINAL DATASETS

# demograficos <- read_delim("demograficos.csv", delim = "|")
# ldh <- read_delim("LDH_obx.csv", delim = "|")
# anatomia_patologica <- read_delim("anatomia_patologica.csv", delim = "|")
# anatomia_patologica_organo <- read_delim("anatomia_patologica_organo.csv", delim = "|")
# datos_Pablo <- read_delim("BBDD_Pablo.csv", delim = "|")


#DEFINE FINAL COHORT

pacientes_finales <- unique(anatomia_final5$CODPACI)
cat("Final cohort patients:", length(pacientes_finales), "\n")


#FILTER DATASETS TO FINAL COHORT

demograficos_final <- demograficos %>%
  filter(CODPACI %in% pacientes_finales)

ldh_final <- ldh %>%
  filter(CODPACI %in% pacientes_finales)

anatomia_final_cohorte <- anatomia_final5

datos_derma_final <- datos_Pablo %>%
  filter(CODPACI %in% pacientes_finales)


# DATASET COVERAGE IN FINAL COHORT

dataset_coverage <- tibble(
  Dataset = c(
    "Demographic records",
    "Laboratory LDH dataset",
    "Dermatology follow-up dataset",
    "Final pathology cohort"
  ),
  Records = c(
    nrow(demograficos_final),
    nrow(ldh_final),
    nrow(datos_derma_final),
    nrow(anatomia_final_cohorte)
  ),
  `Unique patients` = c(
    n_distinct(demograficos_final$CODPACI),
    n_distinct(ldh_final$CODPACI),
    n_distinct(datos_derma_final$CODPACI),
    n_distinct(anatomia_final_cohorte$CODPACI)
  )
)

tabla_dataset_coverage <- dataset_coverage %>%
  gt() %>%
  tab_header(title = md("**Dataset Coverage in the Final Cohort**")) %>%
  tab_style(
    style = list(cell_fill(color = "#DCEBFA"), cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F8FC"),
    locations = cells_body(rows = seq(1, nrow(dataset_coverage), 2))
  ) %>%
  tab_options(
    table.border.top.color = "#5B9BD5",
    table.border.bottom.color = "#5B9BD5",
    heading.border.bottom.color = "#5B9BD5",
    table.font.size = "12px",
    data_row.padding = px(5)
  )

tabla_dataset_coverage


#DEMOGRAPHIC PREPROCESSING

diagnosis_dates <- datos_derma_final %>%
  select(CODPACI, FECHA_DIAGNOSTICO) %>%
  filter(!is.na(FECHA_DIAGNOSTICO)) %>%
  group_by(CODPACI) %>%
  summarise(FECHA_DIAGNOSTICO = min(as.Date(FECHA_DIAGNOSTICO)), .groups = "drop")

demograficos_final <- demograficos %>%
  filter(CODPACI %in% pacientes_finales) %>%
  select(CODPACI, FECHANAC, SEXO, FECHA_EXITUS, ESTADO) %>%
  distinct() %>%
  left_join(diagnosis_dates, by = "CODPACI") %>%
  mutate(
    FECHANAC = as.Date(FECHANAC),
    FECHA_DIAGNOSTICO = as.Date(FECHA_DIAGNOSTICO),
    FECHA_EXITUS = as.Date(FECHA_EXITUS),
    
    SEXO = str_trim(as.character(SEXO)),
    
    SEXO = case_when(
      SEXO %in% c("V", "Hombre", "Male") ~ "Male",
      SEXO %in% c("M", "Mujer", "Female") ~ "Female",
      TRUE ~ NA_character_
    ),
    
    SEXO = factor(SEXO, levels = c("Male", "Female")),
    
    edad_diagnostico = floor(interval(FECHANAC, FECHA_DIAGNOSTICO) / years(1)),
    
    grupo_edad = case_when(
      edad_diagnostico < 40 ~ "<40",
      edad_diagnostico >= 40 & edad_diagnostico < 60 ~ "40-59",
      edad_diagnostico >= 60 & edad_diagnostico < 80 ~ "60-79",
      edad_diagnostico >= 80 ~ "80+",
      TRUE ~ NA_character_
    ),
    
    grupo_edad = factor(grupo_edad, levels = c("<40", "40-59", "60-79", "80+"))
  )


#DEMOGRAPHIC SUMMARY TABLE

demographic_summary <- demograficos_final %>%
  summarise(
    `Patients` = n_distinct(CODPACI),
    `Female patients` = sum(SEXO == "Female", na.rm = TRUE),
    `Male patients` = sum(SEXO == "Male", na.rm = TRUE),
    `Mean age at diagnosis` = round(mean(edad_diagnostico, na.rm = TRUE), 1),
    `Median age at diagnosis` = median(edad_diagnostico, na.rm = TRUE),
    `Minimum age at diagnosis` = min(edad_diagnostico, na.rm = TRUE),
    `Maximum age at diagnosis` = max(edad_diagnostico, na.rm = TRUE)
  )


demographic_summary_long <- demographic_summary %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


tabla_demografica <- demographic_summary_long %>%
  gt() %>%
  tab_header(title = md("**Demographic Characteristics of the Final Cohort**")) %>%
  tab_style(
    style = list(cell_fill(color = "#DCEBFA"), cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F8FC"),
    locations = cells_body(rows = seq(1, nrow(demographic_summary_long), 2))
  ) %>%
  tab_options(
    table.border.top.color = "#5B9BD5",
    table.border.bottom.color = "#5B9BD5",
    heading.border.bottom.color = "#5B9BD5",
    table.font.size = "12px",
    data_row.padding = px(5)
  )

tabla_demografica

age_stats_sex <- demograficos_final %>%
  group_by(SEXO) %>%
  summarise(
    mean_age = mean(edad_diagnostico, na.rm = TRUE),
    median_age = median(edad_diagnostico, na.rm = TRUE),
    min_age = min(edad_diagnostico, na.rm = TRUE),
    max_age = max(edad_diagnostico, na.rm = TRUE),
    .groups = "drop"
  )

plot_age_sex <- ggplot(demograficos_final, aes(x = SEXO, y = edad_diagnostico)) +
  geom_boxplot(fill = "#DCEBFA", color = "#2C7FB8", width = 0.55, outlier.shape = 16) +
  
  # Mean
  geom_point(
    data = age_stats_sex,
    aes(x = SEXO, y = mean_age),
    color = "#D9534F",
    size = 3
  ) +
  
  # Min and max
  geom_point(
    data = age_stats_sex,
    aes(x = SEXO, y = min_age),
    color = "#1A1A1A",
    size = 2
  ) +
  geom_point(
    data = age_stats_sex,
    aes(x = SEXO, y = max_age),
    color = "#1A1A1A",
    size = 2
  ) +
  
  # Labels
  geom_text(
    data = age_stats_sex,
    aes(
      x = c(1.28, 2.28),
      y = mean_age + 2,
      label = paste0("Mean = ", round(mean_age, 1))
    ),
    hjust = 0,
    color = "#D9534F",
    size = 3.5
  ) +
  geom_text(
    data = age_stats_sex,
    aes(
      x = c(1.28, 2.28),
      y = median_age - 2,
      label = paste0("Median = ", round(median_age, 1))
    ),
    hjust = 0,
    color = "#2C7FB8",
    size = 3.5
  ) +
  geom_text(
    data = age_stats_sex,
    aes(x = SEXO, y = min_age, label = paste0("Min: ", min_age)),
    vjust = 1.5,
    size = 3.3
  ) +
  geom_text(
    data = age_stats_sex,
    aes(x = SEXO, y = max_age, label = paste0("Max: ", max_age)),
    vjust = -0.7,
    size = 3.3
  ) +
  
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of patients age at diagnosis by sex",
    x = "Sex",
    y = "Age at diagnosis"
  )

plot_age_sex


#SEX DISTRIBUTION PLOT

sexo_plot_data <- demograficos_final %>%
  count(SEXO) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, "\n(", percentage, "%)")
  )

plot_sexo <- ggplot(sexo_plot_data, aes(x = SEXO, y = n, fill = SEXO)) +
  
  geom_col(width = 0.6, color = "black", alpha = 0.9) +
  
  geom_text(
    aes(label = label),
    color = "black",
    fontface = "bold",
    size = 4.5,
    lineheight = 0.9
  ) +
  
  scale_fill_manual(
    values = c(
      "Male" = "#4A90E2",
      "Female" = "#F48FB1"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Sex distribution in the final cohort",
    y = "Number of patients"
  )

plot_sexo


#AGE AT DIAGNOSIS DISTRIBUTION PLOT

plot_edad <- ggplot(
  demograficos_final,
  aes(x = edad_diagnostico, fill = SEXO)
) +
  geom_histogram(
    bins = 20,
    color = "white",
    alpha = 0.9
  ) +
  facet_wrap(~ SEXO, ncol = 1) +
  scale_fill_manual(
    values = c(
      "Male" = "#4A90E2",
      "Female" = "#F48FB1"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = "Age at diagnosis distribution by sex",
    x = "Age at diagnosis",
    y = "Number of patients"
  )


plot_edad


#AGE GROUP DISTRIBUTION PLOT


edad_grupos <- demograficos_final %>%
  count(grupo_edad, SEXO) %>%
  group_by(grupo_edad) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup()

plot_grupo_edad <- ggplot(
  edad_grupos,
  aes(x = grupo_edad, y = n, fill = SEXO)
) +
  
  geom_col(
    position = "dodge",
    width = 0.7,
    color = "black",
    alpha = 0.9
  ) +
  
  geom_text(
    aes(label = paste0(n, "\n(", percentage, "%)")),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 3.5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "Male" = "#4A90E2",
      "Female" = "#F48FB1"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  ) +
  
  labs(
    title = "Age distribution by sex",
    x = "Age group",
    y = "Number of patients"
  )

plot_grupo_edad


#LDH PREPROCESSING AND SUMMARY

ldh_final <- ldh_final %>%
  mutate(
    VALOR = as.numeric(VALOR),
    FECHA = as.Date(FECHA),
    ldh_alta = VALOR >= 225
  ) %>%
  filter(!is.na(VALOR), !is.na(FECHA))

# Number of LDH measurements per patient
ldh_por_paciente <- ldh_final %>%
  count(CODPACI, name = "n_ldh_measurements")

# Integrated LDH summary
ldh_summary <- tibble(
  `Patients with LDH` = n_distinct(ldh_final$CODPACI),
  `Total LDH measurements` = nrow(ldh_final),
  `Median LDH measurements per patient` = median(ldh_por_paciente$n_ldh_measurements),
  `Mean LDH measurements per patient` = round(mean(ldh_por_paciente$n_ldh_measurements), 1),
  `Minimum LDH measurements per patient` = min(ldh_por_paciente$n_ldh_measurements),
  `Maximum LDH measurements per patient` = max(ldh_por_paciente$n_ldh_measurements),
  `Patients with at least one elevated LDH` = n_distinct(ldh_final$CODPACI[ldh_final$ldh_alta == TRUE]),
  `Percentage with at least one elevated LDH` = round(
    n_distinct(ldh_final$CODPACI[ldh_final$ldh_alta == TRUE]) /
      n_distinct(ldh_final$CODPACI) * 100,
    1
  )
)

ldh_summary_long <- ldh_summary %>%
  pivot_longer(
    cols = everything(),
    names_to = "Indicator",
    values_to = "Value"
  )

tabla_ldh_summary <- ldh_summary_long %>%
  gt() %>%
  tab_header(title = md("**LDH Summary in the Final Cohort**")) %>%
  tab_style(
    style = list(cell_fill(color = "#DCEBFA"), cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F8FC"),
    locations = cells_body(rows = seq(1, nrow(ldh_summary_long), 2))
  ) %>%
  tab_options(
    table.border.top.color = "#5B9BD5",
    table.border.bottom.color = "#5B9BD5",
    heading.border.bottom.color = "#5B9BD5",
    table.font.size = "12px",
    data_row.padding = px(5)
  )

tabla_ldh_summary


# 11. LDH MEASUREMENTS PER PATIENT PLOT


plot_ldh_mediciones <- ggplot(ldh_por_paciente, aes(x = n_ldh_measurements)) +
  geom_histogram(bins = 30, fill = "#2C7FB8", color = "white") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Number of LDH measurements per patient",
    x = "Number of LDH measurements",
    y = "Number of patients"
  )

plot_ldh_mediciones

# Resumen de mediciones LDH por paciente
ldh_measurement_summary <- ldh_por_paciente %>%
  summarise(
    `Patients with LDH` = n(),
    `Median LDH measurements` = median(n_ldh_measurements),
    `Mean LDH measurements` = round(mean(n_ldh_measurements), 1),
    `Minimum LDH measurements` = min(n_ldh_measurements),
    `Maximum LDH measurements` = max(n_ldh_measurements)
  )

ldh_measurement_summary

ldh_por_paciente_grupos <- ldh_por_paciente %>%
  mutate(
    grupo_mediciones = case_when(
      n_ldh_measurements == 1 ~ "1",
      n_ldh_measurements >= 2 & n_ldh_measurements <= 5 ~ "2-5",
      n_ldh_measurements >= 6 & n_ldh_measurements <= 10 ~ "6-10",
      n_ldh_measurements >= 11 & n_ldh_measurements <= 25 ~ "11-25",
      n_ldh_measurements >= 26 & n_ldh_measurements <= 50 ~ "26-50",
      n_ldh_measurements > 50 ~ ">50"
    ),
    grupo_mediciones = factor(
      grupo_mediciones,
      levels = c("1", "2-5", "6-10", "11-25", "26-50", ">50")
    )
  ) %>%
  count(grupo_mediciones) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, "\n(", percentage, "%)")
  )

plot_ldh_mediciones <- ggplot(
  ldh_por_paciente_grupos,
  aes(x = grupo_mediciones, y = n)
) +
  geom_col(
    fill = "#2C7FB8",
    color = "black",
    width = 0.7,
    alpha = 0.9
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  labs(
    title = "Distribution of LDH measurements per patient",
    x = "Number of LDH measurements per patient",
    y = "Number of patients"
  ) +
  expand_limits(y = max(ldh_por_paciente_grupos$n) * 1.15)

plot_ldh_mediciones


# BOXPLOT 1 — LDH MEASUREMENTS PER PATIENT


# Statistics
ldh_stats <- ldh_por_paciente %>%
  summarise(
    mean_ldh = round(mean(n_ldh_measurements), 1),
    median_ldh = median(n_ldh_measurements),
    min_ldh = min(n_ldh_measurements),
    max_ldh = max(n_ldh_measurements)
  )

# Patients with at least one elevated LDH
ldh_patients_high <- ldh_final %>%
  group_by(CODPACI) %>%
  summarise(
    high_ldh = any(VALOR >= 225),
    .groups = "drop"
  ) %>%
  summarise(
    n_patients_high = sum(high_ldh),
    percentage_high = round(sum(high_ldh) / n() * 100, 1)
  )

plot_ldh_box <- ggplot(
  ldh_por_paciente,
  aes(x = "", y = n_ldh_measurements)
) +
  
  geom_boxplot(
    fill = "#DCEBFA",
    color = "#2C7FB8",
    width = 0.3,
    outlier.color = "#D9534F",
    outlier.size = 2
  ) +
  
  # Mean point
  geom_point(
    data = ldh_stats,
    aes(y = mean_ldh),
    color = "#D9534F",
    size = 3
  ) +
  
  # Mean label
  geom_text(
    data = ldh_stats,
    aes(
      y = mean_ldh + 6,
      label = paste0("Mean = ", mean_ldh)
    ),
    color = "#D9534F",
    size = 4
  ) +
  
  # Median label
  geom_text(
    data = ldh_stats,
    aes(
      y = median_ldh - 6,
      label = paste0("Median = ", median_ldh)
    ),
    color = "#2C7FB8",
    size = 4
  ) +
  
  # Min label
  geom_text(
    data = ldh_stats,
    aes(
      y = min_ldh,
      label = paste0("Min = ", min_ldh)
    ),
    vjust = 1.5,
    size = 3.5
  ) +
  
  # Max label
  geom_text(
    data = ldh_stats,
    aes(
      y = max_ldh,
      label = paste0("Max = ", max_ldh)
    ),
    vjust = -0.7,
    size = 3.5
  ) +
  
  # Elevated LDH patients
  annotate(
    "text",
    x = 1.12,
    y = ldh_stats$max_ldh * 0.82,
    label = paste0(
      ldh_patients_high$n_patients_high,
      " patients with at least one \nLDH measurement ≥ 225 U/L (",
      ldh_patients_high$percentage_high,
      "%)"
    ),
    color = "#D9534F",
    hjust = 0,
    size = 4,
    fontface = "bold"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Distribution of LDH measurements per patient",
    y = "Number of LDH measurements"
  )

plot_ldh_box


# BOXPLOT 2 — LDH VALUE DISTRIBUTION

# Statistics of LDH values
ldh_value_stats <- ldh_final %>%
  summarise(
    mean_ldh = round(mean(VALOR, na.rm = TRUE), 1),
    median_ldh = round(median(VALOR, na.rm = TRUE), 1),
    min_ldh = min(VALOR, na.rm = TRUE),
    max_ldh = max(VALOR, na.rm = TRUE)
  )

# Measurements above threshold
ldh_measurements_high <- ldh_final %>%
  summarise(
    n_high_measurements = sum(VALOR >= 225, na.rm = TRUE),
    percentage_high = round(mean(VALOR >= 225, na.rm = TRUE) * 100, 1)
  )

plot_ldh_values <- ggplot(ldh_final, aes(x = "", y = VALOR)) +
  geom_boxplot(
    fill = "#DCEBFA",
    color = "#2C7FB8",
    width = 0.3,
    outlier.color = "#D9534F",
    outlier.size = 1.8
  ) +
  
  geom_point(
    data = ldh_value_stats,
    aes(y = mean_ldh),
    color = "#D9534F",
    size = 3
  ) +
  
  geom_text(
    data = ldh_value_stats,
    aes(x = 1.18, y = mean_ldh + 120, label = paste0("Mean = ", mean_ldh)),
    color = "#D9534F",
    hjust = 0,
    size = 4
  ) +
  
  geom_text(
    data = ldh_value_stats,
    aes(x = 1.18, y = median_ldh - 120, label = paste0("Median = ", median_ldh)),
    color = "#2C7FB8",
    hjust = 0,
    size = 4
  ) +
  
  geom_text(
    data = ldh_value_stats,
    aes(x = 0.92, y = min_ldh, label = paste0("Min = ", min_ldh)),
    hjust = 1,
    vjust = 1.3,
    size = 3.5
  ) +
  
  geom_text(
    data = ldh_value_stats,
    aes(x = 0.92, y = max_ldh, label = paste0("Max = ", max_ldh)),
    hjust = 1,
    vjust = -0.5,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = 1.28,
    y = ldh_value_stats$max_ldh * 0.88,
    label = paste0(
      ldh_measurements_high$n_high_measurements,
      " LDH measurements\n≥ 225 U/L (",
      ldh_measurements_high$percentage_high,
      "%)"
    ),
    color = "#D9534F",
    hjust = 0,
    size = 4,
    fontface = "bold"
  ) +
  
  coord_cartesian(xlim = c(0.65, 1.8), clip = "off") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 80, 10, 10)
  ) +
  
  labs(
    title = "Distribution of LDH values",
    y = "LDH value (U/L)"
  )

plot_ldh_values

# Number of LDH measurements per year
ldh_per_year <- ldh %>%
  filter(!is.na(FECHA)) %>%
  mutate(year = year(FECHA)) %>%
  count(year, name = "n_measurements")

plot_ldh_per_year <- ggplot(ldh_per_year,
                            aes(x = year, y = n_measurements)) +
  
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#2C7FB8",
    size = 2.5
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "LDH measurements per year",
    x = "Year",
    y = "Number of LDH measurements"
  )

plot_ldh_per_year

#PATHOLOGY REPORT DENSITY

anato_por_paciente <- anatomia_final_cohorte %>%
  count(CODPACI, name = "n_pathology_reports")

pathology_summary <- tibble(
  `Patients with pathology reports` = n_distinct(anatomia_final_cohorte$CODPACI),
  `Total pathology-related events` = nrow(anatomia_final_cohorte),
  `Median pathology reports per patient` = median(anato_por_paciente$n_pathology_reports),
  `Maximum pathology reports per patient` = max(anato_por_paciente$n_pathology_reports)
)

pathology_summary_long <- pathology_summary %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Value")

tabla_pathology_summary <- pathology_summary_long %>%
  gt() %>%
  tab_header(title = md("**Pathology Report Summary in the Final Cohort**")) %>%
  tab_style(
    style = list(cell_fill(color = "#DCEBFA"), cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F8FC"),
    locations = cells_body(rows = seq(1, nrow(pathology_summary_long), 2))
  ) %>%
  tab_options(
    table.border.top.color = "#5B9BD5",
    table.border.bottom.color = "#5B9BD5",
    heading.border.bottom.color = "#5B9BD5",
    table.font.size = "12px",
    data_row.padding = px(5)
  )

tabla_pathology_summary

# BOXPLOT — PATHOLOGY REPORTS PER PATIENT

# Number of pathology reports per patient
anato_por_paciente <- anatomia_final_cohorte %>%
  count(CODPACI, name = "n_pathology_reports")

# Statistics
anato_stats <- anato_por_paciente %>%
  summarise(
    mean_reports = round(mean(n_pathology_reports), 1),
    median_reports = median(n_pathology_reports),
    min_reports = min(n_pathology_reports),
    max_reports = max(n_pathology_reports)
  )

plot_anato_reports <- ggplot(
  anato_por_paciente,
  aes(x = "", y = n_pathology_reports)
) +
  
  geom_boxplot(
    fill = "#DCEBFA",
    color = "#2C7FB8",
    width = 0.3,
    outlier.color = "#D9534F",
    outlier.size = 2
  ) +
  
  # Mean point
  geom_point(
    data = anato_stats,
    aes(y = mean_reports),
    color = "#D9534F",
    size = 3
  ) +
  
  # Mean label
  geom_text(
    data = anato_stats,
    aes(
      x = 1.15,
      y = mean_reports + 1.5,
      label = paste0("Mean = ", mean_reports)
    ),
    color = "#D9534F",
    hjust = 0,
    size = 4
  ) +
  
  # Median label
  geom_text(
    data = anato_stats,
    aes(
      x = 1.15,
      y = median_reports - 1.5,
      label = paste0("Median = ", median_reports)
    ),
    color = "#2C7FB8",
    hjust = 0,
    size = 4
  ) +
  
  # Min label
  geom_text(
    data = anato_stats,
    aes(
      x = 0.92,
      y = min_reports,
      label = paste0("Min = ", min_reports)
    ),
    hjust = 1,
    vjust = 1.3,
    size = 3.5
  ) +
  
  # Max label
  geom_text(
    data = anato_stats,
    aes(
      x = 0.92,
      y = max_reports,
      label = paste0("Max = ", max_reports)
    ),
    hjust = 1,
    vjust = -0.5,
    size = 3.5
  ) +
  
  coord_cartesian(xlim = c(0.65, 1.8), clip = "off") +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 80, 10, 10)
  ) +
  
  labs(
    title = "Distribution of pathology reports per patient",
    y = "Number of pathology reports"
  )

plot_anato_reports

#pathology reports per year 
pathology_per_year <- anatomia_final5 %>%
  filter(!is.na(fecha_prueba)) %>%
  mutate(year = year(fecha_prueba)) %>%
  count(year, name = "n_reports")

plot_pathology_year <- ggplot(pathology_per_year,
                              aes(x = year, y = n_reports)) +
  
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#2C7FB8",
    size = 2.5
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Pathology reports per year",
    x = "Year",
    y = "Number of pathology reports"
  )

plot_pathology_year



#LONGITUDINAL EVENT RECONSTRUCTION

# After executing NLP script 
if (exists("df_resultado")) {

  eventos_anatomia <- df_resultado %>%
    filter(CODPACI %in% pacientes_finales) %>%
    transmute(
      CODPACI,
      tipo_evento = "Pathology event",
      fecha_evento = as.Date(fecha_prueba),
      es_especimen,
      tcg = tcg_global,
      n3 = n3_global,
      valor_ldh = NA_real_,
      ldh_alta = NA,
      diagnostico_anatomia = diagnostico,
      diagnostico_micro = t_diagnostico,
      texto_micro = t_micro
    )

  eventos_ldh <- ldh_final %>%
    transmute(
      CODPACI,
      tipo_evento = "LDH event",
      fecha_evento = FECHA,
      es_especimen = NA_character_,
      tcg = NA,
      n3 = NA,
      valor_ldh = VALOR,
      ldh_alta = ldh_alta,
      diagnostico_anatomia = NA_character_,
      diagnostico_micro = NA_character_,
      texto_micro = NA_character_
    )

  eventos_clinicos <- datos_derma_final %>%
    select(CODPACI, FECHA_DIAGNOSTICO, FECHA_EXITUS, FECHA_ULTIMA_REVISION) %>%
    distinct() %>%
    pivot_longer(
      cols = c(FECHA_DIAGNOSTICO, FECHA_EXITUS, FECHA_ULTIMA_REVISION),
      names_to = "tipo_evento",
      values_to = "fecha_evento"
    ) %>%
    filter(!is.na(fecha_evento)) %>%
    mutate(
      tipo_evento = recode(
        tipo_evento,
        FECHA_DIAGNOSTICO = "Diagnosis",
        FECHA_EXITUS = "Exitus",
        FECHA_ULTIMA_REVISION = "Last follow-up"
      ),
      fecha_evento = as.Date(fecha_evento),
      es_especimen = NA_character_,
      tcg = NA,
      n3 = NA,
      valor_ldh = NA_real_,
      ldh_alta = NA,
      diagnostico_anatomia = NA_character_,
      diagnostico_micro = NA_character_,
      texto_micro = NA_character_
    )

  timeline_longitudinal <- bind_rows(
    eventos_clinicos,
    eventos_anatomia,
    eventos_ldh
  ) %>%
    arrange(CODPACI, fecha_evento)

  datos_paciente <- demograficos_final %>%
    select(CODPACI, FECHA_DIAGNOSTICO, edad_diagnostico) %>%
    distinct()

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
    select(-edad_diagnostico)

  event_summary <- eventos_con_fechas %>%
    count(tipo_evento, name = "Events") %>%
    arrange(desc(Events))

  tabla_event_summary <- event_summary %>%
    gt() %>%
    tab_header(title = md("**Longitudinal Event Types in the Final Cohort**")) %>%
    tab_style(
      style = list(cell_fill(color = "#DCEBFA"), cell_text(weight = "bold")),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_fill(color = "#F4F8FC"),
      locations = cells_body(rows = seq(1, nrow(event_summary), 2))
    ) %>%
    tab_options(
      table.border.top.color = "#5B9BD5",
      table.border.bottom.color = "#5B9BD5",
      heading.border.bottom.color = "#5B9BD5",
      table.font.size = "12px",
      data_row.padding = px(5)
    )

  tabla_event_summary

  eventos_por_paciente <- eventos_con_fechas %>%
    count(CODPACI, name = "n_events")

  plot_eventos_paciente <- ggplot(eventos_por_paciente, aes(x = n_events)) +
    geom_histogram(bins = 30, fill = "#2C7FB8", color = "white") +
    theme_minimal(base_size = 12) +
    labs(
      title = "Number of longitudinal events per patient",
      x = "Number of events",
      y = "Number of patients"
    )

  plot_eventos_paciente

} else {
  message("df_resultado was not found. Longitudinal event reconstruction requires the NLP/pathology extraction dataset.")
}

# FOLLOW-UP AND TEMPORAL EXPLORATORY ANALYSIS

followup_final <- datos_derma_final %>%
  select(
    CODPACI,
    FECHA_DIAGNOSTICO,
    FECHA_ULTIMA_REVISION,
    FECHA_EXITUS,
    ESTADO
  ) %>%
  distinct() %>%
  left_join(
    demograficos_final %>% select(CODPACI, SEXO),
    by = "CODPACI"
  ) %>%
  mutate(
    FECHA_DIAGNOSTICO = as.Date(FECHA_DIAGNOSTICO),
    FECHA_ULTIMA_REVISION = as.Date(FECHA_ULTIMA_REVISION),
    FECHA_EXITUS = as.Date(FECHA_EXITUS),
    
    # If no exitus, last revision is used as censoring/follow-up date
    fecha_fin_seguimiento = if_else(
      !is.na(FECHA_EXITUS),
      FECHA_EXITUS,
      FECHA_ULTIMA_REVISION
    ),
    
    evento_exitus = !is.na(FECHA_EXITUS),
    
    tiempo_diagnostico_fin_anios =
      as.numeric(fecha_fin_seguimiento - FECHA_DIAGNOSTICO) / 365.25,
    
    tiempo_revision_exitus_anios =
      as.numeric(FECHA_EXITUS - FECHA_ULTIMA_REVISION) / 365.25
  ) %>%
  filter(
    !is.na(FECHA_DIAGNOSTICO),
    !is.na(fecha_fin_seguimiento),
    tiempo_diagnostico_fin_anios >= 0
  )

# Exitus por sexo 
exitus_by_sex <- followup_final %>%
  group_by(SEXO) %>%
  summarise(
    total_patients = n_distinct(CODPACI),
    exitus_patients = sum(evento_exitus, na.rm = TRUE),
    percentage_exitus = round(exitus_patients / total_patients * 100, 1),
    .groups = "drop"
  )

exitus_by_sex

plot_exitus_sex <- ggplot(exitus_by_sex, aes(x = SEXO, y = exitus_patients, fill = SEXO)) +
  geom_col(width = 0.6, color = "black", alpha = 0.9) +
  geom_text(
    aes(label = paste0(exitus_patients, "\n(", percentage_exitus, "%)")),
    color = "black",
    fontface = "bold",
    size = 4.5
  ) +
  scale_fill_manual(
    values = c(
      "Male" = "#4A90E2",
      "Female" = "#F48FB1"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Exitus distribution by sex",
    y = "Number of deceased patients"
  )

plot_exitus_sex


#FOLLOW-UP TIME SINCE DIAGNOSIS 
followup_summary <- followup_final %>%
  summarise(
    patients = n_distinct(CODPACI),
    exitus_patients = sum(evento_exitus),
    censored_patients = sum(!evento_exitus),
    mean_followup_years = round(mean(tiempo_diagnostico_fin_anios, na.rm = TRUE), 1),
    median_followup_years = round(median(tiempo_diagnostico_fin_anios, na.rm = TRUE), 1),
    min_followup_years = round(min(tiempo_diagnostico_fin_anios, na.rm = TRUE), 1),
    max_followup_years = round(max(tiempo_diagnostico_fin_anios, na.rm = TRUE), 1)
  )

followup_summary

plot_followup_box <- ggplot(followup_final, aes(x = "", y = tiempo_diagnostico_fin_anios)) +
  geom_boxplot(
    fill = "#DCEBFA",
    color = "#2C7FB8",
    width = 0.3,
    outlier.color = "#D9534F",
    outlier.size = 2
  ) +
  geom_point(
    aes(y = mean(tiempo_diagnostico_fin_anios, na.rm = TRUE)),
    color = "#D9534F",
    size = 3
  ) +
  annotate(
    "text",
    x = 1.18,
    y = followup_summary$mean_followup_years,
    label = paste0("Mean = ", followup_summary$mean_followup_years, " years"),
    hjust = 0,
    color = "#D9534F",
    size = 4
  ) +
  annotate(
    "text",
    x = 1.18,
    y = followup_summary$median_followup_years,
    label = paste0("Median = ", followup_summary$median_followup_years, " years"),
    hjust = 0,
    color = "#2C7FB8",
    size = 4
  ) +
  coord_cartesian(xlim = c(0.65, 1.7), clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 80, 10, 10)
  ) +
  labs(
    title = "Follow-up duration from diagnosis",
    y = "Years from diagnosis to last clinical record"
  )

plot_followup_box

#TIME BETWEEN LAST REVISION AND EXITUS 
revision_exitus_summary <- followup_final %>%
  filter(evento_exitus, !is.na(tiempo_revision_exitus_anios)) %>%
  summarise(
    deceased_patients = n_distinct(CODPACI),
    mean_revision_to_exitus_years = round(mean(tiempo_revision_exitus_anios, na.rm = TRUE), 2),
    median_revision_to_exitus_years = round(median(tiempo_revision_exitus_anios, na.rm = TRUE), 2),
    min_revision_to_exitus_years = round(min(tiempo_revision_exitus_anios, na.rm = TRUE), 2),
    max_revision_to_exitus_years = round(max(tiempo_revision_exitus_anios, na.rm = TRUE), 2)
  )

revision_exitus_summary

mean_revision_exitus <- followup_final %>%
  filter(
    evento_exitus,
    !is.na(tiempo_revision_exitus_anios),
    tiempo_revision_exitus_anios >= 0
  ) %>%
  summarise(
    mean_value = mean(tiempo_revision_exitus_anios)
  ) %>%
  pull(mean_value)

plot_revision_exitus <- followup_final %>%
  filter(
    evento_exitus,
    !is.na(tiempo_revision_exitus_anios),
    tiempo_revision_exitus_anios >= 0
  ) %>%
  
  ggplot(aes(x = tiempo_revision_exitus_anios)) +
  
  geom_histogram(
    bins = 15,
    fill = "#5B8FD1",
    color = "black",
    alpha = 0.8
  ) +
  
  geom_vline(
    xintercept = mean_revision_exitus,
    color = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  annotate(
    "text",
    x = mean_revision_exitus,
    y = Inf,
    label = paste0("Mean = ", round(mean_revision_exitus, 2), " years"),
    color = "red",
    fontface = "bold",
    vjust = 2,
    hjust = -0.1,
    size = 5
  ) +
  
  labs(
    title = "Time between last clinical revision and exitus",
    x = "Years between last revision and death",
    y = "Number of patients"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

plot_revision_exitus

#NEW MF DIAGNOSIS PER YEAR
diagnoses_per_year <- followup_final %>%
  mutate(year_diagnosis = year(FECHA_DIAGNOSTICO)) %>%
  count(year_diagnosis, name = "new_diagnoses") %>%
  filter(!is.na(year_diagnosis))

plot_diagnoses_year <- ggplot(diagnoses_per_year, 
                              aes(x = year_diagnosis, y = new_diagnoses)) +
  
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#2C7FB8",
    size = 2.5
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "New mycosis fungoides diagnoses per year",
    x = "Year of diagnosis",
    y = "Number of new diagnoses"
  )

plot_diagnoses_year

#NUMBER OF PATIENTS WITH FOLLOW-UP PER YEAR 
year_range <- seq(
  min(year(followup_final$FECHA_DIAGNOSTICO), na.rm = TRUE),
  max(year(followup_final$fecha_fin_seguimiento), na.rm = TRUE)
)

patients_followup_year <- expand_grid(
  CODPACI = followup_final$CODPACI,
  year = year_range
) %>%
  left_join(
    followup_final %>%
      select(CODPACI, FECHA_DIAGNOSTICO, fecha_fin_seguimiento),
    by = "CODPACI"
  ) %>%
  filter(
    year >= year(FECHA_DIAGNOSTICO),
    year <= year(fecha_fin_seguimiento)
  ) %>%
  count(year, name = "patients_in_followup")

plot_followup_year <- ggplot(patients_followup_year, aes(x = year, y = patients_in_followup)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#2C7FB8", size = 2) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Patients under follow-up per year",
    x = "Year",
    y = "Number of patients in follow-up"
  )

plot_followup_year

# TEMPORAL COHORT EVOLUTION
# Diagnoses + patients under follow-up

# New diagnoses per year
diagnoses_per_year <- followup_final %>%
  mutate(year = year(FECHA_DIAGNOSTICO)) %>%
  count(year, name = "new_diagnoses") %>%
  filter(!is.na(year))

# Patients under follow-up per year
year_range <- seq(
  min(year(followup_final$FECHA_DIAGNOSTICO), na.rm = TRUE),
  max(year(followup_final$fecha_fin_seguimiento), na.rm = TRUE)
)

patients_followup_year <- expand_grid(
  CODPACI = followup_final$CODPACI,
  year = year_range
) %>%
  left_join(
    followup_final %>%
      select(CODPACI, FECHA_DIAGNOSTICO, fecha_fin_seguimiento),
    by = "CODPACI"
  ) %>%
  filter(
    year >= year(FECHA_DIAGNOSTICO),
    year <= year(fecha_fin_seguimiento)
  ) %>%
  count(year, name = "patients_in_followup")

# Merge both datasets
temporal_evolution <- diagnoses_per_year %>%
  full_join(patients_followup_year, by = "year")

# Plot
plot_temporal_evolution <- ggplot(temporal_evolution, aes(x = year)) +
  
  # Patients under follow-up
  geom_line(
    aes(y = patients_in_followup, color = "Patients in follow-up"),
    linewidth = 1.3
  ) +
  
  geom_point(
    aes(y = patients_in_followup, color = "Patients in follow-up"),
    size = 2
  ) +
  
  # New diagnoses
  geom_line(
    aes(y = new_diagnoses * 8, color = "New diagnoses"),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = new_diagnoses * 8, color = "New diagnoses"),
    size = 2
  ) +
  
  scale_y_continuous(
    name = "Patients in follow-up",
    
    sec.axis = sec_axis(
      ~ . / 8,
      name = "New diagnoses"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Patients in follow-up" = "#2C7FB8",
      "New diagnoses" = "#D95F02"
    )
  ) +
  
  labs(
    title = "Temporal evolution of the cohort",
    x = "Year",
    color = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

plot_temporal_evolution