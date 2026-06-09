library(dplyr)
library(survival)
library(survminer)

#FECHA DE EXITUS FINAL

exitus_km <- exitus_final %>%
  mutate(
    FECHA_EXITUS = as.Date(FECHA_EXITUS),
    FECHA_EXITUS_PABLO = as.Date(FECHA_EXITUS_PABLO),
    FECHA_ULTIMA_REVISION = as.Date(FECHA_ULTIMA_REVISION),
    
    fecha_exitus_final = case_when(
      !is.na(FECHA_EXITUS) ~ FECHA_EXITUS,
      is.na(FECHA_EXITUS) & !is.na(FECHA_EXITUS_PABLO) ~ FECHA_EXITUS_PABLO,
      TRUE ~ as.Date(NA)
    ),
    
    evento_muerte = if_else(!is.na(fecha_exitus_final), 1, 0)
  ) %>%
  select(
    CODPACI,
    fecha_exitus_final,
    FECHA_ULTIMA_REVISION,
    evento_muerte
  )


#CLIPI AÑO RELATIVO desde primer evento clipi

clipi_relativo <- clipi %>%
  mutate(
    fecha_evento = as.Date(fecha_evento)
  ) %>%
  filter(
    !is.na(CLIPI),
    !is.na(fecha_evento)
  ) %>%
  arrange(CODPACI, fecha_evento) %>%
  group_by(CODPACI) %>%
  mutate(
    fecha_primer_clipi = first(fecha_evento),
    anio_relativo = floor(
      as.numeric(fecha_evento - fecha_primer_clipi) / 365.25
    )
  ) %>%
  ungroup() %>%
  filter(
    !is.na(anio_relativo),
    anio_relativo >= 0
  )

View(clipi_relativo)


#KM CON ULTIMO CLIPI POR PACIENTE DE CADA AÑO RELATIVO

clipi_ultimo_anual <- clipi_relativo %>%
  arrange(CODPACI, anio_relativo, fecha_evento) %>%
  group_by(CODPACI, anio_relativo) %>%
  slice_tail(n = 1) %>%
  ungroup()

km_anual <- clipi_ultimo_anual %>%
  left_join(exitus_km, by = "CODPACI") %>%
  mutate(
    fecha_fin_seguimiento = if_else(
      evento_muerte == 1,
      fecha_exitus_final,
      FECHA_ULTIMA_REVISION
    ),
    fecha_fin_seguimiento = as.Date(fecha_fin_seguimiento),
    
    tiempo_seguimiento_dias = as.numeric(fecha_fin_seguimiento - fecha_evento),
    
    grupo_CLIPI = case_when(
      CLIPI == 0 ~ "CLIPI 0",
      CLIPI == 1 ~ "CLIPI 1",
      CLIPI == 2 ~ "CLIPI 2",
      CLIPI >= 3 ~ "CLIPI 3-4",
      TRUE ~ NA_character_
    ),
    
    grupo_CLIPI = factor(
      grupo_CLIPI,
      levels = c("CLIPI 0", "CLIPI 1", "CLIPI 2", "CLIPI 3-4")
    )
  ) %>%
  filter(
    !is.na(tiempo_seguimiento_dias),
    tiempo_seguimiento_dias >= 0,
    !is.na(evento_muerte),
    !is.na(grupo_CLIPI)
  )

km_fit_anual <- survfit(
  Surv(tiempo_seguimiento_dias, evento_muerte) ~ grupo_CLIPI,
  data = km_anual
)
summary(km_fit_anual)


km_plot <- ggsurvplot(
  km_fit_anual,
  data = km_anual,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  xscale = "d_y",
  break.time.by = 365.25,
  xlab = "Follow-up time (years)",
  ylab = "Overall survival probability",
  title = NULL,
  legend.title = "Annual CLIPi",
  font.x = c(18),
  font.y = c(18),
  font.tickslab = c(16),
  font.legend = c(16),
  font.main = c(18),
  risk.table.height = 0.28,
  risk.table.fontsize = 5,
  tables.theme = theme_cleantable()
)

km_plot$plot <- km_plot$plot +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

km_plot$plot <- km_plot$plot +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(km_plot)

# Guardar figura
ggsave(
  filename = "kaplan_meier_annual_clipi.png",
  plot = km_plot$plot,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "kaplan_meier_annual_clipi_complete.png",
  plot = print(km_plot),
  width = 12,
  height = 9,
  dpi = 300
)


#OVERALL SURVIVAL A 5 AÑOS DESDE KM anual

summary_5y_anual <- summary(
  km_fit_anual,
  times = 365.25 * 5
)

os_5y_anual <- data.frame(
  grupo_CLIPI = summary_5y_anual$strata,
  OS_5_years = round(summary_5y_anual$surv * 100, 1),
  lower_CI = round(summary_5y_anual$lower * 100, 1),
  upper_CI = round(summary_5y_anual$upper * 100, 1),
  n_risk = summary_5y_anual$n.risk,
  n_events = summary_5y_anual$n.event
)

View(os_5y_anual)

write.csv(
  os_5y_anual,
  "os_5y_anual_clipi.csv",
  row.names = FALSE
)

km_plot_5y <- ggsurvplot(
  km_fit_anual,
  data = km_anual,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  xscale = "d_y",
  break.time.by = 365.25,
  xlim = c(0, 365.25 * 5),
  xlab = "Follow-up time (years)",
  ylab = "Overall survival probability",
  title = "Five-year Kaplan-Meier survival curves according to annual CLIPi",
  legend.title = "Annual CLIPi"
)

print(km_plot_5y)

ggsave(
  filename = "kaplan_meier_annual_clipi_5y.png",
  plot = print(km_plot_5y),
  width = 10,
  height = 9,
  dpi = 300
)


#COX LONGITUDINAL CORREGIDO CON INTERVALOS TEMPORALES

cox_longitudinal <- clipi_relativo %>%
  left_join(exitus_km, by = "CODPACI") %>%
  mutate(
    fecha_evento = as.Date(fecha_evento),
    fecha_fin_seguimiento = if_else(
      evento_muerte == 1,
      fecha_exitus_final,
      FECHA_ULTIMA_REVISION
    ),
    fecha_fin_seguimiento = as.Date(fecha_fin_seguimiento)
  ) %>%
  filter(
    !is.na(fecha_evento),
    !is.na(fecha_fin_seguimiento),
    fecha_evento <= fecha_fin_seguimiento,
    !is.na(evento_muerte),
    !is.na(CLIPI)
  ) %>%
  arrange(CODPACI, fecha_evento) %>%
  group_by(CODPACI) %>%
  mutate(
    fecha_inicio_paciente = first(fecha_evento),
    fecha_siguiente_evento = lead(fecha_evento),
    
    fecha_fin_intervalo = case_when(
      !is.na(fecha_siguiente_evento) &
        fecha_siguiente_evento <= fecha_fin_seguimiento ~ fecha_siguiente_evento,
      TRUE ~ fecha_fin_seguimiento
    ),
    
    tstart = as.numeric(fecha_evento - fecha_inicio_paciente),
    tstop = as.numeric(fecha_fin_intervalo - fecha_inicio_paciente),
    
    evento_intervalo = if_else(
      evento_muerte == 1 &
        fecha_fin_intervalo == fecha_fin_seguimiento,
      1,
      0
    )
  ) %>%
  ungroup() %>%
  filter(
    !is.na(tstart),
    !is.na(tstop),
    tstop > tstart
  )

# Comprobar número real de eventos incluidos en el Cox

sum(cox_longitudinal$evento_intervalo)

cox_longitudinal %>%
  group_by(CODPACI) %>%
  summarise(
    muerte = max(evento_intervalo),
    .groups = "drop"
  ) %>%
  summarise(
    pacientes = n(),
    pacientes_fallecidos = sum(muerte)
  )

# Cox principal longitudinal: CLIPI continuo

cox_clipi_longitudinal <- coxph(
  Surv(tstart, tstop, evento_intervalo) ~ CLIPI,
  data = cox_longitudinal
)

summary(cox_clipi_longitudinal)

# Tabla HR CLIPI longitudinal

cox_summary <- summary(cox_clipi_longitudinal)

cox_results <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  HR = round(cox_summary$coefficients[, "exp(coef)"], 2),
  lower_CI = round(cox_summary$conf.int[, "lower .95"], 2),
  upper_CI = round(cox_summary$conf.int[, "upper .95"], 2),
  p_value = signif(cox_summary$coefficients[, "Pr(>|z|)"], 3)
)

View(cox_results)

# Cox exploratorio longitudinal con componentes individuales

cox_componentes_longitudinal <- coxph(
  Surv(tstart, tstop, evento_intervalo) ~
    edad_flag +
    ldh_flag +
    tcg_flag +
    n3_flag,
  data = cox_longitudinal
)

summary(cox_componentes_longitudinal)

# Forest plot

forest_plot <- ggforest(
  cox_componentes_longitudinal,
  data = cox_longitudinal,
  fontsize = 2
)

print(forest_plot)

ggsave(
  filename = "cox_componentes_forest_plot.png",
  plot = forest_plot,
  width = 12,
  height = 9,
  dpi = 300
)
