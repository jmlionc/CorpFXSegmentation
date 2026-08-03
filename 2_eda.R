# Librerías Necesarias ----
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)
library(MASS)
library(stringr)
library(e1071) # Cálculo de Momentos Estadísticos
library(nortest) # Pruebas de Normalidad
library(FSA) # Análisis Post-Hoc
library(psych)
library(ggcorrplot)

# Carga de Datos ----

base_transaccional <- readRDS("base_transaccional.rds")
base_clientes <- readRDS("base_clientes.rds")

# Análisis Descriptivo Exploratorio ----

## 5.2.1. Análisis de Microestructura y Dinámica Transaccional ----

### Caracterización Univariada, Colas Pesadas e Inflación de Ceros ----

# 0. Gráficas de la Dsitribución

# Colores ----

"#D97724"
"#5F7A8C"
"#1F3B4D"
"#D9D9D9"
"#7f8c8d"
"gray40"
"gray30"
"gray20"

# Gráfica de Ingresos Totales (Log) ----
# 1. Preparación de datos (Transformación Log)
df_plot <- base_transaccional %>%
  mutate(log_revenue = log(USD_Revenue+1))
# 2. Gráfica optimizada
plot_hist_density_total <- ggplot(df_plot, aes(x = log_revenue)) +
  # Histograma
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, 
                 fill = "#D9D9D9", 
                 color = "white", 
                 alpha = 0.8) +
  # Curva de densidad suavizada
  geom_density(fill = "#5F7A8C", 
               color = "#1F3B4D", 
               alpha = 0.35, 
               linewidth = 1.2) +
  # Etiquetas y temas
  labs(
    title = "Distribución del Ingreso",
    subtitle = "Representación de la asimetría y curtosis en el revenue transaccional",
    x = "Log(USD Revenue + 1)", 
    y = "Densidad"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )


# Gráfica de Ingresos > 0 (Log)  ----

# 1. Preparación de datos (Transformación Log)
df_plot <- base_transaccional %>%
  filter(USD_Revenue > 0) %>% # Filtramos para que el log tenga sentido
  mutate(log_revenue = log(USD_Revenue))

# 2. Gráfica optimizada
plot_hist_density <- ggplot(df_plot, aes(x = log_revenue)) +
  # Histograma
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, 
                 fill = "#D9D9D9", 
                 color = "white", 
                 alpha = 0.8) +
  # Curva de densidad suavizada
  geom_density(fill = "#5F7A8C", 
               color = "#1F3B4D", 
               alpha = 0.35, 
               linewidth = 1.2) +
  # Etiquetas y temas
  labs(
    title = "Distribución del Ingreso > 0",
    subtitle = "Representación de la asimetría y curtosis en el revenue transaccional",
    x = "Log(USD Revenue)", 
    y = "Densidad"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )




# Gráfica de Ingresos Sin Transformar  ----

df_plot <- base_transaccional

# 2. Gráfica optimizada
plot_hist_density_notrans <- ggplot(df_plot, aes(x = USD_Revenue)) +
  # Histograma
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, 
                 fill = "#D9D9D9", 
                 color = "white", 
                 alpha = 0.8) +
  # Curva de densidad suavizada
  geom_density(fill = "#5F7A8C", 
               color = "#1F3B4D", 
               alpha = 0.35, 
               linewidth = 1.2) +
  # Etiquetas y temas
  labs(
    title = "Distribución del Ingreso",
    subtitle = "Representación de la asimetría y curtosis en el revenue transaccional",
    x = "USD Revenue", 
    y = "Densidad"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

plot_hist_density_notrans
plot_hist_density_total
plot_hist_density


# 3. Guardar en PDF (estándar académico)
ggsave("dist_log_revenue_total.pdf", plot = plot_hist_density_total, 
       width = 7, height = 4.5, device = cairo_pdf)
ggsave("dist_log_revenue.pdf", plot = plot_hist_density, 
       width = 7, height = 4.5, device = cairo_pdf)
ggsave("dist_log_revenue_notrans.pdf", plot = plot_hist_density_notrans, 
       width = 7, height = 4.5, device = cairo_pdf)

# Gráfica de Volumenes (Log) ----
# 1. Preparación de datos (Transformación Log)
df_plot <- base_transaccional %>%
  mutate(log_vol = log(USD_Volume))
# 2. Gráfica optimizada
plot_hist_density_vol <- ggplot(df_plot, aes(x = log_vol)) +
  # Histograma
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, 
                 fill = "#D9D9D9", 
                 color = "white", 
                 alpha = 0.8) +
  # Curva de densidad suavizada
  geom_density(fill = "#5F7A8C", 
               color = "#1F3B4D", 
               alpha = 0.35, 
               linewidth = 1.2) +
  # Etiquetas y temas
  labs(
    title = "Distribución del volumen",
    subtitle = "Representación de la asimetría y curtosis en el volumen transaccional",
    x = "Log(USD Volumen)", 
    y = "Densidad"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

plot_hist_density_vol
ggsave("dist_log_vol.pdf", plot = plot_hist_density_vol, 
       width = 7, height = 4.5, device = cairo_pdf)


# 1. Asimetría (Fisher) y Curtosis de Exceso
# Calculamos los estadísticos para documentar el apartamiento de la normalidad
caracterizacion_univariada <- base_transaccional %>%
  summarise(
    across(
      c(USD_Volume, USD_Revenue, Spread_BPS),
      list(
        media = ~mean(.x, na.rm = TRUE),
        asimetria = ~e1071::skewness(.x, na.rm = TRUE),
        curtosis_ex = ~e1071::kurtosis(.x, na.rm = TRUE) # e1071 calcula curtosis de exceso por defecto
      )
    )
  )

# Pivoteamos la tabla para facilitar su lectura y posterior exportación a LaTeX
tabla_momentos <- caracterizacion_univariada %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(media|asimetria|curtosis_ex)")

cat("--- Momentos Estadísticos (Asimetría y Curtosis) ---\n")
print(tabla_momentos)

# 2. Pruebas de Bondad de Ajuste para el Volumen (Hipótesis Lognormal)
# Filtramos V > 0 estricamente para evitar indeterminaciones matemáticas con el logaritmo
log_volumen <- log(base_transaccional$USD_Volume[base_transaccional$USD_Volume > 0])

log_revenue <- log(base_transaccional$USD_Revenue[base_transaccional$USD_Revenue > 0])

cat("\n--- Prueba de bondad de ajuste: Kolmogorov-Smirnov (Lilliefors) ---\n")
test_lilliefors <- nortest::lillie.test(log_volumen)
print(test_lilliefors)

cat("\n--- Prueba de bondad de ajuste: Anderson-Darling ---\n")
# Anderson-Darling es más sensible a las colas de la distribución
test_ad <- nortest::ad.test(log_volumen)
print(test_ad)

# 3. Inflación de Ceros (Hurdle Structure)
# Cuantificación de la probabilidad exacta en el origen P(S = 0)
prob_zero_spread <- mean(base_transaccional$Spread_BPS == 0, na.rm = TRUE)
cat("\nProbabilidad empírica de Spread = 0 (P(S=0)): ", 
    scales::percent(prob_zero_spread, accuracy = 0.01), "\n")

# Creamos una variable indicadora binaria para la contingencia
base_transaccional <- base_transaccional %>%
  mutate(is_zero_spread = ifelse(Spread_BPS == 0, "Zero", "Non-Zero"))

# Dependencia Condicional: Pruebas Chi-Cuadrado de Pearson
cat("\n--- Prueba Chi-cuadrado: Zero-Spread vs Canal de Ejecución ---\n")
tabla_canal <- table(base_transaccional$is_zero_spread, base_transaccional$Channel)
print(tabla_canal)
test_chi_canal <- chisq.test(tabla_canal)
print(test_chi_canal)

cat("\n--- Prueba Chi-cuadrado: Zero-Spread vs Producto ---\n")
tabla_producto <- table(base_transaccional$is_zero_spread, base_transaccional$Product)
print(tabla_producto)
test_chi_producto <- chisq.test(tabla_producto)
print(test_chi_producto)

### Microeconomía del Spread: Economías de Escala y Efecto Canal-Producto ----

# 1. Prueba de Mann-Whitney U (Wilcoxon Rank-Sum)
# Comparamos la distribución del Spread entre los canales Digital y Manual
# Usamos un test no paramétrico debido al fuerte apartamiento de la normalidad
cat("--- Prueba de Mann-Whitney U: Spread_BPS vs Channel ---\n")
test_mw_canal <- wilcox.test(Spread_BPS ~ Channel, data = base_transaccional, exact = FALSE)
print(test_mw_canal)

# 2. Visualización y Modelación de Economías de Escala (Gráfico Científico)
# Filtramos spreads y volúmenes mayores a cero para la transformación logarítmica
df_economias <- base_transaccional %>%
  filter(USD_Volume > 0, Spread_BPS > 0)

plot_economias_escala_alto_contraste <- ggplot(df_economias, aes(x = log(USD_Volume), y = log(Spread_BPS))) +
  # Puntos ultra-pequeños (size = 0.6) y más transparentes (alpha = 0.08) para romper el solapamiento
  geom_point(aes(color = Channel), alpha = 0.08, size = 0.6) +
  # Línea LOESS en gris oscuro para mantener la neutralidad metodológica
  geom_smooth(method = "loess", color = "#2F3E46", linewidth = 1.4, se = TRUE, fill = "#D9D9D9") +
  # Paleta de Alto Contraste: Azul Marino Institucional vs Bronce Corporativo
  scale_color_manual(values = c("Digital" = "#D97724", "Manual" = "#1F3B4D")) +
  labs(
    title = "Elasticidad Precio-Volumen y Economías de Escala",
    subtitle = "Análisis no paramétrico del spread cobrado frente al tamaño de la transacción",
    x = "Log(USD Volume)",
    y = "Log(Spread BPS)",
    color = "Canal de Ejecución"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif"),
    legend.position = "bottom",
    legend.title = element_text(family = "serif")
  ) +
  # Forzamos que los puntos de la leyenda sean grandes y 100% opacos para una lectura perfecta
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

# Guardar el PDF con el nuevo contraste
ggsave("plot_economias_escala.pdf", plot = plot_economias_escala_alto_contraste, 
       width = 7, height = 5, device = cairo_pdf)

### Análisis Temporal de Alta Frecuencia ----

# 1. Preparación y extracción de dimensiones temporales desde 'Deal_Date'
base_transaccional <- base_transaccional %>%
  mutate(
    # 1. Aseguramos el parseo correcto de la fecha y hora
    Deal_Date_Parsed = parse_date_time(Deal_Date, orders = c("ymd HMS", "ymd", "dmy HMS", "dmy")),
    
    # 2. CALCULO CRUCIAL: Hora decimal continua (Ej: 08:30:00 -> 8.5)
    Hour_Continuous = hour(Deal_Date_Parsed) + (minute(Deal_Date_Parsed) / 60) + (second(Deal_Date_Parsed) / 3600),
    
    # Mantenemos las variables discretas para los resúmenes tabulares
    Hour = hour(Deal_Date_Parsed),
    Day_Of_Week = wday(Deal_Date_Parsed, label = TRUE, abbr = FALSE, locale = "es_CO.UTF-8"),
    Day_Of_Month = day(Deal_Date_Parsed),
    
    # Estructura de Terciles del Mes
    Month_Tercile = case_when(
      Day_Of_Month <= 10 ~ "Early",
      Day_Of_Month <= 20 ~ "Mid",
      TRUE               ~ "Late"
    ),
    Month_Tercile = factor(Month_Tercile, levels = c("Early", "Mid", "Late"))
  )

# --- Gráfico KDE Intradía Continuo y Suavizado ---
plot_intradia_kde_suave <- ggplot(base_transaccional, aes(x = Hour_Continuous)) +
  # Usamos 'adjust' para suavizar y eliminar los picos discretos artificiales
  geom_density(fill = "#5F7A8C", color = "#1F3B4D", alpha = 0.4, linewidth = 1.2, adjust = 1.2) +
  # Fijamos los quiebres del eje X de 7 AM a 4 PM (16 h)
  scale_x_continuous(breaks = seq(7, 16, by = 1), limits = c(6.5, 16.5)) + 
  labs(
    title = "Perfil Temporal de la Actividad Intradía",
    subtitle = "Densidad de kernel continua ajustada a la microestructura cambiaria colombiana (7:00 - 16:00)",
    x = "Hora del Día (Formato 24h)",
    y = "Densidad Estimada de Operaciones"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

# Guardamos el PDF definitivo
ggsave("plot_intradia_kde.pdf", plot = plot_intradia_kde_suave, width = 10, height = 4, device = cairo_pdf)

# --- 3. Dimensión Intra-Semana e Intra-Mes: Análisis No Paramétrico ---

plot(base_transaccional$Day_Of_Week)
prop.table(table(base_transaccional$Day_Of_Week))

cat("--- Contraste de Kruskal-Wallis: Volumen vs Día de la Semana ---\n")
kw_semana <- kruskal.test(USD_Volume ~ Day_Of_Week, data = base_transaccional)
print(kw_semana)

plot(base_transaccional$Month_Tercile)
prop.table(table(base_transaccional$Month_Tercile))

cat("\n--- Contraste de Kruskal-Wallis: Volumen vs Tercil del Mes ---\n")
kw_mes <- kruskal.test(USD_Volume ~ Month_Tercile, data = base_transaccional)
print(kw_mes)

# Análisis Post-Hoc de Dunn
if (kw_mes$p.value < 0.05) {
  cat("\n--- Análisis Post-Hoc de Dunn (Ajuste Holm-Bonferroni) para Terciles del Mes ---\n")
  post_hoc_mes <- FSA::dunnTest(Spread_BPS ~ Month_Tercile, data = base_transaccional, method = "holm")
  print(post_hoc_mes)
}

# --- 4. Visualización de Asimetrías Intra-Mes (Boxplot Académico) ---
plot_intrames_boxplot <- ggplot(base_transaccional, aes(x = Month_Tercile, y = log(USD_Volume))) +
  geom_boxplot(aes(fill = Month_Tercile), color = "#1F3B4D", alpha = 0.7, outlier.alpha = 0.05, outlier.size = 0.5) +
  scale_fill_manual(values = c("Early" = "#D9D9D9", "Mid" = "#5F7A8C", "Late" = "#D97724")) +
  labs(
    title = "Distribución Condicional del Volumen por Tercil Mensual",
    subtitle = "Identificación de patrones estacionales e incrementos de volatilidad intra-mes",
    x = "Período del Mes (Terciles)",
    y = "Log(Volumen)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif"),
    legend.position = "none"
  )

ggsave("plot_intrames_boxplot.pdf", plot = plot_intrames_boxplot, width = 7, height = 4.5, device = cairo_pdf)


### Modelos Predictivos de Ingresos (Modelo Lineal OLS Baseline) ----
  
  # 1. Especificación del Modelo Lineal Ordinario (OLS)
  # Evaluamos el logaritmo del ingreso en función del volumen, canal y su interacción
  # Nota: Si usas log(USD_Revenue), recuerda que excluimos los ceros estrictos o usamos log(USD_Revenue + 1)
  # Aquí usaremos la aproximación estándar condicional a ingresos positivos para consistencia con LaTeX
  
  df_modelos <- base_transaccional %>%
  filter(USD_Revenue > 0) # Enfoque en la parte continua (Lognormal condicional)

cat("--- Estimación del Modelo Lineal OLS Baseline ---\n")
modelo_ols_baseline <- lm(log(USD_Revenue) ~ log(USD_Volume) * Channel, data = df_modelos)
summary_ols <- summary(modelo_ols_baseline)
print(summary_ols)

# 2. Extracción de Métricas de Ajuste Académicas
r_cuadrado <- summary_ols$r.squared
r_cuadrado_adj <- summary_ols$adj.r.squared
f_est <- summary_ols$fstatistic

cat("\n--- Diagnóstico de Ajuste Global ---\n")
cat("R-cuadrado:", round(r_cuadrado, 4), "\n")
cat("R-cuadrado Ajustado:", round(r_cuadrado_adj, 4), "\n")

# 3. Gráfico de Diagnóstico Clínico: Análisis de Residuos (Homocedasticidad)
# Evaluamos visualmente si el supuesto de varianza constante se rompe
df_diagnostico <- data.frame(
  Valores_Ajustados = modelo_ols_baseline$fitted.values,
  Residuos = modelo_ols_baseline$residuals
)

plot_diagnostico_ols <- ggplot(df_diagnostico, aes(x = Valores_Ajustados, y = Residuos)) +
  # Puntos con alta transparencia usando tu paleta institucional
  geom_point(color = "#5F7A8C", alpha = 0.1, size = 0.8) +
  # Línea de referencia horizontal en cero
  geom_hline(yintercept = 0, color = "#1F3B4D", linetype = "dashed", linewidth = 1) +
  # Tendencia local para visibilizar patrones en los residuos
  geom_smooth(method = "loess", color = "#D97724", linewidth = 1.1, se = FALSE) +
  labs(
    title = "Diagnóstico de Residuos: Valores Ajustados vs. Residuos",
    subtitle = "Evaluación del supuesto de homocedasticidad para el modelo OLS baseline",
    x = "Valores Ajustados (Predicción de Log Revenue)",
    y = "Residuos Estudiantiles/Ordinarios"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

ggsave("plot_diagnostico_ols.pdf", plot = plot_diagnostico_ols, width = 7, height = 4.5, device = cairo_pdf)


### Modelo de Dos Partes (Hurdle Model - Parte 1: Logit) ----

# 1. Crear la variable dependiente binaria en la base completa (sin filtrar ceros)
base_transaccional <- base_transaccional %>%
  mutate(Genera_Ingreso = if_else(USD_Revenue > 0, 1, 0))

cat("--- Estimación de la Parte 1 del Hurdle Model (Especificación Logit) ---\n")
# Usamos glm() que es nativo de R, no requiere librerías externas
modelo_logit_barrera <- glm(
  Genera_Ingreso ~ log(USD_Volume) + Channel, 
  data = base_transaccional, 
  family = binomial(link = "logit")
)

summary_logit <- summary(modelo_logit_barrera)
print(summary_logit)

# 2. Cálculo de los Odds Ratios (Razones de Probabilidad) - Corregido con cbind
odds_ratios <- exp(coef(modelo_logit_barrera))
intervalos_conf <- exp(confint.default(modelo_logit_barrera))

cat("\n--- Odds Ratios e Intervalos de Confianza (95%) ---\n")
tabla_odds <- cbind(Odds_Ratio = odds_ratios, intervalos_conf)
print(round(tabla_odds, 4))

### Balance del Flujo, Pipeline de Divisas y Direccionalidad ----

# 1. Construcción de variables bajo la especificación formal de la tesis
base_transaccional <- base_transaccional %>%
  mutate(
    # Construcción de la variable categórica generalizada de flujo neto
    Flow_Direction_Net = case_when(
      Buy_CCY == "USD"  ~ "Buy USD",
      Sell_CCY == "USD" ~ "Sell USD",
      TRUE              ~ "Non-USD"
    ),
    Flow_Direction_Net = factor(Flow_Direction_Net, levels = c("Buy USD", "Sell USD", "Non-USD")),
    
    # Concatenación para la variable del par de divisas (Currency Pair)
    Currency_Pair = paste0(Buy_CCY, "/", Sell_CCY),
    
    # Cálculo del diferencial o spread absoluto frente a la tasa de referencia interbancaria
    Rate_Differential = Deal_Rate - Market_Rate
  )

# 2. Generación de la Tabla de Contingencia Bidimensional (Frecuencias Observadas: O_rc)
tabla_contingencia <- table(base_transaccional$Flow_Direction_Net, base_transaccional$Currency_Pair)

cat("==================================================================\n")
cat("  PARTE I: MATRIZ DE FRECUENCIAS OBSERVADAS (O_rc)               \n")
cat("==================================================================\n")
print(tabla_contingencia)

# 3. Ejecución Formal del Contraste de Chi-Cuadrado de Independencia
chisq_test_flujo <- chisq.test(tabla_contingencia)

cat("\n==================================================================\n")
cat("  PARTE II: RESULTADOS FORMALES DEL CONTRASTE CHI-CUADRADO         \n")
cat("==================================================================\n")
print(chisq_test_flujo)

cat("\n==================================================================\n")
cat("  PARTE III: MATRIZ DE FRECUENCIAS ESPERADAS BAJO SIMETRÍA (E_rc) \n")
cat("==================================================================\n")
print(round(chisq_test_flujo$expected, 2))

# 4. Análisis Analítico del Impacto en el Diferencial de Tasas
cat("\n==================================================================\n")
cat("  PARTE IV: DIFERENCIAL PROMEDIO POR FLUJO Y PAR DE DIVISAS       \n")
cat("==================================================================\n")
resumen_diferenciales <- base_transaccional %>%
  group_by(Flow_Direction_Net, Currency_Pair) %>%
  summarise(
    N_Transacciones = n(),
    Diferencial_Medio = mean(Rate_Differential, na.rm = TRUE),
    Volumen_Total_USD = sum(USD_Volume, na.rm = TRUE),
    .groups = 'drop'
  )
print(as.data.frame(resumen_diferenciales))

## 5.2.2. Análisis Portafolio de Clientes ----

### Desigualdad, Concentración de Ingresos y Efecto Pareto ----

library(ineq)

gini_volumen <- ineq(base_clientes$Bank_Volume, type = "Gini")
gini_ingreso  <- ineq(base_clientes$Total_Revenue, type = "Gini")

cat("Coeficiente de Gini (Volumen):", round(gini_volumen, 4), "\n")
cat("Coeficiente de Gini (Ingreso):", round(gini_ingreso, 4), "\n")

# 3. Visualización de la Curva de Lorenz (Diagnóstico de Pareto)
# 1. Preparar datos para la curva de Lorenz
# Necesitamos la librería ineq para el cálculo y ggplot2 para el estilo
lorenz_data <- Lc(base_clientes$Total_Revenue)

df_lorenz <- data.frame(
  p = lorenz_data$p,
  L = lorenz_data$L
)

# 2. Gráfica optimizada con estilo académico
plot_lorenz_fin <- ggplot(df_lorenz, aes(x = p, y = L)) +
  # Línea de la curva de Lorenz
  geom_line(color = "#1F3B4D", linewidth = 1.5) +
  # Área bajo la curva para resaltar la desigualdad
  geom_area(fill = "#5F7A8C", alpha = 0.2) +
  # Línea de equidad perfecta (45 grados)
  geom_abline(intercept = 0, slope = 1, color = "#D97724", 
              linetype = "dashed", linewidth = 1) +
  # Etiquetas y temas
  labs(
    title = "Curva de Lorenz: Concentración del Ingreso",
    subtitle = "Análisis de la desigualdad en la contribución al revenue por cliente",
    x = "Proporción Acumulada de Clientes (p)",
    y = "Proporción Acumulada de Ingresos (L(p))"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

# Guardar con estilo cairo_pdf
ggsave("lorenz_revenue.pdf", plot = plot_lorenz_fin, width = 7, height = 4.5, device = cairo_pdf)

lorenz_data2 <- Lc(base_clientes$Bank_Volume)

df_lorenz2 <- data.frame(
  p = lorenz_data2$p,
  L = lorenz_data2$L
)

# 2. Gráfica optimizada con estilo académico
plot_lorenz_fin2 <- ggplot(df_lorenz2, aes(x = p, y = L)) +
  # Línea de la curva de Lorenz
  geom_line(color = "#1F3B4D", linewidth = 1.5) +
  # Área bajo la curva para resaltar la desigualdad
  geom_area(fill = "#5F7A8C", alpha = 0.2) +
  # Línea de equidad perfecta (45 grados)
  geom_abline(intercept = 0, slope = 1, color = "#D97724", 
              linetype = "dashed", linewidth = 1) +
  # Etiquetas y temas
  labs(
    title = "Curva de Lorenz: Concentración del Volumen",
    subtitle = "Análisis de la desigualdad en la contribución al Volumen por cliente",
    x = "Proporción Acumulada de Clientes (p)",
    y = "Proporción Acumulada de Volumen (L(p))"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

# Guardar con estilo cairo_pdf
ggsave("lorenz_volumen.pdf", plot = plot_lorenz_fin2, width = 7, height = 4.5, device = cairo_pdf)

### Evaluación de Desempeño y Asignación Comercial (Sales Performance) ----

# 1. Análisis descriptivo a nivel de transacción por Sales_ID
resumen_ventas <- base_transaccional %>%
  group_by(Sales_ID) %>%
  summarise(
    # n_distinct cuenta cuántos clientes únicos atendió ese ejecutivo
    Clientes_Atendidos = n_distinct(Client_ID),
    Ingreso_Medio_Por_TRX = mean(USD_Revenue, na.rm = TRUE),
    Volumen_Medio_Por_TRX = mean(USD_Volume, na.rm = TRUE),
    # Freq_TRX es simplemente el conteo de filas (operaciones)
    Freq_TRX = n(),
    .groups = 'drop'
  )

print(resumen_ventas)

# 2. Prueba no paramétrica de Kruskal-Wallis 
# Evaluamos si la distribución del INGRESO por operación varía por ejecutivo
kw_resultado <- kruskal.test(USD_Revenue ~ as.factor(Sales_ID), data = base_transaccional)
print(kw_resultado)

# 3. Prueba de Levene (Homogeneidad de Varianza)
# Evalúa si la "volatilidad" del ingreso por operación es consistente entre vendedores
levene_resultado <- car::leveneTest(USD_Revenue ~ as.factor(Sales_ID), data = base_transaccional)
print(levene_resultado)


plot_sales_perf <- ggplot(base_transaccional, aes(x = as.factor(Sales_ID), y = USD_Revenue, fill = as.factor(Sales_ID))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Ocultamos outliers extremos para mejorar visibilidad
  scale_y_log10() + # Usamos log debido a la alta asimetría detectada
  labs(title = "Distribución de Ingresos por Ejecutivo",
       x = "ID del Ejecutivo", y = "Log(Total Revenue)") +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif")
  )

# 1. Definición del gráfico con tu paleta de colores corporativa
plot_sales_perf <- ggplot(base_transaccional, aes(x = as.factor(Sales_ID), y = USD_Revenue, fill = as.factor(Sales_ID))) +
  # Boxplot con líneas estructurales en bronce corporativo
  geom_boxplot(
    color = "#1F3B4D", 
    alpha = 0.75, 
    outlier.shape = NA, 
    linewidth = 0.8
  ) + 
  # Escala logarítmica para corregir la alta asimetría de los ingresos
  scale_y_log10() + 
  # Asignación manual de colores según la segmentación analítica descubierta
  scale_fill_manual(
    values = c(
      "Sales 1" = "#D9D9D9",  # Azul Pizarra (Segmento Masivo)
      "Sales 2" = "#5F7A8C",  # Azul Marino (Segmento Relacional/Corporativo)
      "Sales 3" = "#5F7A8C", 
      "Sales 4" = "#5F7A8C", 
      "Sales 5" = "#5F7A8C"
    )
  ) +
  # Etiquetas académicas y formales
  labs(
    title = "Distribución de Ingresos por Ejecutivo",
    subtitle = "Análisis de dispersión y asimetría transaccional en el desempeño comercial",
    x = "Identificador del Ejecutivo (Sales ID)", 
    y = "Log(USD Revenue)"
  ) +
  # Aplicación de tu plantilla de diseño unificada (Serif + Classic)
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", hjust = 0.5),
    plot.subtitle = element_text(family = "serif", hjust = 0.5, color = "gray30"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif"),
    legend.position = "none" # Removemos la leyenda porque el eje X ya identifica los grupos
  )

# 2. Visualización e impresión en PDF de alta fidelidad
plot_sales_perf
ggsave("plot_sales_performance.pdf", plot = plot_sales_perf, 
       width = 7.5, height = 4.5, device = cairo_pdf)

# Comparación visual del Modelo Masivo vs. Valor Agregado
df_perf <- data.frame(
  Sales_Group = c("Sales 1 (Masivo)", "Sales 2-5 (Corporativo)"),
  Ingreso_Medio = c(1162, mean(c(2920, 3017, 2973, 3074)))
)

ggplot(df_perf, aes(x = Sales_Group, y = Ingreso_Medio, fill = Sales_Group)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("#5F7A8C", "#1F3B4D")) +
  labs(title = "Especialización Funcional: Ingreso Medio por Operación",
       y = "USD Revenue por Transacción", x = "") +
  theme_classic() + theme(legend.position = "none")
