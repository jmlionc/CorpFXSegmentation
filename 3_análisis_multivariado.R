# ============================================================================#
# ANÁLISIS MULTIVARIADO COMPLETO #
# ============================================================================#

library(dplyr)
library(psych)        # KMO, cortest.bartlett
library(caret)        # findCorrelation
library(ggcorrplot)
library(factoextra)

# ============================================================================#
## PARTE I: VERIFICACIÓN DE DEPENDENCIAS DEL DGP ----
# ============================================================================#

# Spread_vs_Benchmark = Average_Spread_BPS - f(Average_Deal_Size)
cat("\ncorr(Spread_vs_Benchmark, Average_Spread_BPS):\n")
round(cor(base_clientes$Spread_vs_Benchmark,
              base_clientes$Average_Spread_BPS), 4)

# CLV_Proxy incorpora factor gamma(3,3) de incertidumbre
cat("\ncorr(CLV_Proxy, Total_Revenue):\n")

round(cor(base_clientes$CLV_Proxy,
              base_clientes$Total_Revenue), 4)

# log-ratio de volúmenes comprados/vendidos
cat("\ncorr(USD_Flow_Direction, Pct_Buy_USD):\n")
round(cor(base_clientes$USD_Flow_Direction,
              base_clientes$Pct_Buy_USD), 4)

# Entropía solo sobre pares no-COP
cat("\ncorr(Diversification_Score, Porcentaje_COP):\n")
round(cor(base_clientes$Diversification_Score,
              base_clientes$Porcentaje_COP), 4)

# ============================================================================#
## PARTE II: ESPACIO DE CARACTERÍSTICAS FINAL — 14 VARIABLES ----
# ============================================================================#

cat("\n\n==================================================================\n")
cat("  MATRIZ DEPURADA FINAL (14 VARIABLES)\n")
cat("==================================================================\n")

# Variables excluidas del vector PCA y justificación:
#
#   Dependencia algebraica exacta en DGP v1.0 (r=1 verificado):
#     Total_Revenue, Average_Deal_Size, Average_Revenue,
#     Revenue_Share_Pct, Pct_Buy_USD, Pct_Sell_USD
#
#   Dependencia funcional / circularidad en DGP v1.0:
#     CLV_Proxy (v1.0), Total_Wallet, sow_score
#
#   Colinealidad empírica extrema:
#     Active_Days (r=0.981 con Count_of_Deals)
#     Revenue_per_Day (r=0.937 con Total_Revenue — conservada como descriptiva)
#
#   Variables rediseñadas que SÍ entran (Grupo 2 del DGP v3.0):
#     Spread_vs_Benchmark (v3.0), CLV_Proxy (v3.0),
#     USD_Flow_Direction (v3.0), Share_of_Wallet (v3.0)
#
#   Variables nuevas que SÍ entran (Grupo 3 del DGP v3.0):
#     HHI_Temporal, CV_Volume, Diversification_Score

vars_finales <- c(
  "Bank_Volume", "Count_of_Deals",                # Escala y Frecuencia
  "Average_Spread_BPS", "Spread_vs_Benchmark",    # Rentabilidad Relativa/Absoluta
  "Spread_Price_Sensitivity",                     # Elasticidad Precio
  "Porcentaje_Spot", "Porcentaje_Digital",        # Operativa Digital/Producto
  "USD_Flow_Direction", "Diversification_Score",  # Riesgo Cambiario y Entropía
  "CV_Volume", "HHI_Temporal",                    # Patrones de Consistencia
  "Recency_Days", "CLV_Proxy", "Share_of_Wallet"  # Lealtad y Ciclo de Vida
)

X_final <- base_clientes %>%
  dplyr::select(all_of(vars_finales)) %>%
  na.omit()

cat("\nVariables retenidas (", length(vars_finales), "):\n", sep = "")
print(vars_finales)
cat("\nObservaciones:", nrow(X_final), "\n")

cor_final <- cor(X_final, method = "pearson")
cat("\n--- Matriz de correlación final ---\n")
print(round(cor_final, 4))

det_final <- det(cor_final)
cat("\nDeterminante:", round(det_final, 6), "\n")
if (det_final > 1e-10) {
  cat("  -> Matriz apta para inversión y PCA (no singular).\n")
} else {
  cat("  -> ALERTA: matriz cercana a singular.\n")
}

vif_final <- diag(solve(cor_final))
cat("\n--- VIF de la matriz final ---\n")
print(round(vif_final, 4))
cat("\nVariables con VIF > 10 (candidatas a revisión):",
    paste(names(vif_final[vif_final > 10]), collapse = ", "), "\n")

high_cor_final <- findCorrelation(cor_final, cutoff = 0.80)
if (length(high_cor_final) == 0) {
  cat("\nSin redundancia residual (cutoff 0.80).\n")
} else {
  cat("\nRedundancia residual (cutoff 0.80):",
      paste(vars_finales[high_cor_final], collapse = ", "), "\n")
}

# Correlograma institucional
theme_academic <- theme_classic(base_size = 12) +
  theme(
    text         = element_text(family = "serif"),
    plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

plot_correlograma_final <- ggcorrplot(
  cor_final,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 3,
  colors   = c("#D97724", "white", "#1F3B4D"),
  title    = "Matriz de Correlación — Espacio de Características Depurado",
  ggtheme  = theme_classic(base_size = 12)
) +
  theme(
    plot.title   = element_text(face = "bold", family = "serif",
                                hjust = 0.5, size = 14),
    axis.text.x  = element_text(family = "serif", angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif")
  )
print(plot_correlograma_final)
ggsave("correlograma_final.pdf", plot = plot_correlograma_final,
       width = 9, height = 8, device = cairo_pdf)

# ============================================================================#
## PARTE III: PRUEBAS DE ADECUACIÓN MUESTRAL PARA PCA ----
# ============================================================================#

cat("\n\n==================================================================\n")
cat("  PARTE III: ADECUACIÓN MUESTRAL — MATRIZ FINAL CONJUNTA\n")
cat("==================================================================\n")

bartlett_final <- cortest.bartlett(cor_final, n = nrow(X_final))
cat("\nBartlett: chisq =", round(bartlett_final$chisq, 2),
    "| df =", bartlett_final$df,
    "| p =", format.pval(bartlett_final$p.value, digits = 4), "\n")
cat("Interpretación: p < 0.05 rechaza H0 (matriz identidad) — PCA pertinente.\n")

kmo_final <- KMO(cor_final)
cat("\nKMO global:", round(kmo_final$MSA, 4), "\n")
cat("Escala Kaiser (1974): <0.50 inaceptable | 0.50-0.59 mediocre |\n")
cat("                       0.60-0.69 aceptable | 0.70-0.79 bueno |\n")
cat("                       0.80-0.89 notable   | >=0.90 excelente\n")
cat("\nMSAi por variable:\n")
print(round(kmo_final$MSAi, 4))
cat("\nVariables con MSAi < 0.50 (candidatas a exclusión adicional):",
    paste(names(kmo_final$MSAi[kmo_final$MSAi < 0.50]), collapse = ", "), "\n")

# ============================================================================#
## PARTE IV: PCA — EJECUCIÓN Y DIAGNÓSTICO ----
# ============================================================================#

cat("\n\n==================================================================\n")
cat("  PARTE IV: PCA — EJECUCIÓN Y DIAGNÓSTICO\n")
cat("==================================================================\n")

pca_result  <- prcomp(X_final, center = TRUE, scale. = TRUE)
eigenvalues <- pca_result$sdev^2
var_exp     <- eigenvalues / sum(eigenvalues) * 100
cum_var     <- cumsum(var_exp)
n_kaiser    <- sum(eigenvalues > 1)

cat("\n--- Resumen de varianza explicada ---\n")
print(summary(pca_result))

cat("\nEigenvalues:\n")
print(round(eigenvalues, 4))

cat("\nCriterio de Kaiser (eigenvalue > 1): retener", n_kaiser, "componentes\n")
cat("Varianza acumulada con", n_kaiser, "componentes:",
    round(cum_var[n_kaiser], 2), "%\n")

# Scree plot
scree_data <- data.frame(
  Component = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  Var_Pct    = var_exp,
  Cum_Var    = cum_var
)

library(ggplot2)
library(patchwork)
library(ggrepel)

p_scree <- ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "#D97724", linewidth = 0.7) +
  geom_line(color = "#1F3B4D", linewidth = 1) +
  geom_point(aes(color = Eigenvalue > 1), size = 3) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#1F3B4D"),
                     guide = "none") +
  scale_x_continuous(breaks = 1:length(eigenvalues)) +
  labs(title = "Scree Plot — Criterio de Kaiser",
       subtitle = "Línea punteada: eigenvalue = 1",
       x = "Componente Principal", y = "Eigenvalue") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))

p_cumvar <- ggplot(scree_data, aes(x = Component, y = Cum_Var)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 90,
           fill = "#5F7A8C", alpha = 0.1) +
  geom_hline(yintercept = 80, linetype = "dashed",
             color = "#D97724", linewidth = 0.5) +
  geom_hline(yintercept = 90, linetype = "dotted",
             color = "#D97724", linewidth = 0.5) +
  geom_line(color = "#1F3B4D", linewidth = 1) +
  geom_point(aes(color = cut(Cum_Var, breaks = c(0, 80, 90, 100),
                             labels = c("Bajo", "Objetivo", "Alto"))),
             size = 3) +
  scale_color_manual(values = c("Bajo" = "#2c3e50",
                                "Objetivo" = "#e67e22",
                                "Alto" = "#2c3e50"),
                     guide = "none") +
  geom_text_repel(aes(label = paste0(round(Cum_Var, 1), "%")),
                  nudge_y = 1.5, size = 3, family = "serif",
                  segment.color = NA) +
  scale_x_continuous(breaks = 1:length(eigenvalues)) +
  labs(title = "Varianza Acumulada",
       subtitle = "Zona sombreada: intervalo 80%-90%",
       x = "Número de Componentes", y = "Varianza Acumulada (%)") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))

final_pca_plot <- (p_scree / p_cumvar) +
  plot_annotation(
    title = "Análisis de Componentes Principales — Espacio de Características v3.0",
    theme = theme(plot.title = element_text(face = "bold", size = 13,
                                            hjust = 0.5, family = "serif"))
  )
print(final_pca_plot)
ggsave("final_pca_plot1.pdf", plot = final_pca_plot,
       width = 7, height = 9, device = cairo_pdf)

####  Version 2 del PCA Plot ###

# Ejecución del PCA sobre datos matemáticamente sanos
pca_result <- prcomp(X_final, center = TRUE, scale. = TRUE)
summary(pca_result)

# 1. Extraer la información de varianza del PCA
eig <- get_eigenvalue(pca_result)

# 2. Preparar el data frame para ggplot
eig12 <- eig %>%
  mutate(Component = 1:n()) #%>%
  #slice(1:12)

# Asegurar dependencias y estilo global
library(patchwork)
library(ggrepel)
library(ggplot2)

# Mejorar el tema para aplicarlo a ambos
theme_academic <- theme_classic(base_size = 13) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray30", hjust = 0.5),
    axis.line = element_line(color = "gray70"),
    axis.ticks = element_line(color = "gray70")
  )

# --- Gráfico 1: Varianza Individual ---
p1 <- ggplot(eig12, aes(x = factor(Component), y = variance.percent)) +
  geom_col(fill = "#1F3B4D", width = 0.7) +
  geom_text(aes(label = paste0(round(variance.percent, 1), "%")), 
            vjust = -0.5, size = 3.5, family = "serif") +
  labs(title = "Varianza Individual", subtitle = "Contribución por componente",
       x = "Componente Principal", y = "Varianza Explicada (%)") +
  coord_cartesian(clip = 'off') + # Permite que los textos no se corten
  theme_academic

# --- Gráfico 2: Varianza Acumulada ---
p2 <- ggplot(eig12, aes(x = Component, y = cumulative.variance.percent)) +
  # Resaltar la zona objetivo (80% - 90%)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 90, 
           fill = "#5F7A8C", alpha = 0.1) +
  
  # Líneas de umbral
  geom_hline(yintercept = 80, linetype = "dashed", color = "#D97724", linewidth = 0.5) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "#D97724", linewidth = 0.5) +
  
  # Línea principal
  geom_line(color = "#1F3B4D", linewidth = 1) +
  
  # Puntos condicionales: color según zona
  geom_point(aes(color = cut(cumulative.variance.percent, 
                             breaks = c(0, 80, 90, 100), 
                             labels = c("Bajo", "Objetivo", "Alto"))), 
             size = 3) +
  scale_color_manual(values = c("Bajo" = "#2c3e50", "Objetivo" = "#e67e22", "Alto" = "#2c3e50")) +
  
  # --- SOLUCIÓN AL OVERLAPPING ---
  geom_text_repel(aes(label = paste0(round(cumulative.variance.percent, 1), "%")), 
                  nudge_y = 2,          # Empuja los textos ligeramente hacia arriba
                  nudge_x = -0.4,       # Los mueve un poco hacia la izquierda de la línea
                  direction = "y",      # Prioriza el movimiento vertical
                  segment.color = NA,   # Evita que se dibujen líneas guía molestas
                  size = 3, 
                  family = "serif") +
  
  
  labs(title = "Varianza Acumulada", subtitle = "Contribución acumulada por componente",
       x = "Número de Componentes", y = "Varianza acumulada (%)") +
  guides(color = "none") + # Ocultar leyenda
  scale_x_continuous(breaks = 1:14) +
  coord_cartesian(clip = 'off') + # Evita que los números altos se corten en el borde superior
  theme_academic

# --- Panel Final (Ajuste a 2 filas x 1 columna) ---
# Se utiliza '/' en patchwork para apilar verticalmente
final_pca_plot <- (p1 / p2) +
  plot_annotation(
    title = "Análisis de Componentes Principales",
    theme = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, family = "serif"))
  )

final_pca_plot

# --- Exportación (Ajuste de proporciones) ---
# El height se aumenta a 8.5 o 9 para evitar que los gráficos se deformen al estar apilados
ggsave("final_pca_plot2.pdf", plot = final_pca_plot, 
       width = 7, height = 8, device = cairo_pdf)

# ScreePlot

library(factoextra)
library(ggplot2)

scree_plot <- fviz_eig(
  pca_result,
  addlabels = TRUE,
  barfill = "#1F3B4D",
  barcolor = "#1F3B4D",
  linecolor = "#D97724",
  labelsize = 4
) +
  labs(
    title = "Análisis de Componentes Principales",
    subtitle = "Porcentaje de varianza explicada por cada componente principal",
    x = "Componente principal",
    y = "Varianza explicada (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "serif",
      size = 15,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      family = "serif",
      size = 11,
      color = "gray30",
      hjust = 0.5
    ),
    axis.title = element_text(
      family = "serif",
      size = 12
    ),
    axis.text = element_text(
      family = "serif",
      size = 11
    ),
    axis.line = element_line(
      linewidth = 0.5,
      color = "#1F3B4D"
    ),
    panel.grid.major.y = element_line(
      color = "gray90",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank()
  )

scree_plot



scree_plot <- scree_plot +
  theme(
    text = element_text(family = "serif")
  ) +
  scale_color_manual(values = c("#D97724"))

scree_plot


# ScreePlot 2

# Asegurar dependencias y estilo global
library(patchwork)
library(ggrepel)

# Mejorar el tema para aplicarlo a ambos
theme_academic <- theme_classic(base_size = 13) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray30", hjust = 0.5),
    axis.line = element_line(color = "gray70"),
    axis.ticks = element_line(color = "gray70")
  )

# --- Gráfico 1: Varianza Individual ---
p1 <- ggplot(eig12, aes(x = factor(Component), y = variance.percent)) +
  geom_col(fill = "#1F3B4D", width = 0.7) +
  geom_text(aes(label = paste0(round(variance.percent, 1), "%")), 
            vjust = -0.5, size = 3.5, family = "serif") +
  labs(title = "Varianza Individual", subtitle = "Contribución por componente",
       x = "Componente Principal", y = "Varianza Explicada (%)") +
  coord_cartesian(clip = 'off') + # Permite que los textos no se corten
  theme_academic

# --- Gráfico 2: Varianza Acumulada ---
p2 <- ggplot(eig12, aes(x = Component, y = cumulative.variance.percent)) +
  # Resaltar la zona objetivo (80% - 90%)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 90, 
           fill = "#5F7A8C", alpha = 0.1) +
  
  # Líneas de umbral
  geom_hline(yintercept = 80, linetype = "dashed", color = "#D97724", linewidth = 0.5) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "#D97724", linewidth = 0.5) +
  
  # Línea principal
  geom_line(color = "#1F3B4D", linewidth = 1) +
  
  # Puntos condicionales: color según zona
  geom_point(aes(color = cut(cumulative.variance.percent, 
                             breaks = c(0, 80, 90, 100), 
                             labels = c("Bajo", "Objetivo", "Alto"))), 
             size = 3) +
  scale_color_manual(values = c("Bajo" = "#2c3e50", "Objetivo" = "#e67e22", "Alto" = "#2c3e50")) +
  
  # --- SOLUCIÓN AL OVERLAPPING ---
  geom_text_repel(aes(label = paste0(round(cumulative.variance.percent, 1), "%")), 
                  nudge_y = 1,          # Empuja los textos ligeramente hacia arriba
                  nudge_x = -0.4,       # Los mueve un poco hacia la izquierda de la línea
                  direction = "y",      # Prioriza el movimiento vertical
                  segment.color = NA,   # Evita que se dibujen líneas guía molestas
                  size = 3, 
                  family = "serif") +
  
  
  labs(title = "Varianza Acumulada", subtitle = "Contribución acumulada por componente",
       x = "Número de Componentes", y = "Varianza acumulada (%)") +
  guides(color = "none") + # Ocultar leyenda
  scale_x_continuous(breaks = 1:12) +
  coord_cartesian(clip = 'off') + # Evita que los números altos se corten en el borde superior
  theme_academic

# --- Panel Final (Ajuste a 2 filas x 1 columna) ---
# Se utiliza '/' en patchwork para apilar verticalmente
final_pca_plot <- (p1 / p2) +
  plot_annotation(
    title = "Análisis de Componentes Principales",
    theme = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, family = "serif"))
  )

final_pca_plot

# --- Exportación (Ajuste de proporciones) ---
# El height se aumenta a 8.5 o 9 para evitar que los gráficos se deformen al estar apilados
ggsave("final_pca_plot.pdf", plot = final_pca_plot, 
       width = 7, height = 8, device = cairo_pdf)
# ============================================================================#
## PARTE V: ANÁLISIS POST-PCA ----
# ============================================================================#

# Tema académico
theme_academic <- theme_classic(base_size = 13) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray30", hjust = 0.5),
    axis.line = element_line(color = "gray70"),
    axis.ticks = element_line(color = "gray70"),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "plain")
  )

# Biplot de variables
biplot2 <- fviz_pca_var(
  pca_result,
  col.var       = "contrib",
  gradient.cols = c("#F7F7F7", "#1F3B4D"),
  repel         = TRUE,
  labelsize     = 4,
  arrowsize     = 0.8
) +
  labs(
    title = "Biplot de Variables",
    subtitle = "Contribución de las variables a los dos primeros componentes principales",
    x = paste0("PC1 (", round(eig12$variance.percent[1], 1), "%)"),
    y = paste0("PC2 (", round(eig12$variance.percent[2], 1), "%)"),
    color = "Contribución"
  ) +
  theme_academic

biplot2

ggsave("biplot2.pdf", plot = biplot2, 
       width = 7, height = 8, device = cairo_pdf)

# ============================================================================#
## PARTE VI: Loadings ----
# ============================================================================#

# Loadings
cat("\n--- Loadings (primeras", n_kaiser, "componentes) ---\n")
print(round(pca_result$rotation[, 1:n_kaiser], 4))


# Heatmap de loadings
library(pheatmap)
loadings_df <- as.data.frame(pca_result$rotation[, 1:n_kaiser])
pheatmap(loadings_df,
         color            = colorRampPalette(c("#D97724", "white", "#1F3B4D"))(100),
         cluster_rows     = TRUE,
         cluster_cols     = FALSE,
         main             = "Mapa de Cargas Factoriales (Loadings)",
         display_numbers  = TRUE,
         number_format    = "%.2f",
         fontsize_number  = 8)

library(tidyverse)

theme_academic <- theme_classic(base_size = 13) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray30", hjust = 0.5),
    axis.line = element_line(color = "gray70"),
    axis.ticks = element_line(color = "gray70"),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "plain")
  )

# 1. Calcular el clustering jerárquico de las filas
matriz_loadings <- pca_result$rotation[, 1:n_kaiser]
hc_filas <- hclust(dist(matriz_loadings))
variables_ordenadas <- hc_filas$labels[hc_filas$order]

# 2. Preparar el DataFrame en formato largo para ggplot
loadings_long <- as.data.frame(matriz_loadings) %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Componente", values_to = "Loading") %>%
  mutate(Variable = factor(Variable, levels = variables_ordenadas))

# 3. Construcción del gráfico con el Tema Académico unificado
heatmaploadings <- ggplot(loadings_long, aes(x = Componente, y = Variable, fill = Loading)) +
  # Celdas con bordes blancos delgados y limpios
  geom_tile(color = "white", lwd = 0.4) +
  
  # TEXTO INTERNO: Se fuerza 'family = "serif"' para que coincida con tu documento
  geom_text(aes(label = sprintf("%.2f", Loading),
                color = abs(Loading) > 0.45), 
            size = 3.2, fontface = "bold", family = "serif") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2C3E50"), guide = "none") +
  
  # Escala de color institucional simétrica fija entre -1 y 1 (Centro en 0 = Blanco)
  scale_fill_gradient2(low = "#D97724", mid = "white", high = "#1F3B4D", 
                       midpoint = 0, limits = c(-1, 1), name = "Carga") +
  
  # Etiquetas de títulos
  labs(title = "Mapa de Cargas Factoriales (Loadings)",
       subtitle = "Contribución y direccionalidad de las características \n conductuales en los componentes retenidos",
       x = NULL, y = NULL) +
  
  # [PASO CLAVE]: Acoplamos tu estructura base académica
  theme_academic + 
  
  # Ajustes específicos obligatorios para el formato Heatmap (Heredando 'serif')
  theme(
    plot_title = element_text(face = "bold", size = 13, color = "#1F3B4D", margin = margin(b = 4), hjust = 0.5),
    plot_subtitle = element_text(size = 9.5, color = "grey40", margin = margin(b = 15), hjust = 0.5),
    axis.text = element_text(color = "#2C3E50", face = "bold"),
    axis.text.x = element_text(size = 10),
    
    # Limpieza de líneas excedentes (un heatmap se ve más elegante sin líneas de eje externas)
    axis.line = element_blank(),  
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    
    # Formato de la leyenda institucional
    legend.title = element_text(size = 9, face = "bold", color = "#1F3B4D"),
    legend.height = unit(1.5, "cm")
  )

# Desplegar gráfico en consola
heatmaploadings

# Exportación de alta calidad conservando fuentes vectoriales
ggsave("heatmaploadings.pdf", plot = heatmaploadings, 
       width = 7, height = 6, device = cairo_pdf)

# ============================================================================#
## PARTE V: DECISIÓN Y PREPARACIÓN DEL DATASET PARA CLUSTERING ----
# ============================================================================#

cat("\n\n==================================================================\n")
cat("  PARTE V: DECISIÓN METODOLÓGICA Y DATASET FINAL\n")
cat("==================================================================\n")

if (kmo_final$MSA >= 0.60) {
  cat("\nKMO =", round(kmo_final$MSA, 4),
      "-> ADECUADO para PCA (>= 0.60)\n")
  cat("Se utilizan los scores de las primeras", n_kaiser,
      "componentes para clustering.\n")
  X_clustering <- pca_result$x[, 1:n_kaiser]
  metodo_usado <- paste0("PCA (", n_kaiser, " componentes, ",
                         round(cum_var[n_kaiser], 1), "% varianza)")
} else {
  cat("\nKMO =", round(kmo_final$MSA, 4),
      "-> INSUFICIENTE para PCA (< 0.60)\n")
  cat("Se procede con clustering directo sobre las",
      length(vars_finales), "variables estandarizadas.\n")
  X_clustering <- scale(X_final)
  metodo_usado <- paste0("Variables originales estandarizadas (",
                         length(vars_finales), " variables)")
}

cat("\nMétodo de clustering:", metodo_usado, "\n")
cat("Dimensiones del dataset final:", dim(X_clustering), "\n")

# Elbow method
set.seed(230125)
fviz_nbclust(X_clustering, kmeans, method = "wss") +
  labs(title    = "Determinación del número óptimo de clusters (WSS)",
       subtitle = paste("Método:", metodo_usado)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))

# Silhouette
fviz_nbclust(X_clustering, kmeans, method = "silhouette") +
  labs(title    = "Determinación del número óptimo de clusters (Silhouette)",
       subtitle = paste("Método:", metodo_usado)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))

saveRDS(X_clustering, "X_clustering_final.rds")
cat("\nDataset final guardado en X_clustering_final.rds\n")
cat("Listo para K-Means.\n")