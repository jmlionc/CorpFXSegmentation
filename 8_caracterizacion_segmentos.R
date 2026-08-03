# =============================================================================
# NIVEL 1: CARACTERIZACIÓN DESCRIPTIVA DE LOS 2 SEGMENTOS FINALES
# NIVEL 2: DIAGNÓSTICO DE HETEROGENEIDAD INTERNA
# =============================================================================
# Segmentación final: k=2, espacio PCA-4, N=495 (excluidos 5 outliers)
# Segmento 1: Corporate (~97 clientes)
# Segmento 2: Transactional (~395-398 clientes)
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cluster)     # silhouette
library(factoextra)  # hopkins
library(fmsb)        # radar chart
library(reshape2)    # melt para heatmaps y boxplots largos

set.seed(230125)

# -----------------------------------------------------------------------------
# 0. PREPARACIÓN: etiquetas de segmento sobre la base depurada
# -----------------------------------------------------------------------------
# Usar la partición de K-Means k=2 sin outliers (km_k2_clean)
# y las variables originales de base_clientes sin los 5 outliers

base_clean <- base_clientes[-outliers_idx, ]
base_clean$Segmento <- ifelse(km_k2_clean$cluster == 1,
                              "Corporate", "Transactional")

# Verificación
cat("Distribución de segmentos:\n")
print(table(base_clean$Segmento))

# =============================================================================
# NIVEL 1A: PERFIL ESTADÍSTICO COMPLETO POR SEGMENTO
# =============================================================================
cat("\n\n==================================================================\n")
cat("  NIVEL 1A: PERFIL ESTADÍSTICO POR SEGMENTO\n")
cat("==================================================================\n")

# Variables a perfilar: las 14 del espacio PCA + descriptivas excluidas
vars_perfil <- c(
  # Variables del espacio PCA
  "Bank_Volume", "Count_of_Deals", "Average_Spread_BPS",
  "Spread_vs_Benchmark", "Spread_Price_Sensitivity",
  "Porcentaje_Spot", "Porcentaje_Digital", "Porcentaje_COP",
  "USD_Flow_Direction", "Diversification_Score",
  "CV_Volume", "HHI_Temporal", "Recency_Days",
  "CLV_Proxy", "Share_of_Wallet",
  # Variables descriptivas excluidas del PCA (útiles para caracterización)
  "Total_Revenue", "Average_Deal_Size", "Average_Revenue",
  "Active_Days", "Revenue_per_Day"
)

perfil_segmentos <- base_clean %>%
  group_by(Segmento) %>%
  summarise(across(all_of(vars_perfil), list(
    Media   = ~ mean(., na.rm = TRUE),
    Mediana = ~ median(., na.rm = TRUE),
    SD      = ~ sd(., na.rm = TRUE),
    P25     = ~ quantile(., 0.25, na.rm = TRUE),
    P75     = ~ quantile(., 0.75, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop")

cat("\nMedianas por segmento (variables clave):\n")
vars_key <- c("Bank_Volume", "Count_of_Deals", "Average_Spread_BPS",
              "Porcentaje_Digital", "CLV_Proxy", "Recency_Days",
              "HHI_Temporal", "Spread_Price_Sensitivity")

for (v in vars_key) {
  corp <- median(base_clean[[v]][base_clean$Segmento == "Corporate"], na.rm = TRUE)
  tran <- median(base_clean[[v]][base_clean$Segmento == "Transactional"], na.rm = TRUE)
  cat(sprintf("  %-30s | Corporate: %12.3f | Transactional: %12.3f\n",
              v, corp, tran))
}

# =============================================================================
# NIVEL 1B: PRUEBAS ESTADÍSTICAS DE DIFERENCIA (MANN-WHITNEY U)
# Con corrección de Bonferroni por comparaciones múltiples
# =============================================================================
cat("\n\n==================================================================\n")
cat("  NIVEL 1B: PRUEBAS MANN-WHITNEY U (CON CORRECCIÓN BONFERRONI)\n")
cat("==================================================================\n")

n_tests <- length(vars_perfil)
resultados_mw <- data.frame()

for (v in vars_perfil) {
  x_corp <- base_clean[[v]][base_clean$Segmento == "Corporate"]
  x_tran <- base_clean[[v]][base_clean$Segmento == "Transactional"]
  
  test <- wilcox.test(x_corp, x_tran, exact = FALSE)
  
  media_corp <- mean(x_corp, na.rm = TRUE)
  media_tran <- mean(x_tran, na.rm = TRUE)
  r_effect <- abs(test$statistic) /
    sqrt(length(x_corp[!is.na(x_corp)]) * length(x_tran[!is.na(x_tran)]))
  
  resultados_mw <- rbind(resultados_mw, data.frame(
    Variable      = v,
    Media_Corp    = round(media_corp, 4),
    Media_Trans   = round(media_tran, 4),
    W_statistic   = round(test$statistic, 2),
    p_value       = test$p.value,
    p_bonferroni  = min(test$p.value * n_tests, 1),
    Effect_r      = round(r_effect, 4)
  ))
}

resultados_mw <- resultados_mw %>%
  mutate(Significativa = ifelse(p_bonferroni < 0.05, "***", "ns"))

cat("\n--- Resultados ordenados por significancia ---\n")
print(resultados_mw[order(resultados_mw$p_bonferroni), ], row.names = FALSE)

# Guardar tabla completa
write.csv(resultados_mw, "mann_whitney_segmentos.csv", row.names = FALSE)

# =============================================================================
# NIVEL 1C: VISUALIZACIONES
# =============================================================================

theme_seg <- theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, family = "serif", size = 9,
                                     color = "gray30"),
        legend.position = "bottom",
        legend.title = element_blank())

paleta_seg <- c("Corporate" = "#1F3B4D", "Transactional" = "#D97724")

# 1C-i: Boxplots de las variables más discriminantes (mayor efecto r)
vars_top <- resultados_mw %>%
  filter(p_bonferroni < 0.05) %>%
  arrange(desc(Effect_r)) %>%
  head(8) %>%
  pull(Variable)

cat("\nVariables más discriminantes entre segmentos (top 8 por efecto r):\n")
print(vars_top)

datos_long <- base_clean %>%
  dplyr::select(Segmento, all_of(vars_top)) %>%
  melt(id.vars = "Segmento")

p_boxplots <- ggplot(datos_long, aes(x = Segmento, y = value, fill = Segmento)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5) +
  scale_fill_manual(values = paleta_seg) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  labs(title = "Distribución de Variables Clave por Segmento",
       x = NULL, y = NULL) +
  theme_seg +
  theme(strip.background = element_rect(fill = "#1F3B4D"),
        strip.text = element_text(color = "white", face = "bold",
                                  size = 8, family = "serif"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
print(p_boxplots)
ggsave("boxplots_segmentos.pdf", p_boxplots, width = 11, height = 7,
       device = cairo_pdf)

# ==============================================================================
# 1C-ii: Radar chart con fmsb (NORMALIZACIÓN GLOBAL CORREGIDA)
# ==============================================================================

library(dplyr)

vars_radar <- c("Bank_Volume", "Count_of_Deals", "Average_Spread_BPS",
                "Porcentaje_Digital", "Porcentaje_COP", "CLV_Proxy",
                "Recency_Days", "HHI_Temporal", "Spread_Price_Sensitivity",
                "Share_of_Wallet")

# 1. Calcular los centroides (promedios reales por segmento) sin alterar la escala aún
centroides_radar <- base_clean %>%
  group_by(Segmento) %>%
  summarise(across(all_of(vars_radar), ~ mean(., na.rm = TRUE)), .groups = "drop")

# 2. CALCULAR LOS LÍMITES GLOBALES (Basados en toda la población de clientes)
# Esto evita el error de que un segmento sea forzado a 0% y el otro a 100%
min_globales <- apply(base_clean[, vars_radar], 2, min, na.rm = TRUE)
max_globales <- apply(base_clean[, vars_radar], 2, max, na.rm = TRUE)

# 3. Normalizar los centroides utilizando los límites poblacionales
# (x - min_global) / (max_global - min_global)
radar_norm <- as.data.frame(t(apply(centroides_radar[, vars_radar], 1, function(x) {
  (x - min_globales) / (max_globales - min_globales + 1e-10)
})))

# Reasignar el nombre de las columnas
colnames(radar_norm) <- vars_radar

# 4. Construir la estructura estricta que exige fmsb:
# Fila 1: Límites máximos (1)
# Fila 2: Límites mínimos (0)
# Filas siguientes: Los datos reales normalizados
radar_fmsb <- rbind(
  rep(1, length(vars_radar)),
  rep(0, length(vars_radar)),
  radar_norm[centroides_radar$Segmento == "Corporate", ],
  radar_norm[centroides_radar$Segmento == "Transactional", ]
)

# 5. Generación y exportación del gráfico de radar
pdf("radar_chart_segmentos.pdf", width = 8, height = 8)

fmsb::radarchart(as.data.frame(radar_fmsb),
                 axistype = 1,
                 pcol  = c("#1F3B4D", "#D97724"),
                 pfcol = c(rgb(31,59,77, maxColorValue=255, alpha=60),
                           rgb(217,119,36, maxColorValue=255, alpha=60)),
                 plwd  = 2,
                 cglcol = "gray70", cglty = 1, cglwd = 0.8,
                 axislabcol = "gray40",
                 vlcex = 0.75,
                 title = "Perfil Comparativo de Centroides por Segmento\n(Variables normalizadas 0-1)")

legend("topright", legend = c("Corporate", "Transactional"),
       col = c("#1F3B4D", "#D97724"), lty = 1, lwd = 2,
       bty = "n", cex = 0.9)

dev.off()

cat("Radar chart corregido y guardado mediante fmsb: radar_chart_segmentos.pdf\n")
# =============================================================================
# NIVEL 2A: SILUETA INTRA-SEGMENTO
# =============================================================================
cat("\n\n==================================================================\n")
cat("  NIVEL 2A: SILUETA INTRA-SEGMENTO\n")
cat("==================================================================\n")

idx_corp <- which(base_clean$Segmento == "Corporate")
idx_tran <- which(base_clean$Segmento == "Transactional")

X_corp <- X_pca4_clean[idx_corp, ]
X_tran <- X_pca4_clean[idx_tran, ]

# Silueta interna: necesita al menos k=2 sub-grupos para calcularse
# Usamos k=2 para medir si hay estructura interna aprovechable
sil_interna <- function(X, nombre, k_test = 2:5) {
  cat("\n--- Silueta intra-segmento:", nombre, "---\n")
  for (k in k_test) {
    if (nrow(X) > k * 2) {
      set.seed(230125)
      km_int <- kmeans(X, centers = k, nstart = 25)
      sil_int <- silhouette(km_int$cluster, dist(X))
      cat(sprintf("  k=%d: silueta promedio = %.4f | tamaños: %s\n",
                  k, mean(sil_int[, 3]),
                  paste(table(km_int$cluster), collapse = "/")))
    }
  }
}

sil_interna(X_corp, "Corporate (n=97)")
sil_interna(X_tran, "Transactional (n=395)")

# =============================================================================
# NIVEL 2B: VARIANZA DE PC2-PC4 DENTRO DE CADA SEGMENTO
# =============================================================================
cat("\n\n==================================================================\n")
cat("  NIVEL 2B: VARIANZA DE PC2-PC4 DENTRO DE CADA SEGMENTO\n")
cat("==================================================================\n")

cat("==================================================================\n")
cat("  VARIANZA INTRA-SEGMENTO (DESCOMPOSICIÓN CORRECTA SS)\n")
cat("==================================================================\n")
cat("Descomposición: SS_total = SS_entre + SS_dentro\n")
cat("Todas las sumas de cuadrados usan la media GLOBAL como referencia.\n\n")

varianza_intra_correcta <- data.frame()

for (pc in 1:4) {
  
  scores     <- X_pca4_clean[, pc]
  segmentos  <- base_clean$Segmento
  media_global <- mean(scores)
  n_total    <- length(scores)
  
  # SS total: suma de cuadrados respecto a la media global
  SS_total <- sum((scores - media_global)^2)
  
  # SS dentro (within): para cada observación, cuadrado de la desviación
  # respecto a la MEDIA DE SU PROPIO SEGMENTO
  medias_seg <- tapply(scores, segmentos, mean)
  SS_dentro  <- sum((scores - medias_seg[segmentos])^2)
  
  # SS entre (between): por definición SS_total = SS_entre + SS_dentro
  SS_entre   <- SS_total - SS_dentro
  
  # Proporciones (todas en [0, 1] por construcción)
  Pct_dentro <- SS_dentro / SS_total * 100
  Pct_entre  <- SS_entre  / SS_total * 100
  
  # SS dentro por segmento (desagregado)
  idx_corp <- segmentos == "Corporate"
  idx_tran <- segmentos == "Transactional"
  
  SS_dentro_corp <- sum((scores[idx_corp] - medias_seg["Corporate"])^2)
  SS_dentro_tran <- sum((scores[idx_tran] - medias_seg["Transactional"])^2)
  
  Pct_corp <- SS_dentro_corp / SS_total * 100
  Pct_tran <- SS_dentro_tran / SS_total * 100
  
  varianza_intra_correcta <- rbind(varianza_intra_correcta, data.frame(
    Componente   = paste0("PC", pc),
    SS_Total     = round(SS_total, 2),
    SS_Entre     = round(SS_entre, 2),
    SS_Dentro    = round(SS_dentro, 2),
    Pct_Entre    = round(Pct_entre, 1),
    Pct_Dentro   = round(Pct_dentro, 1),
    SS_Corp      = round(SS_dentro_corp, 2),
    SS_Trans     = round(SS_dentro_tran, 2),
    Pct_Corp_SS  = round(Pct_corp, 1),
    Pct_Trans_SS = round(Pct_tran, 1)
  ))
}

cat("--- Descomposición completa SS por componente ---\n\n")
print(varianza_intra_correcta, row.names = FALSE)

cat("\nVerificación (SS_Entre + SS_Dentro debe ser igual a SS_Total):\n")
for (i in 1:nrow(varianza_intra_correcta)) {
  r <- varianza_intra_correcta[i, ]
  check <- abs(r$SS_Entre + r$SS_Dentro - r$SS_Total) < 0.01
  cat(sprintf("  %s: SS_Entre (%.2f) + SS_Dentro (%.2f) = %.2f | SS_Total = %.2f | OK: %s\n",
              r$Componente, r$SS_Entre, r$SS_Dentro,
              r$SS_Entre + r$SS_Dentro, r$SS_Total,
              ifelse(check, "SI", "ERROR")))
}

cat("\n--- Tabla resumida para la tesis ---\n")
cat("(todos los porcentajes están en [0,100] por construcción correcta)\n\n")

tabla_tesis <- varianza_intra_correcta %>%
  dplyr::select(Componente, Pct_Entre, Pct_Dentro, Pct_Corp_SS, Pct_Trans_SS)

names(tabla_tesis) <- c("Componente", "% Entre grupos",
                        "% Dentro grupos", "% Dentro Corp.", "% Dentro Trans.")
print(tabla_tesis, row.names = FALSE)

cat("\nInterpretación:\n")
cat("  '% Entre grupos': cuánta variabilidad del PC separa a Corporate de Transactional.\n")
cat("  '% Dentro grupos': cuánta variabilidad del PC existe al interior de los segmentos.\n")
cat("  '% Dentro Corp.': fracción del SS_total explicada por dispersión intra-Corporate.\n")
cat("  '% Dentro Trans.': fracción del SS_total explicada por dispersión intra-Transactional.\n")
cat("  Suma de columnas 3+4+5 puede != 100 solo si hay rounding; suma Col2+Col3 = 100 exacto.\n")
# =============================================================================
# NIVEL 2C: TEST DE HOPKINS
# =============================================================================
cat("\n\n==================================================================\n")
cat("  NIVEL 2C: ESTADÍSTICO DE HOPKINS\n")
cat("==================================================================\n")
cat("Interpretación: H > 0.65 indica tendencia al clustering (sub-estructura\n")
cat("latente). H ≈ 0.50 indica distribución uniforme (sin sub-estructura).\n\n")

# Hopkins para el conjunto completo (referencia)
h_total <- get_clust_tendency(X_pca4_clean, n = 30, graph = FALSE)
cat("Hopkins (total, N=495):", round(h_total$hopkins_stat, 4), "\n")

# Hopkins para cada segmento
h_corp <- get_clust_tendency(X_corp, n = min(30, floor(nrow(X_corp) * 0.1)),
                             graph = FALSE)
h_tran <- get_clust_tendency(X_tran, n = min(30, floor(nrow(X_tran) * 0.1)),
                             graph = FALSE)

cat("Hopkins (Corporate, n=97):", round(h_corp$hopkins_stat, 4), "\n")
cat("Hopkins (Transactional, n=395):", round(h_tran$hopkins_stat, 4), "\n")

cat("\nInterpretación:\n")
for (seg in list(list("Corporate", h_corp$hopkins_stat),
                 list("Transactional", h_tran$hopkins_stat))) {
  h_val <- seg[[2]]
  if (h_val > 0.65) {
    cat(sprintf("  %s (H=%.4f): evidencia de sub-estructura latente.\n",
                seg[[1]], h_val))
    cat("  -> Candidato para sub-segmentación anidada en trabajo futuro.\n")
  } else {
    cat(sprintf("  %s (H=%.4f): sin evidencia de sub-estructura relevante.\n",
                seg[[1]], h_val))
    cat("  -> Segmento internamente cohesivo; sub-segmentación no justificada.\n")
  }
}

# =============================================================================
# SÍNTESIS FINAL
# =============================================================================
cat("\n\n==================================================================\n")
cat("  SÍNTESIS: CARACTERIZACIÓN Y HETEROGENEIDAD INTERNA\n")
cat("==================================================================\n")

cat("\n1. Variables con diferencia significativa entre segmentos (Bonferroni p<0.05):\n")
print(resultados_mw %>%
        filter(p_bonferroni < 0.05) %>%
        arrange(desc(Effect_r)) %>%
        dplyr::select(Variable, Media_Corp, Media_Trans, Effect_r,
                      Significativa), row.names = FALSE)

cat("\n2. Resumen de Hopkins intra-segmento:\n")
cat(sprintf("   Corporate (n=97):      H = %.4f\n", h_corp$hopkins_stat))
cat(sprintf("   Transactional (n=395): H = %.4f\n", h_tran$hopkins_stat))

cat("\nArchivos guardados:\n")
cat("  - boxplots_segmentos.pdf\n")
cat("  - radar_chart_segmentos.pdf\n")
cat("  - mann_whitney_segmentos.csv\n")
