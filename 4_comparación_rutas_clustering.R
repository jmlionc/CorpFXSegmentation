# =============================================================================
# COMPARACIÓN DE 4 RUTAS HACIA CLUSTERING
# Rutas: PCA-4 (Kaiser), PCA-5, PCA-6 (80% varianza), Directo (14 variables)
# =============================================================================
# Para cada ruta se evalúa la calidad del clustering K-means bajo k=2..8
# usando múltiples índices de validación interna, y se selecciona la
# combinación (ruta, k) que ofrezca la mejor evidencia convergente.
# =============================================================================

library(dplyr)
library(factoextra)
library(cluster)      # silhouette
library(clusterCrit)  # Calinski-Harabasz, Davies-Bouldin, etc.
library(NbClust)      # múltiples índices simultáneos

set.seed(230125)

# -----------------------------------------------------------------------------
# 0. Preparación de las 4 rutas
# -----------------------------------------------------------------------------
# Se asume que X_final (14 variables, sin escalar) y pca_result ya existen
# en el entorno desde el script analisis_final_completo_v2.R

X_directo <- scale(X_final)                       # Ruta D: directo
X_pca4    <- pca_result$x[, 1:4]                   # Ruta A: Kaiser
X_pca5    <- pca_result$x[, 1:5]                   # Ruta B: intermedia
X_pca6    <- pca_result$x[, 1:6]                   # Ruta C: 80% varianza

rutas <- list(
  "A_PCA4_Kaiser"    = X_pca4,
  "B_PCA5"           = X_pca5,
  "C_PCA6_80pct"     = X_pca6,
  "D_Directo_14vars" = X_directo
)

cat("==================================================================\n")
cat("  DIMENSIONES DE CADA RUTA\n")
cat("==================================================================\n")
for (nombre in names(rutas)) {
  cat(sprintf("  %-20s: %d obs x %d dim\n", nombre,
              nrow(rutas[[nombre]]), ncol(rutas[[nombre]])))
}

# -----------------------------------------------------------------------------
# 1. Función de evaluación: corre K-means para k=2..8 y calcula índices
# -----------------------------------------------------------------------------
evaluar_ruta <- function(X, nombre_ruta, k_range = 2:8) {
  resultados <- data.frame()
  
  for (k in k_range) {
    set.seed(230125)
    km <- kmeans(X, centers = k, nstart = 25, iter.max = 100)
    
    # Silhouette promedio
    sil <- silhouette(km$cluster, dist(X))
    sil_avg <- mean(sil[, 3])
    
    # Calinski-Harabasz (vía clusterCrit, requiere matriz numérica)
    ch <- tryCatch(
      intCriteria(as.matrix(X), as.integer(km$cluster), "Calinski_Harabasz")$calinski_harabasz,
      error = function(e) NA
    )
    
    # Davies-Bouldin (menor es mejor)
    db <- tryCatch(
      intCriteria(as.matrix(X), as.integer(km$cluster), "Davies_Bouldin")$davies_bouldin,
      error = function(e) NA
    )
    
    # WSS (within-cluster sum of squares) para método del codo
    wss <- km$tot.withinss
    
    resultados <- rbind(resultados, data.frame(
      Ruta = nombre_ruta, K = k,
      Silhouette = sil_avg, Calinski_Harabasz = ch,
      Davies_Bouldin = db, WSS = wss
    ))
  }
  return(resultados)
}

# -----------------------------------------------------------------------------
# 2. Ejecutar evaluación para las 4 rutas
# -----------------------------------------------------------------------------
cat("\n==================================================================\n")
cat("  EVALUANDO LAS 4 RUTAS (k = 2 a 8)...\n")
cat("==================================================================\n")

resultados_totales <- data.frame()
for (nombre in names(rutas)) {
  cat("\nProcesando:", nombre, "...\n")
  res <- evaluar_ruta(rutas[[nombre]], nombre)
  resultados_totales <- rbind(resultados_totales, res)
}

cat("\n--- Resultados completos ---\n")
print(resultados_totales, row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Identificar el mejor (ruta, k) por cada índice
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  MEJOR (RUTA, K) POR CADA ÍNDICE\n")
cat("==================================================================\n")

mejor_silhouette <- resultados_totales[which.max(resultados_totales$Silhouette), ]
mejor_ch         <- resultados_totales[which.max(resultados_totales$Calinski_Harabasz), ]
mejor_db         <- resultados_totales[which.min(resultados_totales$Davies_Bouldin), ]

cat("\nMejor Silhouette (mayor es mejor):\n")
print(mejor_silhouette, row.names = FALSE)

cat("\nMejor Calinski-Harabasz (mayor es mejor):\n")
print(mejor_ch, row.names = FALSE)

cat("\nMejor Davies-Bouldin (menor es mejor):\n")
print(mejor_db, row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. Resumen por ruta: mejor silhouette alcanzado en cualquier k
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  RESUMEN: MEJOR DESEMPEÑO ALCANZABLE POR RUTA\n")
cat("==================================================================\n")

resumen_ruta <- resultados_totales %>%
  group_by(Ruta) %>%
  summarise(
    Mejor_K_Silhouette = K[which.max(Silhouette)],
    Max_Silhouette      = max(Silhouette, na.rm = TRUE),
    Mejor_K_CH          = K[which.max(Calinski_Harabasz)],
    Max_CH              = max(Calinski_Harabasz, na.rm = TRUE),
    Mejor_K_DB          = K[which.min(Davies_Bouldin)],
    Min_DB              = min(Davies_Bouldin, na.rm = TRUE)
  )
print(as.data.frame(resumen_ruta), row.names = FALSE)

# -----------------------------------------------------------------------------
# 5. Visualización comparativa
# -----------------------------------------------------------------------------
library(ggplot2)

p_sil <- ggplot(resultados_totales, aes(x = K, y = Silhouette, color = Ruta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Índice de Silueta por Ruta y Número de Clusters",
       x = "Número de Clusters (k)", y = "Silhouette Promedio") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))
print(p_sil)
ggsave("comparacion_silhouette.pdf", p_sil, width = 8, height = 5,
       device = cairo_pdf)

p_ch <- ggplot(resultados_totales, aes(x = K, y = Calinski_Harabasz, color = Ruta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Índice Calinski-Harabasz por Ruta y Número de Clusters",
       x = "Número de Clusters (k)", y = "Calinski-Harabasz") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))
print(p_ch)
ggsave("comparacion_calinski_harabasz.pdf", p_ch, width = 8, height = 5,
       device = cairo_pdf)

p_db <- ggplot(resultados_totales, aes(x = K, y = Davies_Bouldin, color = Ruta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Índice Davies-Bouldin por Ruta y Número de Clusters",
       subtitle = "Menor valor indica mejor separación",
       x = "Número de Clusters (k)", y = "Davies-Bouldin") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))
print(p_db)
ggsave("comparacion_davies_bouldin.pdf", p_db, width = 8, height = 5,
       device = cairo_pdf)

p_wss <- ggplot(resultados_totales, aes(x = K, y = WSS, color = Ruta)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Suma de Cuadrados Intra-Cluster (WSS) por Ruta",
       x = "Número de Clusters (k)", y = "WSS") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5))
print(p_wss)
ggsave("comparacion_wss.pdf", p_wss, width = 8, height = 5, device = cairo_pdf)

# -----------------------------------------------------------------------------
# 6. NbClust: votación de ~26 índices simultáneos (solo para rutas PCA,
#    computacionalmente costoso en alta dimensión / muestras grandes)
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  NbClust: VOTACIÓN DE MÚLTIPLES ÍNDICES (puede tardar varios minutos)\n")
cat("==================================================================\n")

for (nombre in names(rutas)) {
  cat("\n--- NbClust para:", nombre, "---\n")
  nb_result <- tryCatch({
    NbClust(rutas[[nombre]], distance = "euclidean", min.nc = 2, max.nc = 8,
            method = "kmeans", index = "alllong")
  }, error = function(e) {
    cat("  Error o advertencia en NbClust:", conditionMessage(e), "\n")
    NULL
  })
  if (!is.null(nb_result)) {
    cat("  K óptimo según mayoría de índices:",
        names(which.max(table(nb_result$Best.nc[1, ]))), "\n")
  }
}

# -----------------------------------------------------------------------------
# 7. Guardado de resultados
# -----------------------------------------------------------------------------
saveRDS(resultados_totales, "comparacion_4_rutas_resultados.rds")
write.csv(resultados_totales, "comparacion_4_rutas_resultados.csv",
          row.names = FALSE)
saveRDS(nb_result, "nb_result.rds")
cat("\n\nProceso completo. Resultados guardados.\n")
cat("Siguiente paso: revisar resumen_ruta y los gráficos para decidir\n")
cat("la combinación (ruta, k) final con mejor evidencia convergente.\n")


# =============================================================================
# VISUALIZACIÓN DE RESULTADOS NbClust — SIN RE-EJECUTAR
# =============================================================================
# Usa:
#   (1) El objeto nb_result ya guardado en el entorno (ruta D_Directo_14vars,
#       el único conservado del loop original).
#   (2) Las votaciones de las otras 3 rutas (A, B, C), reconstruidas
#       manualmente a partir del output de consola ya obtenido — no requiere
#       volver a correr NbClust, que es la parte costosa en tiempo.
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# 1. Votación de la ruta D, extraída directamente del objeto nb_result
# -----------------------------------------------------------------------------
# Estructura real verificada: nb_result$Best.nc es una matriz [2 x 30]
#   Fila 1: número de clusters (k) propuesto por cada uno de los 30 índices
#   Fila 2: valor del estadístico/índice correspondiente
# Algunos índices pueden no converger (NA) y se excluyen automáticamente
# por table() al tabular la fila 1.

cat("--- Estructura de Best.nc (verificación) ---\n")
str(nb_result$Best.nc)
cat("\nDimensiones:", dim(nb_result$Best.nc), "\n")

k_propuestos <- nb_result$Best.nc[1, ]
k_propuestos <- k_propuestos[!is.na(k_propuestos)]

cat("\nValores de k propuestos por cada índice (fila 1 de Best.nc):\n")
print(k_propuestos)

votos_D <- as.data.frame(table(k_propuestos))
names(votos_D) <- c("K", "Votos")
votos_D$K     <- as.integer(as.character(votos_D$K))
votos_D$Votos <- as.integer(votos_D$Votos)
votos_D$Ruta  <- "D_Directo_14vars"

cat("\n--- Votación Ruta D (desde el objeto nb_result, verificada) ---\n")
print(votos_D)
cat("Total de índices con voto válido:", sum(votos_D$Votos), "\n")

# -----------------------------------------------------------------------------
# 2. Votación de las rutas A, B, C — reconstruida del output de consola ya
#    obtenido en la corrida original (no requiere re-ejecutar NbClust)
# -----------------------------------------------------------------------------
votos_A <- data.frame(
  Ruta = "A_PCA4_Kaiser",
  K    = c(2, 3, 4, 5, 8),
  Votos = c(2, 14, 2, 3, 2)
)

votos_B <- data.frame(
  Ruta = "B_PCA5",
  K    = c(2, 3, 4, 5, 6, 7, 8),
  Votos = c(2, 14, 2, 1, 1, 2, 1)
)

votos_C <- data.frame(
  Ruta = "C_PCA6_80pct",
  K    = c(2, 3, 4, 6, 8),
  Votos = c(2, 13, 4, 1, 3)
)

votos_df <- bind_rows(votos_A, votos_B, votos_C, votos_D) %>%
  mutate(Ruta_label = case_when(
    Ruta == "A_PCA4_Kaiser"    ~ "PCA-4 (Kaiser)",
    Ruta == "B_PCA5"           ~ "PCA-5",
    Ruta == "C_PCA6_80pct"     ~ "PCA-6 (80%)",
    Ruta == "D_Directo_14vars" ~ "Directo (14 var.)",
    TRUE ~ Ruta
  )) %>%
  mutate(Ruta_label = factor(Ruta_label,
                             levels = c("PCA-4 (Kaiser)", "PCA-5",
                                        "PCA-6 (80%)", "Directo (14 var.)")))

cat("\n--- Tabla de votación consolidada (4 rutas) ---\n")
print(votos_df)

# -----------------------------------------------------------------------------
# OPCIÓN A: Barras de votación, panel por ruta (4 facetas)
# -----------------------------------------------------------------------------
p_votos <- ggplot(votos_df, aes(x = factor(K), y = Votos, fill = K == 3)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Votos), vjust = -0.4, size = 3.5, family = "serif") +
  scale_fill_manual(values = c("FALSE" = "#A9B7C0", "TRUE" = "#D97724"),
                    guide = "none") +
  facet_wrap(~ Ruta_label, nrow = 1) +
  labs(title = "Votación de Índices NbClust por Ruta de Reducción Dimensional",
       subtitle = "Número de índices que proponen cada k como óptimo (~20-22 índices válidos por ruta)",
       x = "Número de Clústeres (k)", y = "N.\u00ba de índices") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12,
                                  family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 8.5, color = "gray30",
                                     family = "serif"),
        axis.title = element_text(family = "serif"),
        axis.text = element_text(family = "serif"),
        axis.line = element_line(color = "gray70"),
        axis.ticks = element_line(color = "gray70"),
        strip.background = element_rect(fill = "#1F3B4D"),
        strip.text = element_text(color = "white", face = "bold", size = 9,
                                  family = "serif"))

print(p_votos)
ggsave("nbclust_votacion_por_ruta.pdf", p_votos, width = 11, height = 4.5,
       device = cairo_pdf)

cat("\nGráfico guardado: nbclust_votacion_por_ruta.pdf\n")

# -----------------------------------------------------------------------------
# OPCIÓN B (opcional): Panel resumen 2x2 — requiere resultados_totales
# (Silhouette, Calinski-Harabasz, Davies-Bouldin) ya en el entorno, del
# script comparacion_4_rutas.R. Si no lo tienes cargado, omite este bloque.
# -----------------------------------------------------------------------------
if (exists("resultados_totales")) {
  
  theme_panel <- theme_classic(base_size = 11) +
    theme(
      text             = element_text(family = "serif"),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 11,
                                      family = "serif"),
      plot.subtitle    = element_text(hjust = 0.5, size = 8.5, color = "gray30",
                                      family = "serif"),
      axis.title       = element_text(family = "serif"),
      axis.text        = element_text(family = "serif"),
      axis.line        = element_line(color = "gray70"),
      axis.ticks       = element_line(color = "gray70"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(family = "serif", size = 9)
    )
  
  # Paleta institucional consistente con el resto de figuras de la tesis
  # (correlograma, scree plot, biplot, heatmap de loadings):
  # Bronce (#D97724) - Azul Marino (#1F3B4D), con tonos intermedios
  # derivados para las 4 rutas comparadas.
  paleta_rutas <- c(
    "A_PCA4_Kaiser"    = "#1F3B4D",  # Azul marino (ruta ganadora)
    "B_PCA5"           = "#5F7A8C",  # Azul medio
    "C_PCA6_80pct"     = "#A9B7C0",  # Azul claro/gris
    "D_Directo_14vars" = "#D97724"   # Bronce (contraste, ruta directa)
  )
  
  paleta_rutas_label <- c(
    "PCA-4 (Kaiser)"     = "#1F3B4D",
    "PCA-5"              = "#5F7A8C",
    "PCA-6 (80%)"        = "#A9B7C0",
    "Directo (14 var.)"  = "#D97724"
  )
  
  p1 <- ggplot(resultados_totales, aes(x = K, y = Silhouette, color = Ruta)) +
    geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2:8) +
    scale_color_manual(values = paleta_rutas) +
    labs(title = "(a) Coeficiente de Silueta", x = "k", y = "Silueta") +
    theme_panel
  
  p2 <- ggplot(resultados_totales, aes(x = K, y = Calinski_Harabasz, color = Ruta)) +
    geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2:8) +
    scale_color_manual(values = paleta_rutas) +
    labs(title = "(b) Índice Calinski-Harabasz", x = "k", y = "CH") +
    theme_panel
  
  p3 <- ggplot(resultados_totales, aes(x = K, y = Davies_Bouldin, color = Ruta)) +
    geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2:8) +
    scale_color_manual(values = paleta_rutas) +
    labs(title = "(c) Índice Davies-Bouldin", x = "k", y = "DB (menor=mejor)") +
    theme_panel
  
  p4 <- ggplot(votos_df, aes(x = factor(K), y = Votos, fill = Ruta_label)) +
    geom_col(position = "dodge", width = 0.75) +
    scale_fill_manual(values = paleta_rutas_label) +
    labs(title = "(d) Votación NbClust (consenso multi-índice)",
         x = "k", y = "N.\u00ba de índices") +
    theme_panel
  
  panel_completo <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          text = element_text(family = "serif"))
  
  panel_completo <- panel_completo +
    plot_annotation(
      title = "Evidencia Convergente para la Selección de Ruta y Número de Clústeres",
      theme = theme(plot.title = element_text(face = "bold", size = 13,
                                              hjust = 0.5, family = "serif"))
    )
  
  print(panel_completo)
  ggsave("panel_evidencia_clustering_completo.pdf", panel_completo,
         width = 11, height = 9, device = cairo_pdf)
  
  cat("Gráfico guardado: panel_evidencia_clustering_completo.pdf\n")
  
} else {
  cat("\n'resultados_totales' no está en el entorno — se omite Opción B.\n")
  cat("Cárgalo desde comparacion_4_rutas_resultados.rds si lo necesitas:\n")
  cat('  resultados_totales <- readRDS("comparacion_4_rutas_resultados.rds")\n')
}