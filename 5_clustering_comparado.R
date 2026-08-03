# =============================================================================
# CLUSTERING COMPARADO: K-MEANS, WARD JERÁRQUICO, HÍBRIDO Y PAM
# Sobre el espacio PCA-4 (criterio de Kaiser), k=3
# =============================================================================
# Incluye validación cruzada entre algoritmos mediante:
#   - Inspección del dendrograma (estructura independiente de k fijado)
#   - Índice de Rand Ajustado (ARI) y Información Mutua Ajustada (AMI)
#     entre las particiones de los 4 métodos
# =============================================================================

library(dplyr)
library(cluster)       # pam(), silhouette()
library(factoextra)    # visualizaciones
library(mclust)        # adjustedRandIndex
library(aricode)       # AMI (alternativa/complemento a mclust)
library(ggplot2)
library(ggdendro)      # dendrograma estilizado con ggplot2

set.seed(230125)

# X_pca4 ya debe existir en el entorno (pca_result$x[, 1:4])
k_final <- 3

cat("==================================================================\n")
cat("  1. K-MEANS (ya ejecutado en la comparación de rutas, k=3)\n")
cat("==================================================================\n")

km_final <- kmeans(X_pca4, centers = k_final, nstart = 50, iter.max = 200)
cat("Tamaños de cluster (K-Means):", table(km_final$cluster), "\n")
cat("WSS total:", round(km_final$tot.withinss, 2), "\n")
sil_km <- silhouette(km_final$cluster, dist(X_pca4))
cat("Silueta promedio:", round(mean(sil_km[, 3]), 4), "\n")

# =============================================================================
# 2. WARD JERÁRQUICO — SIN FIJAR K A PRIORI
# =============================================================================
cat("\n\n==================================================================\n")
cat("  2. CLUSTERING JERÁRQUICO (WARD.D2)\n")
cat("==================================================================\n")

dist_pca4 <- dist(X_pca4, method = "euclidean")
hc_ward   <- hclust(dist_pca4, method = "ward.D2")

# Alturas de fusión: el "salto" más grande señala el número natural de
# clusters de forma independiente a cualquier k fijado de antemano.
alturas <- rev(hc_ward$height)
saltos  <- data.frame(
  K_resultante = 2:(length(alturas)),
  Altura       = alturas[2:length(alturas)],
  Salto        = -diff(alturas)[1:(length(alturas)-1)]
)
cat("\n--- Top 10 saltos de altura más grandes (candidatos a número de clusters) ---\n")
print(head(saltos[order(-saltos$Salto), ], 10))

cat("\nEl mayor salto de altura ocurre en K =",
    saltos$K_resultante[which.max(saltos$Salto)],
    "(evidencia independiente de la estructura del dendrograma)\n")

# Dendrograma estilizado, paleta institucional, con corte en k=3 marcado
dend_data <- dendro_data(hc_ward)
altura_corte <- mean(c(
  sort(hc_ward$height, decreasing = TRUE)[k_final],
  sort(hc_ward$height, decreasing = TRUE)[k_final - 1]
))

p_dendro <- ggplot() +
  geom_segment(data = dend_data$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "#1F3B4D", linewidth = 0.4) +
  geom_hline(yintercept = altura_corte, linetype = "dashed",
             color = "#D97724", linewidth = 0.8) +
  labs(title = "Dendrograma — Clustering Jerárquico (Ward.D2)",
       subtitle = paste0("Línea de corte para k = ", k_final,
                         " clústeres (altura = ", round(altura_corte, 1), ")"),
       x = "Clientes (observaciones)", y = "Altura de fusión (distancia Ward)") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, family = "serif", color = "gray30"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
print(p_dendro)
ggsave("dendrograma_ward.pdf", p_dendro, width = 10, height = 6, device = cairo_pdf)

# Corte del dendrograma en k=3
ward_clusters <- cutree(hc_ward, k = k_final)
cat("\nTamaños de cluster (Ward, cortado en k=3):", table(ward_clusters), "\n")
sil_ward <- silhouette(ward_clusters, dist_pca4)
cat("Silueta promedio (Ward):", round(mean(sil_ward[, 3]), 4), "\n")

# =============================================================================
# 3. HÍBRIDO (WARD + K-MEANS): centroides de Ward como semilla de K-Means
# =============================================================================
cat("\n\n==================================================================\n")
cat("  3. MÉTODO HÍBRIDO (WARD -> K-MEANS)\n")
cat("==================================================================\n")

# Centroides obtenidos de la partición de Ward, usados como inicialización
# determinística de K-Means (en vez de inicialización aleatoria nstart)
centroides_ward <- X_pca4 %>%
  as.data.frame() %>%
  mutate(cluster = ward_clusters) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) %>%
  dplyr::select(-cluster) %>%
  as.matrix()

km_hibrido <- kmeans(X_pca4, centers = centroides_ward, iter.max = 200)
cat("Tamaños de cluster (Híbrido):", table(km_hibrido$cluster), "\n")
sil_hibrido <- silhouette(km_hibrido$cluster, dist_pca4)
cat("Silueta promedio (Híbrido):", round(mean(sil_hibrido[, 3]), 4), "\n")

# =============================================================================
# 4. PAM (PARTITIONING AROUND MEDOIDS) — robusto a outliers
# =============================================================================
cat("\n\n==================================================================\n")
cat("  4. PAM (K-MEDOIDS)\n")
cat("==================================================================\n")

pam_final <- pam(X_pca4, k = k_final, metric = "euclidean", stand = FALSE)
cat("Tamaños de cluster (PAM):", table(pam_final$clustering), "\n")
cat("Silueta promedio (PAM, reportada internamente):",
    round(pam_final$silinfo$avg.width, 4), "\n")

# Identificación de los medoides (observaciones reales representativas)
cat("\nÍndices de los medoides seleccionados:", pam_final$id.med, "\n")

# =============================================================================
# 5. TABLA COMPARATIVA DE LOS 4 MÉTODOS
# =============================================================================
cat("\n\n==================================================================\n")
cat("  5. COMPARACIÓN DE DESEMPEÑO ENTRE MÉTODOS\n")
cat("==================================================================\n")

comparacion_metodos <- data.frame(
  Metodo = c("K-Means", "Ward Jerárquico", "Híbrido (Ward+K-Means)", "PAM"),
  Silueta_Promedio = c(
    round(mean(sil_km[, 3]), 4),
    round(mean(sil_ward[, 3]), 4),
    round(mean(sil_hibrido[, 3]), 4),
    round(pam_final$silinfo$avg.width, 4)
  ),
  Tamano_Cluster_1 = c(table(km_final$cluster)[1], table(ward_clusters)[1],
                       table(km_hibrido$cluster)[1], table(pam_final$clustering)[1]),
  Tamano_Cluster_2 = c(table(km_final$cluster)[2], table(ward_clusters)[2],
                       table(km_hibrido$cluster)[2], table(pam_final$clustering)[2]),
  Tamano_Cluster_3 = c(table(km_final$cluster)[3], table(ward_clusters)[3],
                       table(km_hibrido$cluster)[3], table(pam_final$clustering)[3])
)
print(comparacion_metodos, row.names = FALSE)

# =============================================================================
# 6. VALIDACIÓN CRUZADA ENTRE ALGORITMOS: ARI Y AMI
# =============================================================================
# Si las particiones de métodos con sesgos estructurales distintos
# (K-Means: esférico; Ward: jerárquico-varianza; PAM: basado en medoides)
# coinciden en gran medida, esto constituye evidencia fuerte de que la
# estructura de 3 grupos es real y no un artefacto de un único algoritmo.
# =============================================================================
cat("\n\n==================================================================\n")
cat("  6. VALIDACIÓN CRUZADA: ÍNDICE DE RAND AJUSTADO (ARI) e INFORMACIÓN\n")
cat("     MUTUA AJUSTADA (AMI) ENTRE PARTICIONES\n")
cat("==================================================================\n")

particiones <- list(
  KMeans  = km_final$cluster,
  Ward    = ward_clusters,
  Hibrido = km_hibrido$cluster,
  PAM     = pam_final$clustering
)

metodos_nombres <- names(particiones)
n_metodos <- length(metodos_nombres)

ari_matrix <- matrix(NA, n_metodos, n_metodos,
                     dimnames = list(metodos_nombres, metodos_nombres))
ami_matrix <- matrix(NA, n_metodos, n_metodos,
                     dimnames = list(metodos_nombres, metodos_nombres))

for (i in 1:n_metodos) {
  for (j in 1:n_metodos) {
    ari_matrix[i, j] <- adjustedRandIndex(particiones[[i]], particiones[[j]])
    ami_matrix[i, j] <- AMI(particiones[[i]], particiones[[j]])
  }
}

cat("\n--- Matriz de Índice de Rand Ajustado (ARI) ---\n")
cat("Escala: 0 = concordancia aleatoria | 1 = concordancia perfecta\n")
print(round(ari_matrix, 4))

cat("\n--- Matriz de Información Mutua Ajustada (AMI) ---\n")
cat("Escala: 0 = independencia estadística | 1 = concordancia perfecta\n")
print(round(ami_matrix, 4))

ari_promedio <- mean(ari_matrix[upper.tri(ari_matrix)])
ami_promedio <- mean(ami_matrix[upper.tri(ami_matrix)])
cat("\nARI promedio entre todos los pares de métodos:", round(ari_promedio, 4), "\n")
cat("AMI promedio entre todos los pares de métodos:", round(ami_promedio, 4), "\n")

if (ari_promedio >= 0.70) {
  cat("\n-> Concordancia ALTA entre métodos: fuerte evidencia de que la\n")
  cat("   estructura de 3 grupos es robusta y no depende del algoritmo.\n")
} else if (ari_promedio >= 0.50) {
  cat("\n-> Concordancia MODERADA entre métodos: estructura plausible pero\n")
  cat("   con sensibilidad parcial al algoritmo. Revisar observaciones\n")
  cat("   discrepantes antes de finalizar la segmentación.\n")
} else {
  cat("\n-> Concordancia BAJA entre métodos: la estructura de 3 grupos\n")
  cat("   podría ser sensible al algoritmo elegido. Se recomienda revisar\n")
  cat("   la especificación de k y/o el espacio de características.\n")
}

# Heatmap de la matriz ARI
library(reshape2)
ari_melt <- melt(ari_matrix)
p_ari <- ggplot(ari_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 3)), family = "serif", size = 4) +
  scale_fill_gradient2(low = "white", mid = "#A9B7C0", high = "#1F3B4D",
                       midpoint = 0.5, limits = c(0, 1), name = "ARI") +
  labs(title = "Concordancia entre Métodos de Clustering (ARI)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"))
print(p_ari)
ggsave("heatmap_ari_metodos.pdf", p_ari, width = 6, height = 5, device = cairo_pdf)

# =============================================================================
# 7. SELECCIÓN DEL MÉTODO FINAL
# =============================================================================
cat("\n\n==================================================================\n")
cat("  7. SELECCIÓN DEL MÉTODO FINAL\n")
cat("==================================================================\n")

mejor_metodo <- comparacion_metodos$Metodo[which.max(comparacion_metodos$Silueta_Promedio)]
cat("\nMétodo con mayor silueta promedio:", mejor_metodo, "\n")
cat("Esta selección se complementa con la evidencia de concordancia (ARI/AMI)\n")
cat("reportada en la sección 6, y con la inspección del dendrograma de Ward\n")
cat("(sección 2) como validación estructural independiente del número de\n")
cat("clústeres.\n")

# Guardar todas las particiones para la etapa de caracterización de clusters
saveRDS(particiones, "particiones_4_metodos.rds")
saveRDS(comparacion_metodos, "comparacion_metodos_clustering.rds")

cat("\n\nProceso completo. Resultados guardados.\n")
