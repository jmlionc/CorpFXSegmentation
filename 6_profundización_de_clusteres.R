# =============================================================================
# PROFUNDIZACIÓN: ARQUETIPO, PAM MULTI-K, DBSCAN
# Responde a las 4 preguntas sobre el cluster de tamaño n=5
# =============================================================================

library(dplyr)
library(cluster)
library(fpc)        # cluster.stats
library(dbscan)     # dbscan(), kNNdistplot
library(ggplot2)

set.seed(230125)

# =============================================================================
# 1. CRUCE CON ARQUETIPO DEL DGP (Corporate / Transactional)
# =============================================================================
cat("==================================================================\n")
cat("  1. CRUCE: CLUSTERS vs. ARQUETIPO ORIGINAL DEL DGP\n")
cat("==================================================================\n")

# Requiere que clientes_df (con Archetype) o una columna equivalente en
# base_clientes esté disponible. Si Archetype no se guardó en base_clientes,
# se reconstruye por posición de Client_ID (los primeros 100 = Corporate).

if ("Archetype" %in% names(base_clientes)) {
  archetype_vec <- base_clientes$Archetype
} else {
  # Reconstrucción por diseño conocido del DGP: primeros 100 = Corporate
  archetype_vec <- ifelse(as.integer(gsub("CLI_", "", base_clientes$Client_ID)) <= 100,
                          "Corporate", "Transactional")
}

cat("\n--- Tabla cruzada: K-Means (3 clusters) x Arquetipo ---\n")
print(table(KMeans_Cluster = km_final$cluster, Arquetipo = archetype_vec))

cat("\n--- Tabla cruzada: Ward x Arquetipo ---\n")
print(table(Ward_Cluster = ward_clusters, Arquetipo = archetype_vec))

cat("\n--- Identificación de los 5 clientes en el cluster minoritario (K-Means) ---\n")
tab_km <- table(km_final$cluster)
cluster_pequeno_id <- names(tab_km)[which.min(tab_km)]
clientes_n5 <- which(km_final$cluster == as.integer(cluster_pequeno_id))
cat("Índices de los 5 clientes:", clientes_n5, "\n")
cat("Sus arquetipos:", archetype_vec[clientes_n5], "\n")

cat("\n--- Perfil descriptivo de estos 5 clientes (variables originales) ---\n")
print(base_clientes[clientes_n5, c("Client_ID", "Bank_Volume", "Count_of_Deals",
                                   "Total_Revenue", "CLV_Proxy", "Average_Spread_BPS")])

cat("\n--- Percentil de estos clientes en Bank_Volume y CLV_Proxy ---\n")
for (idx in clientes_n5) {
  pct_vol <- round(mean(base_clientes$Bank_Volume <= base_clientes$Bank_Volume[idx]) * 100, 1)
  pct_clv <- round(mean(base_clientes$CLV_Proxy <= base_clientes$CLV_Proxy[idx]) * 100, 1)
  cat(sprintf("  %s: percentil Bank_Volume = %.1f%% | percentil CLV_Proxy = %.1f%%\n",
              base_clientes$Client_ID[idx], pct_vol, pct_clv))
}

# =============================================================================
# 2. PAM CON MÚLTIPLES K (2 a 8) — ¿también prefiere k=3?
# =============================================================================
cat("\n\n==================================================================\n")
cat("  2. PAM PARA k = 2 a 8\n")
cat("==================================================================\n")

pam_resultados <- data.frame()
for (k in 2:8) {
  pam_k <- pam(X_pca4, k = k, metric = "euclidean", stand = FALSE)
  pam_resultados <- rbind(pam_resultados, data.frame(
    K = k,
    Silueta = pam_k$silinfo$avg.width,
    Tamanos = paste(table(pam_k$clustering), collapse = "/")
  ))
}
cat("\n--- Desempeño de PAM por k ---\n")
print(pam_resultados, row.names = FALSE)

mejor_k_pam <- pam_resultados$K[which.max(pam_resultados$Silueta)]
cat("\nMejor k según silueta de PAM:", mejor_k_pam, "\n")

# Visualización
p_pam_k <- ggplot(pam_resultados, aes(x = K, y = Silueta)) +
  geom_line(color = "#1F3B4D", linewidth = 1) +
  geom_point(aes(color = K == mejor_k_pam), size = 3) +
  scale_color_manual(values = c("FALSE" = "#A9B7C0", "TRUE" = "#D97724"),
                     guide = "none") +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Silueta de PAM por Número de Clústeres",
       x = "k", y = "Silueta promedio") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"))
print(p_pam_k)
ggsave("pam_silueta_por_k.pdf", p_pam_k, width = 7, height = 5, device = cairo_pdf)

# =============================================================================
# 3. DBSCAN — ¿son los 5 clientes outliers de densidad genuinos?
# =============================================================================
cat("\n\n==================================================================\n")
cat("  3. DBSCAN — DIAGNÓSTICO DE OUTLIERS DE DENSIDAD\n")
cat("==================================================================\n")

# Paso 1: elegir eps mediante el gráfico de distancia al k-ésimo vecino
minPts_dbscan <- 5

cat("\nGenerando gráfico de k-NN distance para seleccionar eps...\n")
p_kNN <- kNNdistplot(X_pca4, k = minPts_dbscan)
abline(h = seq(0, 5, 0.5), col = "gray80", lty = 3)
title("Gráfico de Distancia k-NN (selección de eps para DBSCAN)")

# Eps candidato: inspeccionar el "codo" del gráfico anterior.
eps_candidatos <- c(0.5, 0.8, 1.0, 1.2, 1.5, 2.0)

cat("\n--- Resultados de DBSCAN para distintos eps (minPts =", minPts_dbscan, ") ---\n")
for (eps_val in eps_candidatos) {
  db_result <- dbscan::dbscan(X_pca4, eps = eps_val, minPts = minPts_dbscan)
  n_clusters <- length(unique(db_result$cluster[db_result$cluster != 0]))
  n_noise    <- sum(db_result$cluster == 0)
  cat(sprintf("  eps=%.2f -> %d clusters densos, %d puntos etiquetados como RUIDO\n",
              eps_val, n_clusters, n_noise))
}

# Ejecutar con el eps que produzca una estructura razonable (ajustar tras
# inspeccionar el gráfico k-NN y los resultados anteriores)
eps_final <- 1.0  # AJUSTAR según el codo observado en kNNdistplot
db_final  <- dbscan::dbscan(X_pca4, eps = eps_final, minPts = minPts_dbscan)

cat("\n--- DBSCAN final (eps =", eps_final, ", minPts =", minPts_dbscan, ") ---\n")
cat("Distribución de clusters (0 = ruido/outlier):\n")
print(table(db_final$cluster))

cat("\n--- ¿Los 5 clientes del cluster pequeño de K-Means son marcados como ruido? ---\n")
cat("Etiqueta DBSCAN de esos 5 clientes:", db_final$cluster[clientes_n5], "\n")
cat("(0 = SÍ son outliers de densidad; >0 = pertenecen a un cluster denso)\n")

# Visualización en el plano PC1-PC2
df_dbscan_plot <- as.data.frame(X_pca4[, 1:2])
df_dbscan_plot$DBSCAN_cluster <- factor(db_final$cluster)
df_dbscan_plot$es_n5 <- seq_len(nrow(df_dbscan_plot)) %in% clientes_n5

p_dbscan <- ggplot(df_dbscan_plot, aes(x = PC1, y = PC2,
                                       color = DBSCAN_cluster)) +
  geom_point(aes(size = es_n5, shape = es_n5), alpha = 0.7) +
  scale_size_manual(values = c("FALSE" = 1.5, "TRUE" = 4), guide = "none") +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), guide = "none") +
  scale_color_manual(values = c("0" = "#D97724", "1" = "#1F3B4D",
                                "2" = "#5F7A8C", "3" = "#A9B7C0")) +
  labs(title = "DBSCAN sobre el Espacio PCA-4 (proyección PC1-PC2)",
       subtitle = "Triángulos = los 5 clientes del cluster minoritario en K-Means/Ward",
       x = "PC1", y = "PC2", color = "Cluster DBSCAN\n(0 = ruido)") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, family = "serif", size = 9))
print(p_dbscan)
ggsave("dbscan_diagnostico.pdf", p_dbscan, width = 8, height = 6, device = cairo_pdf)

cat("\n\nProceso de profundización completo.\n")
cat("Revisar: (1) tabla cruzada arquetipo, (2) perfil de los 5 clientes,\n")
cat("(3) tabla PAM multi-k, (4) si DBSCAN marca a los 5 como ruido (cluster=0).\n")

