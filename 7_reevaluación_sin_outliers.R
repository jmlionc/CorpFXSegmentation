# =============================================================================
# RE-EVALUACIÓN COMPLETA TRAS EXCLUSIÓN DE OUTLIERS DE DENSIDAD
# =============================================================================
# Se excluyen los 5 clientes confirmados como ruido por DBSCAN
# (CLI_118, CLI_214, CLI_260, CLI_271, CLI_390; índices: 118,214,260,271,390)
# y se repite el protocolo completo de determinación de k y comparación de
# métodos sobre los 495 clientes restantes, manteniendo el mismo espacio
# PCA-4 (sin recalcular el PCA, para no alterar la base de comparación).
# =============================================================================

library(dplyr)
library(cluster)
library(factoextra)
library(clusterCrit)
library(NbClust)
library(mclust)
library(aricode)
library(ggplot2)
library(ggdendro)
library(reshape2)

set.seed(230125)

# -----------------------------------------------------------------------------
# 0. Exclusión de los 5 outliers confirmados por DBSCAN
# -----------------------------------------------------------------------------
outliers_idx <- clientes_n5   # 118, 214, 260, 271, 390 (de la corrida anterior)
cat("==================================================================\n")
cat("  EXCLUSIÓN DE OUTLIERS CONFIRMADOS\n")
cat("==================================================================\n")
cat("Clientes excluidos:", base_clientes$Client_ID[outliers_idx], "\n")
cat("N original:", nrow(X_pca4), "| N tras exclusión:", nrow(X_pca4) - length(outliers_idx), "\n")

X_pca4_clean <- X_pca4[-outliers_idx, ]
cat("Dimensiones de X_pca4_clean:", dim(X_pca4_clean), "\n")

# -----------------------------------------------------------------------------
# 1. RE-DETERMINACIÓN DE K: Silhouette, Calinski-Harabasz, Davies-Bouldin
#    (K-Means) — análogo al script comparacion_4_rutas.R, ahora sin outliers
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  1. K-MEANS SIN OUTLIERS: ÍNDICES DE VALIDACIÓN PARA k=2..8\n")
cat("==================================================================\n")

resultados_clean <- data.frame()
for (k in 2:8) {
  set.seed(230125)
  km_k <- kmeans(X_pca4_clean, centers = k, nstart = 50, iter.max = 200)
  sil_k <- silhouette(km_k$cluster, dist(X_pca4_clean))
  ch_k  <- tryCatch(
    intCriteria(as.matrix(X_pca4_clean), as.integer(km_k$cluster),
                "Calinski_Harabasz")$calinski_harabasz,
    error = function(e) NA)
  db_k  <- tryCatch(
    intCriteria(as.matrix(X_pca4_clean), as.integer(km_k$cluster),
                "Davies_Bouldin")$davies_bouldin,
    error = function(e) NA)
  
  resultados_clean <- rbind(resultados_clean, data.frame(
    K = k, Silhouette = mean(sil_k[, 3]),
    Calinski_Harabasz = ch_k, Davies_Bouldin = db_k,
    Tamanos = paste(table(km_k$cluster), collapse = "/")
  ))
}
cat("\n--- Resultados K-Means sin outliers ---\n")
print(resultados_clean, row.names = FALSE)

cat("\nMejor k por Silhouette:", resultados_clean$K[which.max(resultados_clean$Silhouette)], "\n")
cat("Mejor k por Calinski-Harabasz:", resultados_clean$K[which.max(resultados_clean$Calinski_Harabasz)], "\n")
cat("Mejor k por Davies-Bouldin:", resultados_clean$K[which.min(resultados_clean$Davies_Bouldin)], "\n")

# Gráfico comparativo
p_sil_clean <- ggplot(resultados_clean, aes(x = K, y = Silhouette)) +
  geom_line(color = "#1F3B4D", linewidth = 1) +
  geom_point(size = 2.5, color = "#1F3B4D") +
  scale_x_continuous(breaks = 2:8) +
  labs(title = "Silueta K-Means tras Exclusión de Outliers (N=495)",
       x = "k", y = "Silueta promedio") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"))
print(p_sil_clean)
ggsave("silueta_kmeans_sin_outliers.pdf", p_sil_clean, width = 7, height = 5,
       device = cairo_pdf)

# -----------------------------------------------------------------------------
# 2. WARD JERÁRQUICO SIN OUTLIERS
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  2. WARD JERÁRQUICO SIN OUTLIERS\n")
cat("==================================================================\n")

dist_clean <- dist(X_pca4_clean, method = "euclidean")
hc_ward_clean <- hclust(dist_clean, method = "ward.D2")

alturas_clean <- rev(hc_ward_clean$height)
saltos_clean <- data.frame(
  K_resultante = 2:(length(alturas_clean)),
  Altura = alturas_clean[2:length(alturas_clean)],
  Salto = -diff(alturas_clean)[1:(length(alturas_clean)-1)]
)
cat("\n--- Top 10 saltos de altura (Ward, sin outliers) ---\n")
print(head(saltos_clean[order(-saltos_clean$Salto), ], 10))
cat("\nMayor salto en K =", saltos_clean$K_resultante[which.max(saltos_clean$Salto)], "\n")

# Dendrograma actualizado
dend_data_clean <- dendro_data(hc_ward_clean)
p_dendro_clean <- ggplot() +
  geom_segment(data = dend_data_clean$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "#1F3B4D", linewidth = 0.4) +
  labs(title = "Dendrograma Ward.D2 — Sin Outliers (N=495)",
       x = "Clientes", y = "Altura de fusión") +
  theme_classic(base_size = 12) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "serif"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())
print(p_dendro_clean)
ggsave("dendrograma_ward_sin_outliers.pdf", p_dendro_clean, width = 10, height = 6,
       device = cairo_pdf)

# -----------------------------------------------------------------------------
# 3. PAM SIN OUTLIERS, MULTI-K
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  3. PAM SIN OUTLIERS PARA k = 2..8\n")
cat("==================================================================\n")

pam_clean_resultados <- data.frame()
for (k in 2:8) {
  pam_k <- pam(X_pca4_clean, k = k, metric = "euclidean", stand = FALSE)
  pam_clean_resultados <- rbind(pam_clean_resultados, data.frame(
    K = k, Silueta = pam_k$silinfo$avg.width,
    Tamanos = paste(table(pam_k$clustering), collapse = "/")
  ))
}
cat("\n--- PAM sin outliers ---\n")
print(pam_clean_resultados, row.names = FALSE)
mejor_k_pam_clean <- pam_clean_resultados$K[which.max(pam_clean_resultados$Silueta)]
cat("\nMejor k según PAM (sin outliers):", mejor_k_pam_clean, "\n")

# -----------------------------------------------------------------------------
# 4. NbClust SIN OUTLIERS
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  4. NbClust SIN OUTLIERS (puede tardar unos minutos)\n")
cat("==================================================================\n")

nb_clean <- tryCatch({
  NbClust(X_pca4_clean, distance = "euclidean", min.nc = 2, max.nc = 8,
          method = "kmeans", index = "alllong")
}, error = function(e) {
  cat("Error en NbClust:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(nb_clean)) {
  cat("\nK óptimo según mayoría de índices (sin outliers):",
      names(which.max(table(nb_clean$Best.nc[1, ]))), "\n")
  cat("\nDistribución completa de votos:\n")
  print(table(nb_clean$Best.nc[1, ]))
}

# -----------------------------------------------------------------------------
# 5. SÍNTESIS: ¿k=2 o k=3 tras remover outliers?
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  5. SÍNTESIS DE EVIDENCIA SIN OUTLIERS\n")
cat("==================================================================\n")

cat("\nResumen de k óptimo por criterio (N=495, sin outliers):\n")
cat("  Silhouette (K-Means):      k =",
    resultados_clean$K[which.max(resultados_clean$Silhouette)], "\n")
cat("  Calinski-Harabasz:         k =",
    resultados_clean$K[which.max(resultados_clean$Calinski_Harabasz)], "\n")
cat("  Davies-Bouldin:            k =",
    resultados_clean$K[which.min(resultados_clean$Davies_Bouldin)], "\n")
cat("  Salto de altura (Ward):    k =",
    saltos_clean$K_resultante[which.max(saltos_clean$Salto)], "\n")
cat("  PAM:                       k =", mejor_k_pam_clean, "\n")
if (!is.null(nb_clean)) {
  cat("  NbClust (mayoría):         k =",
      names(which.max(table(nb_clean$Best.nc[1, ]))), "\n")
}

# -----------------------------------------------------------------------------
# 6. SI CONVERGE A k=2: comparación directa con la dicotomía Corporate/
#    Transactional original, para verificar si el clustering simplemente
#    recupera el arquetipo del DGP
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  6. VERIFICACIÓN: ¿k=2 RECUPERA LA DICOTOMÍA ARQUETIPO ORIGINAL?\n")
cat("==================================================================\n")

km_k2_clean <- kmeans(X_pca4_clean, centers = 2, nstart = 50, iter.max = 200)
archetype_clean <- archetype_vec[-outliers_idx]

cat("\n--- Tabla cruzada: K-Means k=2 (sin outliers) x Arquetipo original ---\n")
tab_cruzada <- table(Cluster = km_k2_clean$cluster, Arquetipo = archetype_clean)
print(tab_cruzada)

# Índice de concordancia entre la partición k=2 y el arquetipo real del DGP
ari_arquetipo <- adjustedRandIndex(km_k2_clean$cluster,
                                   as.integer(factor(archetype_clean)))
cat("\nARI entre clustering k=2 y arquetipo verdadero del DGP:",
    round(ari_arquetipo, 4), "\n")
cat("(ARI cercano a 1 confirmaría que el clustering recupera casi\n")
cat(" exactamente la dicotomía Corporate/Transactional programada en el DGP)\n")

# -----------------------------------------------------------------------------
# 7. TAMBIÉN VERIFICAR k=3 SIN OUTLIERS: ¿reaparece más limpio?
# -----------------------------------------------------------------------------
cat("\n\n==================================================================\n")
cat("  7. ESTRUCTURA DE k=3 SIN OUTLIERS (PARA COMPARACIÓN)\n")
cat("==================================================================\n")

km_k3_clean <- kmeans(X_pca4_clean, centers = 3, nstart = 50, iter.max = 200)
cat("Tamaños de cluster (k=3, sin outliers):", table(km_k3_clean$cluster), "\n")
sil_k3_clean <- silhouette(km_k3_clean$cluster, dist_clean)
cat("Silueta promedio (k=3, sin outliers):", round(mean(sil_k3_clean[, 3]), 4), "\n")

cat("\n--- Tabla cruzada: K-Means k=3 (sin outliers) x Arquetipo ---\n")
print(table(Cluster = km_k3_clean$cluster, Arquetipo = archetype_clean))

# Guardado
saveRDS(list(
  X_pca4_clean = X_pca4_clean,
  resultados_clean = resultados_clean,
  pam_clean_resultados = pam_clean_resultados,
  nb_clean = nb_clean,
  km_k2_clean = km_k2_clean,
  km_k3_clean = km_k3_clean,
  ari_arquetipo = ari_arquetipo
), "reevaluacion_sin_outliers.rds")

cat("\n\nProceso completo. Revisar la síntesis (sección 5) para la decisión final.\n")