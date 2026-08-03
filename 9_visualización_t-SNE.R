# =============================================================================
# VISUALIZACIÓN t-SNE — CONFIRMACIÓN VISUAL DE LA ESTRUCTURA DE CLUSTERING
# =============================================================================
# t-SNE (van der Maaten & Hinton, 2008) como herramienta complementaria de
# visualización: no modifica ni reemplaza el pipeline analítico (PCA + K-Means),
# sino que proyecta el espacio PCA-4 en 2D no-lineal para confirmar
# visualmente la separación geométrica de los segmentos y comunicar resultados
# a audiencias no técnicas.
#
# Inputs requeridos (ya en el entorno):
#   - X_pca4_clean : matriz 495x4 de scores PCA sin outliers
#   - base_clean   : base_clientes sin los 5 outliers, con columna $Segmento
#   - outliers_idx : índices de las 5 observaciones excluidas
# =============================================================================

library(Rtsne)      # implementación de t-SNE
library(ggplot2)
library(ggrepel)    # etiquetas sin solapamiento
library(patchwork)
library(dplyr)

set.seed(230125)

reevaluacion_sin_outliers <- readRDS("reevaluacion_sin_outliers.rds")
X_pca4_clean <- reevaluacion_sin_outliers$X_pca4_clean
km_k2_clean <- reevaluacion_sin_outliers$km_k2_clean

# =============================================================================
# 1. SELECCIÓN DE HIPERPARÁMETROS
# =============================================================================
# Perplexity: controla el balance entre estructura local y global.
# Regla práctica (van der Maaten, 2008): perplexity entre 5 y 50;
# para n=495, valores de 20-50 son razonables.
# Se prueban 3 valores para mostrar estabilidad de la solución.

perplexity_vals <- c(20, 30, 50)

cat("==================================================================\n")
cat("  t-SNE: SENSIBILIDAD A LA PERPLEXITY\n")
cat("==================================================================\n")

# Eliminar duplicados exactos (Rtsne los rechaza)
X_tsne_input <- unique(X_pca4_clean)
n_unique <- nrow(X_tsne_input)
cat("Observaciones únicas para t-SNE:", n_unique, "(de 495)\n\n")

resultados_tsne <- list()

for (perp in perplexity_vals) {
  set.seed(230125)
  tsne_out <- Rtsne(X_tsne_input,
                    dims        = 2,
                    perplexity  = perp,
                    max_iter    = 1000,
                    theta       = 0.5,      # Barnes-Hut approximation
                    pca         = FALSE,    # ya estamos en espacio PCA
                    normalize   = TRUE,
                    verbose     = FALSE)
  
  resultados_tsne[[as.character(perp)]] <- tsne_out$Y
  cat(sprintf("  Perplexity=%d: convergencia en iteración %d | KL divergencia = %.4f\n",
              perp, length(tsne_out$itercosts),
              tail(tsne_out$itercosts, 1)))
}

# =============================================================================
# 2. DATASET FINAL PARA VISUALIZACIÓN (perplexity=30, estándar de la literatura)
# =============================================================================
cat("\n\nUsando perplexity=30 para la visualización final (estándar de la\n")
cat("literatura para muestras entre 200 y 1000 observaciones).\n")

base_clientes <- readRDS("base_clientes.rds")
base_clean <- base_clientes[-c(118, 214, 260, 271, 390), ]
base_clean$Segmento <- ifelse(km_k2_clean$cluster == 1,
                              "Corporate", "Transactional")
set.seed(230125)
tsne_final <- Rtsne(X_pca4_clean,
                    dims       = 2,
                    perplexity = 30,
                    max_iter   = 1000,
                    theta      = 0.5,
                    pca        = FALSE,
                    normalize  = TRUE,
                    verbose    = FALSE)

df_tsne <- data.frame(
  tSNE1   = tsne_final$Y[, 1],
  tSNE2   = tsne_final$Y[, 2],
  Segmento = base_clean$Segmento
)

# =============================================================================
# 3. GRÁFICO PRINCIPAL: SEGMENTOS EN ESPACIO t-SNE (para audiencia técnica y
#    no técnica)
# =============================================================================

paleta_seg <- c("Corporate" = "#1F3B4D", "Transactional" = "#D97724")

# Centroides de cada segmento en el espacio t-SNE (para etiquetas)
centroides_tsne <- df_tsne %>%
  group_by(Segmento) %>%
  summarise(tSNE1 = median(tSNE1), tSNE2 = median(tSNE2), .groups = "drop")

p_tsne_main <- ggplot(df_tsne, aes(x = tSNE1, y = tSNE2, color = Segmento)) +
  # Nube de puntos por segmento
  geom_point(aes(shape = Segmento), size = 1.8, alpha = 0.75) +
  # Elipses de concentración al 95%
  stat_ellipse(aes(fill = Segmento), geom = "polygon",
               type = "norm", level = 0.95, alpha = 0.08, linewidth = 0.6) +
  # Etiquetas de segmento en el centroide
  geom_label(data = centroides_tsne,
             aes(label = Segmento, fill = Segmento),
             color = "white", fontface = "bold", size = 3.5,
             family = "serif", label.r = unit(0.3, "lines"),
             label.padding = unit(0.4, "lines"),
             show.legend = FALSE) +
  scale_color_manual(values = paleta_seg) +
  scale_fill_manual(values = paleta_seg) +
  scale_shape_manual(values = c("Corporate" = 17, "Transactional" = 16)) +
  labs(
    title    = "Estructura de Segmentación en el Espacio t-SNE",
    subtitle = "Proyección bidimensional no lineal del espacio PCA-4 (N=495, perplexity=30)",
    x        = "Dimensión t-SNE 1",
    y        = "Dimensión t-SNE 2",
    caption  = "Nota: los ejes t-SNE no tienen interpretación algebraica directa.\nLas distancias entre grupos son indicativas de separación geométrica local."
  ) +
  theme_classic(base_size = 12) +
  theme(
    text         = element_text(family = "serif"),
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 13,
                                family = "serif"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray30",
                                 family = "serif"),
    plot.caption = element_text(hjust = 0, size = 7.5, color = "gray40",
                                family = "serif"),
    legend.position = "none",
    axis.title   = element_text(family = "serif")
  )

print(p_tsne_main)
ggsave("tsne_segmentos_k2.pdf", p_tsne_main, width = 8, height = 6,
       device = cairo_pdf)
cat("\nGráfico principal guardado: tsne_segmentos_k2.pdf\n")

# =============================================================================
# 4. PANEL DE SENSIBILIDAD A LA PERPLEXITY (para audiencia técnica)
# =============================================================================
# Muestra que la separación de segmentos es estable bajo distintos valores
# de perplexity, reforzando que el resultado no es un artefacto del
# hiperparámetro.

plots_perp <- list()

for (perp in perplexity_vals) {
  
  set.seed(230125)
  tsne_p <- Rtsne(X_pca4_clean, dims = 2, perplexity = perp,
                  max_iter = 1000, theta = 0.5, pca = FALSE,
                  normalize = TRUE, verbose = FALSE)
  
  df_p <- data.frame(tSNE1 = tsne_p$Y[, 1], tSNE2 = tsne_p$Y[, 2],
                     Segmento = base_clean$Segmento)
  
  plots_perp[[as.character(perp)]] <- ggplot(df_p,
                                             aes(x = tSNE1, y = tSNE2, color = Segmento)) +
    geom_point(size = 1.0, alpha = 0.65) +
    stat_ellipse(aes(fill = Segmento), geom = "polygon",
                 type = "norm", level = 0.95, alpha = 0.10,
                 linewidth = 0.5, show.legend = FALSE) +
    scale_color_manual(values = paleta_seg) +
    scale_fill_manual(values = paleta_seg) +
    labs(title = paste0("Perplexity = ", perp),
         x = "t-SNE 1", y = "t-SNE 2") +
    theme_classic(base_size = 10) +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(face = "bold", hjust = 0.5,
                                    family = "serif", size = 10),
          legend.position = "none",
          axis.title = element_text(family = "serif", size = 8))
}

panel_perp <- (plots_perp[["20"]] | plots_perp[["30"]] | plots_perp[["50"]]) +
  plot_annotation(
    title    = "Estabilidad de la Solución t-SNE ante Distintos Valores de Perplexity",
    subtitle = "La separación de segmentos Corporate / Transactional es robusta al hiperparámetro",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 11, hjust = 0.5,
                                   family = "serif"),
      plot.subtitle = element_text(size = 8.5, hjust = 0.5, color = "gray30",
                                   family = "serif")
    )
  )

print(panel_perp)
ggsave("tsne_sensibilidad_perplexity.pdf", panel_perp, width = 12, height = 4.5,
       device = cairo_pdf)
cat("Panel de sensibilidad guardado: tsne_sensibilidad_perplexity.pdf\n")

# =============================================================================
# 5. VERIFICACIÓN CUANTITATIVA: SEPARABILIDAD LINEAL EN ESPACIO t-SNE
# =============================================================================
# Mide qué tan bien separados están los segmentos en el espacio 2D de t-SNE
# usando el índice de silueta sobre las coordenadas t-SNE.
# Si silueta_tsne ≈ silueta_pca4 (0.526), la proyección 2D preserva bien
# la estructura de separación. Si es mayor, t-SNE revela separación más clara.

library(cluster)
dist_tsne <- dist(tsne_final$Y)
segmento_num <- as.integer(factor(base_clean$Segmento))
sil_tsne <- silhouette(segmento_num, dist_tsne)

cat("\n==================================================================\n")
cat("  VERIFICACIÓN: SILUETA EN ESPACIO t-SNE\n")
cat("==================================================================\n")
cat(sprintf("  Silueta promedio en espacio PCA-4:  0.5263 (referencia)\n"))
cat(sprintf("  Silueta promedio en espacio t-SNE:  %.4f\n",
            mean(sil_tsne[, 3])))
cat("  (Valores similares confirman que t-SNE preserva la estructura\n")
cat("   de separación del espacio PCA-4 en 2 dimensiones.)\n")

# =============================================================================
# 6. GRÁFICO PARA AUDIENCIA NO TÉCNICA (versión simplificada sin ejes técnicos)
# =============================================================================

etiquetas_negocio <- centroides_tsne %>%
  mutate(
    Label = case_when(
      Segmento == "Corporate"     ~ "Clientes Corporativos\n(Alta escala · Spread bajo · Actividad diaria)",
      Segmento == "Transactional" ~ "Clientes Transaccionales\n(Escala media · Spread alto · Actividad episódica)"
    )
  )

p_tsne_negocio <- ggplot(df_tsne, aes(x = tSNE1, y = tSNE2,
                                      color = Segmento, fill = Segmento)) +
  stat_ellipse(geom = "polygon", type = "norm", level = 0.90,
               alpha = 0.15, linewidth = 0.8) +
  geom_point(aes(shape = Segmento), size = 2, alpha = 0.6) +
  geom_label_repel(
    data = etiquetas_negocio,
    aes(label = Label),
    color = "white", fontface = "bold", size = 3.2,
    family = "serif",
    fill = c("#1F3B4D", "#D97724"),
    label.padding = unit(0.5, "lines"),
    label.r = unit(0.4, "lines"),
    min.segment.length = 0,
    box.padding = 1.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = paleta_seg) +
  scale_fill_manual(values = paleta_seg) +
  scale_shape_manual(values = c("Corporate" = 17, "Transactional" = 16)) +
  labs(
    title    = "Dos Segmentos Naturales en la Cartera de Clientes FX",
    subtitle = "Cada punto representa un cliente. Los grupos emergen del comportamiento transaccional observado.",
    x        = NULL, y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    text          = element_text(family = "serif"),
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 13,
                                 family = "serif"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray30",
                                 family = "serif"),
    axis.text     = element_blank(),
    axis.ticks    = element_blank(),
    axis.line     = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "gray98", color = NA)
  )

print(p_tsne_negocio)
ggsave("tsne_audiencia_negocio.pdf", p_tsne_negocio, width = 9, height = 6.5,
       device = cairo_pdf)
cat("\nGráfico para audiencia no técnica guardado: tsne_audiencia_negocio.pdf\n")

cat("\n==================================================================\n")
cat("  ARCHIVOS GENERADOS\n")
cat("==================================================================\n")
cat("  tsne_segmentos_k2.pdf            -> Gráfico técnico principal\n")
cat("  tsne_sensibilidad_perplexity.pdf -> Panel de robustez al hiperparámetro\n")
cat("  tsne_audiencia_negocio.pdf       -> Versión para audiencia ejecutiva\n")
