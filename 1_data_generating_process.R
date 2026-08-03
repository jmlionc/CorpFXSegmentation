# =============================================================================
# SCRIPT COMPLETO: GENERACIÓN DE DATASETS FX
# =============================================================================

library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)
library(MASS)
library(stringr)

set.seed(230125)

# =============================================================================
# PARTE 1: BASE TRANSACCIONAL — SIN MODIFICACIONES
# =============================================================================

n_clients             <- 500
n_sales               <- 5
n_transactions_target <- 35000

clientes_df <- tibble(
  Client_ID   = paste0("CLI_", str_pad(1:n_clients, 3, pad = "0")),
  Client_Name = paste("Empresa", 1:n_clients)
) %>%
  mutate(
    Archetype  = c(rep("Corporate", 100), rep("Transactional", 400)),
    Sales_ID   = case_when(
      row_number() <= 160 ~ sample(paste("Sales", 2:5), size = n(), replace = TRUE),
      TRUE ~ "Sales 1"
    ),
    lambda_deals = if_else(
      Archetype == "Corporate",
      rgamma(n(), shape = 5, rate = 0.05),
      rgamma(n(), shape = 2, rate = 0.08)
    ),
    prob_digital     = if_else(Archetype == "Corporate",
                               runif(n(), 0.6, 0.9),
                               runif(n(), 0.8, 1.0)),
    base_vol_meanlog = if_else(Archetype == "Corporate", 13.97, 10.97)
  )

factor_ajuste <- n_transactions_target / sum(clientes_df$lambda_deals)
clientes_df <- clientes_df %>%
  mutate(
    lambda_deals = lambda_deals * factor_ajuste,
    Target_Deals = rpois(n(), lambda_deals)
  )

fechas_2025 <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "days")
df_fechas <- tibble(Fecha = fechas_2025) %>%
  mutate(
    Dia_Semana = wday(Fecha, label = TRUE, abbr = FALSE, week_start = 1),
    Dia_Mes    = day(Fecha)
  ) %>%
  filter(Dia_Semana %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))

prob_wday <- c("lunes" = 0.15, "martes" = 0.25,
               "miércoles" = 0.23, "jueves" = 0.20, "viernes" = 0.18)

df_fechas <- df_fechas %>%
  mutate(
    Prob_Wday   = prob_wday[as.character(Dia_Semana)],
    Tercil      = case_when(
      Dia_Mes <= 10 ~ "Early",
      Dia_Mes <= 20 ~ "Mid",
      TRUE          ~ "Late"
    ),
    Prob_Tercil = case_when(
      Tercil == "Early" ~ 0.25,
      Tercil == "Mid"   ~ 0.35,
      Tercil == "Late"  ~ 0.40
    ),
    Prob_Final  = Prob_Wday * Prob_Tercil
  )

generar_horas <- function(n) {
  horas_operacion   <- 9
  minutos_totales   <- horas_operacion * 60
  minutos_simulados <- rbeta(n, shape1 = 2, shape2 = 5) * minutos_totales
  hora_apertura     <- as.POSIXct("2026-01-01 07:30:00")
  format(hora_apertura + dseconds(minutos_simulados * 60), "%H:%M:%S")
}

base_transaccional <- clientes_df %>%
  uncount(Target_Deals) %>%
  mutate(
    Deal_Number    = paste0("TRX_", str_pad(row_number(), 6, pad = "0")),
    Deal_Date_Only = sample(df_fechas$Fecha, size = n(), replace = TRUE,
                            prob = df_fechas$Prob_Final),
    Deal_Time      = generar_horas(n()),
    Deal_Date      = as.POSIXct(paste(Deal_Date_Only, Deal_Time)),
    Channel        = ifelse(runif(n()) <= 0.80, "Digital", "Manual"),
    Product        = sample(c("Spot", "Forward"), size = n(),
                            replace = TRUE, prob = c(0.95, 0.05)),
    Settlement_Lag = ifelse(Product == "Spot",
                            sample(0:2, n(), replace = TRUE),
                            sample(3:180, n(), replace = TRUE)),
    Flow_Direction = sample(c("Sell USD", "Buy USD", "Non-USD"),
                            size = n(), replace = TRUE,
                            prob = c(0.50, 0.40, 0.10)),
    Currency_Pair  = case_when(
      Flow_Direction == "Non-USD" ~
        sample(c("EUR/GBP", "EUR/JPY", "GBP/JPY"), n(), replace = TRUE),
      runif(n()) <= 0.95 ~ "USD/COP",
      TRUE ~ sample(c("EUR/USD", "GBP/USD"), n(), replace = TRUE)
    ),
    Buy_CCY  = case_when(
      Flow_Direction == "Buy USD"  ~ "USD",
      Flow_Direction == "Sell USD" ~
        if_else(Currency_Pair == "USD/COP", "COP", substr(Currency_Pair, 1, 3)),
      TRUE ~ substr(Currency_Pair, 1, 3)
    ),
    Sell_CCY = case_when(
      Flow_Direction == "Buy USD"  ~
        if_else(Currency_Pair == "USD/COP", "COP", substr(Currency_Pair, 5, 7)),
      Flow_Direction == "Sell USD" ~ "USD",
      TRUE ~ substr(Currency_Pair, 5, 7)
    ),
    USD_Volume     = pmax(1001, rlnorm(n(), meanlog = base_vol_meanlog, sdlog = 1.2)),
    logit_zero     = -2.5 + 0.0000005 * USD_Volume + 0.5 * (Channel == "Digital"),
    prob_zero      = plogis(logit_zero),
    is_zero_spread = runif(n()) < prob_zero
  )

threshold_15 <- quantile(base_transaccional$prob_zero, 0.85)
base_transaccional$is_zero_spread <- base_transaccional$prob_zero >= threshold_15

base_transaccional <- base_transaccional %>%
  mutate(
    log_spread  = 8.95 - 0.35 * log(USD_Volume) -
      0.2 * (Channel == "Digital") + rnorm(n(), 0, 0.3),
    Spread_BPS  = ifelse(is_zero_spread, 0, exp(log_spread)),
    USD_Revenue = USD_Volume * (Spread_BPS / 10000),
    Market_Rate = case_when(
      Currency_Pair == "USD/COP" ~ rnorm(n(), mean = 4052.86, sd = 120),
      Currency_Pair == "EUR/USD" ~ rnorm(n(), mean = 1.1306,  sd = 0.03),
      Currency_Pair == "GBP/USD" ~ rnorm(n(), mean = 1.2850,  sd = 0.03),
      Currency_Pair == "EUR/GBP" ~
        rnorm(n(), mean = 1.1306, sd = 0.01) / rnorm(n(), mean = 1.2850, sd = 0.01),
      Currency_Pair == "EUR/JPY" ~
        rnorm(n(), mean = 1.1306, sd = 0.01) * rnorm(n(), mean = 150.00, sd = 4.00),
      Currency_Pair == "GBP/JPY" ~
        rnorm(n(), mean = 1.2850, sd = 0.01) * rnorm(n(), mean = 150.00, sd = 4.00),
      TRUE ~ 1.00
    ),
    Market_Rate = if_else(Currency_Pair == "USD/COP",
                          pmax(3706.94, pmin(4410.50, Market_Rate)),
                          Market_Rate),
    Deal_Rate   = case_when(
      Currency_Pair == "USD/COP" & Flow_Direction == "Buy USD"  ~
        Market_Rate - (Market_Rate * (Spread_BPS / 10000)),
      Currency_Pair == "USD/COP" & Flow_Direction == "Sell USD" ~
        Market_Rate + (Market_Rate * (Spread_BPS / 10000)),
      Flow_Direction == "Buy USD"  ~
        Market_Rate - (Market_Rate * (Spread_BPS / 10000)),
      Flow_Direction == "Sell USD" ~
        Market_Rate + (Market_Rate * (Spread_BPS / 10000)),
      TRUE ~
        Market_Rate * (1 + if_else(runif(n()) > 0.5, 1, -1) * (Spread_BPS / 10000))
    )
  ) %>%
  dplyr::select(
    Deal_Number, Deal_Date, Client_Name, Client_ID, Sales_ID, Channel,
    Product, Settlement_Lag, USD_Revenue, USD_Volume, Spread_BPS,
    Buy_CCY, Sell_CCY, Market_Rate, Deal_Rate, Flow_Direction, Currency_Pair
  )

# =============================================================================
# PARTE 2: BASE CLIENTES — VERSIÓN FINAL CON CORRECCIONES INTEGRADAS
# =============================================================================

fecha_referencia       <- as.POSIXct("2025-12-31 23:59:59")
mediana_portafolio_bps <- median(base_transaccional$Spread_BPS[
  base_transaccional$Spread_BPS > 0], na.rm = TRUE)
revenue_total_banco    <- sum(base_transaccional$USD_Revenue, na.rm = TRUE)

# -----------------------------------------------------------------------------
# 2.1 Paso 1: métricas de agregación directa (Grupo 1) + variables
#     que se calculan íntegramente dentro del group_by
# -----------------------------------------------------------------------------
base_clientes <- base_transaccional %>%
  group_by(Client_ID, Client_Name, Sales_ID) %>%
  summarise(
    
    # ---- GRUPO 1: variables de agregación directa (sin cambios) ----
    Count_of_Deals     = n(),
    Bank_Volume        = sum(USD_Volume),
    Total_Revenue      = sum(USD_Revenue),
    Average_Deal_Size  = mean(USD_Volume),
    Average_Revenue    = mean(USD_Revenue),
    Average_Spread_BPS = mean(Spread_BPS),
    Porcentaje_Spot    = mean(Product == "Spot"),
    Porcentaje_Digital = mean(Channel == "Digital"),
    Porcentaje_COP     = mean(Currency_Pair == "USD/COP"),
    Revenue_Share_Pct  = Total_Revenue / revenue_total_banco,
    Active_Days        = n_distinct(as.Date(Deal_Date)),
    Ultima_Trx         = max(Deal_Date),
    Recency_Days       = as.numeric(difftime(fecha_referencia,
                                             Ultima_Trx, units = "days")),
    Pct_Buy_USD        = mean(Flow_Direction == "Buy USD"),
    Pct_Sell_USD       = mean(Flow_Direction == "Sell USD"),
    Pct_NonUSD         = mean(Flow_Direction == "Non-USD"),
    
    # ---- GRUPO 2a: Spread_Price_Sensitivity — Spearman ----
    # Correlación de Spearman entre volumen y spread intra-cliente.
    # Más robusta que Pearson ante la distribución lognormal del volumen
    # y ante el efecto de operaciones atípicas de un solo cliente.
    # Signo negativo esperado: economías de escala individuales.
    Spread_Price_Sensitivity = ifelse(
      Count_of_Deals > 1,
      cor(Spread_BPS, USD_Volume, method = "spearman"),
      NA_real_
    ),
    
    # ---- GRUPO 2b: USD_Flow_Direction — log-ratio de VOLUMEN ----
    # CORRECCIÓN v3.0: el índice normalizado por frecuencia (v2.0) seguía
    # con r=0.957 frente a Pct_Buy_USD porque Non-USD (~10%) hace que el
    # denominador sea casi constante. Se reemplaza por el log-ratio del
    # VOLUMEN comprado vs. vendido en USD, que captura si el cliente mueve
    # más capital comprando o vendiendo dólares, independientemente de la
    # frecuencia de operaciones.
    # Rango: (-Inf, +Inf) en teoría; típico [-4, +4].
    # > 0: comprador neto en términos de monto (importador / cobertura de deuda)
    # < 0: vendedor neto en términos de monto (exportador / repatriación)
    # ≈ 0: cliente balanceado o predominantemente Non-USD
    # Referencia: Kyle (1985) Order Imbalance, literatura de microestructura FX.
    USD_Flow_Direction = log(
      (sum(USD_Volume[Flow_Direction == "Buy USD"],  na.rm = TRUE) + 1) /
        (sum(USD_Volume[Flow_Direction == "Sell USD"], na.rm = TRUE) + 1)
    ),
    
    # ---- GRUPO 3a: CV_Volume ----
    # Coeficiente de variación intra-cliente del volumen.
    # Complementa Average_Deal_Size de forma genuinamente independiente:
    # dos clientes con igual promedio pueden diferir mucho en consistencia
    # de tamaño (uno siempre opera ~$1M, otro alterna $100K y $10M).
    CV_Volume = ifelse(Count_of_Deals > 1,
                       sd(USD_Volume) / mean(USD_Volume),
                       0),
    
    # ---- GRUPO 3b: Revenue_per_Day ----
    # Intensidad de revenue por día de actividad.
    # NOTA: Esta variable tiene r=0.937 con Total_Revenue (diagnosticado
    # en la fase de verificación) porque Active_Days tiene rango comprimido
    # relativo a Total_Revenue. Se conserva en base_clientes como variable
    # descriptiva para perfilamiento posterior de clusters, pero NO se
    # incluye en el vector de características del PCA (vars_finales).
    Revenue_per_Day = Total_Revenue / Active_Days,
    
    # ---- GRUPO 3c: HHI_Temporal ----
    # Índice Herfindahl-Hirschman sobre la distribución de transacciones
    # en los días activos. HHI = sum(s_k^2).
    # HHI → 1: actividad episódica (todo en pocos días).
    # HHI → 1/Active_Days: actividad perfectamente distribuida.
    # Captura "episodicidad vs. regularidad" de forma no lineal.
    HHI_Temporal = sum((table(as.Date(Deal_Date)) / Count_of_Deals)^2),
    
    # ---- GRUPO 3d: Diversification_Score — entropía sobre pares no-COP ----
    # CORRECCIÓN v3.0: la entropía global tenía r=-0.901 con Porcentaje_COP
    # porque USD/COP domina el 85.6% de las transacciones, haciendo que la
    # entropía global sea casi función monotónica de la participación COP.
    # Se redefine como la entropía de Shannon calculada SOLO sobre los pares
    # de monedas distintos de USD/COP. Mide diversificación del portafolio
    # internacional del cliente, independientemente de su exposición al peso.
    # H = 0: cliente que nunca operó pares internacionales (o solo un par).
    # H > 0: cliente con actividad distribuida en múltiples pares no-COP.
    Diversification_Score = {
      pares_nonCOP <- Currency_Pair[Currency_Pair != "USD/COP"]
      if (length(pares_nonCOP) == 0) {
        0
      } else {
        freq <- table(pares_nonCOP) / length(pares_nonCOP)
        -sum(freq * log(freq + 1e-10))
      }
    },
    
    .groups = "drop"
  ) %>%
  
  # ---------------------------------------------------------------------------
# 2.2 Paso 2: variables que requieren mutate (usan columnas del paso 1)
# ---------------------------------------------------------------------------
mutate(
  
  # Reemplazar NAs de clientes con una sola operación
  Spread_Price_Sensitivity = replace_na(Spread_Price_Sensitivity, 0),
  
  # ---- GRUPO 2c: Spread_vs_Benchmark ----
  # Desviación del spread promedio del cliente respecto al spread esperado
  # por el modelo de economías de escala estimado en el EDA:
  #   E[Spread | Volume = v] = exp(8.95 - 0.35 * log(v))
  # Spread_vs_Benchmark > 0: cliente paga más de lo que su escala predice
  #   (bajo poder de negociación o relación no competida).
  # Spread_vs_Benchmark < 0: cliente paga menos de lo predicho
  #   (acuerdos preferenciales, relación estratégica, alta competencia).
  # Tiene varianza genuina porque Average_Deal_Size varía por cliente
  # y la función es logarítmica (no lineal).
  spread_esperado     = exp(8.95 - 0.35 * log(Average_Deal_Size)),
  Spread_vs_Benchmark = Average_Spread_BPS - spread_esperado,
  
  # ---- GRUPO 2d: CLV_Proxy — CORRECCIÓN v3.0 ----
  # Estimador del valor de vida del cliente con incertidumbre de proyección.
  # Componentes:
  #   (1) Total_Revenue * exp(-Recency_Days/365): valor histórico ajustado
  #       por decaimiento exponencial ante inactividad reciente.
  #   (2) (1 + 0.3 * Porcentaje_Digital): prima de retención para clientes
  #       digitales (Hoehle & Venkatesh, 2015).
  #   (3) rgamma(shape=3, rate=3): factor de incertidumbre individual.
  #       shape=3 (vs. 6 de v2.0) eleva el CV del factor de ~0.41 a ~0.58,
  #       reduciendo la correlación con Total_Revenue de r=0.900 a ~0.75-0.82.
  CLV_Proxy = Total_Revenue *
    exp(-Recency_Days / 365) *
    (1 + 0.3 * Porcentaje_Digital) *
    rgamma(n(), shape = 3, rate = 3),
  
  # ---- GRUPO 2e: sow_score y Share_of_Wallet ----
  # sow_score reformulado (v2.0): elimina Average_Deal_Size para evitar
  # circularidad; sd=1.5 en lugar de 0.3 para desacoplar Share_of_Wallet
  # de sus determinantes (r esperado 0.40-0.65 vs. r≈0.98 original).
  sow_score = 0.8 +
    1.5 * Porcentaje_Digital +
    1.2 * Porcentaje_COP +
    0.8 * as.numeric(scale(Active_Days)) +
    -0.6 * as.numeric(scale(Recency_Days)) +
    rnorm(n(), mean = 0, sd = 1.5),
  
  Share_of_Wallet = 0.20 + (0.98 - 0.20) * (1 / (1 + exp(-sow_score))),
  Total_Wallet    = Bank_Volume / Share_of_Wallet
)

# =============================================================================
# PARTE 3: GUARDADO
# =============================================================================
saveRDS(base_transaccional, "base_transaccional.rds")
saveRDS(base_clientes,      "base_clientes.rds")
cat("\nArchivos guardados: base_transaccional.rds, base_clientes.rds\n")
