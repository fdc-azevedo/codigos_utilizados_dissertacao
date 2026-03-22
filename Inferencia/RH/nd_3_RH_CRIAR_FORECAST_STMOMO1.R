# =============================================================================
# 3_renshaw_nd_CRIAR_FORECAST_STMOMO
# - Forecast RH usando StMoMo::forecast
# - Série observada 2000–2024 (real) padronizada por idade
# - Série de treino ajustada 2000–2019 via fitted() (mantida separada)
# - Pesos fixos no último ano do treino
# - IC 95% separado em 2020–2024 e 2025–2029
# - Gráfico no mesmo padrão do Lee-Carter
# =============================================================================

library(StMoMo)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1) CHECAGENS
# -----------------------------------------------------------------------------
stopifnot(exists("fit_renshaw_nd"))
stopifnot(inherits(fit_renshaw_nd, "fitStMoMo"))
stopifnot(exists("periodo_treino_renshaw_nd"))
stopifnot(exists("DF_FAIXA"))
stopifnot(exists("faixas_excluir"))

# -----------------------------------------------------------------------------
# 2) DEFINIÇÕES
# -----------------------------------------------------------------------------
ano_base_pesos_renshaw_nd <- tail(periodo_treino_renshaw_nd, 1)
anos_obs_renshaw_nd  <- 2000:2024
anos_proj_renshaw_nd <- 2020:2029


# -----------------------------------------------------------------------------
# 3) BASE OBSERVADA (2000–2024) PARA PESOS + HISTÓRICO
# -----------------------------------------------------------------------------
dados_prep_obs_renshaw_nd <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% anos_obs_renshaw_nd) %>%
  filter(LOCAL == "Nordeste") %>%
  rename(
    ANO = Atributo,
    POP = Pop,
    MORTES = CM,
    FAIXA_ORIG = FAIXA
  ) %>%
  mutate(
    idade_min = as.numeric(sub("-.*", "", FAIXA_ORIG)),
    idade_max = as.numeric(sub(".*-", "", FAIXA_ORIG)),
    IDADE_CENTRAL = (idade_min + idade_max) / 2
  )

stopifnot(ano_base_pesos_renshaw_nd %in% dados_prep_obs_renshaw_nd$ANO)

# Pesos etários fixos no último ano do treino
w_idade_renshaw_nd <- dados_prep_obs_renshaw_nd %>%
  filter(ANO == ano_base_pesos_renshaw_nd) %>%
  group_by(IDADE_CENTRAL) %>%
  summarise(w = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  mutate(w = w / sum(w))

# Alinhar pesos às idades do modelo
w_alinhado_renshaw_nd <- data.frame(IDADE_CENTRAL = fit_renshaw_nd$ages) %>%
  left_join(w_idade_renshaw_nd, by = "IDADE_CENTRAL") %>%
  filter(!is.na(w))

idx_renshaw_nd <- match(w_alinhado_renshaw_nd$IDADE_CENTRAL, fit_renshaw_nd$ages)
stopifnot(all(is.finite(idx_renshaw_nd)))

w_vec_renshaw_nd <- w_alinhado_renshaw_nd$w / sum(w_alinhado_renshaw_nd$w)

# -----------------------------------------------------------------------------
# 4) SÉRIE OBSERVADA REAL PADRONIZADA (2000–2024)
# -----------------------------------------------------------------------------
taxa_observada_renshaw_nd <- dados_prep_obs_renshaw_nd %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(w_idade_renshaw_nd, by = "IDADE_CENTRAL") %>%
  filter(!is.na(w)) %>%
  mutate(taxa_obs_100k = (D / pmax(E, 1e-12)) * 1e5) %>%
  group_by(ANO) %>%
  summarise(Taxa = sum(taxa_obs_100k * w, na.rm = TRUE), .groups = "drop") %>%
  transmute(
    Ano = ANO,
    Taxa = Taxa,
    Tipo = "Observado",
    Periodo = "2000-2024"
  )

# -----------------------------------------------------------------------------
# 5) SÉRIE DE TREINO AJUSTADA (2000–2019) VIA fitted()
#    Mantida separada, mas não entra no gráfico final
# -----------------------------------------------------------------------------
taxas_fit_hist_renshaw_nd <- fitted(fit_renshaw_nd, type = "rates")[, as.character(periodo_treino_renshaw_nd), drop = FALSE] * 1e5

taxa_treino_renshaw_nd <- data.frame(
  Ano = periodo_treino_renshaw_nd,
  Taxa = as.numeric(
    colSums(
      taxas_fit_hist_renshaw_nd[idx_renshaw_nd, , drop = FALSE] * w_vec_renshaw_nd,
      na.rm = TRUE
    )
  ),
  Tipo = "Treino",
  Periodo = "2000-2019"
)

# -----------------------------------------------------------------------------
# 6) FORECAST RH
# -----------------------------------------------------------------------------
gc_len_renshaw_nd <- length(fit_renshaw_nd$gc)
kt_len_renshaw_nd <- length(as.numeric(fit_renshaw_nd$kt[1, ]))

gc_order_1_renshaw_nd <- if (gc_len_renshaw_nd >= 8) c(0, 1, 0) else c(0, 0, 0)
kt_order_1_renshaw_nd <- if (kt_len_renshaw_nd >= 8) c(0, 1, 0) else c(0, 0, 0)

h <- length(anos_proj_renshaw_nd)

for_renshaw_nd <- forecast(
  fit_renshaw_nd,
  jumpchoice = "actual",
  h = h,
  level = 95,
  kt.order = kt_order_1_renshaw_nd,
  kt.include.constant = TRUE,
  gc.order = gc_order_1_renshaw_nd,
  gc.include.constant = TRUE
)

# Taxas projetadas médias
taxas_proj_renshaw_nd <- for_renshaw_nd$rates * 1e5

taxa_projetada_renshaw_nd <- data.frame(
  Ano = as.numeric(for_renshaw_nd$years),
  Taxa = as.numeric(
    colSums(
      taxas_proj_renshaw_nd[idx_renshaw_nd, , drop = FALSE] * w_vec_renshaw_nd,
      na.rm = TRUE
    )
  ),
  Tipo = "Projetado",
  Periodo = ifelse(as.numeric(for_renshaw_nd$years) %in% 2020:2024, "2020-2024", "2025-2029")
)

# Auditoria dos índices
kt_forecast_renshaw_nd <- for_renshaw_nd$kt.f
gc_forecast_renshaw_nd <- for_renshaw_nd$gc.f

# -----------------------------------------------------------------------------
# 7) IC 95% VIA SIMULAÇÃO
# -----------------------------------------------------------------------------
set.seed(123)

nsim_renshaw_nd <- 2000
sim_renshaw_nd <- simulate(fit_renshaw_nd, 
                           nsim = nsim_renshaw_nd, 
                           jumpchoice = "actual",
                           h = h)

taxas_sim_renshaw_nd <- sim_renshaw_nd$rates
stopifnot(!is.null(taxas_sim_renshaw_nd))
stopifnot(length(dim(taxas_sim_renshaw_nd)) == 3)

taxas_sim100k_renshaw_nd <- taxas_sim_renshaw_nd * 1e5

# Resultado: matriz [ano, simulacao]
taxa_pad_sim_renshaw_nd <- apply(
  taxas_sim100k_renshaw_nd[idx_renshaw_nd, , , drop = FALSE],
  c(2, 3),
  function(v) sum(v * w_vec_renshaw_nd, na.rm = TRUE)
)

taxa_mean_renshaw_nd <- apply(taxa_pad_sim_renshaw_nd, 1, mean, na.rm = TRUE)
taxa_lwr_renshaw_nd  <- apply(taxa_pad_sim_renshaw_nd, 1, quantile, probs = 0.025, na.rm = TRUE)
taxa_upr_renshaw_nd  <- apply(taxa_pad_sim_renshaw_nd, 1, quantile, probs = 0.975, na.rm = TRUE)

ic_taxa_renshaw_nd <- data.frame(
  Ano = anos_proj_renshaw_nd,
  Taxa = as.numeric(taxa_mean_renshaw_nd),
  Taxa_lower_95 = as.numeric(taxa_lwr_renshaw_nd),
  Taxa_upper_95 = as.numeric(taxa_upr_renshaw_nd),
  Periodo = ifelse(anos_proj_renshaw_nd %in% 2020:2024, "2020-2024", "2025-2029")
)

# -----------------------------------------------------------------------------
# 8) GRÁFICO FINAL
#    Mesmo padrão do Lee-Carter:
#    - observado real 2000–2024
#    - projetado 2020–2029
#    - IC verde 2020–2024
#    - IC azul 2025–2029
# -----------------------------------------------------------------------------
dados_totais_renshaw_nd <- bind_rows(taxa_observada_renshaw_nd, taxa_projetada_renshaw_nd)
dados_ic_verde_renshaw_nd <- subset(ic_taxa_renshaw_nd, Periodo == "2020-2024")
dados_ic_azul_renshaw_nd  <- subset(ic_taxa_renshaw_nd, Periodo == "2025-2029")

cores_manual_renshaw_nd <- c(
  "2000-2024" = "black",
  "2020-2024" = "#2E8B57",
  "2025-2029" = "#1E90FF"
)

p_forecast_renshaw_nd <- ggplot() +
  geom_ribbon(
    data = dados_ic_verde_renshaw_nd,
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#2E8B57", alpha = 0.2
  ) +
  geom_ribbon(
    data = dados_ic_azul_renshaw_nd,
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#1E90FF", alpha = 0.2
  ) +
  geom_line(
    data = subset(dados_totais_renshaw_nd, Tipo == "Observado"),
    aes(x = Ano, y = Taxa, color = Periodo),
    linewidth = 1.2
  ) +
  geom_line(
    data = subset(dados_totais_renshaw_nd, Tipo == "Projetado"),
    aes(x = Ano, y = Taxa, color = Periodo),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_point(
    data = dados_totais_renshaw_nd,
    aes(x = Ano, y = Taxa, color = Periodo),
    size = 2
  ) +
  geom_vline(
    xintercept = ano_base_pesos_renshaw_nd + 0.5,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8,
    alpha = 0.8
  ) +
  scale_color_manual(values = cores_manual_renshaw_nd) +
  scale_x_continuous(breaks = c(2000, 2009, 2019, 2029)) +
  scale_y_continuous(breaks = seq(10, 50, by = 10),limits = c(5, 60), ) +
  labs(
    title = "Nordeste",
    x = "Ano",
    y = "Taxa por 100.000 mulheres",
    color = "Período"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
  )

plot(p_forecast_renshaw_nd)

# -----------------------------------------------------------------------------
# 9) RMSE E MAPE
#    Comparação correta: observado real vs projetado, em 2020–2024
# -----------------------------------------------------------------------------
anos_eval_renshaw_nd <- 2020:2024

obs_eval_renshaw_nd <- taxa_observada_renshaw_nd %>%
  filter(Ano %in% anos_eval_renshaw_nd) %>%
  arrange(Ano)

pred_eval_renshaw_nd <- taxa_projetada_renshaw_nd %>%
  filter(Ano %in% anos_eval_renshaw_nd) %>%
  arrange(Ano)

stopifnot(nrow(obs_eval_renshaw_nd) == length(anos_eval_renshaw_nd))
stopifnot(nrow(pred_eval_renshaw_nd) == length(anos_eval_renshaw_nd))

y_obs_renshaw_nd <- obs_eval_renshaw_nd$Taxa
y_hat_renshaw_nd <- pred_eval_renshaw_nd$Taxa

RMSE_nd <- sqrt(mean((y_hat_renshaw_nd - y_obs_renshaw_nd)^2))
MAPE_nd <- mean(abs((y_obs_renshaw_nd - y_hat_renshaw_nd) / y_obs_renshaw_nd)) * 100

RMSE_nd
MAPE_nd
