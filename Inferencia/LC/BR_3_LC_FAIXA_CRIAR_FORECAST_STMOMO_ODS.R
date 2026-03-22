# =============================================================================
# 3_LC_FAIXA_CRIAR_FORECAST_STMOMO
# - Projeção Lee-Carter via StMoMo::forecast(fitStMoMo)
# - Taxa padronizada por idade (pesos fixos no último ano do treino)
# - IC 95% separado em 2020–2024 e 2025–2029
# - Overlay: taxa REALIZADA 2020–2024 (bruta)
# - Linha meta: ODS 3.4 = (taxa 2014)/3
# =============================================================================

library(StMoMo)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1) CHECAGENS (contrato com Step 1)
# -----------------------------------------------------------------------------
stopifnot(exists("LCfit"), inherits(LCfit, "fitStMoMo"))
stopifnot(exists("dados_stmomo"))
stopifnot(exists("periodo_treino"))

# -----------------------------------------------------------------------------
# 2) PADRONIZAÇÃO ETÁRIA (pesos fixos no último ano do treino)
# -----------------------------------------------------------------------------
ultimo_ano <- tail(periodo_treino, 1)
stopifnot(as.character(ultimo_ano) %in% colnames(dados_stmomo$Ext))

prop_idade <- dados_stmomo$Ext[, as.character(ultimo_ano)] |>
  (\(x) x / sum(x))()

# -----------------------------------------------------------------------------
# 3) HISTÓRICO AJUSTADO (in-sample) — modelo, não observado (robusto)
# -----------------------------------------------------------------------------
taxa_historica <- fitted(LCfit, type = "rates")[, as.character(periodo_treino), drop = FALSE] %>%
  as.matrix() %>%                            # garante 2D
  (`*`)(1e5) %>%                             # por 100k
  (\(m) as.numeric(t(prop_idade) %*% m))() %>% # média ponderada por idade (1 x T)
  tibble::tibble(
    Ano = periodo_treino,
    Taxa = .,
    Serie = "Ajustado (LC)",
    Periodo = "2000-2019"
  )

# -----------------------------------------------------------------------------
# 4) FORECAST (StMoMo) + IC via kt.f (propagado para taxas)
# -----------------------------------------------------------------------------
anos_proj <- 2020:2029
LCfor <- forecast(LCfit, h = length(anos_proj), level = 95)

stopifnot(!is.null(LCfor$kt.f))
ax <- as.numeric(LCfit$ax)
bx <- as.numeric(LCfit$bx)

taxa_projetada <- tibble(
  Ano = anos_proj,
  kt_mean = as.numeric(LCfor$kt.f$mean),
  kt_low  = as.numeric(LCfor$kt.f$lower),
  kt_up   = as.numeric(LCfor$kt.f$upper)
) |>
  rowwise() |>
  mutate(
    Taxa = sum(exp(ax + bx * kt_mean) * 1e5 * prop_idade, na.rm = TRUE),
    Taxa_lower_95 = sum(exp(ax + bx * kt_low)  * 1e5 * prop_idade, na.rm = TRUE),
    Taxa_upper_95 = sum(exp(ax + bx * kt_up)   * 1e5 * prop_idade, na.rm = TRUE),
    Serie = "Projetado (LC)",
    Periodo = if_else(Ano <= 2024, "2020-2024", "2025-2029")
  ) |>
  ungroup() |>
  select(Ano, Taxa, Taxa_lower_95, Taxa_upper_95, Serie, Periodo)

# -----------------------------------------------------------------------------
# 5) REALIZADO (observado) 2020–2024 — overlay no gráfico
# -----------------------------------------------------------------------------
taxa_real_2020mais <- DF_FAIXA %>%
  filter(!FAIXA %in% c("0-29", "90+")) %>%
  rename(ANO = Atributo, POP = Pop, MORTES = CM, FAIXA_ORIG = FAIXA) %>%
  mutate(
    idade_min = as.numeric(sub("-.*", "", FAIXA_ORIG)),
    idade_max = as.numeric(sub(".*-", "", FAIXA_ORIG)),
    IDADE_CENTRAL = (idade_min + idade_max) / 2
  ) %>%
  filter(LOCAL == "Brasil", ANO %in% 2020:2024) %>%
  group_by(ANO) %>%
  summarise(Taxa = sum(MORTES, na.rm = TRUE) / sum(POP, na.rm = TRUE) * 1e5, .groups = "drop") %>%
  transmute(Ano = ANO, Taxa, Serie = "Realizado (bruto)", Periodo = "2020-2024")

# -----------------------------------------------------------------------------
# 6) LINHA ODS 3.4 = taxa(2014)/3
#    - usa a taxa AJUSTADA 2014 (do modelo) para ser consistente com o gráfico
#      (se quiser a BRUTA de 2014, eu troco para DF_FAIXA)
# -----------------------------------------------------------------------------
meta_ods_34 <- taxa_historica %>%
  filter(Ano == 2015) %>%
  summarise(meta = first(Taxa) * (2/3)) %>%
  pull(meta)

# -----------------------------------------------------------------------------
# 7) GRÁFICO (IC por período + overlay realizado + meta ODS)
# -----------------------------------------------------------------------------
cores_manual <- c("2000-2019"="black", "2020-2024"="#2E8B57", "2025-2029"="#1E90FF")

p_forecast_brasil <-
  ggplot() +
  # IC 2020–2024
  geom_ribbon(
    data = taxa_projetada %>% filter(Periodo == "2020-2024"),
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#2E8B57", alpha = 0.20
  ) +
  # IC 2025–2029
  geom_ribbon(
    data = taxa_projetada %>% filter(Periodo == "2025-2029"),
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#1E90FF", alpha = 0.20
  ) +
  # Histórico ajustado
  geom_line(
    data = taxa_historica,
    aes(x = Ano, y = Taxa, color = Periodo),
    linewidth = 1.2
  ) +
  geom_point(
    data = taxa_historica,
    aes(x = Ano, y = Taxa, color = Periodo),
    size = 2
  ) +
  # Projetado (pontual)
  geom_line(
    data = taxa_projetada,
    aes(x = Ano, y = Taxa, color = Periodo),
    linewidth = 1.2, linetype = "dashed"
  ) +
  geom_point(
    data = taxa_projetada,
    aes(x = Ano, y = Taxa, color = Periodo),
    size = 2
  ) +
  # Realizado bruto 2020–2024 (overlay)
  geom_line(
    data = taxa_real_2020mais,
    aes(x = Ano, y = Taxa),
    linewidth = 1.2
  ) +
  geom_point(
    data = taxa_real_2020mais,
    aes(x = Ano, y = Taxa),
    size = 2
  ) +
  # corte treino / projeção
  geom_vline(
    xintercept = max(periodo_treino) + 0.5,
    linetype = "dashed", color = "red", alpha = 0.7
  ) +
  # meta ODS 3.4 (tracejada)
  geom_hline(
    yintercept = meta_ods_34,
    linetype = "dashed", linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 2024, y = meta_ods_34,
    label = "ODS 3.4",
    hjust = -0.05, vjust = -0.6, size = 3.6
  ) +
  scale_color_manual(values = cores_manual) +
  scale_x_continuous(breaks = c(2000,2004,2009,2014,2019,2024,2029)) +
  scale_y_continuous(limits = c(15, 35), breaks = seq(15,35, by = 5)) +
  labs(
    title = "Projeção da taxa de mortalidade por câncer de mama (Lee–Carter / StMoMo)",
    subtitle = "Taxa padronizada por idade (pesos fixos no último ano do treino) + Realizado 2020–2024 + Meta ODS 3.4",
    x = "Ano", y = "Taxa por 100.000",
    color = "Período",
    caption = "IC 95% via previsão do índice kt (StMoMo::forecast) e propagação para taxas"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
p_forecast_brasil
# -----------------------------------------------------------------------------
# 8) RESUMOS (1 linha por período, sem warning do dplyr 1.1+)
# -----------------------------------------------------------------------------
resumo_2019 <- taxa_historica %>%
  filter(Ano == max(periodo_treino))

resumo_periodos_brasil <- taxa_projetada %>%
  group_by(Periodo) %>%
  summarise(
    Media = mean(Taxa, na.rm = TRUE),
    Ano_final = max(Ano),
    Low_final = Taxa_lower_95[Ano == Ano_final][1],
    Up_final  = Taxa_upper_95[Ano == Ano_final][1],
    .groups = "drop"
  ) %>%
  mutate(
    IC_final = paste0("[", round(Low_final, 1), " ; ", round(Up_final, 1), "]")
  ) %>%
  select(Periodo, Media, IC_final)
