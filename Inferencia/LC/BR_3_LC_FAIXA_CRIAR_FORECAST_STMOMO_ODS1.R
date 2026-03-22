# =============================================================================
# 3_LC_FAIXA_CRIAR_FORECAST_STMOMO  (REALIZADO + PROJEÇÕES LC COM IC)
#
# Objetos no gráfico (cores por OBJETO):
#   1) Treino 2000–2019: NÃO ENTRA no gráfico (só serve para corte vertical)
#   2) Realizado bruto 2000–2024: PRETO
#   3) Projetado LC 2020–2024: VERDE + IC 95%
#   4) Projetado LC 2025–2029: AZUL  + IC 95%
#
# Observação:
# - O realizado é BRUTO (sum óbitos / sum pop) * 100k.
# - As projeções LC usam StMoMo::forecast() e IC via kt.f propagado para taxas.
# =============================================================================

library(StMoMo)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1) CHECAGENS
# -----------------------------------------------------------------------------
stopifnot(exists("LCfit"), inherits(LCfit, "fitStMoMo"))
stopifnot(exists("dados_stmomo"))
stopifnot(exists("periodo_treino"))
stopifnot(exists("DF_FAIXA"))

# -----------------------------------------------------------------------------
# 2) PESOS ETÁRIOS FIXOS (último ano do treino) — usados para agregação do LC
# -----------------------------------------------------------------------------
ultimo_ano <- tail(periodo_treino, 1)
stopifnot(as.character(ultimo_ano) %in% colnames(dados_stmomo$Ext))

prop_idade <- dados_stmomo$Ext[, as.character(ultimo_ano)] |>
  (\(x) x / sum(x))()

# -----------------------------------------------------------------------------
# 3) REALIZADO BRUTO (2000–2024) — linha preta
# -----------------------------------------------------------------------------
taxa_real_total <- DF_FAIXA %>%
  filter(!FAIXA %in% c("0-29", "90+")) %>%
  rename(ANO = Atributo, POP = Pop, MORTES = CM) %>%
  filter(LOCAL == "Brasil", ANO %in% 2000:2024) %>%
  group_by(ANO) %>%
  summarise(
    Taxa = sum(MORTES, na.rm = TRUE) / sum(POP, na.rm = TRUE) * 1e5,
    .groups = "drop"
  ) %>%
  transmute(
    Ano = ANO,
    Taxa,
    Serie = "Realizado 2000-2024"
  )

# -----------------------------------------------------------------------------
# 4) PROJEÇÃO LEE-CARTER (2020–2029) + IC 95% (via kt.f propagado)
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
) %>%
  rowwise() %>%
  mutate(
    Taxa = sum(exp(ax + bx * kt_mean) * 1e5 * prop_idade, na.rm = TRUE),
    Taxa_lower_95 = sum(exp(ax + bx * kt_low)  * 1e5 * prop_idade, na.rm = TRUE),
    Taxa_upper_95 = sum(exp(ax + bx * kt_up)   * 1e5 * prop_idade, na.rm = TRUE),
    Serie = if_else(Ano <= 2024, "Projetado 2020-2024", "Projetado 2025-2029")
  ) %>%
  ungroup() %>%
  select(Ano, Taxa, Taxa_lower_95, Taxa_upper_95, Serie)

# Subconjuntos explícitos (deixa o script mais “auditável”)
proj_2020_2024 <- taxa_projetada %>% filter(Serie == "Projetado 2020-2024")
proj_2025_2029 <- taxa_projetada %>% filter(Serie == "Projetado 2025-2029")

# -----------------------------------------------------------------------------
# 5) META ODS 3.4 (redução de 1/3 vs 2014) — opcional
#     meta = 2/3 * taxa_bruta_2014
# -----------------------------------------------------------------------------
meta_ods_34 <- taxa_real_total %>%
  filter(Ano == 2014) %>%
  summarise(meta = first(Taxa) * (2/3)) %>%
  pull(meta)

# -----------------------------------------------------------------------------
# 6) CORES POR OBJETO (não por período)
# -----------------------------------------------------------------------------
cores_manual <- c(
  "Realizado 2000-2024" = "black",
  "Projetado 2020-2024" = "#2E8B57",
  "Projetado 2025-2029" = "#1E90FF"
)

# -----------------------------------------------------------------------------
# 7) GRÁFICO (Realizado + Projeções com IC por faixa temporal)
# -----------------------------------------------------------------------------
p_forecast_brasil <-
  ggplot() +
  
  # IC 2020–2024 (verde)
  geom_ribbon(
    data = proj_2020_2024,
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#2E8B57", alpha = 0.20
  ) +
  
  # IC 2025–2029 (azul)
  geom_ribbon(
    data = proj_2025_2029,
    aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
    fill = "#1E90FF", alpha = 0.20
  ) +
  
  # Realizado 2000–2024 (preto)
  geom_line(
    data = taxa_real_total,
    aes(x = Ano, y = Taxa, color = Serie),
    linewidth = 1.2
  ) +
  geom_point(
    data = taxa_real_total,
    aes(x = Ano, y = Taxa, color = Serie),
    size = 2
  ) +
  
  # Projetado 2020–2024 (verde tracejado)
  geom_line(
    data = proj_2020_2024,
    aes(x = Ano, y = Taxa, color = Serie),
    linewidth = 1.2, linetype = "dashed"
  ) +
  geom_point(
    data = proj_2020_2024,
    aes(x = Ano, y = Taxa, color = Serie),
    size = 2
  ) +
  
  # Projetado 2025–2029 (azul tracejado)
  geom_line(
    data = proj_2025_2029,
    aes(x = Ano, y = Taxa, color = Serie),
    linewidth = 1.2, linetype = "dashed"
  ) +
  geom_point(
    data = proj_2025_2029,
    aes(x = Ano, y = Taxa, color = Serie),
    size = 2
  ) +
  
  # Corte treino / projeção
  geom_vline(
    xintercept = max(periodo_treino) + 0.5,
    linetype = "dashed", color = "red", alpha = 0.7
  ) +
  
  # Meta ODS 3.4 (tracejada) — se não quiser, apague este bloco
  geom_hline(
    yintercept = meta_ods_34,
    linetype = "dashed", linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 2024, y = meta_ods_34,
    label = "ODS 3.4**",
    hjust = -0.05, vjust = -0.6, size = 3.6
  ) +
  
  scale_color_manual(values = cores_manual) +
  scale_x_continuous(breaks = c(2000, 2004, 2009, 2014, 2019, 2024, 2029)) +
  scale_y_continuous(limits = c(10, 35), breaks = seq(10, 35, by = 5)) +
  labs(
    title = "Projeção da taxa de mortalidade por câncer de mama (Lee–Carter)* - Brasil",
    x = "Ano",
    y = "Taxa por 100.000 Habitantes",
    color = "Periodo",
    caption = "*IC 95% via previsão do índice kt\n** Redução de 1/3 referente 2015"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

p_forecast_brasil

