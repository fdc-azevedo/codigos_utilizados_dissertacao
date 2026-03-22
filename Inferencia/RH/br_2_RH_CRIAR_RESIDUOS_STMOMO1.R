# =============================================================================
# 2_renshaw_br_CRIAR_RESIDUOS_STMOMO
# - Diagnóstico RH: resíduos de deviance padronizados (renshaw_br & Haberman, 2006)
# - StMoMo como engine (residuals/plot), + plots APC (idade, período, coorte)
# - Sem heatmap, sem cat()
# =============================================================================

library(StMoMo)
library(dplyr)
library(ggplot2)
library(tidyr)

# -----------------------------------------------------------------------------
# 1) CHECAGENS
# -----------------------------------------------------------------------------
#stopifnot(exists("fit_renshaw_br"))
#stopifnot(inherits(fit_renshaw_br, "fitStMoMo"))
#stopifnot(exists("periodo_treino_renshaw_br"))

# -----------------------------------------------------------------------------
# 2) RESÍDUOS: deviance + padronização (metodologia RH 2006)
# -----------------------------------------------------------------------------
# RH (2006): r_xt = sign(y - yhat) * sqrt(dev(x,t) / phi_hat), phi_hat = D/nu :contentReference[oaicite:2]{index=2}
res_dev_renshaw_br <- residuals(fit_renshaw_br, scale = TRUE)
res_mat_renshaw_br <- res_dev_renshaw_br$residuals

# df residual (nu): tentar campos típicos; fallback conservador
df_resid_renshaw_br <- NA_real_
if (!is.null(fit_renshaw_br$df.residual)) df_resid_renshaw_br <- fit_renshaw_br$df.residual
if (is.na(df_resid_renshaw_br) && !is.null(fit_renshaw_br$nobs) && !is.null(fit_renshaw_br$np)) df_resid_renshaw_br <- fit_renshaw_br$nobs - fit_renshaw_br$np
if (is.na(df_resid_renshaw_br) && !is.null(fit_renshaw_br$nobs)) df_resid_renshaw_br <- fit_renshaw_br$nobs  # último fallback

phi_hat_renshaw_br <- fit_renshaw_br$deviance / df_resid_renshaw_br

res_std_mat_renshaw_br <- sign(res_mat_renshaw_br) * sqrt(abs(res_mat_renshaw_br) / phi_hat_renshaw_br)

# -----------------------------------------------------------------------------
# 3) DATAFRAME APC (idade, período, coorte)
# -----------------------------------------------------------------------------
idades_renshaw_br <- as.numeric(rownames(res_std_mat_renshaw_br))
anos_renshaw_br <- as.numeric(colnames(res_std_mat_renshaw_br))

residuos_df_renshaw_br <- expand.grid(Idade = idades_renshaw_br, Ano = anos_renshaw_br) %>%
  mutate(Residuo = as.vector(res_std_mat_renshaw_br), Coorte = Ano - Idade) %>%
  filter(is.finite(Residuo)) %>%
  filter(Ano %in% periodo_treino_renshaw_br)

# Interpretação:
# A leitura “de referência” é a mesma do artigo: procurar padrões sistemáticos por período/idade/coorte
# e ver se o RH remove as ondulações (ripple effects) associadas a coorte. :contentReference[oaicite:3]{index=3}

# -----------------------------------------------------------------------------
# 4) PLOTS NATIVOS StMoMo (rápidos, canônicos)
# -----------------------------------------------------------------------------
res_obj_std_renshaw_br <- residuals(fit_renshaw_br, scale = TRUE)
plot(res_obj_std_renshaw_br, type = "scatter",  main = "Resíduos padronizados (RH) — Brasil")






# -----------------------------------------------------------------------------
# 1) PREPARAR DADOS 
# -----------------------------------------------------------------------------

dados_prep <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%  #########ADICIONADO
  filter(Atributo %in% 2000:2019) %>%   ######### ADICIONADO
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

dados_br <- dados_prep %>%
  filter(LOCAL == "Brasil")

stopifnot(all(is.finite(dados_br$IDADE_CENTRAL)))
stopifnot(all(is.finite(dados_br$ANO)))

# -----------------------------------------------------------------------------
# 2) ANÁLISE DESCRITIVA
# -----------------------------------------------------------------------------

# Taxa por 100k (apenas para EDA)
dados_br <- dados_br %>%
  mutate(TAXA_100K = (MORTES / pmax(POP, 1e-12)) * 1e5)

# EDA 2.1: corte transversal (anos âncora)
anos_disponiveis <- sort(unique(dados_br$ANO))
anos_selecionados <- intersect(c(2000, 2005, 2010, 2015, 2019), anos_disponiveis)

if (length(anos_selecionados) >= 2) {
  p_taxa_idade <- dados_br %>%
    filter(ANO %in% anos_selecionados) %>%
    ggplot(aes(x = IDADE_CENTRAL, y = TAXA_100K, color = as.factor(ANO), group = ANO)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2) +
    labs(
      title = "Brasil",
      x = "Idade central",
      y = "Taxa por 100.000",
      color = "Ano", size = 14
    ) +
    theme_minimal() +
    scale_x_continuous(
      breaks = seq(30, 90, by = 10),
      limits = c(30, 90),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 160, by = 20),
      limits = c(0, 160)
    )
}
p_taxa_idade_br <- p_taxa_idade

# -----------------------------------------------------------------------------
# 3) MATRIZES Dxt/Ext
# -----------------------------------------------------------------------------

grid <- dados_br %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )

idades_unicas <- sort(unique(grid$IDADE_CENTRAL))
anos_unicos   <- sort(unique(grid$ANO))

grid_full <- tidyr::complete(
  grid,
  IDADE_CENTRAL = idades_unicas,
  ANO = anos_unicos,
  fill = list(D = 0, E = 0)
)

Dxt <- grid_full %>%
  select(IDADE_CENTRAL, ANO, D) %>%
  pivot_wider(names_from = ANO, values_from = D) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

Ext <- grid_full %>%
  select(IDADE_CENTRAL, ANO, E) %>%
  pivot_wider(names_from = ANO, values_from = E) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

rownames(Dxt) <- idades_unicas
colnames(Dxt) <- anos_unicos
rownames(Ext) <- idades_unicas
colnames(Ext) <- anos_unicos

if (any(Ext < 0, na.rm = TRUE) || any(Dxt < 0, na.rm = TRUE)) stop("Há valores negativos em Dxt/Ext.")
if (any(Dxt > Ext, na.rm = TRUE)) warning("Encontrado Dxt > Ext em alguma célula (verifique E vs D).")

# StMoMo: para modelo log-Poisson, exposições típicas são centrais ("central")
dados_stmomo <- structure(
  list(
    Dxt = Dxt,
    Ext = Ext,
    ages = as.numeric(rownames(Dxt)),
    years = as.numeric(colnames(Dxt)),
    type = "central",
    series = "Brasil (total)",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# -----------------------------------------------------------------------------
# 4) DEFINIR PERÍODO DE TREINO
# -----------------------------------------------------------------------------

anos_disp <- dados_stmomo$years
periodo_treino <- if (all(2000:2019 %in% anos_disp)) 2000:2019 else anos_disp
idades_fit <- dados_stmomo$ages

# -----------------------------------------------------------------------------
# 5) AJUSTE LEE-CARTER 
# -----------------------------------------------------------------------------

LCfit_br <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

res_obj_LC_br <- residuals(LCfit_br, scale = TRUE)
plot(res_obj_LC_br, type = "scatter",  main = "Resíduos padronizados (LC) — Brasil")