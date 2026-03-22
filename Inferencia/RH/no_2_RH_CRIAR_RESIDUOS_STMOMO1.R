# 2_renshaw_no_CRIAR_RESIDUOS_STMOMO

library(StMoMo)
library(dplyr)
library(ggplot2)
library(tidyr)


# 2) RESÍDUOS: deviance + padronização (metodologia RH 2006)
res_dev_renshaw_no <- residuals(fit_renshaw_no, scale = TRUE)
res_mat_renshaw_no <- res_dev_renshaw_no$residuals

# df residual (nu): tentar campos típicos; fallback conservador
df_resid_renshaw_no <- NA_real_
if (!is.null(fit_renshaw_no$df.residual)) df_resid_renshaw_no <- fit_renshaw_no$df.residual
if (is.na(df_resid_renshaw_no) && !is.null(fit_renshaw_no$nobs) && !is.null(fit_renshaw_no$np)) df_resid_renshaw_no <- fit_renshaw_no$nobs - fit_renshaw_no$np
if (is.na(df_resid_renshaw_no) && !is.null(fit_renshaw_no$nobs)) df_resid_renshaw_no <- fit_renshaw_no$nobs  # último fallback

phi_hat_renshaw_no <- fit_renshaw_no$deviance / df_resid_renshaw_no

res_mat_renshaw_no <- sign(res_mat_renshaw_no) * sqrt(abs(res_mat_renshaw_no) / phi_hat_renshaw_no)

# 3) DATAFRAME APC (idade, período, coorte)
idades_renshaw_no <- as.numeric(rownames(res_mat_renshaw_no))
anos_renshaw_no <- as.numeric(colnames(res_mat_renshaw_no))

residuos_df_renshaw_no <- expand.grid(Idade = idades_renshaw_no, Ano = anos_renshaw_no) %>%
  mutate(Residuo = as.vector(res_mat_renshaw_no), Coorte = Ano - Idade) %>%
  filter(is.finite(Residuo)) %>%
  filter(Ano %in% periodo_treino_renshaw_no)


# 4) PLOTS NATIVOS StMoMo (rápidos, canônicos)
res_obj_std_renshaw_no <- residuals(fit_renshaw_no, scale = TRUE)

plot(res_obj_std_renshaw_no, type = "scatter",  main = "Resíduos padronizados (RH) — Norte")


# 1) PREPARAR DADOS 

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

dados_no <- dados_prep %>%
  filter(LOCAL == "Norte")

stopifnot(all(is.finite(dados_no$IDADE_CENTRAL)))
stopifnot(all(is.finite(dados_no$ANO)))

# -----------------------------------------------------------------------------
# 2) ANÁLISE DESCRITIVA
# -----------------------------------------------------------------------------

# Taxa por 100k (apenas para EDA)
dados_no <- dados_no %>%
  mutate(TAXA_100K = (MORTES / pmax(POP, 1e-12)) * 1e5)

# EDA 2.1: corte transversal (anos âncora)
anos_disponiveis <- sort(unique(dados_no$ANO))
anos_selecionados <- intersect(c(2000, 2005, 2010, 2015, 2019), anos_disponiveis)

if (length(anos_selecionados) >= 2) {
  p_taxa_idade <- dados_no %>%
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
p_taxa_idade_no <- p_taxa_idade

# -----------------------------------------------------------------------------
# 3) MATRIZES Dxt/Ext
# -----------------------------------------------------------------------------

grid <- dados_no %>%
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
    series = "Norte (total)",
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

LCfit_no <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

res_obj_LC_no <- residuals(LCfit_no, scale = TRUE)
plot(res_obj_LC_no, type = "scatter",  main = "Resíduos padronizados (LC) — Norte")
