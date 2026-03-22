# 2_renshaw_nd_CRIAR_RESIDUOS_STMOMO

library(StMoMo)
library(dplyr)
library(ggplot2)
library(tidyr)


# 2) RESÍDUOS: deviance + padronização (metodologia RH 2006)
res_dev_renshaw_nd <- residuals(fit_renshaw_nd, scale = TRUE)
res_mat_renshaw_nd <- res_dev_renshaw_nd$residuals

# df residual (nu): tentar campos típicos; fallback conservador
df_resid_renshaw_nd <- NA_real_
if (!is.null(fit_renshaw_nd$df.residual)) df_resid_renshaw_nd <- fit_renshaw_nd$df.residual
if (is.na(df_resid_renshaw_nd) && !is.null(fit_renshaw_nd$nobs) && !is.null(fit_renshaw_nd$np)) df_resid_renshaw_nd <- fit_renshaw_nd$nobs - fit_renshaw_nd$np
if (is.na(df_resid_renshaw_nd) && !is.null(fit_renshaw_nd$nobs)) df_resid_renshaw_nd <- fit_renshaw_nd$nobs  # último fallback

phi_hat_renshaw_nd <- fit_renshaw_nd$deviance / df_resid_renshaw_nd

res_mat_renshaw_nd <- sign(res_mat_renshaw_nd) * sqrt(abs(res_mat_renshaw_nd) / phi_hat_renshaw_nd)

# 3) DATAFRAME APC (idade, período, coorte)
idades_renshaw_nd <- as.numeric(rownames(res_mat_renshaw_nd))
anos_renshaw_nd <- as.numeric(colnames(res_mat_renshaw_nd))

residuos_df_renshaw_nd <- expand.grid(Idade = idades_renshaw_nd, Ano = anos_renshaw_nd) %>%
  mutate(Residuo = as.vector(res_mat_renshaw_nd), Coorte = Ano - Idade) %>%
  filter(is.finite(Residuo)) %>%
  filter(Ano %in% periodo_treino_renshaw_nd)


# 4) PLOTS NATIVOS StMoMo (rápidos, canônicos)
res_obj_std_renshaw_nd <- residuals(fit_renshaw_nd, scale = TRUE)

plot(res_obj_std_renshaw_nd, type = "scatter",  main = "Resíduos padronizados (RH) — Nordeste")

# Interpretação:
# Nuvem sem estrutura = bom. Padrões persistentes = misfit residual.





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

dados_ndasil <- dados_prep %>%
  filter(LOCAL == "Nordeste")

stopifnot(all(is.finite(dados_ndasil$IDADE_CENTRAL)))
stopifnot(all(is.finite(dados_ndasil$ANO)))

# 2) ANÁLISE DESCRITIVA

# Taxa por 100k (apenas para EDA)
dados_ndasil <- dados_ndasil %>%
  mutate(TAXA_100K = (MORTES / pmax(POP, 1e-12)) * 1e5)

# EDA 2.1: corte transversal (anos âncora)
anos_disponiveis <- sort(unique(dados_ndasil$ANO))
anos_selecionados <- intersect(c(2000, 2005, 2010, 2015, 2019), anos_disponiveis)

if (length(anos_selecionados) >= 2) {
  p_taxa_idade <- dados_ndasil %>%
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
p_taxa_idade_ndasil <- p_taxa_idade

# 3) MATRIZES Dxt/Ext

grid <- dados_ndasil %>%
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

# 4) DEFINIR PERÍODO DE TREINO

anos_disp <- dados_stmomo$years
periodo_treino <- if (all(2000:2019 %in% anos_disp)) 2000:2019 else anos_disp
idades_fit <- dados_stmomo$ages

# 5) AJUSTE LEE-CARTER 

LCfit_nd <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

res_obj_LC_nd <- residuals(LCfit_nd, scale = TRUE)
plot(res_obj_LC_nd, type = "scatter",  main = "Resíduos padronizados (LC) — Nordeste")





































