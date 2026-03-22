# 2_renshaw_su_CRIAR_RESIDUOS_STMOMO

library(StMoMo)
library(dplyr)
library(ggplot2)
library(tidyr)


# 2) RESÍDUOS: deviance + padronização (metodologia RH 2006)
res_dev_renshaw_su <- residuals(fit_renshaw_su, scale = TRUE)
res_mat_renshaw_su <- res_dev_renshaw_su$residuals

# df residual (nu): tentar campos típicos; fallback conservador
df_resid_renshaw_su <- NA_real_
if (!is.null(fit_renshaw_su$df.residual)) df_resid_renshaw_su <- fit_renshaw_su$df.residual
if (is.na(df_resid_renshaw_su) && !is.null(fit_renshaw_su$nobs) && !is.null(fit_renshaw_su$np)) df_resid_renshaw_su <- fit_renshaw_su$nobs - fit_renshaw_su$np
if (is.na(df_resid_renshaw_su) && !is.null(fit_renshaw_su$nobs)) df_resid_renshaw_su <- fit_renshaw_su$nobs  # último fallback

phi_hat_renshaw_su <- fit_renshaw_su$deviance / df_resid_renshaw_su

res_mat_renshaw_su <- sign(res_mat_renshaw_su) * sqrt(abs(res_mat_renshaw_su) / phi_hat_renshaw_su)

# -----------------------------------------------------------------------------
# 3) DATAFRAME APC (idade, período, coorte)
# -----------------------------------------------------------------------------
idades_renshaw_su <- as.numeric(rownames(res_mat_renshaw_su))
anos_renshaw_su <- as.numeric(colnames(res_mat_renshaw_su))

residuos_df_renshaw_su <- expand.grid(Idade = idades_renshaw_su, Ano = anos_renshaw_su) %>%
  mutate(Residuo = as.vector(res_mat_renshaw_su), Coorte = Ano - Idade) %>%
  filter(is.finite(Residuo)) %>%
  filter(Ano %in% periodo_treino_renshaw_su)

# Interpretação:
# A leitura “de referência” é a mesma do artigo: procurar padrões sistemáticos por período/idade/coorte
# e ver se o RH remove as ondulações (ripple effects) associadas a coorte. :contentReference[oaicite:3]{index=3}

# -----------------------------------------------------------------------------
# 4) PLOTS NATIVOS StMoMo (rápidos, canônicos)
res_obj_std_renshaw_su <- residuals(fit_renshaw_su, scale = TRUE)

plot(res_obj_std_renshaw_su, type = "scatter",  main = "Resíduos padronizados (RH) — Sul")




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

dados_sd <- dados_prep %>%
  filter(LOCAL == "Sul")

# 2) ANÁLISE DESCRITIVA

# Taxa por 100k (apenas para EDA)
dados_sd <- dados_sd %>%
  mutate(TAXA_100K = (MORTES / pmax(POP, 1e-12)) * 1e5)

# EDA 2.1: corte transversal (anos âncora)
anos_disponiveis <- sort(unique(dados_sd$ANO))
anos_selecionados <- intersect(c(2000, 2005, 2010, 2015, 2019), anos_disponiveis)



# 3) MATRIZES Dxt/Ext

grid <- dados_sd %>%
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
    series = "Sul (total)",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# 4) DEFINIR PERÍODO DE TREINO

anos_disp <- dados_stmomo$years
periodo_treino <- if (all(2000:2019 %in% anos_disp)) 2000:2019 else anos_disp
idades_fit <- dados_stmomo$ages

# 5) AJUSTE LEE-CARTER 

LCfit_su <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

res_obj_LC_su <- residuals(LCfit_su, scale = TRUE)
plot(res_obj_LC_su, type = "scatter",  main = "Resíduos padronizados (LC) — Sul")



