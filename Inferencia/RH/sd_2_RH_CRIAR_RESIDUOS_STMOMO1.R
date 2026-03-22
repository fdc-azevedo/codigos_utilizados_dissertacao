# 2_renshaw_sd_CRIAR_RESIDUOS_STMOMO

library(StMoMo)
library(dplyr)
library(ggplot2)
library(tidyr)


# 2) RESÍDUOS: deviance + padronização (metodologia RH 2006)
res_dev_renshaw_sd <- residuals(fit_renshaw_sd, scale = TRUE)
res_mat_renshaw_sd <- res_dev_renshaw_sd$residuals

# 3) DATAFRAME APC (idade, período, coorte)
idades_renshaw_sd <- as.numeric(rownames(res_mat_renshaw_sd))
anos_renshaw_sd <- as.numeric(colnames(res_mat_renshaw_sd))

residuos_df_renshaw_sd <- expand.grid(Idade = idades_renshaw_sd, Ano = anos_renshaw_sd) %>%
  mutate(Residuo = as.vector(res_mat_renshaw_sd), Coorte = Ano - Idade) %>%
  filter(is.finite(Residuo)) %>%
  filter(Ano %in% periodo_treino_renshaw_sd)

 e ver se o RH remove as ondulações (ripple effects) associadas a coorte. :contentReference[oaicite:3]{index=3}

# 4) PLOTS NATIVOS StMoMo
res_obj_std_renshaw_sd <- residuals(fit_renshaw_sd, scale = TRUE)

plot(res_obj_std_renshaw_sd, type = "scatter",  main = "Resíduos padronizados (RH) — Sudeste")

stopifnot(isFALSE(fit_renshaw_sd$fail))
stopifnot(isTRUE(fit_renshaw_sd$conv))

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
  filter(LOCAL == "Sudeste")

stopifnot(all(is.finite(dados_sd$IDADE_CENTRAL)))
stopifnot(all(is.finite(dados_sd$ANO)))

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
    series = "Sudeste (total)",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# 4) DEFINIR PERÍODO DE TREINO

anos_disp <- dados_stmomo$years
periodo_treino <- if (all(2000:2019 %in% anos_disp)) 2000:2019 else anos_disp
idades_fit <- dados_stmomo$ages

# 5) AJUSTE LEE-CARTER 

LCfit_sd <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

res_obj_LC_sd <- residuals(LCfit_sd, scale = TRUE)
plot(res_obj_LC_sd, type = "scatter",  main = "Resíduos padronizados (LC) — Sudeste")