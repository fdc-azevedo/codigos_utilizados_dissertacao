# 1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO 

suppressPackageStartupMessages({
  library(StMoMo)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})


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

dados_sul <- dados_prep %>%
  filter(LOCAL == "Sul")

stopifnot(all(is.finite(dados_sul$IDADE_CENTRAL)))
stopifnot(all(is.finite(dados_sul$ANO)))


# 2) ANÁLISE DESCRITIVA 

# Taxa por 100k (apenas para EDA)
dados_sul <- dados_sul %>%
  mutate(TAXA_100K = (MORTES / pmax(POP, 1e-12)) * 1e5)

# EDA 2.1: corte transversal (anos âncora)
anos_disponiveis <- sort(unique(dados_sul$ANO))
anos_selecionados <- intersect(c(2000, 2005, 2010, 2015, 2019), anos_disponiveis)

if (length(anos_selecionados) >= 2) {
  p_taxa_idade <- dados_sul %>%
    filter(ANO %in% anos_selecionados) %>%
    ggplot(aes(x = IDADE_CENTRAL, y = TAXA_100K, color = as.factor(ANO), group = ANO)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2) +
    labs(
      title = "Sul",
      x = "Idade central",
      y = "Taxa por 100.000",
      color = "Ano"
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
  
  print(p_taxa_idade)
}

p_taxa_idade_su <- p_taxa_idade
# 3) MATRIZES Dxt/Ext 

grid <- dados_sul %>%
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

# 4) DEFINIR PERÍODO DE TREINO (somente delimitação; sem forecast)

anos_disp <- dados_stmomo$years
periodo_treino <- if (all(2000:2019 %in% anos_disp)) 2000:2019 else anos_disp
idades_fit <- dados_stmomo$ages


# 5) AJUSTE LEE-CARTER 
set.seed(88)
LCfit <- fit(
  lc(link = "log"),
  data = dados_stmomo,
  ages.fit = idades_fit,
  years.fit = periodo_treino,
  verbose = TRUE
)

# 6) PARÂMETROS (ax, bx, kt)

ax <- as.numeric(LCfit$ax)
bx <- as.numeric(LCfit$bx)         
kt <- as.numeric(LCfit$kt[1, ])   

df_ax <- data.frame(IDADE = LCfit$ages, ax = ax)
df_bx <- data.frame(IDADE = LCfit$ages, bx = bx)
df_kt <- data.frame(ANO = LCfit$years, kt = kt)

p_ax <- ggplot(df_ax, aes(x = IDADE, y = ax)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2) +
  labs(title = "Sul", x = "Idade central", y = expression(alpha[x])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  ) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-11, -6, by = 1),
    limits = c(-11, -6)
  )
p_ax_su <- p_ax
print(p_ax)

p_bx <- ggplot(df_bx, aes(x = IDADE, y = bx)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2) +
  geom_hline(yintercept = mean(df_bx$bx), linetype = "dashed") +
  labs(title = "Sul", x = "Idade central", y = expression(b[x])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  ) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-0.1, 0.4, by = 0.1),
    limits = c(-0.15, 0.45)
  )
p_bx_su <- p_bx
print(p_bx)

p_kt <- ggplot(df_kt, aes(x = ANO, y = kt)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2) +
  labs(title = "Sul", x = "Ano", y = expression(k[t])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  ) +
  scale_x_continuous(
    breaks = c(2000, 2004, 2009, 2014, 2019),
    limits = c(min(df_kt$ANO), max(df_kt$ANO))
  ) +
  scale_y_continuous(
    breaks = seq(-5, 5, by = 2.5),
    limits = c(-5.5, 5))
  
p_kt_su <- p_kt
print(p_kt)
