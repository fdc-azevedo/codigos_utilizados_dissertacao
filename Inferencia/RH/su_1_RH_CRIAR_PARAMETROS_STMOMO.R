# =============================================================================
# 1_RENSHAW_su_CRIAR_PARAMETROS_STMOMO
# - Preparação + estimação de parâmetros do Renshaw-Haberman (StMoMo)
# - Ajuste com LC baseline para starts
# - approxConst = TRUE
# - wxt para clipping de coortes de borda
# =============================================================================

library(StMoMo)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# 1) CHECAGENS
# -----------------------------------------------------------------------------
stopifnot(exists("DF_FAIXA"))
stopifnot(exists("faixas_excluir"))

# -----------------------------------------------------------------------------
# 2) PREPARAR DADOS
# -----------------------------------------------------------------------------
dados_base_renshaw_su <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% 2000:2019) %>%
  filter(LOCAL == "Sul") %>%
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

# -----------------------------------------------------------------------------
# 3) MATRIZES Dxt / Ext
# -----------------------------------------------------------------------------
grid_renshaw_su <- dados_base_renshaw_su %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )

idades_renshaw_su <- sort(unique(grid_renshaw_su$IDADE_CENTRAL))
anos_renshaw_su   <- sort(unique(grid_renshaw_su$ANO))

Dxt_renshaw_su <- grid_renshaw_su %>%
  select(IDADE_CENTRAL, ANO, D) %>%
  pivot_wider(names_from = ANO, values_from = D) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

Ext_renshaw_su <- grid_renshaw_su %>%
  select(IDADE_CENTRAL, ANO, E) %>%
  pivot_wider(names_from = ANO, values_from = E) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

rownames(Dxt_renshaw_su) <- idades_renshaw_su
colnames(Dxt_renshaw_su) <- anos_renshaw_su
rownames(Ext_renshaw_su) <- idades_renshaw_su
colnames(Ext_renshaw_su) <- anos_renshaw_su

dados_stmomo_renshaw_su <- structure(
  list(
    Dxt = Dxt_renshaw_su,
    Ext = Ext_renshaw_su,
    ages = as.numeric(idades_renshaw_su),
    years = as.numeric(anos_renshaw_su),
    type = "central",
    series = "Sul",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# -----------------------------------------------------------------------------
# 4) PERÍODO DE TREINO
# -----------------------------------------------------------------------------
periodo_treino_renshaw_su <- intersect(2000:2019, dados_stmomo_renshaw_su$years)
idades_fit_renshaw_su <- dados_stmomo_renshaw_su$ages

# -----------------------------------------------------------------------------
# 5) LEE-CARTER BASELINE PARA STARTS
# -----------------------------------------------------------------------------
modelo_lc_su <- lc(link = "log")

fit_lc_su <- fit(
  modelo_lc_su,
  data = dados_stmomo_renshaw_su,
  ages.fit = idades_fit_renshaw_su,
  years.fit = periodo_treino_renshaw_su
)

# -----------------------------------------------------------------------------
# 6) ESPECIFICAÇÃO + AJUSTE RH
# -----------------------------------------------------------------------------
modelo_renshaw_su <- rh(
  link = "log",
  cohortAgeFun = "1"
)

set.seed(88)
fit_renshaw_su <- fit(
  modelo_renshaw_su,
  data = dados_stmomo_renshaw_su,
  ages.fit = idades_fit_renshaw_su,
  years.fit = periodo_treino_renshaw_su
)

fit_renshaw_su


# -----------------------------------------------------------------------------
# 7) PARÂMETROS (ax, b1x, b0x, kt, gc) + gráficos
# -----------------------------------------------------------------------------

faixas_labels_renshaw_su <- sapply(
  fit_renshaw_su$ages,
  function(x) paste0(round(x - 2.5), "-", round(x + 2.5))
)

# ax
df_ax_renshaw_su <- data.frame(
  Idade = fit_renshaw_su$ages,
  Faixa = faixas_labels_renshaw_su,
  ax = as.numeric(fit_renshaw_su$ax)
)

p_ax_renshaw_su <- ggplot(df_ax_renshaw_su, aes(x = Idade, y = ax)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Sul", x = "Idade", y = expression(alpha[x])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )+ 
  scale_y_continuous(
    breaks = seq(-12, -6, by = 1),
    limits = c(-12, -6)
  )
p_ax_renshaw_su

# b1x
df_b1x_renshaw_su <- data.frame(
  Idade = fit_renshaw_su$ages,
  Faixa = faixas_labels_renshaw_su,
  b1x = as.numeric(fit_renshaw_su$bx[, 1])
)

p_b1x_renshaw_su <- ggplot(df_b1x_renshaw_su, aes(x = Idade, y = b1x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Sul", x = "Idade", y = expression(b[x]^1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )+
  scale_y_continuous(
    breaks = seq(-2, 2, by = 1),
    limits = c(-2, 2)
  )
p_b1x_renshaw_su

# b0x
df_b0x_renshaw_su <- data.frame(
  Idade = fit_renshaw_su$ages,
  Faixa = faixas_labels_renshaw_su,
  b0x = as.numeric(fit_renshaw_su$b0x)
)

p_b0x_renshaw_su <- ggplot(df_b0x_renshaw_su, aes(x = Idade, y = b0x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Sul", x = "Idade", y = expression(b[x]^0)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )
p_b0x_renshaw_su

# kt
anos_kt_renshaw_su <- colnames(fit_renshaw_su$kt)
if (is.null(anos_kt_renshaw_su)) anos_kt_renshaw_su <- periodo_treino_renshaw_su
anos_kt_renshaw_su <- as.numeric(anos_kt_renshaw_su)

df_kt_renshaw_su <- data.frame(
  Ano = anos_kt_renshaw_su,
  kt = as.numeric(fit_renshaw_su$kt[1, ])
)

p_kt_renshaw_su <- ggplot(df_kt_renshaw_su, aes(x = Ano, y = kt)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Sul", x = "Ano", y = expression(k[t])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  ) +
  scale_x_continuous(
    breaks = c(2000, 2004, 2009, 2014, 2019),
    limits = c(2000, 2019)
  ) +
  scale_y_continuous(
    breaks = seq(-2, 2, by = 1),
    limits = c(-2, 2)
  )
p_kt_renshaw_su

# gc
coortes_renshaw_su <- suppressWarnings(as.numeric(names(fit_renshaw_su$gc)))
if (all(is.na(coortes_renshaw_su))) coortes_renshaw_su <- seq_along(fit_renshaw_su$gc)

df_gc_renshaw_su <- data.frame(
  Coorte = coortes_renshaw_su,
  gc = as.numeric(fit_renshaw_su$gc)
)

p_gc_renshaw_su <- ggplot(df_gc_renshaw_su, aes(x = Coorte, y = gc)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Sul", x = "Coorte (t - x)", y = expression(gamma[t-x])) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )+
  scale_y_continuous(
    breaks = seq(-3, 3, by = 1),
    limits = c(-3, 3)
  )
p_gc_renshaw_su