# 1_RENSHAW_co_CRIAR_PARAMETROS_STMOMO

library(StMoMo)
library(dplyr)
library(tidyr)

# 1) CHECAGENS
stopifnot(exists("DF_FAIXA"))
stopifnot(exists("faixas_excluir"))

# 2) PREPARAR DADOS
dados_base_renshaw_co <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% 2000:2019) %>%
  filter(LOCAL == "Centro-Oeste") %>%
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

# 3) MATRIZES Dxt / Ext
grid_renshaw_co <- dados_base_renshaw_co %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )

idades_renshaw_co <- sort(unique(grid_renshaw_co$IDADE_CENTRAL))
anos_renshaw_co   <- sort(unique(grid_renshaw_co$ANO))

Dxt_renshaw_co <- grid_renshaw_co %>%
  select(IDADE_CENTRAL, ANO, D) %>%
  pivot_wider(names_from = ANO, values_from = D) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

Ext_renshaw_co <- grid_renshaw_co %>%
  select(IDADE_CENTRAL, ANO, E) %>%
  pivot_wider(names_from = ANO, values_from = E) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

rownames(Dxt_renshaw_co) <- idades_renshaw_co
colnames(Dxt_renshaw_co) <- anos_renshaw_co
rownames(Ext_renshaw_co) <- idades_renshaw_co
colnames(Ext_renshaw_co) <- anos_renshaw_co

dados_stmomo_renshaw_co <- structure(
  list(
    Dxt = Dxt_renshaw_co,
    Ext = Ext_renshaw_co,
    ages = as.numeric(idades_renshaw_co),
    years = as.numeric(anos_renshaw_co),
    type = "central",
    series = "Centro-Oeste",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# -----------------------------------------------------------------------------
# 4) PERÍODO DE TREINO
# -----------------------------------------------------------------------------
periodo_treino_renshaw_co <- intersect(2000:2019, dados_stmomo_renshaw_co$years)
idades_fit_renshaw_co <- dados_stmomo_renshaw_co$ages

# -----------------------------------------------------------------------------
# 5) LEE-CARTER BASELINE PARA STARTS
# -----------------------------------------------------------------------------
modelo_lc_co <- lc(link = "log")

fit_lc_co <- fit(
  modelo_lc_co,
  data = dados_stmomo_renshaw_co,
  ages.fit = idades_fit_renshaw_co,
  years.fit = periodo_treino_renshaw_co
)

# -----------------------------------------------------------------------------
# 6) ESPECIFICAÇÃO + AJUSTE RH
# -----------------------------------------------------------------------------
modelo_renshaw_co <- rh(
  link = "log",
  cohortAgeFun = "1"
)

set.seed(88)
fit_renshaw_co <- fit(
  modelo_renshaw_co,
  data = dados_stmomo_renshaw_co,
  ages.fit = idades_fit_renshaw_co,
  years.fit = periodo_treino_renshaw_co
)

fit_renshaw_co


# -----------------------------------------------------------------------------
# 7) PARÂMETROS (ax, b1x, b0x, kt, gc) + gráficos
# -----------------------------------------------------------------------------

faixas_labels_renshaw_co <- sapply(
  fit_renshaw_co$ages,
  function(x) paste0(round(x - 2.5), "-", round(x + 2.5))
)

# ax
df_ax_renshaw_co <- data.frame(
  Idade = fit_renshaw_co$ages,
  Faixa = faixas_labels_renshaw_co,
  ax = as.numeric(fit_renshaw_co$ax)
)

p_ax_renshaw_co <- ggplot(df_ax_renshaw_co, aes(x = Idade, y = ax)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Centro-Oeste", x = "Idade", y = expression(alpha[x])) +
  theme_minimal() + 
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
p_ax_renshaw_co

# b1x
df_b1x_renshaw_co <- data.frame(
  Idade = fit_renshaw_co$ages,
  Faixa = faixas_labels_renshaw_co,
  b1x = as.numeric(fit_renshaw_co$bx[, 1])
)

p_b1x_renshaw_co <- ggplot(df_b1x_renshaw_co, aes(x = Idade, y = b1x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
scale_x_continuous(
  breaks = seq(30, 90, by = 10),
  limits = c(30, 90)) + 
  labs(title = "Centro-Oeste", x = "Idade", y = expression(b[x]^1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )+
  scale_y_continuous(
    breaks = seq(-10, 10, by = 10),
    limits = c(-15, 15)
  )
p_b1x_renshaw_co

# b0x
df_b0x_renshaw_co <- data.frame(
  Idade = fit_renshaw_co$ages,
  Faixa = faixas_labels_renshaw_co,
  b0x = as.numeric(fit_renshaw_co$b0x)
)

p_b0x_renshaw_co <- ggplot(df_b0x_renshaw_co, aes(x = Idade, y = b0x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Centro-Oeste", x = "Idade", y = expression(b[x]^0)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )
p_b0x_renshaw_co

# kt
anos_kt_renshaw_co <- colnames(fit_renshaw_co$kt)
if (is.null(anos_kt_renshaw_co)) anos_kt_renshaw_co <- periodo_treino_renshaw_co
anos_kt_renshaw_co <- as.numeric(anos_kt_renshaw_co)

df_kt_renshaw_co <- data.frame(
  Ano = anos_kt_renshaw_co,
  kt = as.numeric(fit_renshaw_co$kt[1, ])
)

p_kt_renshaw_co <- ggplot(df_kt_renshaw_co, aes(x = Ano, y = kt)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Centro-Oeste", x = "Ano", y = expression(k[t])) +
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
p_kt_renshaw_co

# gc
coortes_renshaw_co <- suppressWarnings(as.numeric(names(fit_renshaw_co$gc)))
if (all(is.na(coortes_renshaw_co))) coortes_renshaw_co <- seq_along(fit_renshaw_co$gc)

df_gc_renshaw_co <- data.frame(
  Coorte = coortes_renshaw_co,
  gc = as.numeric(fit_renshaw_co$gc)
)

p_gc_renshaw_co <- ggplot(df_gc_renshaw_co, aes(x = Coorte, y = gc)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Centro-Oeste", x = "Coorte (t - x)", y = expression(gamma[t-x])) +
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
p_gc_renshaw_co