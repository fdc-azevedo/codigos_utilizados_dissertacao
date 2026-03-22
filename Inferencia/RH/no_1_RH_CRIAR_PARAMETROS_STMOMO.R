# =============================================================================
# 1_RENSHAW_no_CRIAR_PARAMETROS_STMOMO

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
dados_base_renshaw_no <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% 2000:2019) %>%
  filter(LOCAL == "Norte") %>%
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
grid_renshaw_no <- dados_base_renshaw_no %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )

idades_renshaw_no <- sort(unique(grid_renshaw_no$IDADE_CENTRAL))
anos_renshaw_no   <- sort(unique(grid_renshaw_no$ANO))

Dxt_renshaw_no <- grid_renshaw_no %>%
  select(IDADE_CENTRAL, ANO, D) %>%
  pivot_wider(names_from = ANO, values_from = D) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

Ext_renshaw_no <- grid_renshaw_no %>%
  select(IDADE_CENTRAL, ANO, E) %>%
  pivot_wider(names_from = ANO, values_from = E) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

rownames(Dxt_renshaw_no) <- idades_renshaw_no
colnames(Dxt_renshaw_no) <- anos_renshaw_no
rownames(Ext_renshaw_no) <- idades_renshaw_no
colnames(Ext_renshaw_no) <- anos_renshaw_no

dados_stmomo_renshaw_no <- structure(
  list(
    Dxt = Dxt_renshaw_no,
    Ext = Ext_renshaw_no,
    ages = as.numeric(idades_renshaw_no),
    years = as.numeric(anos_renshaw_no),
    type = "central",
    series = "Norte",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# -----------------------------------------------------------------------------
# 4) PERÍODO DE TREINO
# -----------------------------------------------------------------------------
periodo_treino_renshaw_no <- intersect(2000:2019, dados_stmomo_renshaw_no$years)
idades_fit_renshaw_no <- dados_stmomo_renshaw_no$ages

# -----------------------------------------------------------------------------
# 5) LEE-CARTER BASELINE PARA STARTS
# -----------------------------------------------------------------------------
modelo_lc_no <- lc(link = "log")

fit_lc_no <- fit(
  modelo_lc_no,
  data = dados_stmomo_renshaw_no,
  ages.fit = idades_fit_renshaw_no,
  years.fit = periodo_treino_renshaw_no
)

# -----------------------------------------------------------------------------
# 6) ESPECIFICAÇÃO + AJUSTE RH
# -----------------------------------------------------------------------------
modelo_renshaw_no <- rh(
  link = "log",
  cohortAgeFun = "1"
)

set.seed(88)
fit_renshaw_no <- fit(
  modelo_renshaw_no,
  data = dados_stmomo_renshaw_no,
  ages.fit = idades_fit_renshaw_no,
  years.fit = periodo_treino_renshaw_no
)

fit_renshaw_no


# -----------------------------------------------------------------------------
# 7) PARÂMETROS (ax, b1x, b0x, kt, gc) + gráficos
# -----------------------------------------------------------------------------

faixas_labels_renshaw_no <- sapply(
  fit_renshaw_no$ages,
  function(x) paste0(round(x - 2.5), "-", round(x + 2.5))
)

# ax
df_ax_renshaw_no <- data.frame(
  Idade = fit_renshaw_no$ages,
  Faixa = faixas_labels_renshaw_no,
  ax = as.numeric(fit_renshaw_no$ax)
)

p_ax_renshaw_no <- ggplot(df_ax_renshaw_no, aes(x = Idade, y = ax)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Norte", x = "Idade", y = expression(alpha[x])) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )+
  scale_y_continuous(
    breaks = seq(-12, -6, by = 1),
    limits = c(-12, -6)
  )
p_ax_renshaw_no

# b1x
df_b1x_renshaw_no <- data.frame(
  Idade = fit_renshaw_no$ages,
  Faixa = faixas_labels_renshaw_no,
  b1x = as.numeric(fit_renshaw_no$bx[, 1])
)

p_b1x_renshaw_no <- ggplot(df_b1x_renshaw_no, aes(x = Idade, y = b1x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Norte", x = "Idade", y = expression(b[x]^1)) +
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
p_b1x_renshaw_no

# b0x
df_b0x_renshaw_no <- data.frame(
  Idade = fit_renshaw_no$ages,
  Faixa = faixas_labels_renshaw_no,
  b0x = as.numeric(fit_renshaw_no$b0x)
)

p_b0x_renshaw_no <- ggplot(df_b0x_renshaw_no, aes(x = Idade, y = b0x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Norte", x = "Idade", y = expression(b[x]^0)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )
p_b0x_renshaw_no

# kt
anos_kt_renshaw_no <- colnames(fit_renshaw_no$kt)
if (is.null(anos_kt_renshaw_no)) anos_kt_renshaw_no <- periodo_treino_renshaw_no
anos_kt_renshaw_no <- as.numeric(anos_kt_renshaw_no)

df_kt_renshaw_no <- data.frame(
  Ano = anos_kt_renshaw_no,
  kt = as.numeric(fit_renshaw_no$kt[1, ])
)

p_kt_renshaw_no <- ggplot(df_kt_renshaw_no, aes(x = Ano, y = kt)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Norte", x = "Ano", y = expression(k[t])) +
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
p_kt_renshaw_no

# gc
coortes_renshaw_no <- suppressWarnings(as.numeric(names(fit_renshaw_no$gc)))
if (all(is.na(coortes_renshaw_no))) coortes_renshaw_no <- seq_along(fit_renshaw_no$gc)

df_gc_renshaw_no <- data.frame(
  Coorte = coortes_renshaw_no,
  gc = as.numeric(fit_renshaw_no$gc)
)

p_gc_renshaw_no <- ggplot(df_gc_renshaw_no, aes(x = Coorte, y = gc)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Norte", x = "Coorte (t - x)", y = expression(gamma[t-x])) +
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
p_gc_renshaw_no