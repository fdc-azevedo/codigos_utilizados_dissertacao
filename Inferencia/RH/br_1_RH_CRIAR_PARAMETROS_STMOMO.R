# 1_RENSHAW_br_CRIAR_PARAMETROS_STMOMO

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
dados_base_renshaw_br <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% 2000:2019) %>%
  filter(LOCAL == "Brasil") %>%
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
grid_renshaw_br <- dados_base_renshaw_br %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )

idades_renshaw_br <- sort(unique(grid_renshaw_br$IDADE_CENTRAL))
anos_renshaw_br   <- sort(unique(grid_renshaw_br$ANO))

Dxt_renshaw_br <- grid_renshaw_br %>%
  select(IDADE_CENTRAL, ANO, D) %>%
  pivot_wider(names_from = ANO, values_from = D) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

Ext_renshaw_br <- grid_renshaw_br %>%
  select(IDADE_CENTRAL, ANO, E) %>%
  pivot_wider(names_from = ANO, values_from = E) %>%
  arrange(IDADE_CENTRAL) %>%
  select(-IDADE_CENTRAL) %>%
  as.matrix()

rownames(Dxt_renshaw_br) <- idades_renshaw_br
colnames(Dxt_renshaw_br) <- anos_renshaw_br
rownames(Ext_renshaw_br) <- idades_renshaw_br
colnames(Ext_renshaw_br) <- anos_renshaw_br

dados_stmomo_renshaw_br <- structure(
  list(
    Dxt = Dxt_renshaw_br,
    Ext = Ext_renshaw_br,
    ages = as.numeric(idades_renshaw_br),
    years = as.numeric(anos_renshaw_br),
    type = "central",
    series = "Brasil",
    label = "DF_FAIXA"
  ),
  class = "StMoMoData"
)

# -----------------------------------------------------------------------------
# 4) PERÍODO DE TREINO
# -----------------------------------------------------------------------------
periodo_treino_renshaw_br <- intersect(2000:2019, dados_stmomo_renshaw_br$years)
idades_fit_renshaw_br <- dados_stmomo_renshaw_br$ages

# -----------------------------------------------------------------------------
# 5) LEE-CARTER BASELINE PARA STARTS
# -----------------------------------------------------------------------------
modelo_lc_br <- lc(link = "log")

fit_lc_br <- fit(
  modelo_lc_br,
  data = dados_stmomo_renshaw_br,
  ages.fit = idades_fit_renshaw_br,
  years.fit = periodo_treino_renshaw_br
)

# -----------------------------------------------------------------------------
# 6) ESPECIFICAÇÃO + AJUSTE RH
# -----------------------------------------------------------------------------
modelo_renshaw_br <- rh(
  link = "log",
  cohortAgeFun = "1"
)
set.seed(88)
fit_renshaw_br <- fit(
  modelo_renshaw_br,
  data = dados_stmomo_renshaw_br,
  ages.fit = idades_fit_renshaw_br,
  years.fit = periodo_treino_renshaw_br
)

fit_renshaw_br


# -----------------------------------------------------------------------------
# 7) PARÂMETROS (ax, b1x, b0x, kt, gc) + gráficos
# -----------------------------------------------------------------------------

faixas_labels_renshaw_br <- sapply(
  fit_renshaw_br$ages,
  function(x) paste0(round(x - 2.5), "-", round(x + 2.5))
)

# ax
df_ax_renshaw_br <- data.frame(
  Idade = fit_renshaw_br$ages,
  Faixa = faixas_labels_renshaw_br,
  ax = as.numeric(fit_renshaw_br$ax)
)

p_ax_renshaw_br <- ggplot(df_ax_renshaw_br, aes(x = Idade, y = ax)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Brasil", x = "Idade", y = expression(alpha[x])) +
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
p_ax_renshaw_br

# b1x
df_b1x_renshaw_br <- data.frame(
  Idade = fit_renshaw_br$ages,
  Faixa = faixas_labels_renshaw_br,
  b1x = as.numeric(fit_renshaw_br$bx[, 1])
)

p_b1x_renshaw_br <- ggplot(df_b1x_renshaw_br, aes(x = Idade, y = b1x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Brasil", x = "Idade", y = expression(b[x]^1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  ) +
  scale_y_continuous(
    breaks = seq(-2, 2, by = 1),
    limits = c(-2, 2)
  )
p_b1x_renshaw_br

# b0x
df_b0x_renshaw_br <- data.frame(
  Idade = fit_renshaw_br$ages,
  Faixa = faixas_labels_renshaw_br,
  b0x = as.numeric(fit_renshaw_br$b0x)
)

p_b0x_renshaw_br <- ggplot(df_b0x_renshaw_br, aes(x = Idade, y = b0x)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90)) + 
  labs(title = "Brasil", x = "Idade", y = expression(b[x]^0)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 35, l = 10)
  )
p_b0x_renshaw_br

# kt
anos_kt_renshaw_br <- colnames(fit_renshaw_br$kt)
if (is.null(anos_kt_renshaw_br)) anos_kt_renshaw_br <- periodo_treino_renshaw_br
anos_kt_renshaw_br <- as.numeric(anos_kt_renshaw_br)

df_kt_renshaw_br <- data.frame(
  Ano = anos_kt_renshaw_br,
  kt = as.numeric(fit_renshaw_br$kt[1, ])
)

p_kt_renshaw_br <- ggplot(df_kt_renshaw_br, aes(x = Ano, y = kt)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Brasil", x = "Ano", y = expression(k[t])) +
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
p_kt_renshaw_br

# gc
coortes_renshaw_br <- suppressWarnings(as.numeric(names(fit_renshaw_br$gc)))
if (all(is.na(coortes_renshaw_br))) coortes_renshaw_br <- seq_along(fit_renshaw_br$gc)

df_gc_renshaw_br <- data.frame(
  Coorte = coortes_renshaw_br,
  gc = as.numeric(fit_renshaw_br$gc)
)

p_gc_renshaw_br <- ggplot(df_gc_renshaw_br, aes(x = Coorte, y = gc)) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  labs(title = "Brasil", x = "Coorte (t - x)", y = expression(gamma[t-x])) +
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
p_gc_renshaw_br