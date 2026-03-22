# 3_LC_FAIXA_CRIAR_FORECAST_STMOMO

library(StMoMo)
library(dplyr)
library(ggplot2)

# 1) CHECAGENS (contrato com Step 1)
stopifnot(exists("LCfit"))
stopifnot(inherits(LCfit, "fitStMoMo"))
stopifnot(exists("periodo_treino"))
stopifnot(exists("DF_FAIXA"))
stopifnot(exists("faixas_excluir"))

# 2) DEFINIÇÕES
ano_base_pesos <- 2024
anos_obs <- 2000:2024
anos_proj <- 2020:2029

# 3) BASE OBSERVADA (2000–2024) PARA PESOS + HISTÓRICO
dados_prep_obs <- DF_FAIXA %>%
  filter(!FAIXA %in% faixas_excluir) %>%
  filter(Atributo %in% anos_obs) %>%
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

stopifnot(ano_base_pesos %in% dados_prep_obs$ANO)

# Pesos etários fixos em 2024 (Sul)
w_idade <- dados_prep_obs %>%
  filter(LOCAL == "Sul", ANO == ano_base_pesos) %>%
  group_by(IDADE_CENTRAL) %>%
  summarise(w = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  mutate(w = w / sum(w))

# Série observada padronizada (2000–2024)
taxa_historica <- dados_prep_obs %>%
  filter(LOCAL == "Sul", ANO %in% anos_obs) %>%
  group_by(IDADE_CENTRAL, ANO) %>%
  summarise(
    D = sum(MORTES, na.rm = TRUE),
    E = sum(POP,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(w_idade, by = "IDADE_CENTRAL") %>%
  filter(!is.na(w)) %>%                      # garante idades compatíveis com pesos
  mutate(taxa_obs_100k = (D / pmax(E, 1e-12)) * 1e5) %>%
  group_by(ANO) %>%
  summarise(Taxa = sum(taxa_obs_100k * w, na.rm = TRUE), .groups = "drop") %>%
  transmute(
    Ano = ANO,
    Taxa = Taxa,
    Tipo = "Observado",
    Periodo = "2000-2024"
  )

# 4) FORECAST (StMoMo) 2020–2029
h <- length(anos_proj)
LCfor <- forecast(LCfit, h = h, level = 95)

ax <- as.numeric(LCfit$ax)
bx <- as.numeric(LCfit$bx)

kt_mean <- as.numeric(LCfor$kt.f$mean)
kt_low  <- as.numeric(LCfor$kt.f$lower)
kt_up   <- as.numeric(LCfor$kt.f$upper)

taxas_proj_mean_100k <- exp(ax + bx %o% kt_mean) * 1e5
taxas_proj_low_100k  <- exp(ax + bx %o% kt_low)  * 1e5
taxas_proj_up_100k   <- exp(ax + bx %o% kt_up)   * 1e5

# Alinhar pesos ao vetor de idades do LCfit (sem NA)
w_alinhado <- data.frame(IDADE_CENTRAL = LCfit$ages) %>%
  left_join(w_idade, by = "IDADE_CENTRAL") %>%
  filter(!is.na(w))

idx <- match(w_alinhado$IDADE_CENTRAL, LCfit$ages)
stopifnot(all(is.finite(idx)))

w_vec <- w_alinhado$w / sum(w_alinhado$w)

# Reordenar as taxas projetadas para as idades com peso (robusto)
taxa_proj_mean  <- as.numeric(colSums(taxas_proj_mean_100k[idx, , drop = FALSE] * w_vec, na.rm = TRUE))
taxa_proj_lower <- as.numeric(colSums(taxas_proj_low_100k[idx, , drop = FALSE]  * w_vec, na.rm = TRUE))
taxa_proj_upper <- as.numeric(colSums(taxas_proj_up_100k[idx, , drop = FALSE]   * w_vec, na.rm = TRUE))

taxa_projetada <- data.frame(
  Ano = anos_proj,
  Taxa = taxa_proj_mean,
  Tipo = "Projetado",
  Periodo = ifelse(anos_proj %in% 2020:2024, "2020-2024", "2025-2029"),
  Taxa_lower_95 = taxa_proj_lower,
  Taxa_upper_95 = taxa_proj_upper
)

# 5) GRÁFICO
dados_totais <- bind_rows(taxa_historica, taxa_projetada)
dados_ic_verde <- subset(taxa_projetada, Periodo == "2020-2024")
dados_ic_azul  <- subset(taxa_projetada, Periodo == "2025-2029")

cores_manual <- c("2000-2024"="black", "2020-2024"="#2E8B57", "2025-2029"="#1E90FF")

p_forecast_su <- ggplot() +
  geom_ribbon(data = dados_ic_verde, aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
              fill = "#2E8B57", alpha = 0.2) +
  geom_ribbon(data = dados_ic_azul,  aes(x = Ano, ymin = Taxa_lower_95, ymax = Taxa_upper_95),
              fill = "#1E90FF", alpha = 0.2) +
  geom_line(data = subset(dados_totais, Tipo == "Observado"),
            aes(x = Ano, y = Taxa, color = Periodo), linewidth = 1.2) +
  geom_line(data = subset(dados_totais, Tipo == "Projetado"),
            aes(x = Ano, y = Taxa, color = Periodo), linewidth = 1.2, linetype = "dashed") +
  geom_point(data = dados_totais, aes(x = Ano, y = Taxa, color = Periodo), size = 2) +
  geom_vline(xintercept = 2019.5,  linetype = "dashed",  color = "red", linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = cores_manual) +
  scale_x_continuous(breaks = c(2000,2009,2019,2029)) +
  scale_y_continuous(limits = c(5, 60), breaks = seq(10, 50, by = 10)) +
  labs(title = "Sul", x = "Ano", y = "Taxa por 100.000 mulheres", color = "Período") +
  theme_minimal() +
  theme(axis.text.x  = element_text(size = 11),
        axis.text.y  = element_text(size = 11),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )

plot(p_forecast_su)

#RMSE E MAPE
anos_eval <- 2020:2024

obs_eval <- taxa_historica %>%
  filter(Ano %in% anos_eval) %>%
  arrange(Ano)

pred_eval <- taxa_projetada %>%
  filter(Ano %in% anos_eval) %>%
  arrange(Ano)

y_obs <- obs_eval$Taxa
y_hat <- pred_eval$Taxa

# RMSE
RMSE_su <- sqrt(mean((y_hat - y_obs)^2))

# MAPE
MAPE_su <- mean(abs((y_obs - y_hat) / y_obs)) * 100
