library(dplyr)
library(tidyr)
library(writexl)
# -----------------------------
# 1) POPULAÇÃO: wide -> long
# -----------------------------
pop_long <- projecoes_2024_grupo_quinquenal_regioes %>%
  transmute(
    FAIXA = `GRUPO ETÁRIO`,
    LOCAL
  ) %>%
  bind_cols(
    projecoes_2024_grupo_quinquenal_regioes %>% select(all_of(as.character(2000:2029)))
  ) %>%
  pivot_longer(
    cols = all_of(as.character(2000:2029)),
    names_to = "Atributo",
    values_to = "Pop"
  ) %>%
  mutate(
    Atributo = as.integer(Atributo),
    Pop = suppressWarnings(as.numeric(Pop))
  )

# -----------------------------
# 2) CM: contagem de linhas com FLAG_DECLARACAO == "ok"
# -----------------------------
cm_long <- c50_2000_2024 %>%
  filter(FLAG_DECLARACAO == "ok") %>%
  transmute(
    FAIXA = FAIXA_TRATADA,
    LOCAL = REGIAO_TRATADA,
    Atributo = as.integer(ANO_OBITO)
  ) %>%
  filter(!is.na(FAIXA), !is.na(LOCAL), !is.na(Atributo)) %>%
  count(FAIXA, LOCAL, Atributo, name = "CM")

# (opcional) Brasil como soma das regiões, se sua planilha tiver LOCAL == "BR" ou "Brasil"
cm_br <- cm_long %>%
  group_by(FAIXA, Atributo) %>%
  summarise(CM = sum(CM), .groups = "drop") %>%
  mutate(LOCAL = "Brasil")   # ajuste para "Brasil" se for o caso

cm_long <- bind_rows(cm_long, cm_br)

# -----------------------------
# 3) Join final
# -----------------------------
base_CM_POP_IDADE_QUINQUENAL <- pop_long %>%
  mutate(
    FAIXA = if_else(FAIXA == "00-29", "0-29", FAIXA),
    Atributo = as.integer(Atributo)
  ) %>%
  left_join(
    cm_long %>% mutate(Atributo = as.integer(Atributo)),
    by = c("FAIXA", "LOCAL", "Atributo")
  ) %>%
  mutate(CM = coalesce(CM, 0L)) %>%
  select(FAIXA, LOCAL, Atributo, Pop, CM)

# ---- 8) Exportação ----
write_xlsx(base_CM_POP_IDADE_QUINQUENAL,"G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/c50_populacao/base_CM_POP_IDADE_QUINQUENAL.xlsx")


