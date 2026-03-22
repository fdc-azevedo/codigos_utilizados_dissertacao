library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(writexl)

# ---- 1) Tabela de controle dos arquivos ----
base_path <- "G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/database_c50/Raw_Data_c50"

arquivos <- tibble::tibble(
  parte = c("2000_2004", "2005_2009",
            "2010_2014", "2015_2019",
            "2020_2023", "2024"),
  file  = c(
    "c50_2000_2004_parte1.csv", "c50_2005_2009_parte2.csv",
    "c50_2010_2014_parte3.csv", "c50_2015_2019_parte4.csv",
    "c50_2020_2023_parte5.csv", "c50_2024_parte6.csv"
  )
) %>%
  mutate(path = file.path(base_path, file))

# ---- 2) Leitura dos CSVs ----
dfs <- arquivos %>%
  mutate(df = map(path, ~ read_csv(.x, show_col_types = FALSE, progress = FALSE))) %>%
  pull(df)

# ---- 3) Colunas comuns ----
comuns <- reduce(map(dfs, names), intersect)

# ---- 4) Seleção de colunas e consolidação ----
cols_keep <- c("DTOBITO", "DTNASC", "SEXO", "RACACOR", "CODMUNRES", "CAUSABAS")
cols_keep <- intersect(cols_keep, comuns)

if (length(cols_keep) == 0) {
  stop("Nenhuma das colunas esperadas foi encontrada nos arquivos. Verifique nomes/encoding.")
}

c50_raw <- dfs %>%
  map(~ .x %>%
        select(all_of(cols_keep)) %>%
        mutate(across(everything(), as.character))
  ) %>%
  bind_rows()

# ---- 5) Tratamento de datas, ano do óbito e idade ----
c50_tratado <- c50_raw %>%
  mutate(
    DTNASC_chr  = str_replace_all(DTNASC,  "[^0-9]", ""),
    DTOBITO_chr = str_replace_all(DTOBITO, "[^0-9]", ""),
    
    DTNASC_chr  = str_pad(DTNASC_chr,  width = 8, side = "left", pad = "0"),
    DTOBITO_chr = str_pad(DTOBITO_chr, width = 8, side = "left", pad = "0"),
    
    ANO_OBITO = suppressWarnings(as.integer(substr(DTOBITO_chr, 5, 8))),
    
    DTNASC_dt  = dmy(DTNASC_chr),
    DTOBITO_dt = dmy(DTOBITO_chr),
    
    IDADE_TRATADA = if_else(
      !is.na(DTNASC_dt) & !is.na(DTOBITO_dt) & DTOBITO_dt >= DTNASC_dt,
      as.integer(floor(time_length(interval(DTNASC_dt, DTOBITO_dt), "years"))),
      NA_integer_
    )
  ) %>%
  filter(SEXO == "2")

# ---- 6) Faixa etária (DE/PARA) ----
DEPARA_FAIXA <- data.frame(
  Idade = c(0, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
  Faixa_Etaria = c("0-29","30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89","90+")
)

breaks_faixa <- c(DEPARA_FAIXA$Idade, Inf)
labels_faixa <- DEPARA_FAIXA$Faixa_Etaria

c50_tratado <- c50_tratado %>%
  mutate(
    FAIXA_TRATADA = cut(
      IDADE_TRATADA,
      breaks = breaks_faixa,
      right = FALSE,
      labels = labels_faixa,
      include.lowest = TRUE
    ) %>% as.character()
  )

# ---- 7) UF e Região ----
estados <- data.frame(
  COD_UF = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29,
             31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB",
         "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS",
         "MS", "MT", "GO", "DF")
)

c50_2000_2024 <- c50_tratado %>%
  mutate(
    COD_UF = suppressWarnings(as.integer(substr(CODMUNRES, 1, 2)))
  ) %>%
  left_join(estados, by = "COD_UF") %>%
  mutate(
    REGIAO_TRATADA = case_when(
      UF %in% c("RO","AC","AM","RR","PA","AP","TO") ~ "Norte",
      UF %in% c("MA","PI","CE","RN","PB","PE","AL","SE","BA") ~ "Nordeste",
      UF %in% c("MG","ES","RJ","SP") ~ "Sudeste",
      UF %in% c("PR","SC","RS") ~ "Sul",
      UF %in% c("MS","MT","GO","DF") ~ "Centro-Oeste",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    FLAG_DECLARACAO = if_else(
      !is.na(DTOBITO)   & DTOBITO   != "" &
        !is.na(DTNASC)    & DTNASC    != "" &
        !is.na(SEXO)      & SEXO      != "" &
        !is.na(CODMUNRES) & CODMUNRES != "",
      "ok",
      "not"
    ),
    IDADE_TRATADA = suppressWarnings(as.integer(IDADE_TRATADA)),
    ANO_OBITO     = suppressWarnings(as.integer(ANO_OBITO))
  ) %>%
  select(
    FLAG_DECLARACAO,
    UF, REGIAO_TRATADA, CODMUNRES,
    DTOBITO, ANO_OBITO, DTNASC,
    FAIXA_TRATADA, IDADE_TRATADA,
    SEXO, RACACOR, CAUSABAS
  )

# ---- 8) Exportação ----
write_xlsx( c50_2000_2024, "G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/database_c50/c50_2000_2024.xlsx")
