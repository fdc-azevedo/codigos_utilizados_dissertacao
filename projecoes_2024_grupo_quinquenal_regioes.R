library(readxl)
library(dplyr)
library(writexl)

projecoes_2024_grupo_quinquenal_regioes <- read_excel(
  "C:/Users/Felipe/Downloads/projecoes_2024_tab2_grupo_quinquenal (1).xlsx"
) %>%
  filter(SEXO == "Mulheres") %>%
  select(
    `GRUPO ETÁRIO`, SEXO, SIGLA, LOCAL,
     all_of(as.character(2000:2030))) %>%
     filter(SIGLA %in% c("BR","CO", "ND", "SU", "SD", "NO")) %>%
  mutate(
  `GRUPO ETÁRIO` = if_else(
    `GRUPO ETÁRIO` %in% c("00-04","05-09","10-14","15-19","20-24","25-29"), "00-29",`GRUPO ETÁRIO`)) %>%
  group_by(SEXO, SIGLA, LOCAL, `GRUPO ETÁRIO`) %>%
  summarise(
    across(
      all_of(as.character(2000:2030)), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = "drop")



write_xlsx(projecoes_2024_grupo_quinquenal_regioes,"G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/IBGE_PROJ_POP/projecoes_2024_grupo_quinquenal_regioes.xlsx")
