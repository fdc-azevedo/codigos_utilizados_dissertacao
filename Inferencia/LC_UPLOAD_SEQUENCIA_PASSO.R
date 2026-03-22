# MASTER SCRIPT — EXECUÇÃO SEQUENCIAL
library(readxl)

#PASTAS
base_path <- "G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/LEE_CARTER/REGIAO"  

run <- function(file) {
  message("Executando: ", file)
  source(file, local = .GlobalEnv)
}

#OBJETOS
DF_FAIXA <- read_excel("G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/c50_populacao/base_CM_POP_IDADE_QUINQUENAL.xlsx")
faixas_excluir <- c("0-29","90+")

# BRASIL (BRASIL)
run(file.path(base_path, "BR_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "BR_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "BR_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))

# CENTRO-OESTE (CO)
run(file.path(base_path, "CO_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "CO_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "CO_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))

# NORDESTE (ND)
run(file.path(base_path, "ND_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "ND_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "ND_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))

# NORTE (NO)
run(file.path(base_path, "NO_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "NO_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "NO_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))

# SUDESTE (SD)
run(file.path(base_path, "SD_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "SD_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "SD_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))

# SUL (SU)
run(file.path(base_path, "SU_1_LC_FAIXA_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "SU_2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO.R"))
run(file.path(base_path, "SU_3_LC_FAIXA_CRIAR_FORECAST_STMOMO.R"))


message("Execução finalizada com sucesso.")
