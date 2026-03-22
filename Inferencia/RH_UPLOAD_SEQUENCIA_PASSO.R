# =============================================================================
# MASTER SCRIPT — EXECUÇÃO SEQUENCIAL LC / STMOMO1
# Ordem respeita dependência entre objetos
# =============================================================================

base_path <- "G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/RH/REGIAO"  
base_2 <- "G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/RH1" 

run <- function(file) {
  message("Executando: ", file)
  source(file, local = .GlobalEnv)
}
#OBJETOS
library(readxl)
DF_FAIXA <- read_excel("G:/Meu Drive/QX CLOUD/ENCE TRABALHO - FELIPE DANTAS/bases/c50_populacao/base_CM_POP_IDADE_QUINQUENAL.xlsx")
faixas_excluir <- c("0-29","90+")


# -----------------------------------------------------------------------------
# BRASIL (BRASIL)
# -----------------------------------------------------------------------------
run(file.path(base_2, "br_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "br_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "br_3_RH_CRIAR_FORECAST_STMOMO1.R"))

# -----------------------------------------------------------------------------
# CENTRO-OESTE (CO)
# -----------------------------------------------------------------------------
run(file.path(base_2, "co_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "co_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "co_3_RH_CRIAR_FORECAST_STMOMO1.R"))

# -----------------------------------------------------------------------------
# NORDESTE (ND)
# -----------------------------------------------------------------------------
run(file.path(base_2, "nd_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "nd_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "nd_3_RH_CRIAR_FORECAST_STMOMO1.R"))

# -----------------------------------------------------------------------------
# NORTE (NO)
# -----------------------------------------------------------------------------
run(file.path(base_2, "no_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "no_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "no_3_RH_CRIAR_FORECAST_STMOMO1.R"))

# -----------------------------------------------------------------------------
# SUDESTE (SD)
# -----------------------------------------------------------------------------
run(file.path(base_2, "sd_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "sd_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "sd_3_RH_CRIAR_FORECAST_STMOMO1.R"))

# -----------------------------------------------------------------------------
# SUL (SU)
# -----------------------------------------------------------------------------
run(file.path(base_2, "su_1_RH_CRIAR_PARAMETROS_STMOMO.R"))
run(file.path(base_path, "su_2_RH_CRIAR_RESIDUOS_STMOMO1.R"))
run(file.path(base_path, "su_3_RH_CRIAR_FORECAST_STMOMO1.R"))


message("Execução finalizada com sucesso.")

