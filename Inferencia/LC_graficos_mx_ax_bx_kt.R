library(patchwork)

# =============================================================================
# LAYOUT 3x2 SEM ESPAÇOS (TODOS OS PAINÉIS COM MESMO TAMANHO)
# - Linha 1: Norte | Centro-Oeste | Nordeste
# - Linha 2: Sul | Sudeste | (vazio)
# =============================================================================


setwd("G:\\Meu Drive\\QX CLOUD\\ENCE TRABALHO - FELIPE DANTAS\\Gráficos da dissertação\\novos\\LC_PARAMETROS")

# TAXAS POR IDADE
(p_taxa_idade_no | p_taxa_idade_centro | p_taxa_idade_nd) /
  (p_taxa_idade_su | p_taxa_idade_sd     | p_taxa_idade_brasil)+
  plot_annotation(
    title =  "Taxa de Mortalidade Neoplasia maligna de mama")


# Salvar o resultado
ggsave("LC_taxa_mort.png", 
       width = 16, 
       height = 10,
       dpi = 300)
# ax
(p_ax_no | p_ax_centro | p_ax_nd) /
  (p_ax_su | p_ax_sd     | p_ax_brasil)
# Salvar o resultado
ggsave("LC_ax.png", 
       width = 12, 
       height = 6,
       dpi = 300)
# bx
(p_bx_no | p_bx_centro | p_bx_nd) /
  (p_bx_su | p_bx_sd     | p_bx_brasil)
# Salvar o resultado
ggsave("LC_bx.png", 
       width = 12, 
       height = 6,
       dpi = 300)
#kt 
(p_kt_no | p_kt_centro | p_kt_nd) /
  (p_kt_su | p_kt_sd     | p_kt_brasil)

# Salvar o resultado
ggsave("LC_kt.png", 
       width = 12, 
       height = 6,
       dpi = 300)

