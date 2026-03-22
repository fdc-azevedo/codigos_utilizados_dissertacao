library(patchwork)

# =============================================================================
# LAYOUT 3x2 SEM ESPAÇOS (TODOS OS PAINÉIS COM MESMO TAMANHO)
# - Linha 1: Norte | Centro-Oeste | Nordeste
# - Linha 2: Sul | Sudeste | (vazio)
# =============================================================================

setwd("G:\\Meu Drive\\QX CLOUD\\ENCE TRABALHO - FELIPE DANTAS\\Gráficos da dissertação\\novos\\RH_PARAMETROS")

# ax
(p_ax_renshaw_no | p_ax_renshaw_co | p_ax_renshaw_nd) /
  (p_ax_renshaw_su | p_ax_renshaw_sd     | p_ax_renshaw_br)

ggsave("1RH_ax.png",  width = 12,  height = 6, dpi = 300)

# b1x
(p_b1x_renshaw_no | p_b1x_renshaw_co | p_b1x_renshaw_nd) /
  (p_b1x_renshaw_su | p_b1x_renshaw_sd     | p_b1x_renshaw_br)

ggsave("1RH_b1x.png",  width = 12,  height = 6, dpi = 300)

# b0x – Sensibilidade à coorte
(p_b0x_renshaw_no | p_b0x_renshaw_co | p_b0x_renshaw_nd) /
  (p_b0x_renshaw_su | p_b0x_renshaw_sd     | p_b0x_renshaw_br)

ggsave("1RH_b0x.png",  width = 12,  height = 6, dpi = 300)

#kt 
(p_kt_renshaw_no | p_kt_renshaw_co | p_kt_renshaw_nd) /
  (p_kt_renshaw_su | p_kt_renshaw_sd     | p_kt_renshaw_br)

ggsave("1RH_kt.png",  width = 12,  height = 6, dpi = 300)

#kt 
(p_gc_renshaw_no | p_gc_renshaw_co | p_gc_renshaw_nd) /
  (p_gc_renshaw_su | p_gc_renshaw_sd     | p_gc_renshaw_br)
   

# Salvar o resultado
ggsave("1RH_gc.png",  width = 12,  height = 6, dpi = 300)

# forecast
(p_forecast_renshaw_no | p_forecast_renshaw_co | p_forecast_renshaw_nd) /
  (p_forecast_renshaw_su | p_forecast_renshaw_sd   | p_forecast_renshaw_br)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("1RH_forecast.png",  width = 12,  height = 6, dpi = 300)



RMSE_co
RMSE_no
RMSE_nd
RMSE_sd
RMSE_su
RMSE_br

MAPE_co
MAPE_no
MAPE_nd
MAPE_sd
MAPE_su
MAPE_br