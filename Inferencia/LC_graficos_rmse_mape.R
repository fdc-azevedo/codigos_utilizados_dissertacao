library(patchwork)

# =============================================================================
# DIAGNÓSTICOS — LAYOUT 3x2 (SEM ESPAÇOS, TAMANHO UNIFORME)
# - Linha 1: Norte | Centro-Oeste | Nordeste
# - Linha 2: Sul | Sudeste | (vazio)
# =============================================================================

setwd("G:\\Meu Drive\\QX CLOUD\\ENCE TRABALHO - FELIPE DANTAS\\Gráficos da dissertação\\novos\\LC_RESIDUOS")

# 1) Histograma dos resíduos
(p_hist_no   | p_hist_centro | p_hist_nd) /
  (p_hist_su | p_hist_sd     |p_hist_brasil)
# Salvar o resultado
ggsave("LC_hist.png", 
       width = 12, 
       height = 6,
       dpi = 300)
# 2) Q-Q plot dos resíduos
(p_qq_no   | p_qq_centro | p_qq_nd) /
  (p_qq_su | p_qq_sd     | p_qq_brasil)
# Salvar o resultado
ggsave("LC_qq.png", 
       width = 12, 
       height = 6,
       dpi = 300)
# 3) Resíduos vs log(taxa de mortalidade estimada)
(p_res_fit_no   | p_res_fit_centro | p_res_fit_nd) /
  (p_res_fit_su | p_res_fit_sd     | p_res_fit_brasil)
# Salvar o resultado
ggsave("LC_res.png", 
       width = 12, 
       height = 6,
       dpi = 300)
# 4) ACF dos resíduos
(p_acf_no | p_acf_centro | p_acf_nd) /
  (p_acf_su | p_acf_sd   | p_acf_brasil)
# Salvar o resultado
ggsave("LC_acf.png", 
       width = 12, 
       height = 6,
       dpi = 300)
setwd("G:\\Meu Drive\\QX CLOUD\\ENCE TRABALHO - FELIPE DANTAS\\Gráficos da dissertação\\novos\\LC_FORECAST")

# forecast
(p_forecast_no | p_forecast_centro | p_forecast_nd) /
  (p_forecast_su | p_forecast_sd   | p_forecast_brasil)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Salvar o resultado
ggsave("LC_forecast.png", 
       width = 12, 
       height = 6,
       dpi = 300)