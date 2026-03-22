library(dplyr)
library(ggplot2)
library(patchwork)

# ==========================================================
# 1) TRANSFORMAR RESÍDUOS EM DATAFRAME APC
# ==========================================================

res_to_df <- function(fit, years_fit = NULL, scale = TRUE) {
  
  res_obj <- residuals(fit, scale = scale)
  mat <- res_obj$residuals
  
  df <- as.data.frame(as.table(mat))
  names(df) <- c("Idade", "Ano", "Residuo")
  
  df <- df %>%
    mutate(
      Idade  = as.numeric(as.character(Idade)),
      Ano    = as.numeric(as.character(Ano)),
      Coorte = Ano - Idade,
      Residuo = as.numeric(Residuo)
    ) %>%
    filter(is.finite(Residuo))
  
  if (!is.null(years_fit)) {
    df <- df %>% filter(Ano %in% years_fit)
  }
  
  df
}

# ==========================================================
# 2) FUNÇÃO DE PLOT APC (IDADE | PERÍODO | COORTE)
# ==========================================================

plot_res_apc <- function(df, titulo = "", ylim = c(-3, 3)) {
  
  tema_base <- theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
  
  linhas_ref <- list(
    geom_hline(yintercept = 0, linewidth = 0.4, color = "black"),
    geom_hline(yintercept = c(-2, 2),
               linetype = "dashed",
               linewidth = 0.75,
               color = "blue"),
    geom_hline(yintercept = c(-3, 3),
               linetype = "dotted",
               linewidth = 0.4,
               color = "grey40")
  )
  
  p_age <- ggplot(df, aes(Idade, Residuo)) +
    geom_point(alpha = 0.5, size = 0.8) +
    linhas_ref +
    scale_x_continuous(
      breaks = seq(30, 90, by = 20),
      limits = c(30, 90)
    ) +
    coord_cartesian(ylim = ylim) +
    labs(title = paste0(titulo), x = "Idade", y = "Resíduos\n padronizados") +
    tema_base+
    theme(axis.title.y = element_text(size = 11))
  
  p_period <- ggplot(df, aes(Ano, Residuo)) +
    geom_point(alpha = 0.5, size = 0.8) +
    linhas_ref +
    scale_x_continuous(
      breaks = c(2000, 2004, 2009, 2014, 2019),
      limits = c(2000, 2019)
    ) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Período", y = "Resíduos\n padronizados") +
    tema_base+
    theme(axis.title.y = element_text(size = 11))
  
  p_cohort <- ggplot(df, aes(Coorte, Residuo)) +
    geom_point(alpha = 0.5, size = 0.8) +
    linhas_ref +
    scale_x_continuous(
      breaks = seq(1910, 1990, by = 20),
      limits = c(1910, 1990)
    ) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Coorte", y = "Resíduos\n padronizados") +
    tema_base+
    theme(axis.title.y = element_text(size = 11))
  
  p_age | p_period | p_cohort
}

# ==========================================================
# 3) LISTA DE REGIÕES E OBJETOS
# ==========================================================

regioes <- c("no", "nd", "co", "sd", "su","br")

nomes_legiveis <- c(
  no = "Norte",
  nd = "Nordeste",
  co = "Centro-Oeste",
  sd = "Sudeste",
  su = "Sul",
  br= "Brasil"
)

# ==========================================================
# 4) LOOP AUTOMÁTICO
# ==========================================================

plots_regioes <- list()

for (r in regioes) {
  
  fit_lc <- get(paste0("LCfit_", r))
  fit_rh <- get(paste0("fit_renshaw_", r))
  
  df_lc <- res_to_df(fit_lc, years_fit = 2000:2019)
  df_rh <- res_to_df(fit_rh, years_fit = 2000:2019)
  
  p_lc <- plot_res_apc(df_lc, titulo = paste0("Modelo Lee Carter"))
  p_rh <- plot_res_apc(df_rh, titulo = paste0("Modelo Renshaw-Haberman"))
  
  plots_regioes[[r]] <- (p_lc / p_rh) 
}


plots_regioes[["no"]]
plots_regioes[["nd"]]
plots_regioes[["co"]]
plots_regioes[["sd"]]
plots_regioes[["su"]]
plots_regioes[["br"]]

setwd("G:\\Meu Drive\\QX CLOUD\\ENCE TRABALHO - FELIPE DANTAS\\Gráficos da dissertação\\novos\\RH_RESIDUOS")

for (r in names(plots_regioes)) {
  
  nome_legivel <- nomes_legiveis[r]
  
  ggsave(
    filename = paste0("RH_residuos_padronizados_", nome_legivel, ".png"),
    plot = plots_regioes[[r]],
    width = 9,
    height = 6,
    dpi = 300
  )
}