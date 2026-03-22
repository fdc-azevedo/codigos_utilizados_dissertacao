# =============================================================================
# 2_LC_FAIXA_CRIAR_RESIDUOS_STMOMO
# =============================================================================

library(StMoMo)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1) CHECAGENS
# -----------------------------------------------------------------------------
stopifnot(exists("LCfit"))
stopifnot(inherits(LCfit, "fitStMoMo"))

# -----------------------------------------------------------------------------
# 2) RESÍDUOS (DEVIANCE RESIDUALS – SCALED)
# -----------------------------------------------------------------------------
res_obj <- residuals(LCfit, scale = TRUE)
res_mat <- res_obj$residuals

res_vec <- as.numeric(res_mat)
res_vec <- res_vec[is.finite(res_vec)]

# -----------------------------------------------------------------------------
# 3) DIAGNÓSTICOS NATIVOS StMoMo
# -----------------------------------------------------------------------------

#plot(res_obj, type = "scatter")

# Interpretação:
# Nuvem sem padrão sistemático indica bom ajuste global.
# Estruturas persistentes sugerem mis-specification.

# -----------------------------------------------------------------------------
# 4) DIAGNÓSTICOS COMPLEMENTARES
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 4.1 Histograma dos resíduos
# -----------------------------------------------------------------------------

p_hist <- ggplot(data.frame(res = res_vec),aes(x = res)) +
          geom_histogram(aes(y = ..density..),bins = 30,fill = "lightblue",color = "black",alpha = 0.7) +
          stat_function(fun = dnorm,args = list(mean = mean(res_vec), sd = sd(res_vec)), linetype = "dashed") +
          geom_vline(xintercept = 0, linetype = "dashed") +
          theme_minimal() +
  scale_x_continuous(breaks = c(-4,-2, 0, 2, 4),
                     limits = c(-4, 4))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.2), limits = c(0, 0.65))+
  labs(title = "Centro-Oeste", x = "Resíduo",y = "Densidade")+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

p_hist
p_hist_centro <- p_hist

# Interpretação:
# Espera-se simetria aproximada e centro em zero.

# 4.2 Q-Q plot
p_qq <- ggplot(data.frame(res = res_vec), aes(sample = res)) +
        stat_qq(alpha = 0.6) +
        stat_qq_line(linetype = "dashed") +
        theme_minimal() +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3.4))+
  labs( title = "Centro-Oeste",  x = "Quantis teóricos",y = "Quantis observados" )+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

p_qq
p_qq_centro <- p_qq

# Interpretação:
# Desvios nas caudas são comuns em mortalidade.

# 4.3 Resíduos vs ajustado
mx_hat <- fitted(LCfit, type = "rates")
mx_hat_vec <- as.numeric(mx_hat)

ok <- is.finite(res_vec) & is.finite(mx_hat_vec) & mx_hat_vec > 0

df_res_fit <- data.frame(log_mx = log(mx_hat_vec[ok]),res = res_vec[ok])

p_res_fit <- ggplot(df_res_fit, aes(x = log_mx, y = res)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "loess", se = FALSE) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            theme_minimal() +
            scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3.4))+
            labs(title = "Centro-Oeste", x = "ln(m̂ₓ,ₜ)",  y = "Resíduo")+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

p_res_fit
p_res_fit_centro <- p_res_fit

# Interpretação:
# Curvatura indica viés por nível; funil indica heterocedasticidade.

# -----------------------------------------------------------------------------
# 5. TESTES ESTATÍSTICOS
# -----------------------------------------------------------------------------

# 5.1 Teste de normalidade (Shapiro–Wilk)

shapiro_test <- NULL
if (length(res_vec) >= 10 && length(res_vec) <= 5000) {
  shapiro_test <- shapiro.test(res_vec)
}

shapiro_test_centro <- shapiro_test
shapiro_test_centro

# AUTOCORRELAÇÃO DOS RESÍDUOS — DIAGNÓSTICO (ACF)

p_acf_centro <- ggAcf(res_vec, lag.max = 20) + 
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  labs(title = "Centro-Oeste", x = "Defasagem", y = "ACF")+
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

# AUTOCORRELAÇÃO DOS RESÍDUOS — TESTE FORMAL (LJUNG–BOX)

Ljung_Box <- Box.test(
  res_vec,
  lag  = 10,
  type = "Ljung-Box"
)
Ljung_Box_centro <- Ljung_Box

# 6) RESÍDUOS POR IDADE

res_df <- as.data.frame(as.table(res_mat))
colnames(res_df) <- c("IDADE", "ANO", "RES")

res_df$IDADE <- as.numeric(as.character(res_df$IDADE))
res_df$ANO <- as.numeric(as.character(res_df$ANO))

p_res_age <- ggplot(res_df, aes(x = factor(IDADE), y = RES)) +
  geom_boxplot(outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Resíduos por idade - Centro-Oeste", x = "Idade central", y = "Resíduo") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#p_res_age
#p_res_age_centro <- p_res_age