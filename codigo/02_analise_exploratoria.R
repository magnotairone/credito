# =====================================================================
# 5. Análise descritiva
# =====================================================================

# ---------------------------------------------------------------------
# 5.1 Taxa geral de mau pagadores (ever60_mob6)
# ---------------------------------------------------------------------

# proporção de ever60_mob6 = 1
base_final |>
  summarise(
    taxa_mau = mean(ever60_mob6, na.rm = TRUE),
    total_obs = n_distinct(id)
  )

# distribuição geral
base_final |>
  count(ever60_mob6) |>
  mutate(prop = n / sum(n))

# ---------------------------------------------------------------------
# 5.3 Relação entre score_interno e probabilidade de ever60_mob6
# ---------------------------------------------------------------------

df_score <- base_final |>
  mutate(score_bin = ntile(score_interno, 20)) |>
  group_by(score_bin) |>
  summarise(
    score_med = mean(score_interno, na.rm = TRUE),
    taxa_mau = mean(ever60_mob6, na.rm = TRUE)
  )

ggplot(df_score, aes(x = score_med, y = taxa_mau)) +
  geom_line(linewidth = 1, color = "firebrick") +
  geom_point(color = "firebrick") +
  labs(
    title = "Taxa de mau por faixa de score interno",
    x = "Score médio da faixa",
    y = "Taxa de mau (ever60_mob6)"
  ) +
  theme_minimal()


# ---------------------------------------------------------------------
# 5.4 Distribuição de uso_limite_ratio
# ---------------------------------------------------------------------

ggplot(base_final, aes(x = uso_limite_ratio)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribuição do uso do limite (uso_limite_ratio)",
    x = "uso_limite_ratio",
    y = "Frequência"
  ) +
  theme_minimal()



# ---------------------------------------------------------------------
# 5.6 Heatmap: Score vs Uso do limite (duas dimensões comportamentais)
# ---------------------------------------------------------------------

df_heat <- base_final |>
  mutate(
    uso_bin = ntile(uso_limite_ratio, 10),
    score_bin = ntile(score_interno, 10)
  ) |>
  group_by(score_bin, uso_bin) |>
  summarise(
    taxa_mau = mean(ever60_mob6, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_heat, aes(x = uso_bin, y = score_bin, fill = taxa_mau)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap: taxa de mau por score interno e uso do limite",
    x = "Faixa de uso do limite",
    y = "Faixa de score interno"
  ) +
  theme_minimal()

# =====================================================================
# 5.6 Relação entre taxa de mau e tempo de relacionamento
# =====================================================================

# calcular taxa por meses_desde_entrada
df_rel_tempo <- base_final |>
  group_by(meses_desde_entrada) |>
  summarise(
    taxa_mau = mean(ever60_mob6, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# gráfico
ggplot(df_rel_tempo, aes(x = meses_desde_entrada, y = taxa_mau)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "purple") +
  labs(
    title = "Taxa de mau por tempo de relacionamento",
    x = "Meses desde a entrada",
    y = "Taxa de mau (ever60_mob6)"
  ) +
  theme_minimal()

# ---------------------------------------------------------------------
# 5.7 Correlações simples das principais variáveis
# ---------------------------------------------------------------------

vars_corr <- base_final |>
  select(uso_limite_ratio, uso_limite_m3, uso_limite_m6,
         score_interno, score_m3, ever60_mob6)

cor(vars_corr, use = "complete.obs")
