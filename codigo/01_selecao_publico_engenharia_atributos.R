# =====================================================================
# 2. Criar variável alvo: ever60_mob6
# =====================================================================

base <- base |>
  group_by(id) |>
  arrange(data_ref) |>
  mutate(
    ever60_mob6 = slider::slide_int(
      atraso,
      ~ as.integer(any(.x == 1)),
      .before = 0,
      .after = 5
    )
  ) |>
  ungroup()

base |> filter(id == "C02186") |> view()

base |> write_csv("dados/base_com_ever60_mob6.csv")

# =====================================================================
# 3. Seleção de público
# =====================================================================

clientes_com_6m <- base |>
  group_by(id) |>
  summarise(n_meses = max(meses_desde_entrada) + 1, .groups = "drop") |> 
  # +1 pois meses_desde_entrada começa em 0 para o mês de entrada
  filter(n_meses >= 6) |>
  pull(id)

base_clientes_selecionados <- base |>
  filter(id %in% clientes_com_6m)

# Além disso, para evitar observações cujo rótulo "próximos 6 meses" esteja censurado
# — por exemplo, linhas muito próximas do fim do período disponível — filtrar data_ref tal que
# exista janelas futuras completas de 6 meses no conjunto de dados.
(ultimo_data_ref <- max(base$data_ref))
(corte_data_ref <- ultimo_data_ref %m-% months(6))  # último data_ref que ainda tem +6 meses observáveis

base_final <- base_clientes_selecionados |>
  filter(data_ref <= corte_data_ref)

# Resumo
cat("Observações totais originais:", nrow(base), "\n")
cat("Clientes com >= 6 meses:", length(unique(base_clientes_selecionados$id)), "\n")
cat("Observações após filtro clientes >=6m:", nrow(base_clientes_selecionados), "\n")
cat("Observações após filtro para janelas futuras observáveis (treino):", nrow(base_final), "\n")

# =====================================================================
# 4. Engenharia de atributos
# =====================================================================

base_final <- base_final |>
  group_by(id) |>
  arrange(data_ref) |>
  mutate(
    uso_limite_ratio = uso_limite / limite,
    
    # Média móvel de 3 meses do uso do limite
    # Captura suavização do comportamento recente de utilização do crédito.
    uso_limite_m3 = slide_dbl(uso_limite_ratio, mean, .before = 3, .complete = TRUE),
    
    # Média móvel de 6 meses do uso do limite
    # Mede tendência de uso em janelas mais longas, reduzindo ruído mensal.
    uso_limite_m6 = slide_dbl(uso_limite_ratio, mean, .before = 6, .complete = TRUE),
    
    # Média móvel de 3 meses do score interno
    # Representa uma versão suavizada do risco cadastral/histórico, útil para capturar deteriorações graduais.
    score_m3 = slide_dbl(score_interno, mean, .before = 3, .complete = TRUE),
    
    # Tendência de aumento de uso do limite no último mês
    # Sinaliza deterioração imediata na capacidade financeira.
    uso_trend_1m = uso_limite_ratio - lag(uso_limite_ratio, 1),
    
    # Tendência de aumento de uso do limite nos últimos 3 meses
    # Indicador mais robusto do que movimentos abruptos; detecta aceleração de consumo.
    uso_trend_3m = uso_limite_ratio - lag(uso_limite_ratio, 3),
    
    # Flag indicando uso acima de 80% do limite
    stress_flag = as.integer(uso_limite_ratio > 0.8),
    
    # Proporção de meses, nos últimos 6, em que o cliente esteve em situação de estresse (uso > 80%)
    # Mede persistência do comportamento arriscado.
    stress_m6 = slide_dbl(stress_flag, mean, .before = 6, .complete = TRUE),
    
    # Relação limite/renda do cliente
    # Estima quão “alavancado” está o cliente. Limite muito alto relativo à renda sinaliza risco.
    coef_sobrelimitado = limite / renda
  ) |>
  ungroup()

base_final |> filter(id == "C02186") |> view()

base_final |> write_csv("dados/base_final_modelagem.csv")
