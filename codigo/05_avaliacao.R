# =====================================================================
# 7. Avaliacao de resultados
# =====================================================================

preds_test <- predict(final_fit, new_data = test_df, type = "prob")|> bind_cols(test_df)
prob_col <- names(preds_test)[grepl("^\\.pred_1$", names(preds_test))]
preds_test <- preds_test|> rename(prob = !!sym(prob_col))
preds_test <- preds_test|> mutate(ever60_num = as.numeric(as.character(ever60_mob6)))

decil_table <- preds_test|>
  mutate(decil = ntile(desc(prob), 10))|>
  group_by(decil)|>
  summarise(
    n = n(),
    prob_media = mean(prob, na.rm = TRUE),
    taxa_mau = mean(ever60_num, na.rm = TRUE),
    n_mau = sum(ever60_num),
    .groups = "drop"
  )|>
  arrange(decil)|>
  mutate(pct_obs = n / sum(n))

decil_table |> write_csv("dados/decil_table_oot.csv")

# gráfico (taxa mau vs prob média)
ggplot(decil_table, aes(x = factor(decil))) +
  geom_col(aes(y = taxa_mau), fill = "firebrick", alpha = 0.8) +
  geom_line(aes(y = prob_media, group = 1), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = prob_media), color = "steelblue", size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "OOT: taxa de mau por decil (barras) e\n prob média prevista (linha)",
       x = "Decil (1 = maior risco)", y = "Taxa / Probabilidade") +
  theme_minimal(base_size = 12)
