# =====================================================================
# 7. Modelos
# =====================================================================

library(tidyverse)
library(tidymodels)
library(lubridate)

# ---------------------------
# 1) garantir target como factor com níveis c("0","1")
# ---------------------------

train_df <- train_df |> mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("1","0")))
val_df   <- val_df   |> mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("1","0")))
test_df  <- test_df  |> mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("1","0")))


# ---------------------------
# 2) definir features (ajuste se necessário)
# ---------------------------
features <- c(
  "uso_limite_ratio", "uso_limite_m3", "uso_limite_m6",
  "score_m3", "uso_trend_1m", "uso_trend_3m",
  "stress_flag", "stress_m6", "coef_sobrelimitado",
  "meses_desde_entrada", "idade", "renda", "limite"
)


# ---------------------------
# 3) recipe (pré-processamento)
#    - imputar mediana para numéricos
#    - normalizar (útil para regressão logística)
#    - remover variáveis de baixa variação
# ---------------------------

rec <- recipe(ever60_mob6 ~ ., data = train_df |> select(all_of(c("ever60_mob6", features)))) |>
  # se houver NAs em variáveis numéricas: imputar mediana
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_nzv(all_predictors())

# ---------------------------
# 4) specs e workflows
# ---------------------------
log_spec <- logistic_reg() |> set_engine("glm") |> set_mode("classification")

xgb_spec <- boost_tree(
  trees = 500,
  tree_depth = 6,
  learn_rate = 0.05,
  loss_reduction = 1,
  sample_size = 0.8
) |>
  set_engine("xgboost", nthread = 1) |> 
  set_mode("classification")

wf_log <- workflow() |> add_model(log_spec) |> add_recipe(rec)
wf_xgb <- workflow() |> add_model(xgb_spec) |> add_recipe(rec)

# ---------------------------
# 5) ajustar modelos no TRAIN
# ---------------------------
fit_log <- fit(wf_log, data = train_df)
fit_xgb <- fit(wf_xgb, data = train_df)

# ---------------------------
# 6) função utilitária para predizer e calcular métricas
#    - devolve lista com preds (dataframe), auc e sensibilidade
# ---------------------------
eval_model <- function(fit_wf, newdata, positive = "1", threshold = 0.5) {
  preds <- predict(fit_wf, new_data = newdata, type = "prob") |> bind_cols(newdata)
  prob_col <- names(preds)[grepl(paste0("^\\.pred_", positive, "$"), names(preds))]
  if (length(prob_col) != 1) {
    if (".pred_1" %in% names(preds)) prob_col <- ".pred_1" else stop("Não encontrou coluna de probabilidade da classe positiva")
  }
  preds <- preds |> rename(prob = !!sym(prob_col))
  # garantir que truth e estimate tenham MESMOS níveis na MESMA ordem
  # truth:
  if (!is.factor(preds$ever60_mob6)) preds <- preds |> mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("0","1")))
  preds <- preds |> mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("0","1")))
  # AUC
  auc_val <- roc_auc(preds, truth = ever60_mob6, prob)$.estimate
  # pred_label com threshold
  preds <- preds |> mutate(pred_class = factor(if_else(prob >= threshold, "1", "0"), levels = c("0","1")))
  # sanity: ensure levels identical
  if (!identical(levels(preds$ever60_mob6), levels(preds$pred_class))) {
    preds <- preds |> mutate(pred_class = factor(as.character(pred_class), levels = levels(ever60_mob6)))
  }
  sens_val <- sensitivity(preds, truth = ever60_mob6, estimate = pred_class)$.estimate
  tibble(auc = auc_val, sens = sens_val)
}

# 3) avaliar na validação
res_log_val <- eval_model(fit_log, val_df, positive = "1", threshold = 0.5) |> mutate(modelo = "logistica")
res_xgb_val <- eval_model(fit_xgb, val_df, positive = "1", threshold = 0.5) |> mutate(modelo = "xgboost")


res_val <- bind_rows(res_log_val, res_xgb_val) |> select(modelo, auc, sens)
res_val |> arrange(desc(auc)) |> print()

# ---------------------------
# Escolher melhor por AUC (val) e re-treinar em TRAIN+VAL (como antes)
# ---------------------------
best_model <- if (res_val$auc[res_val$modelo == "xgboost"] > res_val$auc[res_val$modelo == "logistica"]) "xgboost" else "logistica"
cat("Melhor modelo segundo AUC na validação:", best_model, "\n")

trainval_df <- bind_rows(train_df, val_df)
final_wf <- if (best_model == "xgboost") wf_xgb else wf_log
final_fit <- fit(final_wf, data = trainval_df)

# ---------------------------
# Avaliação final no TEST (OOT)
# ---------------------------
res_test <- eval_model(final_fit, test_df, positive = "1", threshold = 0.5) |> mutate(modelo = "final_model")
res_test |> select(modelo, sens) |> print()

