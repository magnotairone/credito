# =====================================================================
# 1. Geração de dados sintéticos (clientes x meses)
# =====================================================================

library(tidyverse)
library(lubridate)
library(slider)

set.seed(123)

n_clients <- 6000
months <- seq(ymd("2023-01-01"), ymd("2025-12-01"), by = "months")

# --- gerar atributos por cliente (constantes no tempo) ---
clientes <- tibble(
  id_cliente = 1:n_clients,
  id = sprintf("C%05d", id_cliente),   # id: C00001, C00002, ...
  data_entrada = sample(months, n_clients, replace = TRUE),
  idade_base = sample(21:75, n_clients, replace = TRUE),
  renda = rlnorm(n_clients, meanlog = 8, sdlog = 0.5),
  score_interno = rnorm(n_clients, 650, 60)
)

# ver algumas linhas de exemplo
clientes |> slice_head(n = 10)

# --- montar painel apenas a partir de data_entrada e calcular idade ao longo do tempo ---
base <- expand_grid(
  id_cliente = clientes$id_cliente,
  data_ref = months
) |>
  left_join(clientes, by = "id_cliente") |>
  filter(data_ref >= data_entrada) |>   # mantém só meses a partir da entrada
  mutate(
    # meses passados desde a entrada (inteiro)
    meses_desde_entrada = interval(data_entrada, data_ref) %/% months(1),
    # idade que evolui ao longo do tempo
    idade = idade_base + (meses_desde_entrada %/% 12),
    # limite baseado em renda do cliente (varia levemente no tempo)
    limite = renda * runif(n(), 1.0, 2.5),
    # uso do limite com média proporcional ao limite, adicionando ruído temporal
    uso_limite = pmin(limite, rnorm(n(), 0.4 * limite, 0.2 * limite)),
    uso_limite_ratio = uso_limite / limite,
    # probabilidades de atraso dependem do score base
    prob_atraso = plogis(
      -3 +                                # intercepto base
        (650 - score_interno) / 150 +     # score mais baixo => maior risco
        2 * uso_limite_ratio +            # uso do limite aumenta risco
        + 0.015 * pmax(0, 30 - idade)     # risco maior para idade < 30
        + 0.01  * pmax(0, idade - 65)     # risco maior para > 65
        + 0.05 * meses_desde_entrada      # quanto mais tempo de relacionamento, menor o risco
    ),
    atraso = rbinom(n(), 1, prob_atraso)
  ) |>
  # selecionar e reordenar colunas
  select(id, data_ref, data_entrada, meses_desde_entrada, idade, idade_base,
         renda, limite, uso_limite, atraso, score_interno)

# ver algumas linhas de exemplo
base |> slice_sample(n = 10)

base |> filter(id == "C02186") |> view()

base |> group_by(atraso) |> summarise(n = n()) |> mutate(n = n/nrow(base))
