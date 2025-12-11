# =====================================================================
# 6. Separacao de dados em conjuntos treino/validacao/oot
# =====================================================================

library(lubridate)
library(tidyverse)

base_final <- read_csv("dados/base_final_modelagem.csv")

train_start <- min(base_final$data_ref)
train_end  <- ymd("2024-02-01")
val_end    <- ymd("2024-10-01")
test_end   <- max(base_final$data_ref)

# início da partição seguinte deve ser > (end + 6 meses)
val_start  <- train_end %m+% months(6)  
test_start <- val_end   %m+% months(6)

# construir partições
base_splits <- base_final |>
  mutate(
    partition = case_when(
      data_ref <= train_end           ~ "train",
      data_ref >= val_start & data_ref <= val_end ~ "val",
      data_ref >= test_start          ~ "test",
      TRUE                            ~ "gap"   # datas que caem no espaço proibido 
    )
  )

base_splits |> 
  group_by(partition) |> 
  summarise(n = n())


# separar dfs finais (removendo 'gap')
train_df <- base_splits |> filter(partition == "train")
val_df   <- base_splits |> filter(partition == "val")
test_df  <- base_splits |> filter(partition == "test")
