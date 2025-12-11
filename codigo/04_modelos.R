# =====================================================================
# 7. Separacao de dados em conjuntos treino/validacao/oot
# =====================================================================

library(tidyverse)
library(tidymodels)
library(lubridate)

train_df <- train_df %>% mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("0","1")))
val_df   <- val_df   %>% mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("0","1")))
test_df  <- test_df  %>% mutate(ever60_mob6 = factor(as.character(ever60_mob6), levels = c("0","1")))
