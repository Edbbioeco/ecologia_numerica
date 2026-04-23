# Pacotes ----

library(tidyverse)

library(vegan)

library(ggview)

# Dados ----

## Importar ----

riqueza <- readr::read_table("./dados/riqueza.txt")

## Visualizar ----

riqueza

riqueza |> dplyr::glimpse()

# Curva de abundância ----

## Calcular modelos -----

modelos_curvas <- riqueza |>
  tibble::column_to_rownames(var = "sp1") |>
  vegan::radfit()

modelos_curvas
