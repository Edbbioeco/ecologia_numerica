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

## Remover comunidades com riqueza < 2 ----

riqueza <- riqueza |>
  dplyr::mutate(riqueza = riqueza |>
                  tibble::column_to_rownames(var = "sp1") |>
                  vegan::specnumber()) |>
  dplyr::filter(riqueza > 1) |>
  dplyr::select(-riqueza)

riqueza

riqueza |> dplyr::glimpse()

## Calcular modelos -----

modelos_curvas <- riqueza |>
  tibble::column_to_rownames(var = "sp1") |>
  vegan::radfit()

modelos_curvas

## Plotar ----

modelos_curvas |> plot()
