# Pacotes ----

library(tidyverse)

library(vegan)

library(crayon)

library(performance)

library(ggbeeswarm)

library(ggview)

library(magrittr)

# Dados ----

## Importar ----

dados <- list.files(path = "dados/",
                    full.names = TRUE) |>
  stringr::str_subset("FlorestaTot|macrofauna")

dados

importar_dados <- function(dados){

  dados_importados <- readr::read_table(dados)

  nome <- dados |>
    stringr::str_remove_all("dados/|.txt")

  assign(nome,
         dados_importados,
         envir = globalenv())

}

purrr::map(dados, importar_dados)

## Visualizar ----

dados |>
  stringr::str_remove_all("dados/|.txt") |>
  mget(envir = globalenv())

# Diversidade ----

## Dataset: FlorestaTot ----

### Tratar o dataset ----

FlorestaTot <- FlorestaTot |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Locais",
                      values_to = "abundancia") |>
  tidyr::pivot_wider(names_from = sps,
                     values_from = abundancia)

FlorestaTot

