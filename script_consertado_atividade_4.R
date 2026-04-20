# Pacotes ----

library(tidyverse)

library(vegan)

# Dados ----

## Importar ----

dados <- list.files(path = "./dados",
                    full.names = TRUE)

dados

importar_dados <- function(dados){

  dado_importado <- readr::read_table(dados)

    nome <- dados |>
      stringr::str_remove_all("./dados/|.txt")

    assign(nome,
           dado_importado,
           envir = globalenv())

}

purrr::map(dados, importar_dados)

## Visualizar ----

dados |>
  stringr::str_remove_all("./dados/|.txt") |>
  mget(envir = globalenv())

dados |>
  stringr::str_remove_all("./dados/|.txt") |>
  mget(envir = globalenv()) |>
  dplyr::glimpse()

# Diversidade ----
