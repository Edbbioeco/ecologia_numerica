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

## Dataset: riqueza ----

### Índices de diversidade ----

riqueza_df_div <- data.frame(com = riqueza$sp1,
                             riqueza = riqueza |>
                               dplyr::select(-1) |>
                               vegan::specnumber(),
                             shannon = riqueza |>
                               dplyr::select(-1) |>
                               vegan::diversity() |>
                               as.numeric(),
                             simpson = riqueza |>
                               dplyr::select(-1) |>
                               vegan::diversity(index = "simpson") |>
                               as.numeric()) |>
  dplyr::bind_cols(riqueza |>
                     dplyr::select(-1) |>
                     vegan::renyi(scales = 1:2, hill = TRUE)) |>
  dplyr::rename("Q1" = `1`,
                "Q2" = `2`) |>
  dplyr::mutate(eqpielou = simpson / log(shannon),
                eqhill = Q2 / Q1,
                tratamento = dplyr::case_when(com |>
                                                stringr::str_detect("a$") ~ "A",
                                              com |>
                                                stringr::str_detect("b$") ~ "B",
                                              .default = "C")) |>
  tidyr::drop_na()

riqueza_df_div
