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


### Data frame de diversidade ----

florestatot_df_div <- data.frame(com = FlorestaTot$Locais) |>
  dplyr::bind_cols(FlorestaTot |>
                     dplyr::select(-1) |>
                     vegan::renyi(scales = 0:2, hill = TRUE)) |>
  dplyr::rename("Riqueza" = `0`,
                "Q1" = `1`,
                "Q2" = `2`) |>
  dplyr::mutate(eqhill = Q2 / Q1)

florestatot_df_div

### Gráfico ----

florestatot_df_div |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "tipo",
                      values_to = "Diversidade") |>
  ggplot(aes(com, Diversidade)) +
  geom_col() +
  facet_wrap(~tipo, scales = "free_y") +
  theme_classic() +
  ggview::canvas(height = 10, width = 12)
