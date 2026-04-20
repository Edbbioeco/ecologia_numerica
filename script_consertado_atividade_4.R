# Pacotes ----

library(tidyverse)

library(vegan)

# Dados ----

## Importar ----

riqueza <- readr::read_table("./dados/riqueza.txt")

## Visualizar ----

riqueza

riqueza |> dplyr::glimpse()
