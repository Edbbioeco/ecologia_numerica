# Pacotes ----

library(tidyverse)

library(vegan)

library(crayon)

library(performance)

library(ggbeeswarm)

library(ggview)

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
  dplyr::mutate(eqpielou = simpson / exp(shannon),
                eqhill = Q2 / Q1,
                tratamento = dplyr::case_when(com |>
                                                stringr::str_detect("a$") ~ "A",
                                              com |>
                                                stringr::str_detect("b$") ~ "B",
                                              .default = "C")) |>
  tidyr::drop_na()

riqueza_df_div

### ANOVA ----

### Criar modelo ----

criar_anovas <- function(var){

  paste0("Criado o modelo para: ", var) |>
    crayon::yellow() |>
    message()

  anova <- aov(riqueza_df_div[[var]] ~ tratamento, data = riqueza_df_div)

  paste0("Pressupostos do modelo de: ", var) |>
    crayon::yellow() |>
    message()

  anova |>
    performance::check_model(check = c("normality",
                                       "homogeneity")) |>
    print()

  anova |>
    performance::check_heteroscedasticity() |>
    print()

  anova |>
    performance::check_normality() |>
    print()

  paste0("Estatísticas do modelo de: ", var) |>
    crayon::yellow() |>
    message()

  anova |>
    summary() |>
    print()

}

var <- riqueza_df_div |>
  dplyr::select(2:8) |>
  names()

var

purrr::walk(var, criar_anovas)

### Gráficos ----

riqueza_df_div |>
  tidyr::pivot_longer(cols = 2:8,
                      names_to = "div",
                      values_to = "diversidade") |>
  ggplot(aes(tratamento, diversidade)) +
  ggbeeswarm::geom_quasirandom(size = 3,
                               shape= 21,
                               stroke = 1,
                               fill = "gray40",
                               width = 0.1) +
  facet_wrap(~div, scales = "free_y") +
  theme_classic() +
  ggview::canvas(height = 10, width = 12)

## Dataset: floresta ----

### Tratar data frame do dataset floresta ----

floresta <- floresta |>
  t() %>%
  .[-1, ] |>
  magrittr::set_colnames(floresta |>
                           t() %>%
                           .[1, ]) |>
  as.data.frame() |>
  dplyr::mutate(dplyr::across(.cols = 1:87,
                              .fns = ~as.numeric(.)))

floresta[is.na(floresta)] <- 0

floresta

floresta |> dplyr::glimpse()

### Índices de diversidade ----

floresta_df_div <- data.frame(com = floresta |> rownames(),
                             riqueza = floresta |>
                               vegan::specnumber(),
                             shannon = floresta |>
                               vegan::diversity() |>
                               as.numeric(),
                             simpson = floresta |>
                               vegan::diversity(index = "simpson") |>
                               as.numeric()) |>
  dplyr::bind_cols(floresta |>
                     vegan::renyi(scales = 1:2, hill = TRUE)) |>
  dplyr::rename("Q1" = `1`,
                "Q2" = `2`) |>
  dplyr::mutate(eqpielou = simpson / exp(shannon),
                eqhill = Q2 / Q1,
                tratamento = com |> stringr::str_sub(start = 1, end = 1)) |>
  tidyr::drop_na()

floresta_df_div

### ANOVA ----

### Criar modelo ----

criar_anovas <- function(var){

  paste0("Criado o modelo para: ", var) |>
    crayon::yellow() |>
    message()

  anova <- aov(floresta_df_div[[var]] ~ tratamento, data = floresta_df_div)

  paste0("Pressupostos do modelo de: ", var) |>
    crayon::yellow() |>
    message()

  anova |>
    performance::check_model(check = c("normality",
                                       "homogeneity")) |>
    print()

  anova |>
    performance::check_heteroscedasticity() |>
    print()

  anova |>
    performance::check_normality() |>
    print()

  paste0("Estatísticas do modelo de: ", var) |>
    crayon::yellow() |>
    message()

  anova |>
    summary() |>
    print()

}

var <- floresta_df_div |>
  dplyr::select(2:8) |>
  names()

var

purrr::walk(var, criar_anovas)

### Gráficos ----

floresta_df_div |>
  tidyr::pivot_longer(cols = 2:8,
                      names_to = "div",
                      values_to = "diversidade") |>
  ggplot(aes(tratamento, diversidade)) +
  ggbeeswarm::geom_quasirandom(size = 3,
                               shape= 21,
                               stroke = 1,
                               fill = "gray40",
                               width = 0.1) +
  facet_wrap(~div, scales = "free_y") +
  theme_classic() +
  ggview::canvas(height = 10, width = 12)
