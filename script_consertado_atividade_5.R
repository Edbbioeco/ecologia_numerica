# Pacotes ----

library(tidyverse)

library(vegan)

library(ggview)

library(crayon)

library(performance)

library(DHARMa)

library(ggbeeswarm)

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

## Dataset: macrofauna ----

### Tratar o dataset ----

macrofauna <- macrofauna |>
  dplyr::mutate(Tratamento = locais |>
                  stringr::str_sub(start = 1, end = 1))

macrofauna

macrofauna |> dplyr::glimpse()

### Data frame de diversidade -----

macrofauna_df_div <- data.frame(Locais = macrofauna$locais,
                                Tratamento = c(rep("Superior", 12),
                                               rep("Médio", 18),
                                               rep("Inferior",12))) |>
  dplyr::bind_cols(macrofauna |>
                     dplyr::select(dplyr::where(is.numeric)) |>
                     vegan::renyi(scales = 0:2, hill = TRUE)) |>
  dplyr::rename("Riqueza" = `0`,
                "Q1" = `1`,
                "Q2" = `2`) |>
  dplyr::mutate(eqhill = Q2 / Q1,
                eqhill = dplyr::case_when(eqhill >= 1 ~ 0.9999,
                                          eqhill <= 0 ~ 0.0001,
                                          .default = eqhill |> round(5)))

macrofauna_df_div

### Modelos lineares ----

criar_modelos <- function(var){

  paste0("Criado o modelo para: ", var) |>
    crayon::yellow() |>
    message()

  anova <- aov(macrofauna_df_div[[var]] ~ Tratamento, data = macrofauna_df_div)

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

var <- macrofauna_df_div |>
  dplyr::select(3:6) |>
  names()

var

purrr::walk(var, criar_modelos)

### GLM de poisson: dados de riqueza ----

#### criar o modelo -----

glm_poisson <- glm(Riqueza ~ Tratamento,
                   data = macrofauna_df_div,
                   family = poisson())

#### Performance do modelo ----

glm_poisson |> DHARMa::simulateResiduals(plot = TRUE)

#### Estatísticas do modelo ----

glm_poisson |> summary()

### GLM de Gamma: dados de Q1 e Q2 ----

#### Criar os modelos ----

glm_q1 <- glm(Q1 ~ Tratamento,
              data = macrofauna_df_div,
              family = Gamma())

glm_q2 <- glm(Q2 ~ Tratamento,
              data = macrofauna_df_div,
              family = Gamma())

#### Performance do modelo ----

glm_q1 |> DHARMa::simulateResiduals(plot = TRUE)

glm_q2 |> DHARMa::simulateResiduals(plot = TRUE)

#### Estatísticas do modelo ----

glm_q1 |> summary()

glm_q2 |> summary()

### GLM de Beta ----

#### Criar o modelo ----

glm_beta <- glmmTMB::glmmTMB(eqhill ~ Tratamento,
                             data = macrofauna_df_div,
                             family = glmmTMB::beta_family())

### Performance do modelo ----

glm_beta |> DHARMa::simulateResiduals(plot = TRUE)

#### Estatísticas do modelo ----

glm_beta |> summary()


