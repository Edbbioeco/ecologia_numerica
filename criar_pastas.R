# Criar pastas ----

dir.create("scripts")

dir.create("dados")

# Renomear arquivos ----

## Scripts ----

### Listar scripts ----

scripts <- list.files(pattern = ".R$|.Rmd$") |>
  stringr::str_subset(pattern = "^criar_pastas",
                      negate = TRUE)

scripts

### Renomear scripts ----

file.rename(scripts, paste0("./scripts/", scripts))

## Dados ----

### Listar dados ----

dados <- list.files(pattern = ".csv$|.txt$")

dados

### Renomear scripts ----

file.rename(dados, paste0("./dados/", dados))
