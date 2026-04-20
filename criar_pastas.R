# Criar pastas ----

dir.create("scripts")

dir.create("dados")

# Renomear arquivos ----

## Scripts ----

### Listar scripts ----

scripts <- list.files(pattern = ".R$|.Rmd$")

scripts
