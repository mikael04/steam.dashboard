############################################################################################ #
#'  Script criado para criar o banco de dados (json, mongodb) que alimentará a aplicação
#' 
#'  Autor: Mikael
#'  Data: 29/11/22
############################################################################################ #

# 0. Bibliotecas ----
library(dplyr)
library(data.table)
library(dtplyr)

# source("R/fct_help_genres.R")
# source("R/fct_help_dev_names.R")

# 1. Dados originais
## Lendo tabelas csv
df_games <- data.table::fread("data-raw/steam-data/db-1/games.csv", sep = ',')

df_games_selected <- func_manip_vars(df_games, debug = F)


