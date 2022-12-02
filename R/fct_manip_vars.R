########################################################################################## #
#'  Função criada manipular banco de dados, retornando banco pronto para contagem
#'
#'   
#'  Autor: Mikael
#'  Data: 29/11/22
########################################################################################## #

# 0 - Scripts e bibliotecas ----
library(dplyr)
library(data.table)
library(dtplyr)
# source("R/fct_get_colnames_df.R")

# 1 - Funçao ----
func_manip_vars <- function(df, debug){
  teste_interno <- F
  if(teste_interno){
    df <- df_games
    debug <- T
  }
  ## Padronizando nomes
  df <- func_clean_names(df, debug)
  
  ## Selecionando variávies que serão utilizadas
  df_selected <- df |> 
    dplyr::select(appid, name, release_date, estimated_owners, price, supported_languages,
                  windows, mac, linux, categories, genres)
  
  ## Ajustando o ano
  df_selected <- df_selected |> 
    # dplyr::mutate(release_year = format(as.Date(release_date), "%Y"))
    dplyr::mutate(release_year = sub('.*(?=.{4}$)', '', release_date, perl=T))
  
  ## Ajustando as plataformas
  df_selected <- df_selected |> 
    dplyr::mutate(platform = linux*100 + mac*10 + windows)
  
}