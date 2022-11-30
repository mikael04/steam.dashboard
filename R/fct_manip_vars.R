########################################################################################## #
#'  Função criada manipular banco de dados, retornando banco pronto para contagem
#'
#'   
#'  Autor: Mikael
#'  Data: 29/11/22
########################################################################################## #

# 0 - Scripts e bibliotecas ----
# source("R/fct_get_colnames_df.R")

# 1 - Funçao ----
func_manip_vars <- function(df, debug){
  teste_interno <- F
  if(teste_interno){
    df <- df_games
    debug <- T
  }
  ## Padronizando nomes
  df <- func_clean_names(df)
  
  ## Selecionando variávies que serão utilizadas
  df_selected <- df |> 
    dplyr::select(appid, name, release_date, estimated_owners, price, supported_languages,
                  windows, mac, linux, categories, genres)
}