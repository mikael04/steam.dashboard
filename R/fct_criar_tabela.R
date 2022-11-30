########################################################################################## #
#'  Função criada tabela para banco da steam (seguindo mesmo padrão de criar tabela Coorte)
#'
#'   
#'  Autor: Mikael
#'  Data: 29/11/22
########################################################################################## #

# 0 - Scripts e bibliotecas ----
source("R/fct_get_colnames_df.R")

# 1 - Funçao ----
func_criar_tabela <- function(tabelao, debug){
  teste_interno <- F
  if(teste_interno){
    tabelao <- df_games_selected
    debug <- T
  }
  variaveis <- colnames(tabelao)[-c(1,2,3,4,5)]
  tabelas <- data.frame(nivel = integer(0), Release.date = character(0), language = integer(0), platform = integer(0), n = numeric(0), variavel = character(0), stringsAsFactors = F)
  
  i <- 0
  for(variavel in variaveis){
    i <- i+1
    if(debug){
      print(paste0(round(100*i/length(variaveis)), "%"))
    }
    
    colnames_tabelao <- func_get_colnames_df_unorder(tabelao)
    tabela <- tabelao %>% 
      dplyr::group_by_at(.vars = c(variavel, "UF", "ano_entrada", "idade_entrada_cut", "cod_sexo_pessoa_eq")) %>%
      dplyr::tally() %>%  
      dplyr::collect()
    names(tabela)[1] <- "nivel"
    tabela$variavel <- variavel
    
    tabelas <-  plyr::rbind.fill(tabelas, tabela)
  }
  tabelas
}