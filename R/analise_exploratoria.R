############################################################################################ #
#'  Script criado para analisar os dados da steam
#' 
#'  Autor: Mikael
#'  Data: 13/08/22
############################################################################################ #

# 0. Bibliotecas ----
library(dplyr)
library(data.table)
library(dtplyr)

# 1. Lendo os dados ----
dados <- read.table("data-raw/steam-data/applicationGenres.csv", col.names = paste("V",1:25), fill = T, sep = ",")
dados <- dados[,which(!is.na(dados[1,]))]

## 1.1 Organizando os nomes das colunas
colnames(dados) <- c("ID", "Categoria1", "Categoria2", "Categoria3", "Categoria4",
                     "Categoria5", "Categoria6", "Categoria7", "Categoria8")

## Vetor com todos os generos que aparecem na tabela

func_get_all_genres <- function(dados){
  all_cols <- c("")
  len_dados <- length(dados)
  for(i in 2:len_dados){
    unique_col <- unique(dados[,i])
    all_cols <- unique(c(all_cols, unique_col))
  }
  all_cols[all_cols != ""]
}

all_genres <- sort(func_get_all_genres(dados))

## Criando novo DF, no formato de uma coluna para cada gênero
df <- dados |> 
  dplyr::select(ID)

df[all_genres] <- NA

for(i in 1:length(all_genres)){
  check <- F
  for(j in 1:nrow(df)){
    if(all_genres[i] %in% dados[j,]){
      df[j,i+1] = T
    }else{
      df[j,i+1] = F
    }
  }
}

## Escrevendo novo dataframe
# data.table::fwrite(df, "data-raw/generos_tratados.csv")

func_count_genres <- function(df){
  colnames_df <- colnames(df)
  df_counts <- as.data.frame(colnames_df[colnames_df != 'ID']) |> 
    dplyr::rename(genres = 1)
  df_counts$count <- NA
  for(i in 2:length(df)){
    df_counts[i-1, 2] <- sum(df[,i])
  }
  df_counts
}

## Criando tabela de contagens para cada gênero
df_counts <- func_count_genres(df)

## Gráfico de barras com os 10 gêneros que mais aparecem
df_counts_top10 <- df_counts |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10) |> 
  tidyr::pivot_wider(names_from = genres, values_from = count)

library(ggplot2)

df_counts_top10 <- df_counts |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10)

fig_top10 <- ggplot(df_counts_top10, aes(x = count, y = genres)) +
  geom_col()
fig

df_counts_last10 <- df_counts |> 
  dplyr::arrange(count) |> 
  dplyr::slice_head(n = 10)

## Gráfico com conjunto de dois gêneros mais vistos em conjunto

df_sample <- df |> 
  dplyr::sample_n(size = 30)

df_sample[,3] == T
