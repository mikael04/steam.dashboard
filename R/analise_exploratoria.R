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

# 1. Dados de generos ----
dados <- read.table("data-raw/steam-data/applicationGenres.csv", col.names = paste("V",1:25), fill = T, sep = ",")
dados <- dados[,which(!is.na(dados[1,]))]

## 1.1.1 Organizando os nomes das colunas
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

## 1.1. Tabela de contagens para cada gênero ----
df_counts <- func_count_genres(df)

## Gráfico de barras com os 10 gêneros que mais aparecem
df_counts_top10 <- df_counts |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10) |> 
  tidyr::pivot_wider(names_from = genres, values_from = count)

df_counts_top10 <- df_counts |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10)


### 1.1.1. Gráfico top10 gêneros ----
library(ggplot2)

fig_top10 <- ggplot(df_counts_top10, aes(x = count, y = genres)) +
  geom_col() +
  theme_minimal()
fig_top10

df_counts_last10 <- df_counts |> 
  dplyr::arrange(count) |> 
  dplyr::slice_head(n = 10)

## 1.2. Dois gêneros mais vistos em conjunto ----

df_sample <- df |> 
  dplyr::sample_n(size = 30)

sum(df_sample[, "Action"] & df_sample[,4] == T)
sum(df_double)
df_sample[,3] == T
df_sample[,4] == T
df_sample[,2]

top10_genres <- df_counts_top10 |> 
  dplyr::select(genres) |> 
  dplyr::pull()


## Criando dfs base para contagem de comparativo
df_comp<- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_comp) <- c("genres", "count")
k <- 1
for(i in 1:length(top10_genres)){
  print(paste0("Comecando a buscar duplas no genero ", top10_genres[i]))
  for(j in 2:(ncol(df)-1)){
    print(paste0("Comparando com o genero ", all_genres[j]))
    if(all_genres[j] != top10_genres[i]){
      print(paste0("i = ", i))
      print(paste0("j = ", j))
      count <- sum(df[, i] & df[,j] == T)
      this_comp <- c(paste0(top10_genres[i], " & ", all_genres[j]), count)
      df_comp[k, ] = this_comp
      k=k+1
    }
  }
}

df_comp[,2] = as.numeric(df_comp[,2])


df_duplas_top10 <- df_comp |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10)

### 1.2.1. Gráfico top10 dois gêneros agrupados ----

fig_top10_dup <- ggplot(df_duplas_top10, aes(x = count, y = reorder(genres, count))) +
  geom_col() +
  theme_minimal()
fig_top10_dup

rm(list=ls())

# 2. Dados de desenvolvedores ----

df <- read.csv2("data-raw/steam-data/applicationDevelopers.csv", col.names = paste("V",1:15), fill = T, sep = ",")
df <- df[,which(!is.na(df[1,]))]
## Removendo primeira coluna com "identificador"
df <- df[,2:8]

## Criando vetor com valores únicos de desenvolvedores
all_developers <- c("")
for(i in 1:ncol(df)){
  all_developers <- unique(c(all_developers, unique(df[,i])))
}

## Criando df/vetor com todas as colunas
df_aux <- as.matrix(df[,1])
for(i in 2:ncol(df)){
  df_aux <- append(df_aux, df[,i], after = length(df_aux))
}
df_aux <- as.data.frame(df_aux) |> 
  dplyr::filter(df_aux != "") |> 
  dplyr::rename(devs = df_aux)

## Limpando dados dúbios e pontuações

## Removendo variação Linux/Mac mesma empresa
df_all_words <- stringr::str_remove(df_aux$devs, "Linux")
df_all_words <- stringr::str_remove(df_all_words, "Mac")
df_all_words <- as.data.frame(df_all_words) |> 
  dplyr::rename(devs = df_all_words)

df_all_words_check <- df_all_words |> 
  dplyr::filter(stringr::str_detect(devs, "2K"))

## Limpando pontuações
df_all_words <- gsub("[[:punct:]]", "", df_all_words)
library(dplyr)
df_all_devs_count <- df_all_words |> 
  dplyr::group_by(df_all_words) |> 
  dplyr::summarize(count = n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice_head(n = 10) |> 
  dplyr::rename(devs = df_all_words)


fig_top10_dev <- ggplot(df_all_devs_count, aes(x = count, y = reorder(devs, count))) +
  geom_col() +
  theme_minimal()
fig_top10_dev
