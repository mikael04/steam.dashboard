#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  ## Lendo a base
  steam_db <- feather::read_feather("data-raw/dados-manipulados/database.feather")
  ## mongo user shiny-steam-app
  ## mongo pw   PfXyC4pIR3yD06KK
  ## my ip 200.102.245.225/32
  
  # mongodb+srv://shiny-steam-app:<password>@steamappdb.ogya4fg.mongodb.net/?retryWrites=true&w=majority
  # library(mongolite)
  mongo_db_user <- "shiny-steam-app"
  mongo_db_password <- "PfXyC4pIR3yD06KK"
  mongo_database <- "sample_mflix"
  mongo_collection <- "movies"
  # mongo_clustername <- "cluster123-abc.mongodb.net"
  
  url_srv <- paste0("mongodb+srv://", mongo_db_user, ":", mongo_db_password, "@steamappdb.ogya4fg.mongodb.net/?retryWrites=true&w=majority")
  
  mongo_db <- mongo(collection = mongo_collection, db = mongo_database, url = url_srv, verbose = TRUE)
  
  # db_collect <- mongo_db |> 
  #   dplyr::collect()
  
  mongo_db$count()
  
  my_collection = mongo(collection = "gamesInfo", db = "Steam", url = url_srv) # create connection, database and collection
  my_collection$insert(steam_db)
  
  vars <- my_collection$distinct("variavel")
  years <- my_collection$distinct("release_year")
  languages <- my_collection$distinct("language")
  
  df_lang <- as.data.frame(languages)
}
