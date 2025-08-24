# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(RPostgres)
library(foreach)
library(doParallel)



# DB Connection -----------------------------------------------------------
con <- dbConnect(
  Postgres(),
  host = "localhost",
  port = 5435,
  dbname = "sports_analytics",
  user = "youtube",
  password = 123456
)


# Functions ---------------------------------------------------------------
random_sleep <- function(min, max){Sys.sleep(sample(min:max, size = 1, replace = TRUE))}

# Player List
tm_api_player <- function(team){
  player <- tibble::tibble()
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  
  url <- paste0("https://www.transfermarkt.com/quickselect/players/", team$team_id)
  max_retries <- 5 
  attempts <- 0     
  while (attempts < max_retries) {
    attempts <- attempts + 1
    tryCatch({
      response <- httr::GET(
        url, 
        httr::add_headers(`User-Agent` = user_agent), 
        httr::timeout(240)
      )
      if(response$status_code == 200){
        temp <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) 
        if(is.data.frame(temp)){
          temp <- temp %>% 
            dplyr::mutate(
              link = paste0("https://www.transfermarkt.com", link),
              team_id = team$team_id,
              team = team$team
            ) %>% 
            dplyr::rename(tmid = id, player = name, player_url = link)
          player <- dplyr::bind_rows(player, temp)
        }else{
          warning(paste0("Index:",i,"\nURL'den gelen JSON bir veri çerçevesi değil:\n", url))
        }
        #random_sleep(1,10)
        break
      }else{
        #random_sleep(1,10)
        warning(paste("Index:",i,"\nHTTP hata kodu:", response$status_code, "\nURL:", url))
      }
      
    },error = function(e){
      #random_sleep(1,10)
      message(paste0("Index:", i,"\nError:",e, "\n", url))
    })
  }
  
  return(player)
}

# TM API Team List --------------------------------------------------------
team_df <- dbGetQuery(con, 'SELECT DISTINCT "team_id", "team" FROM tm_api_teams')

# TM API Player List Parallel Programming ---------------------------------
max_cores <- parallel::detectCores()
cl <- makeCluster(max_cores, outfile = "")
registerDoParallel(cl, cores = max_cores)  
iterations <- nrow(team_df)

player_df <- foreach (
  
  i=1:iterations, 
  .combine=bind_rows, 
  .packages = c("tidyverse", "httr", "jsonlite", "glue")
  
) %dopar% {
  
  print(i)
  
  tryCatch({
    temp_df <- tm_api_player(team_df[i,])
  },error=function(e){
    message(paste0("Index:", i, " - ", e))
    temp_df <- NULL
  })
  
  temp_df
}
stopCluster(cl)
rm(cl, iterations)
invisible(gc())


if(dbExistsTable(con, "tm_api_players") == FALSE){
  
  dbWriteTable(con, "tm_api_players", player_df)
  
}else{
  
  query <- paste0('SELECT "tmid" FROM tm_api_players WHERE "tmid" IN (', paste0(player_df$tmid, collapse = ","),')')
  db_check <- dbGetQuery(con, query) %>% pull(tmid)
  
  new_players <- player_df %>% filter(!tmid %in% db_check)
  
  if(nrow(new_players) > 0){
    dbWriteTable(con, "tm_api_players", new_players, append = TRUE)
  }
  rm(new_players, db_check, query)
  
}


# Close DB Connection -----------------------------------------------------
dbDisconnect(con)

