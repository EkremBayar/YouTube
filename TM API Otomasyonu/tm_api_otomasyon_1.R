# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(RPostgres)


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

# Country List
tm_api_country <- function(){
  
  url <- "https://tmapi-alpha.transfermarkt.technology/attributes"
  
  att <- jsonlite::fromJSON(url)
  
  country_list <- dplyr::left_join(
    att$data$countries,
    att$data$confederations,
    by = c("confederationId" = "id")
  ) %>% 
    dplyr::select(c("confederationId", "name.y", "id", "name.x", "flagUrl")) %>% 
    dplyr::mutate(competition_list_url = glue::glue("https://www.transfermarkt.com/wettbewerbe/national/wettbewerbe/{id}")) %>% 
    dplyr::rename(
      area_id = confederationId,
      area = name.y,
      country_id = id,
      country = name.x,
      flag_url = flagUrl
    ) %>% 
    dplyr::arrange(area, country)
  
  return(country_list)
  
}

# Competition List
tm_api_competition <- function(country){
  
  # Scraping Competition List
  competition <- tibble()
  
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  
  for(i in 1:nrow(country)){
    
    print(i)
    
    url <- paste0("https://www.transfermarkt.com/quickselect/competitions/", country$country_id[i])
    
    max_retries <- 5 
    attempts <- 0     
    
    while (attempts < max_retries) {
      
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
                country_id = area$country_id[i],
                country = area$country[i]
              ) %>% 
              dplyr::rename(competition_id = id, competition = name, competition_url = link) %>% 
              dplyr::left_join(area %>% dplyr::select(-c("country")),
                               by = "country_id")
            competition <- dplyr::bind_rows(competition, temp)
          }else{
            warning(paste0("Index:",i,"\nURL'den gelen JSON bir veri çerçevesi değil:\n", url))
          }
          
          break
          
        }
        
      }, error = function(e){
        
        warning(paste0("Index:",i,"\nURL'den gelen JSON bir veri çerçevesi değil:\n", url))
        
      })
      
    }
  }
  
  # Attribute Data
  att <- fromJSON("https://tmapi-alpha.transfermarkt.technology/attributes")
  
  # Combine Competition Detail
  ind <- c(seq(1, nrow(competition), 100))
  competition_detail <- lapply(ind, function(i){
    print(i)
    if(max(ind) == i){
      tm <- i:nrow(competition)
    }else{
      tm <- i:(i + 100 - 1)
    }
    url <- paste0("https://tmapi-alpha.transfermarkt.technology/competitions?", paste0("ids[]=",competition$competition_id[tm], collapse = "&"))
    jsonlite::fromJSON(url)$data
  }) %>% dplyr::bind_rows()
  
  res <- competition_detail %>% 
    dplyr::mutate(
      current_season_id = currentSeason$id, 
      current_season = currentSeason$display,
      country_id = originDetails$countryId,
      area_id = originDetails$continentId,
      relativeUrl = paste0("https://www.transfermarkt.com", relativeUrl),
      competition_is_standalone = baseDetails$isStandalone, 
      competition_is_ongoing = baseDetails$isOngoing, 
      competition_is_tournament = baseDetails$isTournament,
      competition_game_day_count = baseDetails$gameDayCount,
      season_history = sapply(res$historical$images, function(i){
        temp <- i$season[i$season != 0]
        if(length(temp)>1){
          paste0(max(temp), "-", min(temp))
        }else{
          if(is.na(temp) | is.null(temp)){
            NA_character_
          }else{
            max(temp)
          }
        }
      })
    ) %>% 
    dplyr::select(-c("historical", "totalMarketValue", "currentSeason", "currentSeasonId", "originDetails", "baseDetails")) %>% 
    dplyr::rename(
      competition_id = id,
      competition = name,
      competition_short = shortName,
      competition_type_id = typeId
    ) %>% 
    janitor::clean_names() %>% 
    dplyr::left_join(
      att$data$competitionTypes %>% dplyr::rename(competition_type = name),
      by = c("competition_type_id"="id")
    ) %>% 
    dplyr::left_join(
      competition %>% dplyr::select(country_id, country, area_id, area) %>% dplyr::distinct(),
      by = c("country_id")
    ) %>% 
    dplyr::select(
      c("area_id", "area", "country_id", "country", "competition_id", "competition", "competition_short", 
        "competition_type_id", "competition_type", "season_history", "current_season_id", "current_season", "competition_is_standalone", 
        "competition_is_ongoing", "competition_is_tournament", "competition_game_day_count","relative_url", "logo_url"
      )
    ) %>% 
    dplyr::arrange(
      area_id, country, competition_type_id
    )
  
  return(res)
  
}

# Team List
tm_api_team <- function(competition){
  team <- tibble::tibble()
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  for (i in 1:nrow(competition)) {
    print(i)
    url <- paste0("https://www.transfermarkt.com/quickselect/teams/", competition$competition_id[i])
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
                competition_id = competition$competition_id[i],
                competition = competition$competition[i]
              ) %>% 
              dplyr::rename(team_id = id, team = name, team_url = link)
            team <- dplyr::bind_rows(team, temp)
          }else{
            warning(paste0("Index:",i,"\nURL'den gelen JSON bir veri çerçevesi değil:\n", url))
          }
          break
        }else{
          warning(paste("Index:",i,"\nHTTP hata kodu:", response$status_code, "\nURL:", url))
        }
        
      },error = function(e){
        message(paste0("Index:", i,"\nError:",e, "\n", url))
      })
    }
  }
  return(team)
}


# TM API Countries --------------------------------------------------------
if(dbExistsTable(con, "tm_api_countries") == FALSE){
  
  country_df <- tm_api_country()
  
  dbWriteTable(con, "tm_api_countries", country_df)

}else{
  country_df <- dbReadTable(con, "tm_api_countries")
}


# TM API Competitions -----------------------------------------------------
competition_df <- tm_api_competition(country_df)

if(dbExistsTable(con, "tm_api_competitions") == FALSE){
  
  dbWriteTable(con, "tm_api_competitions", competition_df)
  
}else{
  
  query <- paste0('SELECT "competition_id" FROM tm_api_competitions WHERE "competition_id" IN (', paste0("'", competition_df$competition_id, "'", collapse = ","),')')
  db_check <- dbGetQuery(con, query) %>% pull(competition_id)
  
  new_competitions <- competition_df %>% filter(!competition_id %in% db_check)
  
  if(nrow(new_competitions) > 0){
    dbWriteTable(con, "tm_api_competitions", new_competitions, append = TRUE)
  }
  rm(new_competitions, db_check, query)
  
}


# TM API Teams ------------------------------------------------------------
team_df <- tm_api_team(competition_df)

if(dbExistsTable(con, "tm_api_teams") == FALSE){
  
  dbWriteTable(con, "tm_api_teams", team_df)
  
}else{
  
  query <- paste0('SELECT "team_id" FROM tm_api_teams WHERE "team_id" IN (', paste0(team_df$team_id, collapse = ","),')')
  db_check <- dbGetQuery(con, query) %>% pull(team_id)
  
  new_teams <- team_df %>% filter(!team_id %in% db_check)
  
  if(nrow(new_teams) > 0){
    dbWriteTable(con, "tm_api_teams", new_teams, append = TRUE)
  }
  rm(new_teams, db_check, query)

}


# Close DB Connection -----------------------------------------------------
dbDisconnect(con)










