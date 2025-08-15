library(RPostgres)
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

con <- dbConnect(
  Postgres(),
  host = "localhost",
  port = 5435,
  dbname = "sports_analytics",
  user = "youtube",
  password = 123456
)


url <- "https://tmapi-alpha.transfermarkt.technology/attributes"
att <- fromJSON(url)

# Area & Country ----------------------------------------------------------
area_list <- left_join(
  att$data$countries,
  att$data$confederations,
  by = c("confederationId" = "id")
) %>% 
  select(c("confederationId", "name.y", "id", "name.x", "flagUrl")) %>% 
  rename(
    area_id = confederationId,
    area = name.y,
    country_id = id,
    country = name.x,
    flag_url = flagUrl
  ) %>% 
  mutate(
    competition_list_url = glue::glue("https://www.transfermarkt.com/wettbewerbe/national/wettbewerbe/{country_id}")
  )


dbWriteTable(con, "tm_api_countries", area_list)

dbListTables(con)


# Competition -------------------------------------------------------------
tm_api_competition_raw <- function(area){
  
  competition <- tibble()
  
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  
  for(i in 1:nrow(area)){
    
    print(i)
    
    url <- paste0("https://www.transfermarkt.com/quickselect/competitions/", area$country_id[i])
    
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
  return(competition)
}

competition_list <- tm_api_competition_raw(area_list)



# Competition Details -----------------------------------------------------

# https://tmapi-alpha.transfermarkt.technology/competitions?ids[]=TR1
# https://tmapi-alpha.transfermarkt.technology/competitions?ids[]=TR1&ids[]=TR2&ids[]=TR3

tm_api_competition_detailed <- function(competition, att){
  
  ind <- c(seq(1, nrow(competition), 100))
  res <- lapply(ind, function(i){
    print(i)
    if(max(ind) == i){
      tm <- i:nrow(competition)
    }else{
      tm <- i:(i + 100 - 1)
    }
    url <- paste0("https://tmapi-alpha.transfermarkt.technology/competitions?", paste0("ids[]=",competition$competition_id[tm], collapse = "&"))
    jsonlite::fromJSON(url)$data
  }) %>% dplyr::bind_rows()
  
  res <- res %>% 
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
    ) 
  
  return(res)
}
competition_detailed <- tm_api_competition_detailed(competition_list, att)


dbWriteTable(con, "tm_api_competitions", competition_detailed)
























































