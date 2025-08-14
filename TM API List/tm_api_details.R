# Packages ----------------------------------------------------------------
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

# API'ye istek atma ve JSON verisi elde etme
# API sorgu parametreleri kullanma
# for ve while döngüleri ve if koşulu
# Hata Yakalama: tryCatch
# Veri manipülasyonu
# Fonksiyonlaştırma
# Hiyerarşik Veri Toplama
# DRY: Don't Repeat Yourself

# API List ----------------------------------------------------------------
# https://tmapi-alpha.transfermarkt.technology/attributes
# https://www.transfermarkt.com/quickselect/countries
# https://www.transfermarkt.com/quickselect/competitions/189
# https://www.transfermarkt.com/quickselect/teams/ES1
# https://www.transfermarkt.com/quickselect/players/418
# https://tmapi-alpha.transfermarkt.technology/competitions?ids[]=TR1
# https://tmapi-alpha.transfermarkt.technology/clubs?ids[]=36
# https://tmapi-alpha.transfermarkt.technology/players?ids[]=861410

url <- "https://tmapi-alpha.transfermarkt.technology/attributes"

# http::GET ve jsonlite::fromJSON Fonksiyonlaır arasındaki fark:

# Basit bir JSON API'den veri çekecekseniz ve istek parametrelerine ihtiyacınız yoksa, 
# jsonlite::fromJSON işlevini doğrudan URL ile kullanmak en kolay ve hızlı yöntemdir.

# Daha karmaşık senaryolarla (headers(istek başlıkları), kimlik doğrulama, hata yönetimi) 
# karşılaştığınızda veya JSON dışında bir veri formatıyla çalışmanız gerektiğinde, 
# önce httr::GET ile veriyi çekip, ardından yanıtın içeriğini jsonlite::fromJSON ile 
# işlemek en doğru yaklaşımdır.

# Bu iki fonksiyon, aslında birbirinin yerine geçen araçlar değil, 
# birbirini tamamlayan araçlardır. Birlikte kullanıldıklarında, 
# web'den veri çekme ve işleme işlemlerini çok daha güçlü ve esnek hale getirirler.

# fromjson data çekme
att <- fromJSON(url)

names(att)

names(att$data)

# get data çekme
response <- GET(url)

httr::content(response, "text", encoding = "UTF-8") 


fromJSON(httr::content(response, "text", encoding = "UTF-8")) 



# Area & Country ----------------------------------------------------------

att$data$countries %>% View
att$data$confederations %>% View

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



# countries ---------------------------------------------------------------
url <- "https://www.transfermarkt.com/quickselect/countries"
countries <- fromJSON(url) %>% 
  mutate(
    link = glue::glue("https://www.transfermarkt.com{link}")
  ) 


left_join(
  area_list,
  countries, 
  by = c("country_id" = "id")
) %>% View



# Attributes --------------------------------------------------------------
tm_api_attributes <- function(){
  
  url <- "https://tmapi-alpha.transfermarkt.technology/attributes"
  
  att <- jsonlite::fromJSON(url)
  
  area_list <- dplyr::left_join(
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
  
  return(area_list)
  
}

area_list_tidy <- tm_api_attributes()


# Competition -------------------------------------------------------------
# https://www.transfermarkt.com/quickselect/competitions/

country_id <- 189
url <- glue("https://www.transfermarkt.com/quickselect/competitions/{country_id}")
response <- GET(url)
competition <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) 


# Amacımız: Tüm müsabakaları elde etmek!
# Area datası içerisindeki tüm ülkeleri tek tek gez
# Ülke urllerine istek gönder, istek başarılıysa json formatını elde et
# İstek başarısızsa / Hata gerçekleştiyse tekrar dene istek yolla
# 5 deneme sonrası 

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



# Team --------------------------------------------------------------------
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
team <- tm_api_team(competition_detailed)


# Player ------------------------------------------------------------------
tm_api_player <- function(team){
  player <- tibble::tibble()
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  for (i in 1:nrow(team)) {
    print(i)
    url <- paste0("https://www.transfermarkt.com/quickselect/players/", team$team_id[i])
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
                competition_id = team$competition_id[i],
                competition = team$competition[i],
                team_id = team$team_id[i],
                team = team$team[i]
              ) %>% 
              dplyr::rename(tmid = id, player = name, player_url = link)
            player <- dplyr::bind_rows(player, temp)
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
  return(player)
}
player_list <- tm_api_player(team[1:20,])


# Player Bio --------------------------------------------------------------
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"

url <- "https://tmapi-alpha.transfermarkt.technology/players?ids[]=861410"

response <- httr::GET(
  url, 
  httr::add_headers(`User-Agent` = user_agent), 
  httr::timeout(240)
)
temp <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) 

player_bio <- temp$data %>% 
  jsonlite::flatten() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    last_contract_renewal = ifelse(
      !is.na(attributes_last_contract_renewal_year), 
      paste0(attributes_last_contract_renewal_year,"-", attributes_last_contract_renewal_month,"-", attributes_last_contract_renewal_day), 
      NA_character_
    ),
    club_id = sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(clubId)}),
    jersey = sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(shirtNumber)}),
    is_captain = sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(isCaptain)}),
    club_debut = sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(debut)}),
    jersey = sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(shirtNumber)})
  ) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    market_value_cur_short = paste0(market_value_details_current_compact_prefix,market_value_details_current_compact_content, market_value_details_current_compact_suffix),
    market_value_pre_short = paste0(market_value_details_previous_compact_prefix,market_value_details_previous_compact_content, market_value_details_previous_compact_suffix),
    relative_url = paste0("https://www.transfermarkt.com", relative_url)
  ) %>% 
  dplyr::select(
    -c("club_assignments", "portrait_url_source", 
       "attributes_last_contract_renewal_year","attributes_last_contract_renewal_month", "attributes_last_contract_renewal_day",
       "attributes_preferred_foot_id_2","attributes_outfitter_id_2",
       "birth_place_details_gender", "preferences_theme_id", "attributes_position_id",
    ),
    -contains(c("_position_category","side_position_id","_position_name","_currency", "_prefix", "_suffix", "_content"))
  )  %>% 
  dplyr::rename_all(function(i){stringr::str_remove_all(i, "nationalities_|nationality_details_|birth_place_details_|life_dates_|attributes_|nationality_details_nationalities_")}) %>% 
  dplyr::select(
    -c(
      "outfitter_id", "place_of_birth", "preferred_foot_id", "position_id_2", "outfitter_name",
      "position_group", "former_clubs_note", "place_of_birth_additional_info",
      "market_value_details_delta_is_visible"
    )
  ) %>% 
  dplyr::rename(
    tmid = id, 
    player_name = name,
    player_short_name = short_name,
    player_nickname = artist_name,
    player_display_name = display_name,
    player_passport_name = passport_name,
    position_group = position_group_name,
    image_url = portrait_url,
    url = relative_url,
    foot = preferred_foot_name,
    main_position = position_short_name,
    first_side_position = first_side_position_short_name,
    second_side_position = second_side_position_short_name
  ) %>% 
  dplyr::select(
    c("tmid", "jersey",
      "player_name", "player_short_name", "player_nickname", 
      "player_display_name", "player_passport_name",
      "height", "foot", "age", 
      "date_of_birth", "is_date_of_birth_unknown", "date_of_death", "is_date_of_death_unknown", 
      "is_captain", "club_id", "club_debut", "last_contract_renewal",  "contract_until", 
      "country_of_birth_id",  "nationality_id", "second_nationality_id", 
      "position_group", "main_position", "first_side_position", "second_side_position", 
      "market_value_cur_short", "market_value_details_current_value", "market_value_details_current_determined", 
      "market_value_pre_short","market_value_details_previous_value", "market_value_details_previous_determined", 
      "market_value_details_delta_value", "market_value_details_delta_percentage", 
      "market_value_details_delta_type", "market_value_details_highest_value", 
      "market_value_details_highest_determined", 
      "url", "image_url")
  ) %>% 
  dplyr::rename_all(function(i){stringr::str_replace_all(stringr::str_replace_all(stringr::str_remove_all(i, "_details"), "current_value", "cur"),"previous_value", "pre")}) %>% 
  dplyr::rename(
    market_value_delta = market_value_delta_value,
    market_value_highest = market_value_highest_value,
    market_value_cur_determined = market_value_current_determined,
    market_value_pre_determined = market_value_previous_determined
  )

player_bio 






































































