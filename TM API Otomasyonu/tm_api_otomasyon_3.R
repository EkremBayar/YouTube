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
# TM API Player Bio
tm_api_player_bio <- function(player){
  
  random_sleep <- function(min, max){Sys.sleep(sample(min:max, size = 1, replace = TRUE))}
  
  # The function to add missing columns
  add_missing_cols <- function(data, required_cols) {
    
    required_cols <- c("id", "name", "short_name", "artist_name", "display_name", 
                       "relative_url", "portrait_url", "portrait_url_source", "club_assignments", 
                       "life_dates_age", "life_dates_date_of_birth", "life_dates_is_date_of_birth_unknown", 
                       "life_dates_date_of_death", "life_dates_is_date_of_death_unknown", 
                       "birth_place_details_place_of_birth", "birth_place_details_country_of_birth_id", 
                       "birth_place_details_place_of_birth_additional_info", "birth_place_details_gender", 
                       "nationality_details_passport_name", "nationality_details_nationalities_nationality_id", 
                       "nationality_details_nationalities_second_nationality_id", "attributes_height", 
                       "attributes_preferred_foot_id", "attributes_outfitter_id","attributes_outfitter_name", "attributes_position_group", 
                       "attributes_position_group_name", "attributes_position_id", "attributes_first_side_position_id", 
                       "attributes_second_side_position_id", "attributes_contract_until", 
                       "attributes_former_clubs_note", "attributes_last_contract_renewal_year", 
                       "attributes_last_contract_renewal_month", "attributes_last_contract_renewal_day", 
                       "attributes_position_name", "attributes_position_short_name", 
                       "attributes_position_category", 
                       "attributes_first_side_position_name", "attributes_first_side_position_short_name", 
                       "attributes_first_side_position_category", 
                       "attributes_second_side_position_name", "attributes_second_side_position_short_name", 
                       "attributes_second_side_position_category", 
                       "attributes_preferred_foot_name", "preferences_theme_id", "market_value_details_current_value", 
                       "market_value_details_current_currency", "market_value_details_current_determined", 
                       "market_value_details_current_compact_prefix", "market_value_details_current_compact_content", 
                       "market_value_details_current_compact_suffix", "market_value_details_previous_value", 
                       "market_value_details_previous_currency", "market_value_details_previous_determined", 
                       "market_value_details_previous_compact_prefix", "market_value_details_previous_compact_content", 
                       "market_value_details_previous_compact_suffix", "market_value_details_delta_value", 
                       "market_value_details_delta_percentage", "market_value_details_delta_is_visible", 
                       "market_value_details_delta_type", "market_value_details_highest_value", 
                       "market_value_details_highest_currency", "market_value_details_highest_determined", 
                       "market_value_details_highest_compact_prefix", "market_value_details_highest_compact_content", 
                       "market_value_details_highest_compact_suffix")
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        data[[col]] <- NA_character_
      }
    }
    return(data)
  }
  
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"
  
  ind <- c(seq(1, nrow(player), 100))
  player_urls <- sapply(ind, function(i){
    print(i)
    if(max(ind) == i){
      tm <- i:nrow(player)
    }else{
      tm <- i:(i + 100 - 1)
    }
    paste0("https://tmapi-alpha.transfermarkt.technology/players?", paste0("ids[]=",player$tmid[tm], collapse = "&"))
  })
  
  max_cores <- parallel::detectCores()
  cl <- makeCluster(max_cores, outfile = "")
  registerDoParallel(cl, cores = max_cores)  
  iterations <- length(player_urls)
  
  player_bio_df <- foreach (
    
    i=1:iterations, 
    .combine=bind_rows, 
    .packages = c("tidyverse", "httr", "jsonlite", "glue")
    
  ) %dopar% {
    random_sleep(1,5)
    print(i)
    url <- player_urls[i]
    
    player_bio <- tryCatch({
      response <- httr::GET(
        url, 
        httr::add_headers(`User-Agent` = user_agent), 
        httr::timeout(240)
      )
      temp <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) 
      
      player_bio <- temp$data %>% 
        jsonlite::flatten() %>% 
        janitor::clean_names() %>% 
        dplyr::select(-contains("_2")) %>% 
        add_missing_cols() %>% 
        dplyr::mutate(
          last_contract_renewal = ifelse(
            !is.na(attributes_last_contract_renewal_year), 
            paste0(attributes_last_contract_renewal_year,"-", attributes_last_contract_renewal_month,"-", attributes_last_contract_renewal_day), 
            NA_character_
          ),
          club_id = tryCatch({sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(clubId)})},error = function(e){NA_character_}),
          jersey = tryCatch({sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(shirtNumber)})},error = function(e){NA_character_}),
          is_captain = tryCatch({sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(isCaptain)})},error = function(e){NA_character_}),
          club_debut = tryCatch({sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(debut)})},error = function(e){NA_character_}),
          jersey = tryCatch({sapply(club_assignments, function(i){i %>% dplyr::filter(type == "current") %>% dplyr::pull(shirtNumber)})},error = function(e){NA_character_})
        ) %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(
          market_value_cur_short = ifelse(is.na(market_value_details_current_compact_content),NA_character_,paste0(market_value_details_current_compact_prefix,market_value_details_current_compact_content, market_value_details_current_compact_suffix)),
          market_value_pre_short = ifelse(is.na(market_value_details_previous_compact_content),NA_character_,paste0(market_value_details_previous_compact_prefix,market_value_details_previous_compact_content, market_value_details_previous_compact_suffix)),
          relative_url = paste0("https://www.transfermarkt.com", relative_url)
        ) %>% 
        dplyr::select(
          -c("club_assignments", "portrait_url_source", 
             "attributes_last_contract_renewal_year","attributes_last_contract_renewal_month", "attributes_last_contract_renewal_day",
             "birth_place_details_gender", "preferences_theme_id", "attributes_position_id",
          ),
          -contains(c("_position_category","side_position_id","_position_name","_currency", "_prefix", "_suffix", "_content"))
        )  %>% 
        dplyr::rename_all(function(i){stringr::str_remove_all(i, "nationalities_|nationality_details_|birth_place_details_|life_dates_|attributes_|nationality_details_nationalities_")}) %>% 
        dplyr::select(
          -c(
            "outfitter_id", "place_of_birth", "preferred_foot_id", "outfitter_name",
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
          url = relative_url,
          image_url = portrait_url,
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
        ) %>% 
        # data types
        dplyr::mutate_at(
          c("tmid", "jersey", "age", "club_id", "country_of_birth_id", "nationality_id", "second_nationality_id", 
            "market_value_cur", "market_value_pre", "market_value_highest"),
          as.integer
        ) %>% 
        dplyr::mutate(height = as.numeric(height)) %>% 
        dplyr::mutate_at(
          c("date_of_birth", "date_of_death", "club_debut", "last_contract_renewal", "contract_until", "market_value_cur_determined", "market_value_pre_determined", "market_value_highest_determined"),
          as.Date
        ) %>% 
        dplyr::mutate_at(
          c("player_name", "player_short_name", "player_nickname", "player_display_name", 
            "player_passport_name", "foot", "position_group", "main_position", 
            "first_side_position", "second_side_position", "market_value_cur_short", 
            "market_value_pre_short", "market_value_delta", "market_value_delta_percentage", 
            "market_value_delta_type", "url", "image_url"),
          as.character
        ) %>% 
        dplyr::mutate_at(
          c("player_name", "player_short_name", "player_nickname", "player_display_name", "player_passport_name"),
          function(i){ifelse(str_squish(i) == "", NA_character_, i)}
        )
        
      
      
    },error=function(e){
      message(paste0("Index:", i, " - ", e, "\n", url))
      player_bio <- NULL
    })
    
    player_bio
  }
  stopCluster(cl)
  rm(cl, iterations)
  invisible(gc())
  
  
  return(player_bio_df)
}

# Update DB
updateDB <- function(con, dataframe, tablename, where){
  
  # UPDATe kısmında değişken tipleri mutlaka tanımlanmalı yoksa patlıyor!
  dataframe <- dataframe %>%
    dplyr::mutate_if(is.character, function(x){
      ifelse(!is.na(x), paste0("'",stringr::str_replace_all(x, "'", "''"),"'::text"), "NULL::text")
    }) %>%
    dplyr::mutate_if(lubridate::is.Date, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::DATE"), "NULL::DATE")
    }) %>%
    dplyr::mutate_if(lubridate::is.POSIXt, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::DATE"), "NULL::DATE")
    }) %>%
    dplyr::mutate_if(lubridate::is.POSIXct, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::DATE"), "NULL::DATE")
    }) %>%
    dplyr::mutate_if(is.integer, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::integer"), "NULL::integer")
    }) %>%
    dplyr::mutate_if(is.double, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::double precision"), "NULL::double precision")
    }) %>%
    dplyr::mutate_if(is.logical, function(x){
      ifelse(!is.na(x), paste0("'",x,"'::boolean"), "NULL::boolean")
    })
  
  # Tüm Değişkenler in ismi
  vars <- names(dataframe)
  vars <- paste0('"',vars, '" = s."', vars,'"', collapse = ", ")
  
  # Satır bazlı gözlemlerin birleştirilmesi
  values <- apply(dataframe,1,paste0,collapse=", ")
  values <- paste0("(", values, ")", collapse = ", ")
  
  
  # UPDATE işlemi
  query <- paste0(
    'UPDATE ', tablename, ' AS d SET ', vars,
    " FROM ( VALUES ", values, ") AS s(",
    paste0('"',names(dataframe),'"', collapse = ", "),
    ") WHERE ", paste0("d.\"",where,"\" = s.\"",where, "\"", collapse = " AND ")
  )
  
  RPostgres::dbClearResult(RPostgres::dbSendStatement(con, query))
  
}

# TM API Player Bio -------------------------------------------------------
player_df <- dbGetQuery(con, 'SELECT DISTINCT "tmid" FROM tm_api_players')

player_bio <- tm_api_player_bio(player_df)

rm(player_df)

# Country & Club & Competition Details
countries <- dbGetQuery(con, 'SELECT "country_id", "country" FROM tm_api_countries')

query <- '
 SELECT t2."team_id" AS "club_id", t2."team" AS "club",
 MIN(t1."competition_id") AS competition_id,
 MIN(t1."competition") AS competition,
 MIN(t1."competition_type") AS competition_type
 FROM tm_api_competitions AS t1
 LEFT JOIN tm_api_teams AS t2
 ON t1."competition_id" = t2."competition_id"
 WHERE t1."competition_type_id"=1
 GROUP BY
  t2."team_id",
  t2."team"
'
team_comp <- dbGetQuery(con, query)

player_bio <- dplyr::left_join(
  player_bio,
  team_comp,
  by = "club_id"
) %>% 
  dplyr::left_join(
    countries %>% dplyr::rename(country_of_birth_id = country_id, country_of_birth = country),
    by = "country_of_birth_id"
  ) %>% 
  dplyr::left_join(
    countries %>% dplyr::rename(nationality_id = country_id, nationality = country),
    by = "nationality_id"
  ) %>% 
  dplyr::left_join(
    countries %>% dplyr::rename(second_nationality_id = country_id, second_nationality = country),
    by = "second_nationality_id"
  ) %>% 
  dplyr::select(
    c("tmid", "jersey", "player_name", "player_short_name", "player_nickname", 
      "player_display_name", "player_passport_name", "height", "foot", 
      "age", "date_of_birth", "is_date_of_birth_unknown", "date_of_death", 
      "is_date_of_death_unknown", "is_captain", "club_id", "club", "club_debut", 
      "competition_id", "competition", "competition_type", 
      "last_contract_renewal", "contract_until", "country_of_birth_id",  "country_of_birth", 
      "nationality_id", "nationality", "second_nationality_id", "second_nationality", "position_group", 
      "main_position", "first_side_position", "second_side_position", 
      "market_value_cur_short", "market_value_cur", "market_value_cur_determined", 
      "market_value_pre_short", "market_value_pre", "market_value_pre_determined", 
      "market_value_delta", "market_value_delta_percentage", "market_value_delta_type", 
      "market_value_highest", "market_value_highest_determined", "url", "image_url"
    )
  )

rm(countries, query, team_comp)

if(dbExistsTable(con, "tm_api_player_bio") == FALSE){
  
  dbWriteTable(con, "tm_api_player_bio", player_bio)
  
}else{
  
  query <- paste0('SELECT "tmid" FROM tm_api_player_bio WHERE "tmid" IN (', paste0(player_bio$tmid, collapse = ","),')')
  db_check <- dbGetQuery(con, query) %>% pull(tmid)
  
  # New Players
  new_players <- player_bio %>% filter(!tmid %in% db_check)
  
  if(nrow(new_players) > 0){
    dbWriteTable(con, "tm_api_players", new_players, append = TRUE)
  }
  rm(new_players, query)
  
  # Current Players
  player_bio <- player_bio %>% filter(tmid %in% db_check)
  
  if(nrow(player_bio) > 0){
    
    if(nrow(player_bio) > 20000){
      
      ratio <- ceiling(nrow(player_bio) / 20000)
      for(r in 1:ratio){
        if(r != ratio){
          cond <- (20000*(r-1)+1):(20000*(r))
        }else{
          cond <- (20000*(r-1)+1):nrow(player_bio)
        }
        updateDB(con, "tm_api_players", dataframe = player_bio[cond,], where = "tmid")
      }
      rm(ratio, cond)
      
    }else{
      updateDB(con, "tm_api_players", dataframe = player_bio, where = "tmid")
    }
  }
  
  rm(db_check)
  
}


# Close DB Connection -----------------------------------------------------
dbDisconnect(con)

rm(player_bio, con)


# Eksik?
# tm_api_players oyuncu listesinde aktif olmayan oyuncular (futbolu bırakan, ölen) için ne yapılmalı?
