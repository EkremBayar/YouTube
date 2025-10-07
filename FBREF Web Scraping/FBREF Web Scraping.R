# Kütüphaneler ------------------------------------------------------------
library(tidyverse)
library(rvest)
library(httr)
library(RPostgres)

# En Sık Kullanılanacak Web Scraping Kodları ------------------------------
# rvest::read_html()
# rvest::html_elements()
# rvest::html_attrs()
# rvest::html_text()
# rvest::html_table()

# Python karşılıkları
# BeautifulSoup(html_content, 'html.parser')
# soup.find()
# soup.find_all()
# element.get()
# element.get_text()

# Football Reference Hiyerarşi --------------------------------------------
# https://fbref.com/en/
# https://fbref.com/en/comps/
# https://fbref.com/en/comps/9/history/Premier-League-Seasons
# https://fbref.com/en/comps/9/Premier-League-Stats


# Competition -------------------------------------------------------------
fbref_competition <- function(){
  
  url <- "https://fbref.com/en/comps/"
  page <-  rvest::read_html(url)
  
  tables <- page %>% html_elements('div[id^="all_comps_"]') 
  res <- lapply(tables, function(i){
    
    temp <- i %>% 
      html_element("table") %>% 
      html_table() %>% 
      janitor::clean_names() %>% 
      mutate(
        first_season = as.character(first_season),
        last_season = as.character(last_season),
        competition_type = i %>% html_element("h2") %>% html_text(),
        competition_url = i %>% html_elements('[data-stat="league_name"] a') %>% html_attr("href"),
        competition_id = as.character(str_split_i(competition_url, "/", 4)),
        competition_url = glue::glue("https://fbref.com{competition_url}"),
        season_min_url = as.character(paste0("https://fbref.com", i %>% html_elements('[data-stat="minseason"] a') %>% html_attr("href"))),
        season_max_url = as.character(paste0("https://fbref.com", i %>% html_elements('[data-stat="maxseason"] a') %>% html_attr("href")))
      )
    
    cl <- i %>% html_elements('td.left[data-stat="country"]')
    cl_name <- tryCatch({if(length(cl) == 0){
      cl_name <- NA_character_
    }else{
      cl_name <- sapply(
        cl,
        function(j){
          if(length(j) == 0){
            cl_name <- NA_character_
          }else{
            cl_name <- j %>% html_element("a:nth-child(2)") %>% html_text()
            if(length(cl_name)==0){cl_name <- NA_character_}
            cl_name
          }
        }
      )
    }
    },error=function(e){cl_name <- NA_character_})
    cl_id <- tryCatch({if(length(cl) == 0){
      cl_id <- NA_character_
    }else{
      cl_id <- sapply(
        cl,
        function(j){
          if(length(j) == 0){
            cl_id <- NA_character_
          }else{
            cl_id <- j %>% html_element("a:nth-child(2)") %>%html_attr("href") 
            if(length(cl_id)==0){cl_id <- NA_character_}else{cl_id <- ifelse(!is.na(cl_id), cl_id %>% paste0("https://fbref.com",.), cl_id)}
            
          }
        }
      )
    }
    },error=function(e){cl_id <- NA_character_})
    temp <- temp %>% mutate(country = cl_name, country_url = cl_id)
    temp
    
    
  }) %>% 
    bind_rows() 
  
  
  res <- res %>% 
    mutate(
      tier = ifelse(is.na(tier), str_squish(str_split_i(str_split_i(competition_type, " - ", 2), "Tier", 1)), tier),
      tier = ifelse(tier == "", NA_character_, tier),
      tier = as.integer(str_remove_all(tier, "st|nd|rd|th")),
      competition_type = str_squish(str_split_i(competition_type, " - ", 1))
    ) %>% 
    rename(area = governing_body) %>% 
    select(c("area", "country", "country_url", "gender", "competition_id", "competition_name", "tier", "competition_type", "first_season", 
             "last_season",  "competition_url", "season_min_url", "season_max_url"))
  
  return(res)
  
}

competition_df <- fbref_competition()

# DB ----------------------------------------------------------------------
con <- dbConnect(
  Postgres(),
  host = "localhost",
  port = 5435,
  dbname = "sports_analytics",
  user = "youtube",
  password = 123456
)

if(dbExistsTable(con, "fbref_competitions") == FALSE){
  
  dbWriteTable(con, "fbref_competitions", competition_df)
  
}


# Country -----------------------------------------------------------------
fbref_country <- function(){
  url<- "https://fbref.com/en/squads/"
  page <- read_html(url)
  el <- page %>% html_element("table.stats_table") 
  temp <- el %>% html_table() %>% 
    mutate(
      country_url = el %>% html_elements("tbody tr:not(.thead) [data-stat=country] a") %>% html_attr("href") %>% paste0("https://fbref.com" ,.),
      country_id = str_split_i(str_remove_all(country_url, "https://fbref.com/en/country/clubs/"), "/", 1)
    ) %>% 
    janitor::clean_names() %>% 
    select(
      c("country_id", "country",  "governing_body", "number_clubs", "country_url")
    )
  return(temp)
}

country_df <- fbref_country()

if(dbExistsTable(con, "fbref_countries") == FALSE){
  
  dbWriteTable(con, "fbref_countries", country_df)
  
}


# Teams from Countries ----------------------------------------------------
fbref_squad <- function(url){
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"
  ps <- session(url, user_agent(user_agent))
  page <- ps$response %>% read_html()
  el <- page %>% html_element("table.stats_table")
  
  temp <- el %>% html_table() %>% 
    mutate(
      From = as.character(From),
      To = as.character(To),
      Gender = as.character(Gender),
      squad_url = el %>% html_elements("tbody tr:not(.thead) [data-stat=team] a") %>% html_attr("href") %>% paste0("https://fbref.com" ,.),
      squad_id = str_split_i(str_remove_all(squad_url, "https://fbref.com/en/squads/"), "/", 1),
      comp_id = tryCatch({
        el %>% html_elements("tbody tr:not(.thead) [data-stat=comp] a") %>% html_attr("href") %>% str_remove_all("/en/comps/") %>% str_split_i("/", 1) %>% ifelse(length(.)==0, NA_character_, .)},error=function(e){NA_character_})
    ) %>% 
    janitor::clean_names() %>% 
    select(c("squad_id", "squad", "gender", "comp_id", "comp", "from", "to", "squad_url" ))
  return(temp)
}

squad_df <- lapply(country_df$country_url, function(i){
  print(i)
  Sys.sleep(5)
  tryCatch({
    fbref_squad(i)
  },error=function(e){
    message(paste(e, i))
    NULL
  })
}) 

if(dbExistsTable(con, "fbref_squads") == FALSE){
  dbWriteTable(con, "fbref_squads", squad_df)
}


squad_df <- dbReadTable(con, "fbref_squads")


# Teams -------------------------------------------------------------------
query <- "
SELECT * 
FROM fbref_competitions 
WHERE 
	competition_id IN ('9','11', '12', '13', '20') AND
	competition_type = 'Domestic Leagues';
"
big5 <- dbGetQuery(con, query)

team_df <- lapply(1:nrow(big5), function(i){
  Sys.sleep(1)
  url <- big5$season_max_url[i]
  page <- read_html(url)
  el <- page %>% html_elements('table[id$="_overall"]') %>% 
    html_elements("tbody tr [data-stat=team] a")
  data.frame(
    team_id = str_split_i(el %>% html_attr("href"), "/", -2),
    team = el %>% html_text(),
    url = el %>% html_attr("href") %>% paste0("https://fbref.com", .)
  ) %>% 
    mutate(competition_id = big5$competition_id[i],
           competition = big5$competition[i],
           last_season = big5$last_season[i],)
}) %>% bind_rows()

if(dbExistsTable(con, "fbref_teams") == FALSE){
  dbWriteTable(con, "fbref_teams", team_df)
}



# All Players -------------------------------------------------------------
fbref_player_list <- function(team){
  #url <- "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats"
  page <- read_html(team$url)
  el <- page %>% 
    html_elements('div[id^="div_stats_standard_"] [data-stat="player"] a') 
  res <- data.frame(
    competition_id = team$competition_id,
    team_id = team$team_id,
    team = team$team,
    fbref_id = str_split_i(el %>% html_attr("href"), "/", -2),
    player = el %>% html_text(),
    url = el %>% html_attr("href") %>% paste0("https://fbref.com", .)
  )
  return(res)
}

plist <- lapply(1:nrow(team_df), function(i){
  print(i)
  temp <- team_df[i, ]
  Sys.sleep(5)
  tryCatch({
    fbref_player_list(temp)
  },error=function(e){
    message(paste(e, i))
    NULL
  })
}) %>% bind_rows()

if(dbExistsTable(con, "fbref_player_list") == FALSE){
  dbWriteTable(con, "fbref_player_list", plist)
}


# Short Way Playerlist ----------------------------------------------------
fbref_player_list_quick <- function(comp_id){
  url <- paste0("https://fbref.com/en/comps/",comp_id, "/stats/#all_stats_standard")
  page <- read_html(url)
  el <- page %>% html_element("div#all_stats_standard") %>% as.character() 
  stcheck <- el %>% str_detect("all_stats_standard")
  if(length(el[stcheck])>0){
    st <- read_html(str_squish(str_remove_all(el[stcheck], "-->|<!--"))) 
    player <- st %>% html_elements('[data-stat="player"] a')
    team <- st %>% html_elements('[data-stat="team"] a')
    plist_df <- data.frame(
      competition_id = comp_id,
      team_id = team %>% html_attr("href") %>% str_split_i("/", -2),
      team = team %>% html_text(),
      fbref_id = player %>% html_attr("href") %>% str_split_i("/", -2),
      player = player %>% html_text(),
      url =  player %>% html_attr("href") %>% paste0("https://fbref.com", .)
    )
  }else{
    plist_df <- NULL
  }
  return(plist_df)
}

player_list_df <- lapply(c(9:13, 20), function(i){
  print(i)
  Sys.sleep(2)
  tryCatch({
    fbref_player_list_quick(i)
  },error=function(e){
    message(paste(e, i))
    NULL
  })
}) %>% 
  bind_rows()

player_list_df %>% group_by(fbref_id) %>% count(sort = T)

player_list_df %>% group_by(fbref_id) %>% do(head(., 1)) %>% count(sort = T)

nrow(player_list_df)
player_list_df <- player_list_df %>% group_by(fbref_id) %>% do(head(., 1)) %>% ungroup()
nrow(player_list_df)


# Player Bio --------------------------------------------------------------
fbref_player_bio <- function(url){
  
  result <- tryCatch({
    # Player Id
    pid <- str_split_i(str_remove(url, "https://fbref.com/en/players/"), "/", 1)
    
    # Read HTML
    page <- read_html(url)
    
    # Info
    info <- page %>% html_node("div #info")
    
    # Player Image
    pimg <- info %>% html_node("div .media-item img") %>% html_attr("src")
    
    # Player Name
    pname <- info %>% html_node("h1") %>% html_text() %>% str_squish()
    
    # Social Media
    social_media <- info %>% html_nodes("a") %>% html_attr("href")
    twitter <- social_media[str_detect(social_media, "twitter") & !is.na(social_media)]
    instagram <- social_media[str_detect(social_media, "instagram") & !is.na(social_media)]
    twitter <- ifelse(length(twitter) == 0, NA_character_, twitter)
    instagram <- ifelse(length(instagram) == 0, NA_character_, instagram)
    
    # Birth Date
    pdob <- info %>% html_nodes("p") %>% html_nodes("#necro-birth") %>% html_attr("data-birth")
    pdob <- ifelse(length(pdob) == 0, NA_character_, as.character(pdob))
    ppage <- ifelse(is.na(pdob), NA_integer_, as.integer(year(as.period(interval(pdob, Sys.Date()), "year"))))
    
    
    # String Manipulation
    text <- info %>% html_nodes("div#meta") %>% html_nodes("p") %>% html_text() %>% str_squish()
    
    # Foot
    foot <- text[str_detect(text, "Foot")]
    foot <- str_split_i(foot, "Footed:", 2) %>% str_squish()
    foot <- ifelse(length(foot) == 0, NA_character_, foot)
    
    # Position
    position <- text[str_detect(text, "Position")]
    position <- str_remove(position, "Position:")
    position <- str_split_i(position, "▪", 1) %>% str_squish()
    position <- ifelse(length(position) == 0, NA_character_, position)
    
    # Height
    height <- text[str_detect(text, ".*[0-9].*")]
    height <- height[str_detect(height, "cm")]
    height <- height[!str_detect(height, "Born")]
    height <- str_split_i(height, "cm", 1) %>% str_squish() %>% as.integer()
    height <- ifelse(length(height) == 0, NA_integer_, as.integer(height))

    # Weight
    weight <- text[str_detect(text, ".*[0-9].*")]
    weight <- weight[str_detect(weight, "kg")]
    weight <- weight[!str_detect(weight, "Born")]
    weight <- str_split_i(weight, "kg", 1) 
    weight <- ifelse(length(weight) == 0, NA_character_, weight)
    if(!is.na(weight)){
      if(str_detect(weight, "cm")){
        weight <- str_split_i(weight, ", ", 2) %>% str_squish() %>% as.integer()
      }else{
        weight <- weight %>% str_squish() %>% as.integer()
      }
    }
    weight <- as.integer(weight)
    
    # Citizenship
    citizenship <- text[str_detect(text, "Citizenship")]
    citizenship <- str_split_i(citizenship, "Citizenship:", 2) %>% str_squish()
    citizenship <- str_split_i(citizenship, paste0(" ", letters, collapse = "|"), 1)
    citizenship <- ifelse(length(citizenship) == 0, NA_character_, citizenship)
    
    # National Team
    national_team <- text[str_detect(text, "National Team")]
    national_team <- str_split_i(national_team, "National Team:", 2) %>% str_squish()
    national_team <- str_split_i(national_team, paste0(" ", letters, collapse = "|"), 1)
    national_team <- ifelse(length(national_team) == 0, NA_character_, national_team)
    
    # Place of Birth
    place_of_birth <- text[str_detect(text, "Born")]
    place_of_birth <- str_split_i(place_of_birth, " in ", 2) %>% str_squish()
    place_of_birth <- str_split_i(place_of_birth, paste0(" ", letters, collapse = "|"), 1)
    place_of_birth <- ifelse(length(place_of_birth) == 0, NA_character_, place_of_birth)
    
    # Club
    club <- text[str_detect(text, "Club")]
    club <- str_split_i(club, "Club:", 2) %>% str_squish()
    club <- ifelse(length(club) == 0, NA_character_, club)
    
    # Club Id
    club_id <- info %>% html_nodes("p") %>% html_nodes("a") %>% html_attr("href")
    club_id <- club_id[str_detect(club_id, "/en/squads/")]
    club_id <- str_remove(club_id, "/en/squads/")
    club_id <- str_split_i(club_id, "/", 1)
    club_id <- ifelse(length(club_id) == 0, NA_character_, club_id)
    
    # Full Name
    full_name <- text[!str_detect(text, ".*[0-9].*|Position|Footed|Born|Citizenship|National Team|Club|Twitter|Instagram|Wages")] %>% 
      str_squish()
    full_name <- ifelse(length(full_name) == 0, NA_character_, full_name)
    full_name <- ifelse(full_name == "", NA_character_, full_name)
    
    # Wages + Contract Exp
    wc <- text[str_detect(text, "Wages")]
    wc <- str_remove_all(wc, ",|€|Wages:|. Via Capology.")
    wages_weekly <- as.integer(str_extract(str_split_i(wc, "Weekly", 1) %>% str_squish(),"\\d+"))
    wages_weekly <- ifelse(length(wages_weekly) == 0, NA_integer_, wages_weekly)
    wages_annual <- 52*wages_weekly
    contract_expires <- str_split_i(wc, "Expires", 2) %>% str_squish() %>% lubridate::my()
    contract_expires <- ifelse(length(contract_expires) == 0, NA_character_, as.character(contract_expires))
    
    
    # Awards
    awards <- page %>% html_nodes("#bling li") %>% html_text() %>% paste0(collapse = " | ")
    awards <- ifelse(length(awards) == 0, NA_character_, awards)
    awards <- ifelse(awards == "", NA_character_, awards)
    
    # Additional Notes
    # Transfermarkt Profile
    tr <- page %>% html_elements("div.section_wrapper") %>% as.character() 
    trcheck <- tr %>% str_detect("transfermarkt")
    if(length(tr[trcheck])==0){
      tr_url <- NA_character_
      tmid <- NA_character_
    }else{
      tr_url <- read_html(str_squish(str_remove_all(tr[trcheck], "-->|<!--"))) %>% 
        html_element('a[href*="transfermarkt.com"]') %>% 
        html_attr('href')
      tmid <- basename(tr_url)
    }
    
    
    
    # Dataframe
    result <- data.frame(
      fbref_id = pid,
      player_name = pname,
      full_name = full_name, 
      citizenship = citizenship, 
      place_of_birth = place_of_birth,
      age = ppage,
      dob = pdob,
      height = height,
      weight = weight,
      foot = foot,
      position = position,
      club_id = club_id,
      club = club, 
      national_team = national_team,
      wages_weekly = wages_weekly, 
      wages_annual = wages_annual, 
      contract_expires = contract_expires, 
      twitter = twitter,
      instagram = instagram,
      awards = awards,
      fbref_url = url,
      image_url = pimg,
      tm_id = tmid,
      tm_url = tr_url
    ) %>% 
      mutate(
        citizenship = ifelse(is.na(citizenship) & !is.na(national_team), national_team, citizenship)
      )
    
  }, error = function(e){
    message(e)
    result <- NULL
  })
  
  return(result)
}

pbio_df <- tibble()
for(i in 1:nrow(plist)){
  Sys.sleep(5)
  print(i)
  tryCatch({
    temp <- fbref_player_bio(plist$url[i])
    pbio_df <- bind_rows(pbio_df, temp)
  },error = function(e){
    message(paste(e, i, plist$url[i]))
  })
}

if(dbExistsTable(con, "fbref_player_bio") == FALSE){
  dbWriteTable(con, "fbref_player_bio", pbio_df, append = T)
}




# Player Stats ------------------------------------------------------------
fbref_player_stats <- function(comp_id, season = NULL){
  
  stats_url <- paste0(
    "https://fbref.com/en/comps/", comp_id, ifelse(is.null(season), "", paste0("/",season)),
    c(
      "/playingtime/#all_stats_playing_time",
      #"/stats/#all_stats_standard", #1.
      "/keepers/#all_stats_keeper",
      "/keepersadv/#all_stats_keeper_adv",
      "/shooting/#all_stats_shooting",
      "/passing/#all_stats_passing",
      "/passing_types/#all_stats_passing_types",
      "/gca/#all_stats_gca",
      "/defense/#all_stats_defense",
      "/possession/#all_stats_possession",
      #"/playingtime/#all_stats_playing_time",
      "/misc/#all_stats_misc"
    )
  )
  #url_names <- c("standard", "gk_1", "gk_2","shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc")
  url_names <- c("playing_time", "gk_1", "gk_2","shooting", "passing", "passing_types", "gca", "defense", "possession", "misc")
  stats_url <- setNames(stats_url, url_names)
  
  res <- lapply(1:length(stats_url), function(i){
    
    Sys.sleep(5)
    print(i)
    
    url <- stats_url[i]
    selected_id = str_split_i(url, "#", 2)
    
    page <- read_html(url)
    el <- page %>% html_element(paste0("div#", selected_id)) %>% as.character() 
    stcheck <- el %>% str_detect(selected_id)
    st <- read_html(str_squish(str_remove_all(el[stcheck], "-->|<!--"))) 
    temp <- st %>% html_element("table") %>% html_table() %>% distinct() 
    
    names(temp) <- str_squish(str_replace_all(ifelse(names(temp) == "", temp[1,], paste0(names(temp),"_", temp[1,])), " ", "_"))
    names(temp) <- ifelse(!names(temp) %in% c("Rk","Player", "Nation", "Squad","Pos", "Age", "Born", "Matches"), paste0(str_to_upper(ifelse(str_detect(names(stats_url[i]), "gk_1|gk_2"), "gk", names(stats_url[i]))),"_",names(temp)),names(temp))
    
    temp <- temp[-1, ] %>% 
      select(-c("Matches", "Rk")) %>% 
      mutate(
        Nation = str_split_i(Nation, " ", 2),
        Age = str_split_i(Age, "-", 1),
        Id = st %>% html_elements('[data-stat="player"] a') %>% html_attr("href") %>% str_split_i("/",4),
        SquadId = st %>% html_elements('[data-stat="team"] a') %>% html_attr("href") %>% str_split_i("/",4)
      )
    
    if(selected_id != "all_stats_playing_time"){
      temp <- temp %>% select(-c("Player", "Nation", "Pos",  "Age", "Born"))
    }
    
    temp %>% mutate(
      CompetitionId = comp_id,
      Season = ifelse(!is.null(season),season,page %>% html_element("h1") %>% html_text() %>% str_squish() %>% str_split_i(" ", 1))
    )
  })
  
  res <- purrr::reduce(res, full_join, by = c("Id", "SquadId", "Squad", "Season", "CompetitionId")) %>% 
    mutate_all(function(i){ifelse(i == "", NA_character_, i)}) %>% 
    mutate_all(function(i){str_remove_all(i, "\\,")}) %>% 
    mutate_at(setdiff(which(str_detect(., "\\.")), which(names(.) %in% c("Squad", "Player"))) , as.numeric) %>% 
    mutate(across(
      .cols = where(is.character) & !c("Id", "Player", "Nation", "Pos", "SquadId", "Squad", "Season"),
      .fns = as.integer
    )) %>% 
    select(
      c(
        "CompetitionId", "Season", "SquadId", "Squad", "Id", "Player", "Nation", "Pos", "Age", "Born", 
        "PLAYING_TIME_Playing_Time_MP", "PLAYING_TIME_Playing_Time_Min", 
        "PLAYING_TIME_Playing_Time_Mn/MP", "PLAYING_TIME_Playing_Time_Min%", 
        "PLAYING_TIME_Playing_Time_90s", "PLAYING_TIME_Starts_Starts", 
        "PLAYING_TIME_Starts_Mn/Start", "PLAYING_TIME_Starts_Compl", 
        "PLAYING_TIME_Subs_Subs", "PLAYING_TIME_Subs_Mn/Sub", "PLAYING_TIME_Subs_unSub", 
        "PLAYING_TIME_Team_Success_PPM", "PLAYING_TIME_Team_Success_onG", 
        "PLAYING_TIME_Team_Success_onGA", "PLAYING_TIME_Team_Success_+/-", 
        "PLAYING_TIME_Team_Success_+/-90", "PLAYING_TIME_Team_Success_On-Off", 
        "PLAYING_TIME_Team_Success_(xG)_onxG", "PLAYING_TIME_Team_Success_(xG)_onxGA", 
        "PLAYING_TIME_Team_Success_(xG)_xG+/-", "PLAYING_TIME_Team_Success_(xG)_xG+/-90", 
        "PLAYING_TIME_Team_Success_(xG)_On-Off", 
        "GK_Performance_GA", "GK_Performance_GA90", "GK_Performance_SoTA", "GK_Performance_Saves", 
        "GK_Performance_Save%", "GK_Performance_W", "GK_Performance_D", 
        "GK_Performance_L", "GK_Performance_CS", "GK_Performance_CS%", 
        "GK_Penalty_Kicks_PKatt", "GK_Penalty_Kicks_PKA", "GK_Penalty_Kicks_PKsv", 
        "GK_Penalty_Kicks_PKm", "GK_Penalty_Kicks_Save%",  "GK_Goals_GA", 
        "GK_Goals_PKA", "GK_Goals_FK", "GK_Goals_CK", "GK_Goals_OG", 
        "GK_Expected_PSxG", "GK_Expected_PSxG/SoT", "GK_Expected_PSxG+/-", 
        "GK_Expected_/90", "GK_Launched_Cmp", "GK_Launched_Att", "GK_Launched_Cmp%", 
        "GK_Passes_Att_(GK)", "GK_Passes_Thr", "GK_Passes_Launch%", "GK_Passes_AvgLen", 
        "GK_Goal_Kicks_Att", "GK_Goal_Kicks_Launch%", "GK_Goal_Kicks_AvgLen", 
        "GK_Crosses_Opp", "GK_Crosses_Stp", "GK_Crosses_Stp%", "GK_Sweeper_#OPA", 
        "GK_Sweeper_#OPA/90", "GK_Sweeper_AvgDist",  "SHOOTING_Standard_Gls", 
        "SHOOTING_Standard_Sh", "SHOOTING_Standard_SoT", "SHOOTING_Standard_SoT%", 
        "SHOOTING_Standard_Sh/90", "SHOOTING_Standard_SoT/90", "SHOOTING_Standard_G/Sh", 
        "SHOOTING_Standard_G/SoT", "SHOOTING_Standard_Dist", "SHOOTING_Standard_FK", 
        "SHOOTING_Standard_PK", "SHOOTING_Standard_PKatt", "SHOOTING_Expected_xG", 
        "SHOOTING_Expected_npxG", "SHOOTING_Expected_npxG/Sh", "SHOOTING_Expected_G-xG", 
        "SHOOTING_Expected_np:G-xG", "PASSING_Total_Cmp", 
        "PASSING_Total_Att", "PASSING_Total_Cmp%", "PASSING_Total_TotDist", 
        "PASSING_Total_PrgDist", "PASSING_Short_Cmp", "PASSING_Short_Att", 
        "PASSING_Short_Cmp%", "PASSING_Medium_Cmp", "PASSING_Medium_Att", 
        "PASSING_Medium_Cmp%", "PASSING_Long_Cmp", "PASSING_Long_Att", 
        "PASSING_Long_Cmp%", "PASSING_Ast", "PASSING_xAG", "PASSING_Expected_xA", 
        "PASSING_Expected_A-xAG", "PASSING_KP", "PASSING_1/3", "PASSING_PPA", 
        "PASSING_CrsPA", "PASSING_PrgP", "PASSING_TYPES_Att", 
        "PASSING_TYPES_Pass_Types_Live", "PASSING_TYPES_Pass_Types_Dead", 
        "PASSING_TYPES_Pass_Types_FK", "PASSING_TYPES_Pass_Types_TB", 
        "PASSING_TYPES_Pass_Types_Sw", "PASSING_TYPES_Pass_Types_Crs", 
        "PASSING_TYPES_Pass_Types_TI", "PASSING_TYPES_Pass_Types_CK", 
        "PASSING_TYPES_Corner_Kicks_In", "PASSING_TYPES_Corner_Kicks_Out", 
        "PASSING_TYPES_Corner_Kicks_Str", "PASSING_TYPES_Outcomes_Cmp", 
        "PASSING_TYPES_Outcomes_Off", "PASSING_TYPES_Outcomes_Blocks", 
        "GCA_SCA_SCA", "GCA_SCA_SCA90", "GCA_SCA_Types_PassLive", 
        "GCA_SCA_Types_PassDead", "GCA_SCA_Types_TO", "GCA_SCA_Types_Sh", 
        "GCA_SCA_Types_Fld", "GCA_SCA_Types_Def", "GCA_GCA_GCA", "GCA_GCA_GCA90", 
        "GCA_GCA_Types_PassLive", "GCA_GCA_Types_PassDead", "GCA_GCA_Types_TO", 
        "GCA_GCA_Types_Sh", "GCA_GCA_Types_Fld", "GCA_GCA_Types_Def", 
        "DEFENSE_Tackles_Tkl", "DEFENSE_Tackles_TklW", 
        "DEFENSE_Tackles_Def_3rd", "DEFENSE_Tackles_Mid_3rd", "DEFENSE_Tackles_Att_3rd", 
        "DEFENSE_Challenges_Tkl", "DEFENSE_Challenges_Att", "DEFENSE_Challenges_Tkl%", 
        "DEFENSE_Challenges_Lost", "DEFENSE_Blocks_Blocks", "DEFENSE_Blocks_Sh", 
        "DEFENSE_Blocks_Pass", "DEFENSE_Int", "DEFENSE_Tkl+Int", "DEFENSE_Clr", 
        "DEFENSE_Err", "POSSESSION_Touches_Touches", 
        "POSSESSION_Touches_Def_Pen", "POSSESSION_Touches_Def_3rd", "POSSESSION_Touches_Mid_3rd", 
        "POSSESSION_Touches_Att_3rd", "POSSESSION_Touches_Att_Pen", "POSSESSION_Touches_Live", 
        "POSSESSION_Take-Ons_Att", "POSSESSION_Take-Ons_Succ", "POSSESSION_Take-Ons_Succ%", 
        "POSSESSION_Take-Ons_Tkld", "POSSESSION_Take-Ons_Tkld%", "POSSESSION_Carries_Carries", 
        "POSSESSION_Carries_TotDist", "POSSESSION_Carries_PrgDist", "POSSESSION_Carries_PrgC", 
        "POSSESSION_Carries_1/3", "POSSESSION_Carries_CPA", "POSSESSION_Carries_Mis", 
        "POSSESSION_Carries_Dis", "POSSESSION_Receiving_Rec", "POSSESSION_Receiving_PrgR", 
        "MISC_Performance_CrdY", "MISC_Performance_CrdR", "MISC_Performance_2CrdY", "MISC_Performance_Fls", 
        "MISC_Performance_Fld", "MISC_Performance_Off", "MISC_Performance_Crs", 
        "MISC_Performance_Int", "MISC_Performance_TklW", "MISC_Performance_PKwon", 
        "MISC_Performance_PKcon", "MISC_Performance_OG", "MISC_Performance_Recov", 
        "MISC_Aerial_Duels_Won", "MISC_Aerial_Duels_Lost", "MISC_Aerial_Duels_Won%"
      )
    ) %>% 
    
    suppressWarnings()
  
  return(res)
  
}

stats_df <- lapply(c(9,11:13, 20), function(i){
  print(i)
  Sys.sleep(5)
  tryCatch({
    fbref_player_stats(comp_id = i, season = NULL)
  },error=function(e){
    message(paste(e, i))
    NULL
  })
}) %>% 
  bind_rows()

if(dbExistsTable(con, "fbref_player_stats") == FALSE){
  dbWriteTable(con, "fbref_player_stats", stats_df)
}

stats_df2 <- lapply(c(9,11:13, 20), function(i){
  print(i)
  Sys.sleep(5)
  tryCatch({
    fbref_player_stats(comp_id = i, season = "2024-2025")
  },error=function(e){
    message(paste(e, i))
    NULL
  })
}) %>% 
  bind_rows()

dbWriteTable(con, "fbref_player_stats", stats_df2, append = TRUE)


