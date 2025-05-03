# bu kodu kullan
a <- head(fifa,1)
a <- a %>% 
  select(player_id, short_name, overall, player_positions, 
         attacking_crossing:goalkeeping_speed)

a %>% 
  select(player_id, attacking_crossing:goalkeeping_reflexes) %>% 
  pivot_longer(!player_id, names_to = "FIFAMetric", values_to = "Value") %>% 
  left_join(coef, by = "FIFAMetric") %>% 
  mutate(Overall_Rating = COEFFICIENT * Value) %>% 
  group_by(player_id, POSITION) %>% 
  summarise(Overall_Rating = round(sum(Overall_Rating, na.rm = T))) %>% 
  ungroup() %>% 
  arrange(player_id, match(POSITION, positions))

col_order <- c("GK", "CB", "RB/LB","LM/RM",  "CDM",  "CM",  "CAM","LW/RW", "CF", "ST")

a %>% 
  select(player_id, any_of(coef$FIFAMetric)) %>% 
  pivot_longer(!player_id, names_to = "FIFAMetric", values_to = "Value") %>% 
  left_join(coef, by = "FIFAMetric") %>% 
  mutate(Overall_Rating = COEFFICIENT * Value) %>% 
  group_by(player_id, POSITION) %>% 
  summarise(Overall_Rating = round(sum(Overall_Rating, na.rm = T))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = player_id, names_from = POSITION, values_from = Overall_Rating) %>% 
  select(player_id, any_of(col_order))


calculate_rating <- function(player_df, coef){
  
  col_order <- c("GK", "CB", "RB/LB","LM/RM",  "CDM",  "CM",  "CAM","LW/RW", "CF", "ST")
  
  ratings <- player_df %>% 
    select(player_id, any_of(coef$FIFAMetric)) %>% 
    pivot_longer(!player_id, names_to = "FIFAMetric", values_to = "Value") %>% 
    left_join(coef, by = "FIFAMetric") %>% 
    mutate(Overall_Rating = COEFFICIENT * Value) %>% 
    group_by(player_id, POSITION) %>% 
    summarise(Overall_Rating = round(sum(Overall_Rating, na.rm = T))) %>% 
    ungroup() 
  
  # Best Position
  best_position <- ratings %>% 
    filter(Overall_Rating == max(Overall_Rating)) %>% 
    group_by(player_id) %>% 
    summarise(
      BestPosition = paste0(POSITION, collapse = ", "), 
      BestRating = max(Overall_Rating)
  )
  
  # Result
  res <- ratings %>% 
    pivot_wider(id_cols = player_id, names_from = POSITION, values_from = Overall_Rating) %>% 
    select(player_id, any_of(col_order)) %>% 
    left_join(best_position, by = "player_id") %>% 
    suppressMessages()
  
  return(res)
}

player_df <- a

x <- rating_calculator(a, coef)

f <- map_df(1:6, function(i){
  rating_calculator(head(fifa,6)[i,], coef)
})

left_join(f, fifa %>% select(player_id, short_name, club_name, player_positions, overall, international_reputation)) %>% View


calculate_rating_wir <- function(player_df, coef){
  
  res <- col_order <- c("GK", "CB", "RB/LB","LM/RM",  "CDM",  "CM",  "CAM","LW/RW", "CF", "ST")
  
  ratings <- player_df %>% 
    select(player_id, any_of(coef$FIFAMetric)) %>% 
    pivot_longer(!player_id, names_to = "FIFAMetric", values_to = "Value") %>% 
    left_join(coef, by = "FIFAMetric") %>% 
    mutate(Overall_Rating = COEFFICIENT * Value) %>% 
    group_by(player_id, POSITION) %>% 
    summarise(Overall_Rating = round(sum(Overall_Rating, na.rm = T))) %>% 
    ungroup() 
  
  # Best Position
  best_position <- ratings %>% 
    filter(Overall_Rating == max(Overall_Rating)) %>% 
    group_by(player_id) %>% 
    summarise(
      BestPosition = paste0(POSITION, collapse = ", "), 
      BestRating = max(Overall_Rating)
    )
  
  # Result
  res <- ratings %>% 
    pivot_wider(id_cols = player_id, names_from = POSITION, values_from = Overall_Rating) %>% 
    select(player_id, any_of(col_order)) %>% 
    left_join(best_position, by = "player_id") %>% 
    left_join(player_df %>% select(player_id, short_name, international_reputation, club_name, player_positions, overall, international_reputation)) %>%
    mutate(
      IRBestRating = case_when(
        BestRating >= 51 & international_reputation == 3 ~ BestRating+1, 
        BestRating >= 36 & BestRating <= 66 & international_reputation == 4 ~ BestRating+1,
        BestRating >= 67 & BestRating <= 99 & international_reputation == 4 ~ BestRating+2,
        BestRating >= 24 & BestRating <= 49 & international_reputation == 5 ~ BestRating+1,
        BestRating >= 50 & BestRating <= 74 & international_reputation == 5 ~ BestRating+1,
        BestRating >= 75 & BestRating <= 99 & international_reputation == 5 ~ BestRating+3,
        .default = BestRating
      )
    ) %>% suppressMessages() 
  
  return(res)
  
}

a <- head(fifa,1)
a <- fifa[13,] %>% 
  select(player_id, short_name, overall, club_name, player_positions, international_reputation,
         attacking_crossing:goalkeeping_speed)
calculator_rating_wir(a, coef) %>% View


