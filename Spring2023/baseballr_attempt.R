library(pacman)
p_load(baseballr, tidyverse)

batter_2020 <- fg_bat_leaders(x = 2020, y = 2020)
batter_2021 <- fg_bat_leaders(x = 2021, y = 2021)
batter_2022 <- fg_bat_leaders(x = 2022, y = 2022)

z_score <- function(x) {
  
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  
  x
}

z_score_approach <- function(df) {
  
  df <- df %>% 
    mutate(z_runs = z_score(R),
           z_hr = z_score(HR),
           z_rbi = z_score(RBI),
           z_sb = z_score(SB),
           z_k = z_score(SO),
           z_avg = z_score(AVG),
           z_ops = z_score(OPS)) %>% 
    select(1:5, starts_with("z_"))
  
  df
}


z_seasons <- map(.x = list(batter_2020, batter_2021, batter_2022), .f = z_score_approach, .progress = TRUE)

z_seasons <- list_rbind(z_seasons)

z_seasons_avg <- z_seasons %>% 
  group_by(playerid, Name) %>% 
  summarise(z_runs = mean(z_runs, na.rm = TRUE),
            z_hr = mean(z_hr, na.rm = TRUE), 
            z_rbi = mean(z_rbi, na.rm = TRUE),
            z_sb = mean(z_sb, na.rm = TRUE),
            z_k = mean(z_k, na.rm = TRUE),
            z_avg = mean(z_avg, na.rm = TRUE),
            z_ops = mean(z_ops, na.rm = TRUE))

z_seasons_avg <- z_seasons_avg %>% 
  mutate(final_z_sum = z_runs + z_hr + z_rbi + z_sb + z_avg + z_ops - z_k)



# pitching it out

pitcher_2020 <- baseballr::fg_pitcher_leaders(x = 2020, y = 2020)
pitcher_2021 <- baseballr::fg_pitcher_leaders(x = 2021, y = 2021)
pitcher_2022 <- baseballr::fg_pitcher_leaders(x = 2022, y = 2022)

z_score_approach <- function(df) {
  
  df <- df %>% 
    mutate(z_w = z_score(W),
           z_era = z_score(ERA),
           z_saves = z_score(SV),
           z_hr = z_score(HR),
           z_k = z_score(SO),
           z_whip = z_score(WHIP),
           z_hold = z_score(HLD)) %>% 
    select(1:5, starts_with("z_"))
  
  df
}

z_pit_seasons <- map(.x = list(pitcher_2020, pitcher_2021, pitcher_2022), .f = z_score_approach, .progress = TRUE)

z_pit_seasons <- list_rbind(z_pit_seasons)

z_pit_seasons <- z_pit_seasons %>% 
  mutate(z_pit_avg = z_w - z_era + z_k - z_hr - z_whip)


write_csv(z_seasons_avg, "data/batter_2023_zscore_baseballr.csv")
write_csv(z_pit_seasons, "data/pitcher_2023_zscore_baseballr.csv")