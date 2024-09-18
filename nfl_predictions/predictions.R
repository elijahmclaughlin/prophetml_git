
# importing data
nfl_data_2024 <- nflreadr::load_pbp(2023:2024) %>%
  dplyr::filter((season == 2023 & week > 7) | season == 2024)

nfl_24 <- nfl_data_2024 %>%
  filter(!is.na(game_id)) %>%
  group_by(game_id, week) %>%
  summarise(
    home_team = first(home_team),
    away_team = first(away_team),
    season = first(season),
    spread = first(spread_line),
    spread_result = first(result),
    total_line = first(total_line),
    total_result = first(total),
    home_score = first(home_score),
    away_score = first(away_score),
    home_team_win = if_else(first(home_score) > first(away_score), 1, 0),
    spread_win = if_else(first(spread_result) > first(spread), 1, 0),
    total_win = if_else(first(total_result) > first(total_line), 1, 0),
    
    # iq
    home_score_diff = first(home_score) - first(away_score),
    away_score_diff = first(away_score) - first(home_score),
    home_avg_yds2go = mean(ydstogo[posteam == first(home_team)], na.rm = TRUE),
    away_avg_yds2go = mean(ydstogo[posteam == first(away_team)], na.rm = TRUE),
    avg_yds2go_diff = home_avg_yds2go - away_avg_yds2go,
    home_avg_ydsgained = mean(yards_gained[posteam == first(home_team)], na.rm = TRUE),
    away_avg_ydsgained = mean(yards_gained[posteam == first(away_team)], na.rm = TRUE),
    avg_ydsgained_diff = home_avg_ydsgained - away_avg_ydsgained,
    home_epa = mean(epa[posteam == first(home_team)], na.rm = TRUE),
    away_epa = mean(epa[posteam == first(away_team)], na.rm = TRUE),
    epa_diff = home_epa - away_epa,
    home_wpa = mean(wpa[posteam == first(home_team)], na.rm = TRUE),
    away_wpa = mean(wpa[posteam == first(away_team)], na.rm = TRUE),
    wpa_diff = home_wpa - away_wpa,
    
    # passing ability
    home_total_air_yards = sum(air_yards[posteam == first(home_team)], na.rm = TRUE),
    away_total_air_yards = sum(air_yards[posteam == first(away_team)], na.rm = TRUE),
    home_avg_air_yards = mean(air_yards[posteam == first(home_team)], na.rm = TRUE),
    away_avg_air_yards = mean(air_yards[posteam == first(away_team)], na.rm = TRUE),
    home_total_pass_yards = sum(passing_yards[posteam == first(home_team)], na.rm = TRUE),
    away_total_pass_yards = sum(passing_yards[posteam == first(away_team)], na.rm = TRUE),
    home_avg_pass_yards = mean(passing_yards[posteam == first(home_team)], na.rm = TRUE),
    away_avg_pass_yards = mean(passing_yards[posteam == first(away_team)], na.rm = TRUE),
    home_td_prob = mean(td_prob[posteam == first(home_team)], na.rm = TRUE),
    away_td_prob = mean(td_prob[posteam == first(away_team)], na.rm = TRUE),
    home_passing_touchdowns = sum(pass_touchdown[posteam == first(home_team)], na.rm = TRUE),
    away_passing_touchdowns = sum(pass_touchdown[posteam == first(away_team)], na.rm = TRUE),
    home_interceptions = sum(interception[posteam == first(home_team)], na.rm = TRUE),
    away_interceptions = sum(interception[posteam == first(away_team)], na.rm = TRUE),
    home_pass_att = sum(pass_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_pass_att = sum(pass_attempt[posteam == first(away_team)], na.rm = TRUE),
    home_inc_passes = sum(incomplete_pass[posteam == first(home_team)], na.rm = TRUE),
    away_inc_passes = sum(incomplete_pass[posteam == first(away_team)], na.rm = TRUE),
    home_complete_pass = home_pass_att - home_inc_passes,
    away_complete_pass = away_pass_att - away_inc_passes,
    home_pass_percentage = home_complete_pass / home_pass_att,
    away_pass_percentage = away_complete_pass / away_pass_att,
    
    # rushing ability
    home_touches = sum(rush_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_touches = sum(rush_attempt[posteam == first(away_team)], na.rm = TRUE),
    home_avg_touches = mean(rush_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_avg_touches = mean(rush_attempt[posteam == first(away_team)], na.rm = TRUE),
    home_rush_yds = sum(rushing_yards[posteam == first(home_team)], na.rm = TRUE),
    away_rush_yds = sum(rushing_yards[posteam == first(away_team)], na.rm = TRUE),
    home_avg_rush_yds = mean(rushing_yards[posteam == first(home_team)], na.rm = TRUE),
    away_avg_rush_yds = mean(rushing_yards[posteam == first(away_team)], na.rm = TRUE),
    home_rush_tds = sum(rush_touchdown[posteam == first(home_team)], na.rm = TRUE),
    away_rush_tds = sum(rush_touchdown[posteam == first(away_team)], na.rm = TRUE),
    home_fumbles = sum(fumble_lost[posteam == first(home_team)], na.rm = TRUE),
    away_fumbles = sum(fumble_lost[posteam == first(away_team)], na.rm = TRUE),
    
    # receiving ability
    home_targets = sum(pass_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_targets = sum(pass_attempt[posteam == first(away_team)], na.rm = TRUE),
    home_receptions = sum(pass_attempt[posteam == first(home_team)], na.rm = TRUE) - sum(incomplete_pass[posteam == first(home_team)], na.rm = TRUE),
    away_receptions = sum(pass_attempt[posteam == first(away_team)], na.rm = TRUE) - sum(incomplete_pass[posteam == first(away_team)], na.rm = TRUE),
    home_rec_yds = sum(receiving_yards[posteam == first(home_team)], na.rm = TRUE),
    away_rec_yds = sum(receiving_yards[posteam == first(away_team)], na.rm = TRUE),
    home_avg_rec_yds = mean(receiving_yards[posteam == first(home_team)], na.rm = TRUE),
    away_avg_rec_yds = mean(receiving_yards[posteam == first(away_team)], na.rm = TRUE),
    home_rec_tds = sum(pass_touchdown[posteam == first(home_team)], na.rm = TRUE),
    away_rec_tds = sum(pass_touchdown[posteam == first(away_team)], na.rm = TRUE),
    
    # defensive ability
    home_def_int = sum(interception[defteam == first(home_team)], na.rm = TRUE),
    away_def_int = sum(interception[defteam == first(away_team)], na.rm = TRUE),
    home_sacks = sum(sack[defteam == first(home_team)], na.rm = TRUE),
    away_sacks = sum(sack[defteam == first(away_team)], na.rm = TRUE),
    home_force_fumb = sum(fumble_forced[defteam == first(home_team)], na.rm = TRUE),
    away_force_fumb = sum(fumble_forced[defteam == first(away_team)], na.rm = TRUE),
    home_safeties = coalesce(sum(safety[defteam == first(home_team)], na.rm = TRUE), 0),
    away_safeties = coalesce(sum(safety[defteam == first(away_team)], na.rm = TRUE), 0),
    home_tfls = sum(tackled_for_loss[defteam == first(home_team)], na.rm = TRUE),
    away_tfls = sum(tackled_for_loss[defteam == first(away_team)], na.rm = TRUE),
    home_qb_hits = sum(qb_hit[defteam == first(home_team)], na.rm = TRUE),
    away_qb_hits = sum(qb_hit[defteam == first(away_team)], na.rm = TRUE),
    home_tackles = sum(solo_tackle[defteam == first(home_team)], na.rm = TRUE),
    away_tackles = sum(solo_tackle[defteam == first(away_team)], na.rm = TRUE),
    
    # kicking ability
    home_fg_prob = mean(fg_prob[posteam == first(home_team)], na.rm = TRUE),
    away_fg_prob = mean(fg_prob[posteam == first(away_team)], na.rm = TRUE),
    home_fg_att = sum(field_goal_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_fg_att = sum(field_goal_attempt[posteam == first(away_team)], na.rm = TRUE),
    home_pat_prob = mean(extra_point_prob[posteam == first(home_team)], na.rm = TRUE),
    away_pat_prob = mean(extra_point_prob[posteam == first(away_team)], na.rm = TRUE),
    home_pat_att = sum(extra_point_attempt[posteam == first(home_team)], na.rm = TRUE),
    away_pat_att = sum(extra_point_attempt[posteam == first(away_team)], na.rm = TRUE),
    .groups = "drop"  # Drop all grouping after summarization
  )  %>%
  dplyr::mutate(
    # iq
    home_score_diff_rk = dplyr::dense_rank(dplyr::desc(home_score_diff)),
    away_score_diff_rk = dplyr::dense_rank(dplyr::desc(away_score_diff)),
    home_avg_yds2go_rk = dplyr::dense_rank(home_avg_yds2go),
    away_avg_yds2go_rk = dplyr::dense_rank(away_avg_yds2go),
    avg_yds2go_diff_rk = dplyr::dense_rank(avg_yds2go_diff),
    home_avg_ydsgained_rk = dplyr::dense_rank(dplyr::desc(home_avg_ydsgained)),
    away_avg_ydsgained_rk = dplyr::dense_rank(dplyr::desc(away_avg_ydsgained)),
    avg_ydsgained_diff_rk = dplyr::dense_rank(dplyr::desc(avg_ydsgained_diff)),
    home_epa_rk = dplyr::dense_rank(dplyr::desc(home_epa)),
    away_epa_rk = dplyr::dense_rank(dplyr::desc(away_epa)),
    epa_diff_rk = dplyr::dense_rank(dplyr::desc(epa_diff)),
    home_wpa_rk = dplyr::dense_rank(dplyr::desc(home_wpa)),
    away_wpa_rk = dplyr::dense_rank(dplyr::desc(away_wpa)),
    wpa_diff_rk = dplyr::dense_rank(dplyr::desc(wpa_diff)),
    
    # passing ability
    home_total_pass_yards_rk = dplyr::dense_rank(dplyr::desc(home_total_pass_yards)),
    away_total_pass_yards_rk = dplyr::dense_rank(dplyr::desc(away_total_pass_yards)),
    home_avg_pass_yards_rk = dplyr::dense_rank(dplyr::desc(home_avg_pass_yards)),
    away_avg_pass_yards_rk = dplyr::dense_rank(dplyr::desc(away_avg_pass_yards)),
    home_passing_touchdowns_rk = dplyr::dense_rank(dplyr::desc(home_passing_touchdowns)),
    away_passing_touchdowns_rk = dplyr::dense_rank(dplyr::desc(away_passing_touchdowns)),
    home_interceptions_rk = dplyr::dense_rank(home_interceptions),
    away_interceptions_rk = dplyr::dense_rank(away_interceptions),
    home_pass_att_rk = dplyr::dense_rank(dplyr::desc(home_pass_att)),
    away_pass_att_rk = dplyr::dense_rank(dplyr::desc(away_pass_att)),
    home_complete_pass_rk = dplyr::dense_rank(dplyr::desc(home_complete_pass)),
    away_complete_pass_rk = dplyr::dense_rank(dplyr::desc(away_complete_pass)),
    home_inc_passes_rk = dplyr::dense_rank(dplyr::desc(home_inc_passes)),
    away_inc_passes_rk = dplyr::dense_rank(dplyr::desc(away_inc_passes)),,
    home_pass_percentage_rk = dplyr::dense_rank(dplyr::desc(home_pass_percentage)),
    away_pass_percentage_rk = dplyr::dense_rank(dplyr::desc(away_pass_percentage)),
    home_total_air_yards_rk = dplyr::dense_rank(dplyr::desc(home_total_air_yards)),
    away_total_air_yards_rk = dplyr::dense_rank(dplyr::desc(away_total_air_yards)),
    home_avg_air_yards_rk = dplyr::dense_rank(dplyr::desc(home_avg_air_yards)),
    away_avg_air_yards_rk = dplyr::dense_rank(dplyr::desc(away_avg_air_yards)),
    home_td_prob_rk = dplyr::dense_rank(dplyr::desc(home_td_prob)),
    away_td_prob_rk = dplyr::dense_rank(dplyr::desc(away_td_prob)),
    
    # rushing stats
    home_rush_yds_rk = dplyr::dense_rank(dplyr::desc(home_rush_yds)),
    away_rush_yds_rk = dplyr::dense_rank(dplyr::desc(away_rush_yds)),
    home_avg_rush_yds_rk = dplyr::dense_rank(dplyr::desc(home_avg_rush_yds)),
    away_avg_rush_yds_rk = dplyr::dense_rank(dplyr::desc(away_avg_rush_yds)),
    home_touches_rk = dplyr::dense_rank(dplyr::desc(home_touches)),
    away_touches_rk = dplyr::dense_rank(dplyr::desc(away_touches)),
    home_avg_touches_rk = dplyr::dense_rank(dplyr::desc(home_avg_touches)),
    away_avg_touches_rk = dplyr::dense_rank(dplyr::desc(away_avg_touches)),
    home_rush_tds_rk = dplyr::dense_rank(dplyr::desc(home_rush_tds)),
    away_rush_tds_rk = dplyr::dense_rank(dplyr::desc(away_rush_tds)),
    home_fumbles_rk = dplyr::dense_rank(home_fumbles),
    away_fumbles_rk = dplyr::dense_rank(away_fumbles),
    
    # receiving stats
    home_rec_yds_rk = dplyr::dense_rank(dplyr::desc(home_rec_yds)),
    away_rec_yds_rk = dplyr::dense_rank(dplyr::desc(away_rec_yds)),
    home_avg_rec_yds_rk = dplyr::dense_rank(dplyr::desc(home_avg_rec_yds)),
    away_avg_rec_yds_rk = dplyr::dense_rank(dplyr::desc(away_avg_rec_yds)),
    home_targets_rk = dplyr::dense_rank(dplyr::desc(home_targets)),
    away_targets_rk = dplyr::dense_rank(dplyr::desc(away_targets)),
    home_receptions_rk = dplyr::dense_rank(dplyr::desc(home_receptions)),
    away_receptions_rk = dplyr::dense_rank(dplyr::desc(away_receptions)),
    home_rec_tds_rk = dplyr::dense_rank(dplyr::desc(home_rec_tds)),
    away_rec_tds_rk = dplyr::dense_rank(dplyr::desc(away_rec_tds)),
    
    # kicking ability
    home_fg_prob_rk = dplyr::dense_rank(dplyr::desc(home_fg_prob)),
    away_fg_prob_rk = dplyr::dense_rank(dplyr::desc(away_fg_prob)),
    home_fg_att_rk = dplyr::dense_rank(dplyr::desc(home_fg_att)),
    away_fg_att_rk = dplyr::dense_rank(dplyr::desc(away_fg_att)),
    home_pat_prob_rk = dplyr::dense_rank(dplyr::desc(home_pat_prob)),
    away_pat_prob_rk = dplyr::dense_rank(dplyr::desc(away_pat_prob)),
    home_pat_att_rk = dplyr::dense_rank(dplyr::desc(home_pat_att)),
    away_pat_att_rk = dplyr::dense_rank(dplyr::desc(away_pat_att)),
    
    # defensive ability
    home_def_int_rk = dplyr::dense_rank(dplyr::desc(home_def_int)),
    away_def_int_rk = dplyr::dense_rank(dplyr::desc(away_def_int)),
    home_sacks_rk = dplyr::dense_rank(dplyr::desc(home_sacks)),
    away_sacks_rk = dplyr::dense_rank(dplyr::desc(away_sacks)),
    home_force_fumb_rk = dplyr::dense_rank(dplyr::desc(home_force_fumb)),
    away_force_fumb_rk = dplyr::dense_rank(dplyr::desc(away_force_fumb)),
    home_safeties_rk = dplyr::dense_rank(dplyr::desc(home_safeties)),
    away_safeties_rk = dplyr::dense_rank(dplyr::desc(away_safeties)),
    home_tfls_rk = dplyr::dense_rank(dplyr::desc(home_tfls)),
    away_tfls_rk = dplyr::dense_rank(dplyr::desc(away_tfls)),
    home_qb_hits_rk = dplyr::dense_rank(dplyr::desc(home_qb_hits)),
    away_qb_hits_rk = dplyr::dense_rank(dplyr::desc(away_qb_hits)),
    home_tackles_rk = dplyr::dense_rank(dplyr::desc(home_tackles)),
    away_tackles_rk = dplyr::dense_rank(dplyr::desc(away_tackles)),
    
  ) %>%
  group_by(season) %>%
  dplyr::mutate(
    # normalizing metrics and grading players
    # iq
    home_score_diff_norm = (home_score_diff - min(home_score_diff)) / (max(home_score_diff) - min(home_score_diff)),
    away_score_diff_norm = (away_score_diff - min(away_score_diff)) / (max(away_score_diff) - min(away_score_diff)),
    home_avg_yds2go_norm = (home_avg_yds2go - min(home_avg_yds2go)) / (max(home_avg_yds2go) - min(home_avg_yds2go)),
    away_avg_yds2go_norm = (away_avg_yds2go - min(away_avg_yds2go)) / (max(away_avg_yds2go) - min(away_avg_yds2go)),
    avg_yds2go_diff_norm = (avg_yds2go_diff - min(avg_yds2go_diff)) / (max(avg_yds2go_diff) - min(avg_yds2go_diff)),
    home_avg_ydsgained_norm = (home_avg_ydsgained - min(home_avg_ydsgained)) / (max(home_avg_ydsgained) - min(home_avg_ydsgained)),
    away_avg_ydsgained_norm = (away_avg_ydsgained - min(away_avg_ydsgained)) / (max(away_avg_ydsgained) - min(away_avg_ydsgained)),
    avg_ydsgained_diff_norm = (avg_ydsgained_diff - min(avg_ydsgained_diff)) / (max(avg_ydsgained_diff) - min(avg_ydsgained_diff)),
    home_epa_norm = (home_epa - min(home_epa)) / (max(home_epa) - min(home_epa)),
    away_epa_norm = (away_epa - min(away_epa)) / (max(away_epa) - min(away_epa)),
    epa_diff_norm = (epa_diff - min(epa_diff)) / (max(epa_diff) - min(epa_diff)),
    home_wpa_norm = (home_wpa - min(home_wpa)) / (max(home_wpa) - min(home_wpa)),
    away_wpa_norm = (away_wpa - min(away_wpa)) / (max(away_wpa) - min(away_wpa)),
    wpa_diff_norm = (wpa_diff - min(wpa_diff)) / (max(wpa_diff) - min(wpa_diff)),
    
    # iq rank
    home_score_diff_rk_norm = (home_score_diff_rk - min(home_score_diff_rk)) / (max(home_score_diff_rk) - min(home_score_diff_rk)),
    away_score_diff_rk_norm = (away_score_diff_rk - min(away_score_diff_rk)) / (max(away_score_diff_rk) - min(away_score_diff_rk)),
    home_avg_yds2go_rk_norm = (home_avg_yds2go_rk - min(home_avg_yds2go_rk)) / (max(home_avg_yds2go_rk) - min(home_avg_yds2go_rk)),
    away_avg_yds2go_rk_norm = (away_avg_yds2go_rk - min(away_avg_yds2go_rk)) / (max(away_avg_yds2go_rk) - min(away_avg_yds2go_rk)),
    avg_yds2go_diff_rk_norm = (avg_yds2go_diff_rk - min(avg_yds2go_diff_rk)) / (max(avg_yds2go_diff_rk) - min(avg_yds2go_diff_rk)),
    home_avg_ydsgained_rk_norm = (home_avg_ydsgained_rk - min(home_avg_ydsgained_rk)) / (max(home_avg_ydsgained_rk) - min(home_avg_ydsgained_rk)),
    away_avg_ydsgained_rk_norm = (away_avg_ydsgained_rk - min(away_avg_ydsgained_rk)) / (max(away_avg_ydsgained_rk) - min(away_avg_ydsgained_rk)),
    avg_ydsgained_diff_rk_norm = (avg_ydsgained_diff_rk - min(avg_ydsgained_diff_rk)) / (max(avg_ydsgained_diff_rk) - min(avg_ydsgained_diff_rk)),
    home_epa_rk_norm = (home_epa_rk - min(home_epa_rk)) / (max(home_epa_rk) - min(home_epa_rk)),
    away_epa_rk_norm = (away_epa_rk - min(away_epa_rk)) / (max(away_epa_rk) - min(away_epa_rk)),
    epa_diff_rk_norm = (epa_diff_rk - min(epa_diff_rk)) / (max(epa_diff_rk) - min(epa_diff_rk)),
    home_wpa_rk_norm = (home_wpa_rk - min(home_wpa_rk)) / (max(home_wpa_rk) - min(home_wpa_rk)),
    away_wpa_rk_norm = (away_wpa_rk - min(away_wpa_rk)) / (max(away_wpa_rk) - min(away_wpa_rk)),
    wpa_diff_rk_norm = (wpa_diff_rk - min(wpa_diff_rk)) / (max(wpa_diff_rk) - min(wpa_diff_rk)),
    
    # passing ability
    home_total_air_yards_norm = (home_total_air_yards - min(home_total_air_yards)) / (max(home_total_air_yards) - min(home_total_air_yards)),
    away_total_air_yards_norm = (away_total_air_yards - min(away_total_air_yards)) / (max(away_total_air_yards) - min(away_total_air_yards)),
    home_avg_air_yards_norm = (home_avg_air_yards - min(home_avg_air_yards)) / (max(home_avg_air_yards) - min(home_avg_air_yards)),
    away_avg_air_yards_norm = (away_avg_air_yards - min(away_avg_air_yards)) / (max(away_avg_air_yards) - min(away_avg_air_yards)),
    home_total_pass_yards_norm = (home_total_pass_yards - min(home_total_pass_yards)) / (max(home_total_pass_yards) - min(home_total_pass_yards)),
    away_total_pass_yards_norm = (away_total_pass_yards - min(away_total_pass_yards)) / (max(away_total_pass_yards) - min(away_total_pass_yards)),
    home_avg_pass_yards_norm = (home_avg_pass_yards - min(home_avg_pass_yards)) / (max(home_avg_pass_yards) - min(home_avg_pass_yards)),
    away_avg_pass_yards_norm = (away_avg_pass_yards - min(away_avg_pass_yards)) / (max(away_avg_pass_yards) - min(away_avg_pass_yards)),
    home_td_prob_norm = (home_td_prob - min(home_td_prob)) / (max(home_td_prob) - min(home_td_prob)),
    away_td_prob_norm = (away_td_prob - min(away_td_prob)) / (max(away_td_prob) - min(away_td_prob)),
    home_passing_touchdowns_norm = (home_passing_touchdowns - min(home_passing_touchdowns)) / (max(home_passing_touchdowns) - min(home_passing_touchdowns)),
    away_passing_touchdowns_norm = (away_passing_touchdowns - min(away_passing_touchdowns)) / (max(away_passing_touchdowns) - min(away_passing_touchdowns)),
    home_interceptions_norm = (home_interceptions - min(home_interceptions)) / (max(home_interceptions) - min(home_interceptions)),
    away_interceptions_norm = (away_interceptions - min(away_interceptions)) / (max(away_interceptions) - min(away_interceptions)),
    home_pass_att_norm = (home_pass_att - min(home_pass_att)) / (max(home_pass_att) - min(home_pass_att)),
    away_pass_att_norm = (away_pass_att - min(away_pass_att)) / (max(away_pass_att) - min(away_pass_att)),
    home_inc_passes_norm = (home_inc_passes - min(home_inc_passes)) / (max(home_inc_passes) - min(home_inc_passes)),
    away_inc_passes_norm = (away_inc_passes - min(away_inc_passes)) / (max(away_inc_passes) - min(away_inc_passes)),
    home_complete_pass_norm = (home_complete_pass - min(home_complete_pass)) / (max(home_complete_pass) - min(home_complete_pass)),
    away_complete_pass_norm = (away_complete_pass - min(away_complete_pass)) / (max(away_complete_pass) - min(away_complete_pass)),
    home_pass_percentage_norm = (home_pass_percentage - min(home_pass_percentage)) / (max(home_pass_percentage) - min(home_pass_percentage)),
    away_pass_percentage_norm = (away_pass_percentage - min(away_pass_percentage)) / (max(away_pass_percentage) - min(away_pass_percentage)),
    
    # passing ability rank
    home_total_pass_yards_rk_norm = (home_total_pass_yards_rk - min(home_total_pass_yards_rk)) / (max(home_total_pass_yards_rk) - min(home_total_pass_yards_rk)),
    away_total_pass_yards_rk_norm = (away_total_pass_yards_rk - min(away_total_pass_yards_rk)) / (max(away_total_pass_yards_rk) - min(away_total_pass_yards_rk)),
    home_avg_pass_yards_rk_norm = (home_avg_pass_yards_rk - min(home_avg_pass_yards_rk)) / (max(home_avg_pass_yards_rk) - min(home_avg_pass_yards_rk)),
    away_avg_pass_yards_rk_norm = (away_avg_pass_yards_rk - min(away_avg_pass_yards_rk)) / (max(away_avg_pass_yards_rk) - min(away_avg_pass_yards_rk)),
    home_passing_touchdowns_rk_norm = (home_passing_touchdowns_rk - min(home_passing_touchdowns_rk)) / (max(home_passing_touchdowns_rk) - min(home_passing_touchdowns_rk)),
    away_passing_touchdowns_rk_norm = (away_passing_touchdowns_rk - min(away_passing_touchdowns_rk)) / (max(away_passing_touchdowns_rk) - min(away_passing_touchdowns_rk)),
    home_interceptions_rk_norm = (home_interceptions_rk - min(home_interceptions_rk)) / (max(home_interceptions_rk) - min(home_interceptions_rk)),
    away_interceptions_rk_norm = (away_interceptions_rk - min(away_interceptions_rk)) / (max(away_interceptions_rk) - min(away_interceptions_rk)),
    home_pass_att_rk_norm = (home_pass_att_rk - min(home_pass_att_rk)) / (max(home_pass_att_rk) - min(home_pass_att_rk)),
    away_pass_att_rk_norm = (away_pass_att_rk - min(away_pass_att_rk)) / (max(away_pass_att_rk) - min(away_pass_att_rk)),
    home_complete_pass_rk_norm = (home_complete_pass_rk - min(home_complete_pass_rk)) / (max(home_complete_pass_rk) - min(home_complete_pass_rk)),
    away_complete_pass_rk_norm = (away_complete_pass_rk - min(away_complete_pass_rk)) / (max(away_complete_pass_rk) - min(away_complete_pass_rk)),
    home_inc_passes_rk_norm = (home_inc_passes_rk - min(home_inc_passes_rk)) / (max(home_inc_passes_rk) - min(home_inc_passes_rk)),
    away_inc_passes_rk_norm = (away_inc_passes_rk - min(away_inc_passes_rk)) / (max(away_inc_passes_rk) - min(away_inc_passes_rk)),
    home_pass_percentage_rk_norm = (home_pass_percentage_rk - min(home_pass_percentage_rk)) / (max(home_pass_percentage_rk) - min(home_pass_percentage_rk)),
    away_pass_percentage_rk_norm = (away_pass_percentage_rk - min(away_pass_percentage_rk)) / (max(away_pass_percentage_rk) - min(away_pass_percentage_rk)),
    home_total_air_yards_rk_norm = (home_total_air_yards_rk - min(home_total_air_yards_rk)) / (max(home_total_air_yards_rk) - min(home_total_air_yards_rk)),
    away_total_air_yards_rk_norm = (away_total_air_yards_rk - min(away_total_air_yards_rk)) / (max(away_total_air_yards_rk) - min(away_total_air_yards_rk)),
    home_avg_air_yards_rk_norm = (home_avg_air_yards_rk - min(home_avg_air_yards_rk)) / (max(home_avg_air_yards_rk) - min(home_avg_air_yards_rk)),
    away_avg_air_yards_rk_norm = (away_avg_air_yards_rk - min(away_avg_air_yards_rk)) / (max(away_avg_air_yards_rk) - min(away_avg_air_yards_rk)),
    home_td_prob_rk_norm = (home_td_prob_rk - min(home_td_prob_rk)) / (max(home_td_prob_rk) - min(home_td_prob_rk)),
    away_td_prob_rk_norm = (away_td_prob_rk - min(away_td_prob_rk)) / (max(away_td_prob_rk) - min(away_td_prob_rk)),
    
    # rushing ability
    home_touches_norm = (home_touches - min(home_touches)) / (max(home_touches) - min(home_touches)),
    away_touches_norm = (away_touches - min(away_touches)) / (max(away_touches) - min(away_touches)),
    home_avg_touches_norm = (home_avg_touches - min(home_avg_touches)) / (max(home_avg_touches) - min(home_avg_touches)),
    away_avg_touches_norm = (away_avg_touches - min(away_avg_touches)) / (max(away_avg_touches) - min(away_avg_touches)),
    home_rush_yds_norm = (home_rush_yds - min(home_rush_yds)) / (max(home_rush_yds) - min(home_rush_yds)),
    away_rush_yds_norm = (away_rush_yds - min(away_rush_yds)) / (max(away_rush_yds) - min(away_rush_yds)),
    home_avg_rush_yds_norm = (home_avg_rush_yds - min(home_avg_rush_yds)) / (max(home_avg_rush_yds) - min(home_avg_rush_yds)),
    away_avg_rush_yds_norm = (away_avg_rush_yds - min(away_avg_rush_yds)) / (max(away_avg_rush_yds) - min(away_avg_rush_yds)),
    home_rush_tds_norm = (home_rush_tds - min(home_rush_tds)) / (max(home_rush_tds) - min(home_rush_tds)),
    away_rush_tds_norm = (away_rush_tds - min(away_rush_tds)) / (max(away_rush_tds) - min(away_rush_tds)),
    home_fumbles_norm = (home_fumbles - min(home_fumbles)) / (max(home_fumbles) - min(home_fumbles)),
    away_fumbles_norm = (away_fumbles - min(away_fumbles)) / (max(away_fumbles) - min(away_fumbles)),
    
    # rushing stats rank
    home_rush_yds_rk_norm = (home_rush_yds_rk - min(home_rush_yds_rk)) / (max(home_rush_yds_rk) - min(home_rush_yds_rk)),
    away_rush_yds_rk_norm = (away_rush_yds_rk - min(away_rush_yds_rk)) / (max(away_rush_yds_rk) - min(away_rush_yds_rk)),
    home_avg_rush_yds_rk_norm = (home_avg_rush_yds_rk - min(home_avg_rush_yds_rk)) / (max(home_avg_rush_yds_rk) - min(home_avg_rush_yds_rk)),
    away_avg_rush_yds_rk_norm = (away_avg_rush_yds_rk - min(away_avg_rush_yds_rk)) / (max(away_avg_rush_yds_rk) - min(away_avg_rush_yds_rk)),
    home_touches_rk_norm = (home_touches_rk - min(home_touches_rk)) / (max(home_touches_rk) - min(home_touches_rk)),
    away_touches_rk_norm = (away_touches_rk - min(away_touches_rk)) / (max(away_touches_rk) - min(away_touches_rk)),
    home_avg_touches_rk_norm = (home_avg_touches_rk - min(home_avg_touches_rk)) / (max(home_avg_touches_rk) - min(home_avg_touches_rk)),
    away_avg_touches_rk_norm = (away_avg_touches_rk - min(away_avg_touches_rk)) / (max(away_avg_touches_rk) - min(away_avg_touches_rk)),
    home_rush_tds_rk_norm = (home_rush_tds_rk - min(home_rush_tds_rk)) / (max(home_rush_tds_rk) - min(home_rush_tds_rk)),
    away_rush_tds_rk_norm = (away_rush_tds_rk - min(away_rush_tds_rk)) / (max(away_rush_tds_rk) - min(away_rush_tds_rk)),
    home_fumbles_rk_norm = (home_fumbles_rk - min(home_fumbles_rk)) / (max(home_fumbles_rk) - min(home_fumbles_rk)),
    away_fumbles_rk_norm = (away_fumbles_rk - min(away_fumbles_rk)) / (max(away_fumbles_rk) - min(away_fumbles_rk)),
    
    # receiving ability
    home_targets_norm = (home_targets - min(home_targets)) / (max(home_targets) - min(home_targets)),
    away_targets_norm = (away_targets - min(away_targets)) / (max(away_targets) - min(away_targets)),
    home_receptions_norm = (home_receptions - min(home_receptions)) / (max(home_receptions) - min(home_receptions)),
    away_receptions_norm = (away_receptions - min(away_receptions)) / (max(away_receptions) - min(away_receptions)),
    home_rec_yds_norm = (home_rec_yds - min(home_rec_yds)) / (max(home_rec_yds) - min(home_rec_yds)),
    away_rec_yds_norm = (away_rec_yds - min(away_rec_yds)) / (max(away_rec_yds) - min(away_rec_yds)),
    home_avg_rec_yds_norm = (home_avg_rec_yds - min(home_avg_rec_yds)) / (max(home_avg_rec_yds) - min(home_avg_rec_yds)),
    away_avg_rec_yds_norm = (away_avg_rec_yds - min(away_avg_rec_yds)) / (max(away_avg_rec_yds) - min(away_avg_rec_yds)),
    home_rec_tds_norm = (home_rec_tds - min(home_rec_tds)) / (max(home_rec_tds) - min(home_rec_tds)),
    away_rec_tds_norm = (away_rec_tds - min(away_rec_tds)) / (max(away_rec_tds) - min(away_rec_tds)),
    
    # receiving ability rank
    home_rec_yds_rk_norm = (home_rec_yds_rk - min(home_rec_yds_rk)) / (max(home_rec_yds_rk) - min(home_rec_yds_rk)),
    away_rec_yds_rk_norm = (away_rec_yds_rk - min(away_rec_yds_rk)) / (max(away_rec_yds_rk) - min(away_rec_yds_rk)),
    home_avg_rec_yds_rk_norm = (home_avg_rec_yds_rk - min(home_avg_rec_yds_rk)) / (max(home_avg_rec_yds_rk) - min(home_avg_rec_yds_rk)),
    away_avg_rec_yds_rk_norm = (away_avg_rec_yds_rk - min(away_avg_rec_yds_rk)) / (max(away_avg_rec_yds_rk) - min(away_avg_rec_yds_rk)),
    home_targets_rk_norm = (home_targets_rk - min(home_targets_rk)) / (max(home_targets_rk) - min(home_targets_rk)),
    away_targets_rk_norm = (away_targets_rk - min(away_targets_rk)) / (max(away_targets_rk) - min(away_targets_rk)),
    home_receptions_rk_norm = (home_receptions_rk - min(home_receptions_rk)) / (max(home_receptions_rk) - min(home_receptions_rk)),
    away_receptions_rk_norm = (away_receptions_rk - min(away_receptions_rk)) / (max(away_receptions_rk) - min(away_receptions_rk)),
    home_rec_tds_rk_norm = (home_rec_tds_rk - min(home_rec_tds_rk)) / (max(home_rec_tds_rk) - min(home_rec_tds_rk)),
    away_rec_tds_rk_norm = (away_rec_tds_rk - min(away_rec_tds_rk)) / (max(away_rec_tds_rk) - min(away_rec_tds_rk)),
    
    # defensive ability
    home_def_int_norm = (home_def_int - min(home_def_int)) / (max(home_def_int) - min(home_def_int)),
    away_def_int_norm = (away_def_int - min(away_def_int)) / (max(away_def_int) - min(away_def_int)),
    home_sacks_norm = (home_sacks - min(home_sacks)) / (max(home_sacks) - min(home_sacks)),
    away_sacks_norm = (away_sacks - min(away_sacks)) / (max(away_sacks) - min(away_sacks)),
    home_force_fumb_norm = (home_force_fumb - min(home_force_fumb)) / (max(home_force_fumb) - min(home_force_fumb)),
    away_force_fumb_norm = (away_force_fumb - min(away_force_fumb)) / (max(away_force_fumb) - min(away_force_fumb)),
    home_safeties_norm = coalesce((home_safeties - min(home_safeties)) / (max(home_safeties) - min(home_safeties)), 0),
    away_safeties_norm = coalesce((away_safeties - min(away_safeties)) / (max(away_safeties) - min(away_safeties)), 0),
    home_tfls_norm = (home_tfls - min(home_tfls)) / (max(home_tfls) - min(home_tfls)),
    away_tfls_norm = (away_tfls - min(away_tfls)) / (max(away_tfls) - min(away_tfls)),
    home_qb_hits_norm = (home_qb_hits - min(home_qb_hits)) / (max(home_qb_hits) - min(home_qb_hits)),
    away_qb_hits_norm = (away_qb_hits - min(away_qb_hits)) / (max(away_qb_hits) - min(away_qb_hits)),
    home_tackles_norm = (home_tackles - min(home_tackles)) / (max(home_tackles) - min(home_tackles)),
    away_tackles_norm = (away_tackles - min(away_tackles)) / (max(away_tackles) - min(away_tackles)),
    
    # defensive ability rank
    home_def_int_rk_norm = (home_def_int_rk - min(home_def_int_rk)) / (max(home_def_int_rk) - min(home_def_int_rk)),
    away_def_int_rk_norm = (away_def_int_rk - min(away_def_int_rk)) / (max(away_def_int_rk) - min(away_def_int_rk)),
    home_sacks_rk_norm = (home_sacks_rk - min(home_sacks_rk)) / (max(home_sacks_rk) - min(home_sacks_rk)),
    away_sacks_rk_norm = (away_sacks_rk - min(away_sacks_rk)) / (max(away_sacks_rk) - min(away_sacks_rk)),
    home_force_fumb_rk_norm = (home_force_fumb_rk - min(home_force_fumb_rk)) / (max(home_force_fumb_rk) - min(home_force_fumb_rk)),
    away_force_fumb_rk_norm = (away_force_fumb_rk - min(away_force_fumb_rk)) / (max(away_force_fumb_rk) - min(away_force_fumb_rk)),
    home_safeties_rk_norm = (home_safeties_rk - min(home_safeties_rk)) / (max(home_safeties_rk) - min(home_safeties_rk)),
    away_safeties_rk_norm = (away_safeties_rk - min(away_safeties_rk)) / (max(away_safeties_rk) - min(away_safeties_rk)),
    home_tfls_rk_norm = (home_tfls_rk - min(home_tfls_rk)) / (max(home_tfls_rk) - min(home_tfls_rk)),
    away_tfls_rk_norm = (away_tfls_rk - min(away_tfls_rk)) / (max(away_tfls_rk) - min(away_tfls_rk)),
    home_qb_hits_rk_norm = (home_qb_hits_rk - min(home_qb_hits_rk)) / (max(home_qb_hits_rk) - min(home_qb_hits_rk)),
    away_qb_hits_rk_norm = (away_qb_hits_rk - min(away_qb_hits_rk)) / (max(away_qb_hits_rk) - min(away_qb_hits_rk)),
    home_tackles_rk_norm = (home_tackles_rk - min(home_tackles_rk)) / (max(home_tackles_rk) - min(home_tackles_rk)),
    away_tackles_rk_norm = (away_tackles_rk - min(away_tackles_rk)) / (max(away_tackles_rk) - min(away_tackles_rk)),
    
    # kicking ability
    home_fg_prob_norm = (home_fg_prob - min(home_fg_prob)) / (max(home_fg_prob) - min(home_fg_prob)),
    away_fg_prob_norm = (away_fg_prob - min(away_fg_prob)) / (max(away_fg_prob) - min(away_fg_prob)),
    home_fg_att_norm = (home_fg_att - min(home_fg_att)) / (max(home_fg_att) - min(home_fg_att)),
    away_fg_att_norm = (away_fg_att - min(away_fg_att)) / (max(away_fg_att) - min(away_fg_att)),
    home_pat_prob_norm = (home_pat_prob - min(home_pat_prob)) / (max(home_pat_prob) - min(home_pat_prob)),
    away_pat_prob_norm = (away_pat_prob - min(away_pat_prob)) / (max(away_pat_prob) - min(away_pat_prob)),
    home_pat_att_norm = (home_pat_att - min(home_pat_att)) / (max(home_pat_att) - min(home_pat_att)),
    away_pat_att_norm = (away_pat_att - min(away_pat_att)) / (max(away_pat_att) - min(away_pat_att)),
    
    # kicking ability rank
    home_fg_prob_rk_norm = (home_fg_prob_rk - min(home_fg_prob_rk)) / (max(home_fg_prob_rk) - min(home_fg_prob_rk)),
    away_fg_prob_rk_norm = (away_fg_prob_rk - min(away_fg_prob_rk)) / (max(away_fg_prob_rk) - min(away_fg_prob_rk)),
    home_fg_att_rk_norm = (home_fg_att_rk - min(home_fg_att_rk)) / (max(home_fg_att_rk) - min(home_fg_att_rk)),
    away_fg_att_rk_norm = (away_fg_att_rk - min(away_fg_att_rk)) / (max(away_fg_att_rk) - min(away_fg_att_rk)),
    home_pat_prob_rk_norm = (home_pat_prob_rk - min(home_pat_prob_rk)) / (max(home_pat_prob_rk) - min(home_pat_prob_rk)),
    away_pat_prob_rk_norm = (away_pat_prob_rk - min(away_pat_prob_rk)) / (max(away_pat_prob_rk) - min(away_pat_prob_rk)),
    home_pat_att_rk_norm = (home_pat_att_rk - min(home_pat_att_rk)) / (max(home_pat_att_rk) - min(home_pat_att_rk)),
    away_pat_att_rk_norm = (away_pat_att_rk - min(away_pat_att_rk)) / (max(away_pat_att_rk) - min(away_pat_att_rk)),
    
    # scoring ##########################
    # team iq's
    home_teamiq_raw = (0.4 * home_score_diff_norm + 0.15 * home_epa_norm + 0.1 * epa_diff_norm
                       + 0.15 * home_wpa_norm + 0.1 * wpa_diff_norm + 0.05 * home_avg_yds2go_norm + 0.05 * avg_yds2go_diff_norm
                       + 0.05 * home_avg_ydsgained_norm + 0.05 * avg_ydsgained_diff_norm) * 100,
    
    home_teamiq_rawrk = (1 - abs(0.4 * home_score_diff_rk_norm + 0.15 * home_epa_rk_norm + 0.1 * epa_diff_rk_norm
                                 + 0.15 * home_wpa_rk_norm + 0.1 * wpa_diff_rk_norm + 0.05 * home_avg_yds2go_rk_norm + 0.05 * avg_yds2go_diff_rk_norm
                                 + 0.05 * home_avg_ydsgained_rk_norm + 0.05 * avg_ydsgained_diff_rk_norm)) * 100,
    
    home_teamiq = (home_teamiq_raw + home_teamiq_rawrk) / 2,
    
    away_teamiq_raw = (0.4 * away_score_diff_norm + 0.15 * away_epa_norm + 0.1 * epa_diff_norm
                       + 0.15 * away_wpa_norm + 0.1 * wpa_diff_norm + 0.05 * away_avg_yds2go_norm + 0.05 * avg_yds2go_diff_norm
                       + 0.05 * away_avg_ydsgained_norm + 0.05 * avg_ydsgained_diff_norm) * 100,
    
    away_teamiq_rawrk = (1 - abs(0.4 * away_score_diff_rk_norm + 0.15 * away_epa_rk_norm + 0.1 * epa_diff_rk_norm
                                 + 0.15 * away_wpa_rk_norm + 0.1 * wpa_diff_rk_norm + 0.05 * away_avg_yds2go_rk_norm + 0.05 * avg_yds2go_diff_rk_norm
                                 + 0.05 * away_avg_ydsgained_rk_norm + 0.05 * avg_ydsgained_diff_rk_norm)) * 100,
    
    away_teamiq = (away_teamiq_raw + away_teamiq_rawrk) / 2,
    
    # team passing abilities
    home_passing_raw = (0.1 * home_total_air_yards_norm + 0.1 * home_avg_air_yards_norm + 0.1 * home_total_pass_yards_norm
                        + 0.1 * home_avg_pass_yards_norm + 0.2 * home_td_prob_norm + 0.3 * home_passing_touchdowns_norm
                        - 0.15 * home_interceptions_norm + home_pass_att_norm - 0.05 * home_inc_passes_norm + 0.1 * home_complete_pass_norm
                        + 0.2 * home_pass_percentage_norm) * 100,
    
    home_passing_rawrk = (1 - abs(0.1 * home_total_air_yards_rk_norm + 0.1 * home_avg_air_yards_rk_norm + 0.1 * home_total_pass_yards_rk_norm
                                  + 0.1 * home_avg_pass_yards_rk_norm + 0.2 * home_td_prob_rk_norm + 0.3 * home_passing_touchdowns_rk_norm
                                  - 0.15 * home_interceptions_rk_norm + home_pass_att_rk_norm - 0.05 * home_inc_passes_rk_norm + 0.1 * home_complete_pass_rk_norm
                                  + 0.2 * home_pass_percentage_rk_norm)) * 100,
    
    home_passing = (home_passing_raw + home_passing_rawrk) / 2,
    
    away_passing_raw = (0.1 * away_total_air_yards_norm + 0.1 * away_avg_air_yards_norm + 0.1 * away_total_pass_yards_norm
                        + 0.1 * away_avg_pass_yards_norm + 0.2 * away_td_prob_norm + 0.3 * away_passing_touchdowns_norm
                        - 0.15 * away_interceptions_norm + away_pass_att_norm - 0.05 * away_inc_passes_norm + 0.1 * away_complete_pass_norm
                        + 0.2 * away_pass_percentage_norm) * 100,
    
    away_passing_rawrk = (1 - abs(0.1 * away_total_air_yards_rk_norm + 0.1 * away_avg_air_yards_rk_norm + 0.1 * away_total_pass_yards_rk_norm
                                  + 0.1 * away_avg_pass_yards_rk_norm + 0.2 * away_td_prob_rk_norm + 0.3 * away_passing_touchdowns_rk_norm
                                  - 0.15 * away_interceptions_rk_norm + away_pass_att_rk_norm - 0.05 * away_inc_passes_rk_norm + 0.1 * away_complete_pass_rk_norm
                                  + 0.2 * away_pass_percentage_rk_norm)) * 100,
    
    away_passing = (away_passing_raw + away_passing_rawrk) / 2,
    
    # team rushing abilities
    home_rushing_raw = (0.1 * home_touches_norm + 0.1 * home_avg_touches_norm + 0.25 * home_rush_yds_norm
                        + 0.3 * home_avg_rush_yds_norm + 0.45 * home_rush_tds_norm - 0.2 * home_fumbles_norm) * 100,
    
    home_rushing_rawrk = (1 - abs(0.1 * home_touches_rk_norm + 0.1 * home_avg_touches_rk_norm + 0.25 * home_rush_yds_rk_norm
                                  + 0.3 * home_avg_rush_yds_rk_norm + 0.45 * home_rush_tds_rk_norm - 0.2 * home_fumbles_rk_norm)) * 100,
    
    home_rushing = (home_rushing_raw + home_rushing_rawrk) / 2,
    
    away_rushing_raw = (0.1 * away_touches_norm + 0.1 * away_avg_touches_norm + 0.25 * away_rush_yds_norm
                        + 0.3 * away_avg_rush_yds_norm + 0.45 * away_rush_tds_norm - 0.2 * away_fumbles_norm) * 100,
    
    away_rushing_rawrk = (1 - abs(0.1 * away_touches_rk_norm + 0.1 * away_avg_touches_rk_norm + 0.25 * away_rush_yds_rk_norm
                                  + 0.3 * away_avg_rush_yds_rk_norm + 0.45 * away_rush_tds_rk_norm - 0.2 * away_fumbles_rk_norm)) * 100,
    
    away_rushing = (away_rushing_raw + away_rushing_rawrk) / 2,
    
    # team receiving abilities
    home_receiving_raw = (0.1 * home_targets_norm + 0.1 * home_receptions_norm + 0.15 * home_rec_yds_norm
                          + 0.25 * home_avg_rec_yds_norm + 0.4 * home_rec_tds_norm) * 100,
    
    home_receiving_rawrk = (1 - abs(0.1 * home_targets_rk_norm + 0.1 * home_receptions_rk_norm + 0.15 * home_rec_yds_rk_norm
                                    + 0.25 * home_avg_rec_yds_rk_norm + 0.4 * home_rec_tds_rk_norm)) * 100,
    
    home_receiving = (home_receiving_raw + home_receiving_rawrk) / 2,
    
    away_receiving_raw = (0.1 * away_targets_norm + 0.1 * away_receptions_norm + 0.15 * away_rec_yds_norm
                          + 0.25 * away_avg_rec_yds_norm + 0.4 * away_rec_tds_norm) * 100,
    
    away_receiving_rawrk = (1 - abs(0.1 * away_targets_rk_norm + 0.1 * away_receptions_rk_norm + 0.15 * away_rec_yds_rk_norm
                                    + 0.25 * away_avg_rec_yds_rk_norm + 0.4 * away_rec_tds_rk_norm)) * 100,
    
    away_receiving = (away_receiving_raw + away_receiving_rawrk) / 2,
    
    # team defensive abilities
    home_defense_raw = (0.2 * home_def_int_norm + 0.2 * home_sacks_norm + 0.15 * home_force_fumb_norm
                        + 0.15 * home_safeties_norm + 0.1 * home_tfls_norm + 0.1 * home_qb_hits_norm + 0.1 * home_tackles_norm) * 100,
    
    home_defense_rawrk = (1 - abs(0.2 * home_def_int_rk_norm + 0.2 * home_sacks_rk_norm + 0.15 * home_force_fumb_rk_norm
                                  + 0.15 * home_safeties_rk_norm + 0.1 * home_tfls_rk_norm + 0.1 * home_qb_hits_rk_norm + 0.1 * home_tackles_rk_norm)) * 100,
    
    home_defense = (home_defense_raw + home_defense_rawrk) / 2,
    
    away_defense_raw = (0.2 * away_def_int_norm + 0.2 * away_sacks_norm + 0.15 * away_force_fumb_norm
                        + 0.15 * away_safeties_norm + 0.1 * away_tfls_norm + 0.1 * away_qb_hits_norm + 0.1 * away_tackles_norm) * 100,
    
    away_defense_rawrk = (1 - abs(0.2 * away_def_int_rk_norm + 0.2 * away_sacks_rk_norm + 0.15 * away_force_fumb_rk_norm
                                  + 0.15 * away_safeties_rk_norm + 0.1 * away_tfls_rk_norm + 0.1 * away_qb_hits_rk_norm + 0.1 * away_tackles_rk_norm)) * 100,
    
    away_defense = (away_defense_raw + away_defense_rawrk) / 2,
    
    # team kicking abilities
    home_kicking_raw = (0.35 * home_fg_prob_norm + 0.45 * home_fg_att_norm + 0.1 * home_pat_prob_norm
                        + 0.1 * home_pat_att_norm) * 100,
    
    home_kicking_rawrk = (1 - abs(0.35 * home_fg_prob_rk_norm + 0.45 * home_fg_att_rk_norm + 0.1 * home_pat_prob_rk_norm
                                  + 0.1 * home_pat_att_rk_norm)) * 100,
    
    home_kicking = (home_kicking_raw + home_kicking_rawrk) / 2,
    
    away_kicking_raw = (0.35 * away_fg_prob_norm + 0.45 * away_fg_att_norm + 0.1 * away_pat_prob_norm
                        + 0.1 * away_pat_att_norm) * 100,
    
    away_kicking_rawrk = (1 - abs(0.35 * away_fg_prob_rk_norm + 0.45 * away_fg_att_rk_norm + 0.1 * away_pat_prob_rk_norm
                                  + 0.1 * away_pat_att_rk_norm)) * 100,
    
    away_kicking = (away_kicking_raw + away_kicking_rawrk) / 2,
    
    # team overalls
    home_offense = 0.45 * home_teamiq + 0.35 * home_passing + 0.1 * home_rushing + 0.1 * home_receiving,
    away_offense = 0.45 * away_teamiq + 0.35 * away_passing + 0.1 * away_rushing + 0.1 * away_receiving,
    
    home_team_grade = 0.65 * home_offense + 0.35 * home_defense,
    away_team_grade = 0.65 * away_offense + 0.35 * away_defense,
    
    avg_home_grade = mean(home_team_grade, na.rm = TRUE),
    avg_away_grade = mean(away_team_grade, na.rm = TRUE),
    
    home_team_difference = home_team_grade - away_team_grade
  ) %>%
  ungroup() %>%
  drop_na()

nfl_preds <- nfl_24 %>%
  dplyr::select(
    game_id, week, season, home_team, away_team, home_score, away_score, home_team_win, spread, spread_result, total_line, total_result, spread_win,
    total_win, home_team_grade, away_team_grade, home_offense, away_offense, home_defense, away_defense, avg_home_grade, avg_away_grade,
  ) %>%
  mutate(
    ###
    home_team_grade = lag((home_team_grade)^(2/3)),
    away_team_grade = lag((away_team_grade)^(2/3)),
    home_offense = lag((home_offense)^(2/3)),
    away_offense = lag((away_offense)^(2/3)),
    home_defense = lag((home_defense)^(2/3)),
    away_defense = lag((away_defense)^(2/3)),
    avg_home_grade = lag((avg_home_grade)^(2/3)),
    avg_away_grade = lag((avg_away_grade)^(2/3)),
    
    #### 
    home_offense_away_defense_interaction = home_offense * away_defense,
    home_defense_away_offense_interaction = home_defense * away_offense, 
    home_grade_away_grade_interaction = home_team_grade * away_team_grade,
    
    ####
    home_team_grade_scaled = scale(home_team_grade),
    away_team_grade_scaled = scale(away_team_grade),
    
    ###
    home_team_grade_diff = home_team_grade - lag(home_team_grade),
    away_team_grade_diff = away_team_grade - lag(away_team_grade),
  )

nfl_points_pred <- nfl_teams %>%
  dplyr::select(
    game_id, week, season, home_team, away_team, home_score, away_score,
    home_team_grade, away_team_grade, home_offense, away_offense, avg_home_grade, avg_away_grade
  ) %>%
  group_by(season, week, home_team, away_team) %>%
  reframe(
    home_team = first(home_team),
    away_team = first(away_team),
    home_score = (home_score)^(2/3),
    away_score = (away_score)^(2/3),
    home_team_grade = (home_team_grade)^(2/3),
    away_team_grade = (away_team_grade)^(2/3),
    home_offense = (home_offense)^(2/3),
    away_offense = (away_offense)^(2/3),
    avg_home_grade = (avg_home_grade)^(2/3),
    avg_away_grade = (avg_away_grade)^(2/3),
    .groups = "drop"
  )

###############################

####### predictions #########

# team data function
get_nfl_team_data <- function(df, team_input, season = c(2023, 2024), 
                              columns = c("home_team_grade", "away_team_grade", "avg_home_grade", "avg_away_grade", "home_offense",
                                          "away_offense", "home_defense", "away_defense", "home_offense_away_defense_interaction",
                                          "home_grade_away_grade_interaction", "home_team_grade_diff", "away_team_grade_diff")) {
  
  team_data <- df %>%
    filter(season == season & (home_team == team_input | away_team == team_input))
  
  if (nrow(team_data) == 0) {
    warning("No data available for the specified team and season. Returning NA for all columns.")
    return(setNames(rep(NA, length(columns)), columns))
  }
  
  team_data <- as_tibble(team_data)
  
  team_values <- team_data %>%
    arrange(desc(season), desc(week)) %>%
    dplyr::slice(1:2) %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    as.matrix()
  
  team_values <- team_values[1, ]
  
  return(team_values)
}

# prediction functions
predict_nfl_winner <- function(df, home_team, away_team, season = c(2023, 2024), predictor, calibrate = TRUE) {
  home_values <- get_nfl_team_data(df, home_team, season)
  away_values <- get_nfl_team_data(df, away_team, season)
  
  game_data <- data.frame(
    home_team_grade = home_values["home_team_grade"],
    away_team_grade = away_values["away_team_grade"], 
    avg_home_grade = home_values["avg_home_grade"], 
    avg_away_grade = away_values["avg_away_grade"],
    home_offense = home_values["home_offense"],
    away_offense = away_values["away_offense"],
    home_defense = home_values["home_defense"],
    away_defense = away_values["away_defense"],
    home_offense_away_defense_interaction = home_values["home_offense_away_defense_interaction"],
    home_grade_away_grade_interaction = home_values["home_grade_away_grade_interaction"],
    home_team_grade_diff = home_values["home_team_grade_diff"], 
    away_team_grade_diff = away_values["away_team_grade_diff"]
  )
  
  logistic_home_pred <- predict(logistic_model_home, newdata = game_data, type = "response")
  nb_home_pred <- predict(naive_bayes_model_home, newdata = game_data, type = "raw")[, 2]
  svm_home_pred <- attr(predict(svm_model_home, newdata = game_data, probability = TRUE), "probabilities")[, 2]
  mean_probability = (logistic_home_pred + nb_home_pred + svm_home_pred) / 3

  combined_preds <- data.frame(logistic = logistic_home_pred, naive_bayes = nb_home_pred, svm = svm_home_pred, mean_prob = mean_probability)
  
  final_prob_xgb <- predict(predictor, newdata = as.matrix(combined_preds))
  
  if (calibrate) {
    ensemble_proba <- predict(platt_model, newdata = data.frame(final_prob_xgb = final_prob_xgb), type = "response")
  } else {
    ensemble_proba <- final_prob_xgb
  }
  
  winner_flag <- ensemble_proba > 0.5
  winner <- ifelse(winner_flag, home_team, away_team)
  loser <- ifelse(winner_flag, away_team, home_team)
  win_probability <- ifelse(winner_flag, ensemble_proba, 1 - ensemble_proba)
  
  return(list(winner = winner, loser = loser, win_probability = win_probability))
}

predict_spread_winner <- function(df, home_team, away_team, season = c(2023, 2024), predictor, calibrate = TRUE) {
  home_values <- get_nfl_team_data(df, home_team, season)
  away_values <- get_nfl_team_data(df, away_team, season)
  
  game_data <- data.frame(
    home_team_grade = home_values["home_team_grade"],
    away_team_grade = away_values["away_team_grade"], 
    avg_home_grade = home_values["avg_home_grade"], 
    avg_away_grade = away_values["avg_away_grade"],
    home_offense = home_values["home_offense"],
    away_offense = away_values["away_offense"],
    home_defense = home_values["home_defense"],
    away_defense = away_values["away_defense"],
    home_offense_away_defense_interaction = home_values["home_offense_away_defense_interaction"],
    home_grade_away_grade_interaction = home_values["home_grade_away_grade_interaction"],
    home_team_grade_diff = home_values["home_team_grade_diff"], 
    away_team_grade_diff = away_values["away_team_grade_diff"]
  )
  
  logistic_ats_pred <- predict(logistic_model_ats, newdata = game_data, type = "response")
  nb_ats_pred <- predict(naive_bayes_model_ats, newdata = game_data, type = "raw")[, 2]
  svm_ats_pred <- attr(predict(svm_model_ats, newdata = game_data, probability = TRUE), "probabilities")[, 2]
  ats_prob = (logistic_ats_pred + nb_ats_pred + svm_ats_pred) / 3

  combined_preds <- data.frame(logistic_ats = logistic_ats_pred, naive_bayes_ats = nb_ats_pred, svm_ats = svm_ats_pred, mean_prob = ats_prob)
  final_prob_xgb <- predict(predictor, newdata = as.matrix(combined_preds))
  
  if (calibrate) {
    ensemble_proba <- predict(platt_model_ats, newdata = data.frame(final_prob_xgb = final_prob_xgb), type = "response")
  } else {
    ensemble_proba <- final_prob_xgb
  }
  
  winner_flag <- ensemble_proba > 0.5
  winner <- ifelse(winner_flag, home_team, away_team)
  loser <- ifelse(winner_flag, away_team, home_team)
  win_probability <- ifelse(winner_flag, ensemble_proba, 1 - ensemble_proba)
  
  return(list(winner = winner, loser = loser, win_probability = win_probability))
}

predict_ts_winner <- function(df, home_team, away_team, season = c(2023, 2024), predictor, calibrate = TRUE) {
  home_values <- get_nfl_team_data(df, home_team, season)
  away_values <- get_nfl_team_data(df, away_team, season)
  
  game_data <- data.frame(
    home_team_grade = home_values["home_team_grade"],
    away_team_grade = away_values["away_team_grade"], 
    avg_home_grade = home_values["avg_home_grade"], 
    avg_away_grade = away_values["avg_away_grade"],
    home_offense = home_values["home_offense"],
    away_offense = away_values["away_offense"],
    home_defense = home_values["home_defense"],
    away_defense = away_values["away_defense"],
    home_offense_away_defense_interaction = home_values["home_offense_away_defense_interaction"],
    home_grade_away_grade_interaction = home_values["home_grade_away_grade_interaction"],
    home_team_grade_diff = home_values["home_team_grade_diff"], 
    away_team_grade_diff = away_values["away_team_grade_diff"]
  )
  
  logistic_ts_pred <- predict(logistic_model_ts, newdata = game_data, type = "response")
  nb_ts_pred <- predict(naive_bayes_model_ts, newdata = game_data, type = "raw")[, 2]
  svm_ts_pred <- attr(predict(svm_model_ts, newdata = game_data, probability = TRUE), "probabilities")[, 2]
  interaction = logistic_ts_pred * nb_ts_pred * svm_ts_pred
  
  combined_preds <- data.frame(logistic_ts = logistic_ts_pred, naive_bayes_ts = nb_ts_pred, svm_ts = svm_ts_pred, prob_int = interaction)
  final_prob_xgb <- predict(predictor, newdata = as.matrix(combined_preds))
  
  if (calibrate) {
    ensemble_proba <- predict(platt_model_ts, newdata = data.frame(final_prob_xgb = final_prob_xgb), type = "response")
  } else {
    ensemble_proba <- final_prob_xgb
  }
  
  winner_flag <- ensemble_proba > 0.5
  winner <- ifelse(winner_flag, "OVER", "UNDER")
  loser <- ifelse(winner_flag, "UNDER", "OVER")
  win_probability <- ifelse(winner_flag, ensemble_proba, 1 - ensemble_proba)
  
  return(list(winner = winner, loser = loser, win_probability = win_probability))
}

get_nfl_team_data_points <- function(df, team_input, season = c(2023, 2024), columns = c("home_team_grade", "away_team_grade", "home_offense", "away_offense", "avg_home_grade", "avg_away_grade")) {
  team_data <- df %>%
    filter(season == season & (home_team == team_input | away_team == team_input))
  
  if (nrow(team_data) == 0) {
    warning("No data available for the specified team and season. Returning NA for all columns.")
    return(setNames(rep(NA, length(columns)), columns))
  }
  
  team_values <- team_data %>%
    arrange(desc(season), desc(week)) %>%
    dplyr::slice(1:2) %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    as.matrix()
  
  team_values <- team_values[1, ]
  
  return(team_values)
}

# score projections
predict_nfl_scores <- function(df, home_team, away_team, season = c(2023, 2024), home_model, away_model) {
  home_team_data <- get_nfl_team_data_points(df, home_team, season)
  away_team_data <- get_nfl_team_data_points(df, away_team, season)
  
  home_data <- as.matrix(t(home_team_data))
  away_data <- as.matrix(t(away_team_data))
  
  colnames(home_data) <- names(home_team_data)
  colnames(away_data) <- names(away_team_data)
  
  home_score <- predict(home_model, newdata = home_data)
  away_score <- predict(away_model, newdata = away_data)
  
  return(list(home_score = home_score, away_score = away_score))
}

# user input for home and away teams
home_team_input <- "NYJ"
away_team_input <- "NE"

# home and away team data selection
home_team_data <- get_nfl_team_data(nfl_preds, home_team_input)
away_team_data <- get_nfl_team_data(nfl_preds, away_team_input)

# Predictings
winner_info <- predict_nfl_winner(nfl_preds, home_team_input, away_team_input, season = c(2023, 2024), predictor = xgb_model)
winner_spread <- predict_spread_winner(nfl_preds, home_team_input, away_team_input, season = c(2023, 2024), predictor = xgb_model_ats)
winner_ts <- predict_ts_winner(nfl_preds, home_team_input, away_team_input, season = c(2023, 2024), predictor = xgb_model_ts)
predicted_scores <- predict_nfl_scores(nfl_points_pred, home_team_input, away_team_input, season = c(2023, 2024), xgb_home_model, xgb_away_model)

# Reverse the transformation
original_home_score <- as.numeric(predicted_scores$home_score)^(3/2)
original_away_score <- as.numeric(predicted_scores$away_score)^(3/2)

# results
cat(sprintf("ProphetML predicts %s will win with a %.2f%% probability\n", winner_info$winner, winner_info$win_probability * 100))
cat(sprintf("%s will cover the spread with a %.2f%% probability\n", winner_spread$winner, winner_spread$win_probability * 100))
cat(sprintf("%s versus %s will go %s the total score with a %.2f%% probability\n", home_team_input, away_team_input, winner_ts$winner, winner_ts$win_probability * 100))
cat(sprintf("%s will score %.0f and %s will score %.0f\n", home_team_input, original_home_score, away_team_input, original_away_score))



