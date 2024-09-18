##### college football ! #######

cfb_data <- cfbfastR::load_cfb_pbp(2021:2023)

cfb_teams <- cfb_data %>%
  filter(!is.na(game_id)) %>%
  group_by(game_id, week) %>%
  summarise(
    home_team = first(home),
    away_team = first(away),
    season = first(year),
    home_score = last(pos_team_score[pos_team == first(home_team)]),
    away_score = last(def_pos_team_score[def_pos_team == first(away_team)]),
    home_team_win = if_else(first(home_score) > first(away_score), 1, 0),
    away_team_win = if_else(first(away_score) > first(home_score), 1, 0),
    
    # iq
    home_score_diff = first(home_score) - first(away_score),
    away_score_diff = first(away_score) - first(home_score),
    home_ppa = mean(ppa[pos_team == first(home_team)], na.rm = TRUE),
    away_ppa = mean(ppa[pos_team == first(away_team)], na.rm = TRUE),
    home_epa = mean(EPA[pos_team == first(home_team)], na.rm = TRUE),
    away_epa = mean(EPA[pos_team == first(away_team)], na.rm = TRUE),
    epa_diff = home_epa - away_epa,
    home_wpa = mean(wpa[pos_team == first(home_team)], na.rm = TRUE),
    away_wpa = mean(wpa[pos_team == first(away_team)], na.rm = TRUE),
    wpa_diff = home_wpa - away_wpa,
    
    # passing ability
    home_total_pass_yards = sum(yds_receiving[pos_team == first(home_team)], na.rm = TRUE),
    away_total_pass_yards = sum(yds_receiving[pos_team == first(away_team)], na.rm = TRUE),
    home_avg_pass_yards = mean(yds_receiving[pos_team == first(home_team)], na.rm = TRUE),
    away_avg_pass_yards = mean(yds_receiving[pos_team == first(away_team)], na.rm = TRUE),
    home_passing_touchdowns = sum(pass_td[pos_team == first(home_team)], na.rm = TRUE),
    away_passing_touchdowns = sum(pass_td[pos_team == first(away_team)], na.rm = TRUE),
    home_interceptions = sum(int[pos_team == first(home_team)], na.rm = TRUE),
    away_interceptions = sum(int[pos_team == first(away_team)], na.rm = TRUE),
    home_pass_att = sum(pass_attempt[pos_team == first(home_team)], na.rm = TRUE),
    away_pass_att = sum(pass_attempt[pos_team == first(away_team)], na.rm = TRUE),
    home_complete_pass = sum(completion[pos_team == first(home_team)], na.rm = TRUE),
    away_complete_pass = sum(completion[pos_team == first(away_team)], na.rm = TRUE),
    home_inc_passes = home_pass_att - home_complete_pass,
    away_inc_passes = away_pass_att - away_complete_pass,
    home_pass_percentage = home_complete_pass / home_pass_att,
    away_pass_percentage = away_complete_pass / away_pass_att,
    
    # rushing stats
    home_rush_yds = sum(yds_rushed[pos_team == first(home_team)], na.rm = TRUE),
    away_rush_yds = sum(yds_rushed[pos_team == first(away_team)], na.rm = TRUE),
    home_avg_rush_yds = mean(yds_rushed[pos_team == first(home_team)], na.rm = TRUE),
    away_avg_rush_yds = mean(yds_rushed[pos_team == first(away_team)], na.rm = TRUE),
    home_touches = sum(rush[pos_team == first(home_team)], na.rm = TRUE),
    away_touches = sum(rush[pos_team == first(away_team)], na.rm = TRUE),
    home_avg_touches = mean(rush[pos_team == first(home_team)], na.rm = TRUE),
    away_avg_touches = mean(rush[pos_team == first(away_team)], na.rm = TRUE),
    home_rush_tds = sum(rush_td[pos_team == first(home_team)], na.rm = TRUE),
    away_rush_tds = sum(rush_td[pos_team == first(away_team)], na.rm = TRUE),
    home_fumbles = sum(fumble_stat[pos_team == first(home_team)], na.rm = TRUE),
    away_fumbles = sum(fumble_stat[pos_team == first(away_team)], na.rm = TRUE),
    
    # receiving stats
    home_rec_yds = sum(yds_receiving[pos_team == first(home_team)], na.rm = TRUE),
    away_rec_yds = sum(yds_receiving[pos_team == first(away_team)], na.rm = TRUE),
    home_avg_rec_yds = mean(yds_receiving[pos_team == first(home_team)], na.rm = TRUE),
    away_avg_rec_yds = mean(yds_receiving[pos_team == first(away_team)], na.rm = TRUE),
    home_targets = sum(target[pos_team == first(home_team)], na.rm = TRUE),
    away_targets = sum(target[pos_team == first(away_team)], na.rm = TRUE),
    home_receptions = sum(completion[pos_team == first(home_team)], na.rm = TRUE),
    away_receptions = sum(completion[pos_team == first(away_team)], na.rm = TRUE),
    home_reception_rate = home_receptions / home_targets,
    away_reception_rate = away_receptions / away_targets,
    home_rec_tds = sum(pass_td[pos_team == first(home_team)], na.rm = TRUE),
    away_rec_tds = sum(pass_td[pos_team == first(away_team)], na.rm = TRUE),
    
    # kicking ability 
    home_fg_prob = mean(fg_make_prob[pos_team == first(home_team)], na.rm = TRUE),
    away_fg_prob = mean(fg_make_prob[pos_team == first(away_team)], na.rm = TRUE),
    home_total_fg_yards = sum(yds_fg[pos_team == first(home_team)], na.rm = TRUE),
    away_total_fg_yards = sum(yds_fg[pos_team == first(away_team)], na.rm = TRUE),
    home_avg_fg_yards = mean(yds_fg[pos_team == first(home_team)], na.rm = TRUE),
    away_avg_fg_yards = mean(yds_fg[pos_team == first(away_team)], na.rm = TRUE),
    
    # defensive ability
    home_def_int = sum(int[def_pos_team == first(home_team)], na.rm = TRUE),
    away_def_int = sum(int[def_pos_team == first(away_team)], na.rm = TRUE),
    home_def_p6 = sum(int_td[def_pos_team == first(home_team)], na.rm = TRUE),
    away_def_p6 = sum(int_td[def_pos_team == first(away_team)], na.rm = TRUE),
    home_sacks = sum(sack[def_pos_team == first(home_team)], na.rm = TRUE),
    away_sacks = sum(sack[def_pos_team == first(away_team)], na.rm = TRUE),
    home_force_fumb = sum(fumble_forced_stat[def_pos_team == first(home_team)], na.rm = TRUE),
    away_force_fumb = sum(fumble_forced_stat[def_pos_team == first(away_team)], na.rm = TRUE),
    home_safeties = coalesce(sum(safety[def_pos_team == first(home_team)], na.rm = TRUE), 0),
    away_safeties = coalesce(sum(safety[def_pos_team == first(away_team)], na.rm = TRUE), 0),
    .groups = "drop"  # Drop all grouping after summarization
  )  %>%
  drop_na() %>%
  dplyr::mutate(
    # iq
    home_score_diff_rk = dplyr::dense_rank(dplyr::desc(home_score_diff)),
    away_score_diff_rk = dplyr::dense_rank(dplyr::desc(away_score_diff)),
    home_ppa_rk = dplyr::dense_rank(dplyr::desc(home_ppa)),
    away_ppa_rk = dplyr::dense_rank(dplyr::desc(away_ppa)),
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
    
    home_reception_rate_rk = dplyr::dense_rank(dplyr::desc(home_reception_rate)),
    away_reception_rate_rk = dplyr::dense_rank(dplyr::desc(away_reception_rate)),
    
    home_rec_tds_rk = dplyr::dense_rank(dplyr::desc(home_rec_tds)),
    away_rec_tds_rk = dplyr::dense_rank(dplyr::desc(away_rec_tds)),
    
    # kicking ability 
    home_fg_prob_rk = dplyr::dense_rank(dplyr::desc(home_fg_prob)),
    away_fg_prob_rk = dplyr::dense_rank(dplyr::desc(away_fg_prob)),
    
    home_total_fg_yards_rk = dplyr::dense_rank(dplyr::desc(home_total_fg_yards)),
    away_total_fg_yards_rk = dplyr::dense_rank(dplyr::desc(away_total_fg_yards)),
    
    home_avg_fg_yards_rk = dplyr::dense_rank(dplyr::desc(home_avg_fg_yards)),
    away_avg_fg_yards_rk = dplyr::dense_rank(dplyr::desc(away_avg_fg_yards)),
    
    # defensive ability
    home_def_int_rk = dplyr::dense_rank(dplyr::desc(home_def_int)),
    away_def_int_rk = dplyr::dense_rank(dplyr::desc(away_def_int)),
    
    home_def_p6_rk = dplyr::dense_rank(dplyr::desc(home_def_p6)),
    away_def_p6_rk = dplyr::dense_rank(dplyr::desc(away_def_p6)),
    
    home_sacks_rk = dplyr::dense_rank(dplyr::desc(home_sacks)),
    away_sacks_rk = dplyr::dense_rank(dplyr::desc(away_sacks)),
    
    home_force_fumb_rk = dplyr::dense_rank(dplyr::desc(home_force_fumb)),
    away_force_fumb_rk = dplyr::dense_rank(dplyr::desc(away_force_fumb)),
    
    home_safeties_rk = dplyr::dense_rank(dplyr::desc(home_safeties)),
    away_safeties_rk = dplyr::dense_rank(dplyr::desc(away_safeties)),
  ) %>%
  group_by(season) %>%
  dplyr::mutate(
    # normalizing metrics and grading players
    # iq
    home_score_diff_norm = (home_score_diff - min(home_score_diff)) / (max(home_score_diff) - min(home_score_diff)),
    away_score_diff_norm = (away_score_diff - min(away_score_diff)) / (max(away_score_diff) - min(away_score_diff)),
    home_ppa_norm = (home_ppa - min(home_ppa)) / (max(home_ppa) - min(home_ppa)),
    away_ppa_norm = (away_ppa - min(away_ppa)) / (max(away_ppa) - min(away_ppa)),
    home_epa_norm = (home_epa - min(home_epa)) / (max(home_epa) - min(home_epa)),
    away_epa_norm = (away_epa - min(away_epa)) / (max(away_epa) - min(away_epa)),
    epa_diff_norm = (epa_diff - min(epa_diff)) / (max(epa_diff) - min(epa_diff)),
    home_wpa_norm = (home_wpa - min(home_wpa)) / (max(home_wpa) - min(home_wpa)),
    away_wpa_norm = (away_wpa - min(away_wpa)) / (max(away_wpa) - min(away_wpa)),
    wpa_diff_norm = (wpa_diff - min(wpa_diff)) / (max(wpa_diff) - min(wpa_diff)),
    
    home_score_diff_rk_norm = (home_score_diff_rk - min(home_score_diff_rk)) / (max(home_score_diff_rk) - min(home_score_diff_rk)),
    away_score_diff_rk_norm = (away_score_diff_rk - min(away_score_diff_rk)) / (max(away_score_diff_rk) - min(away_score_diff_rk)),
    home_ppa_rk_norm = (home_ppa_rk - min(home_ppa_rk)) / (max(home_ppa_rk) - min(home_ppa_rk)),
    away_ppa_rk_norm = (away_ppa_rk - min(away_ppa_rk)) / (max(away_ppa_rk) - min(away_ppa_rk)),
    home_epa_rk_norm = (home_epa_rk - min(home_epa_rk)) / (max(home_epa_rk) - min(home_epa_rk)),
    away_epa_rk_norm = (away_epa_rk - min(away_epa_rk)) / (max(away_epa_rk) - min(away_epa_rk)),
    epa_diff_rk_norm = (epa_diff_rk - min(epa_diff_rk)) / (max(epa_diff_rk) - min(epa_diff_rk)),
    home_wpa_rk_norm = (home_wpa_rk - min(home_wpa_rk)) / (max(home_wpa_rk) - min(home_wpa_rk)),
    away_wpa_rk_norm = (away_wpa_rk - min(away_wpa_rk)) / (max(away_wpa_rk) - min(away_wpa_rk)),
    wpa_diff_rk_norm = (wpa_diff_rk - min(wpa_diff_rk)) / (max(wpa_diff_rk) - min(wpa_diff_rk)),
    
    # passing ability
    home_total_pass_yards_norm = (home_total_pass_yards - min(home_total_pass_yards)) / (max(home_total_pass_yards) - min(home_total_pass_yards)),
    away_total_pass_yards_norm = (away_total_pass_yards - min(away_total_pass_yards)) / (max(away_total_pass_yards) - min(away_total_pass_yards)),
    home_avg_pass_yards_norm = (home_avg_pass_yards - min(home_avg_pass_yards)) / (max(home_avg_pass_yards) - min(home_avg_pass_yards)),
    away_avg_pass_yards_norm = (away_avg_pass_yards - min(away_avg_pass_yards)) / (max(away_avg_pass_yards) - min(away_avg_pass_yards)),
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
    
    # passing ability
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
    home_fumbles_norm = coalesce((home_fumbles - min(home_fumbles)) / (max(home_fumbles) - min(home_fumbles)), 0),
    away_fumbles_norm = coalesce((away_fumbles - min(away_fumbles)) / (max(away_fumbles) - min(away_fumbles)), 0),
    
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
    home_fumbles_rk_norm = coalesce((home_fumbles_rk - min(home_fumbles_rk)) / (max(home_fumbles_rk) - min(home_fumbles_rk)), 0),
    away_fumbles_rk_norm = coalesce((away_fumbles_rk - min(away_fumbles_rk)) / (max(away_fumbles_rk) - min(away_fumbles_rk)), 0),
    
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
    
    home_rec_yds_rk_norm = (home_rec_yds_rk - min(home_rec_yds_rk)) / (max(home_rec_yds_rk) - min(home_rec_yds_rk)),
    away_rec_yds_rk_norm = (away_rec_yds_rk - min(away_rec_yds_rk)) / (max(away_rec_yds_rk) - min(away_rec_yds_rk)),
    home_avg_rec_yds_rk_norm = (home_avg_rec_yds_rk - min(home_avg_rec_yds_rk)) / (max(home_avg_rec_yds_rk) - min(home_avg_rec_yds_rk)),
    away_avg_rec_yds_rk_norm = (away_avg_rec_yds_rk - min(away_avg_rec_yds_rk)) / (max(away_avg_rec_yds_rk) - min(away_avg_rec_yds_rk)),
    home_targets_rk_norm = (home_targets_rk - min(home_targets_rk)) / (max(home_targets_rk) - min(home_targets_rk)),
    away_targets_rk_norm = (away_targets_rk - min(away_targets_rk)) / (max(away_targets_rk) - min(away_targets_rk)),
    home_receptions_rk_norm = (home_receptions_rk - min(home_receptions_rk)) / (max(home_receptions_rk) - min(home_receptions_rk)),
    away_receptions_rk_norm = (away_receptions_rk - min(away_receptions_rk)) / (max(away_receptions_rk) - min(away_receptions_rk)),
    home_reception_rate_rk_norm = (home_reception_rate_rk - min(home_reception_rate_rk)) / (max(home_reception_rate_rk) - min(home_reception_rate_rk)),
    away_reception_rate_rk_norm = (away_reception_rate_rk - min(away_reception_rate_rk)) / (max(away_reception_rate_rk) - min(away_reception_rate_rk)),
    home_rec_tds_rk_norm = (home_rec_tds_rk - min(home_rec_tds_rk)) / (max(home_rec_tds_rk) - min(home_rec_tds_rk)),
    away_rec_tds_rk_norm = (away_rec_tds_rk - min(away_rec_tds_rk)) / (max(away_rec_tds_rk) - min(away_rec_tds_rk)),
    
    # defensive ability
    home_def_int_norm = (home_def_int - min(home_def_int)) / (max(home_def_int) - min(home_def_int)),
    away_def_int_norm = (away_def_int - min(away_def_int)) / (max(away_def_int) - min(away_def_int)),
    home_def_p6_norm = (home_def_p6 - min(home_def_p6)) / (max(home_def_p6) - min(home_def_p6)),
    away_def_p6_norm = (away_def_p6 - min(away_def_p6)) / (max(away_def_p6) - min(away_def_p6)),
    home_sacks_norm = (home_sacks - min(home_sacks)) / (max(home_sacks) - min(home_sacks)),
    away_sacks_norm = (away_sacks - min(away_sacks)) / (max(away_sacks) - min(away_sacks)),
    home_force_fumb_norm = coalesce((home_force_fumb - min(home_force_fumb)) / (max(home_force_fumb) - min(home_force_fumb)), 0),
    away_force_fumb_norm = coalesce((away_force_fumb - min(away_force_fumb)) / (max(away_force_fumb) - min(away_force_fumb)), 0),
    home_safeties_norm = coalesce((home_safeties - min(home_safeties)) / (max(home_safeties) - min(home_safeties)), 0),
    away_safeties_norm = coalesce((away_safeties - min(away_safeties)) / (max(away_safeties) - min(away_safeties)), 0),
    
    home_def_int_rk_norm = (home_def_int_rk - min(home_def_int_rk)) / (max(home_def_int_rk) - min(home_def_int_rk)),
    away_def_int_rk_norm = (away_def_int_rk - min(away_def_int_rk)) / (max(away_def_int_rk) - min(away_def_int_rk)),
    home_def_p6_rk_norm = (home_def_p6_rk - min(home_def_p6_rk)) / (max(home_def_p6_rk) - min(home_def_p6_rk)),
    away_def_p6_rk_norm = (away_def_p6_rk - min(away_def_p6_rk)) / (max(away_def_p6_rk) - min(away_def_p6_rk)),
    home_sacks_rk_norm = (home_sacks_rk - min(home_sacks_rk)) / (max(home_sacks_rk) - min(home_sacks_rk)),
    away_sacks_rk_norm = (away_sacks_rk - min(away_sacks_rk)) / (max(away_sacks_rk) - min(away_sacks_rk)),
    home_force_fumb_rk_norm = coalesce((home_force_fumb_rk - min(home_force_fumb_rk)) / (max(home_force_fumb_rk) - min(home_force_fumb_rk)), 0),
    away_force_fumb_rk_norm = coalesce((away_force_fumb_rk - min(away_force_fumb_rk)) / (max(away_force_fumb_rk) - min(away_force_fumb_rk)), 0),
    home_safeties_rk_norm = coalesce((home_safeties_rk - min(home_safeties_rk)) / (max(home_safeties_rk) - min(home_safeties_rk)), 0),
    away_safeties_rk_norm = coalesce((away_safeties_rk - min(away_safeties_rk)) / (max(away_safeties_rk) - min(away_safeties_rk)), 0),
    
    # kicking ability
    home_fg_prob_norm = (home_fg_prob - min(home_fg_prob)) / (max(home_fg_prob) - min(home_fg_prob)),
    away_fg_prob_norm = (away_fg_prob - min(away_fg_prob)) / (max(away_fg_prob) - min(away_fg_prob)),
    home_total_fg_yards_norm = (home_total_fg_yards - min(home_total_fg_yards)) / (max(home_total_fg_yards) - min(home_total_fg_yards)),
    away_total_fg_yards_norm = (away_total_fg_yards - min(away_total_fg_yards)) / (max(away_total_fg_yards) - min(away_total_fg_yards)),
    home_avg_fg_yards_norm = (home_avg_fg_yards - min(home_avg_fg_yards)) / (max(home_avg_fg_yards) - min(home_avg_fg_yards)),
    away_avg_fg_yards_norm = (away_avg_fg_yards - min(away_avg_fg_yards)) / (max(away_avg_fg_yards) - min(away_avg_fg_yards)),
    
    home_fg_prob_rk_norm = (home_fg_prob_rk - min(home_fg_prob_rk)) / (max(home_fg_prob_rk) - min(home_fg_prob_rk)),
    away_fg_prob_rk_norm = (away_fg_prob_rk - min(away_fg_prob_rk)) / (max(away_fg_prob_rk) - min(away_fg_prob_rk)),
    home_total_fg_yards_rk_norm = (home_total_fg_yards_rk - min(home_total_fg_yards_rk)) / (max(home_total_fg_yards_rk) - min(home_total_fg_yards_rk)),
    away_total_fg_yards_rk_norm = (away_total_fg_yards_rk - min(away_total_fg_yards_rk)) / (max(away_total_fg_yards_rk) - min(away_total_fg_yards_rk)),
    home_avg_fg_yards_rk_norm = (home_avg_fg_yards_rk - min(home_avg_fg_yards_rk)) / (max(home_avg_fg_yards_rk) - min(home_avg_fg_yards_rk)),
    away_avg_fg_yards_rk_norm = (away_avg_fg_yards_rk - min(away_avg_fg_yards_rk)) / (max(away_avg_fg_yards_rk) - min(away_avg_fg_yards_rk)),
    
    # scoring ##########################
    # team iq's
    home_teamiq_raw = (0.4 * home_score_diff_norm + 0.2 * home_epa_norm + 0.1 * epa_diff_norm
                   + 0.2 * home_wpa_norm + 0.1 * wpa_diff_norm + 0.1 * home_ppa_norm) * 100,
    
    home_teamiq_rawrk = (1 - abs(0.4 * home_score_diff_rk_norm + 0.2 * home_epa_rk_norm + 0.1 * epa_diff_rk_norm
                                 + 0.2 * home_wpa_rk_norm + 0.1 * wpa_diff_rk_norm + 0.1 * home_ppa_rk_norm)) * 100,
    
    away_teamiq_raw = (0.4 * away_score_diff_norm + 0.2 * away_epa_norm + 0.1 * epa_diff_norm
                   + 0.2 * away_wpa_norm + 0.1 * wpa_diff_norm + 0.1 * away_ppa_norm) * 100,
    
    away_teamiq_rawrk = (1 - abs(0.4 * away_score_diff_rk_norm + 0.2 * away_epa_rk_norm + 0.1 * epa_diff_rk_norm
                                 + 0.2 * away_wpa_rk_norm + 0.1 * wpa_diff_rk_norm + 0.1 * away_ppa_rk_norm)) * 100,
    
    home_teamiq = (home_teamiq_raw + home_teamiq_rawrk) / 2,
    away_teamiq = (away_teamiq_raw + away_teamiq_rawrk) / 2,
    
    # team passing abilities
    home_passing_raw = (0.2 * home_total_pass_yards_norm + 0.2 * home_avg_pass_yards_norm
                    + 0.4 * home_passing_touchdowns_norm - 0.15 * home_interceptions_norm + 0.05 * home_pass_att_norm
                    - 0.05 * home_inc_passes_norm + 0.1 * home_complete_pass_norm + 0.25 * home_pass_percentage_norm) * 100,
    
    home_passing_rawrk = (1 - abs(0.2 * home_total_pass_yards_rk_norm + 0.2 * home_avg_pass_yards_rk_norm
                          + 0.4 * home_passing_touchdowns_rk_norm - 0.15 * home_interceptions_rk_norm + 0.05 * home_pass_att_rk_norm
                          - 0.05 * home_inc_passes_norm + 0.1 * home_complete_pass_norm + 0.25 * home_pass_percentage_norm)) * 100,
    
    away_passing_raw = (0.2 * away_total_pass_yards_norm + 0.2 * away_avg_pass_yards_norm
                    + 0.4 * away_passing_touchdowns_norm - 0.15 * away_interceptions_norm + 0.05 * away_pass_att_norm
                    - 0.05 * away_inc_passes_norm + 0.1 * away_complete_pass_norm + 0.25 * away_pass_percentage_norm) * 100,
    
    away_passing_rawrk = (1 - abs(0.2 * away_total_pass_yards_rk_norm + 0.2 * away_avg_pass_yards_rk_norm
                          + 0.4 * away_passing_touchdowns_rk_norm - 0.15 * away_interceptions_rk_norm + 0.05 * away_pass_att_rk_norm
                          - 0.05 * away_inc_passes_norm + 0.1 * away_complete_pass_norm + 0.25 * away_pass_percentage_norm)) * 100,
    
    home_passing = (home_passing_raw + home_passing_rawrk) / 2,
    away_passing = (away_passing_raw + away_passing_rawrk) / 2,
    
    # team rushing abilities
    home_rushing_raw = (0.1 * home_touches_norm + 0.1 * home_avg_touches_norm + 0.25 * home_rush_yds_norm
                    + 0.3 * home_avg_rush_yds_norm + 0.45 * home_rush_tds_norm - 0.2 * home_fumbles_norm) * 100,
    
    home_rushing_rawrk = (1 - abs(0.1 * home_touches_rk_norm + 0.1 * home_avg_touches_rk_norm + 0.25 * home_rush_yds_rk_norm
                                  + 0.3 * home_avg_rush_yds_rk_norm + 0.45 * home_rush_tds_rk_norm - 0.2 * home_fumbles_rk_norm)) * 100,
    
    away_rushing_raw = (0.1 * away_touches_norm + 0.1 * away_avg_touches_norm + 0.25 * away_rush_yds_norm
                    + 0.3 * away_avg_rush_yds_norm + 0.45 * away_rush_tds_norm - 0.2 * away_fumbles_norm) * 100,
    
    away_rushing_rawrk = (1 - abs(0.1 * away_touches_rk_norm + 0.1 * away_avg_touches_rk_norm + 0.25 * away_rush_yds_rk_norm
                                  + 0.3 * away_avg_rush_yds_rk_norm + 0.45 * away_rush_tds_rk_norm - 0.2 * away_fumbles_rk_norm)) * 100,
    
    home_rushing = (home_rushing_raw + home_rushing_rawrk) / 2,
    away_rushing = (away_rushing_raw + away_rushing_rawrk) / 2,
    
    # team receiving abilities
    home_receiving_raw = (0.1 * home_targets_norm + 0.1 * home_receptions_norm + 0.15 * home_rec_yds_norm
                      + 0.25 * home_avg_rec_yds_norm + 0.4 * home_rec_tds_norm) * 100,
    
    away_receiving_raw = (0.1 * away_targets_norm + 0.1 * away_receptions_norm + 0.15 * away_rec_yds_norm
                      + 0.25 * away_avg_rec_yds_norm + 0.4 * away_rec_tds_norm) * 100,
    
    home_receiving_rawrk = (1 - abs(0.1 * home_targets_rk_norm + 0.1 * home_receptions_rk_norm + 0.15 * home_rec_yds_rk_norm
                                  + 0.25 * home_avg_rec_yds_rk_norm + 0.4 * home_rec_tds_rk_norm)) * 100,
    
    away_receiving_rawrk = (1 - abs(0.1 * away_targets_rk_norm + 0.1 * away_receptions_rk_norm + 0.15 * away_rec_yds_rk_norm
                                  + 0.25 * away_avg_rec_yds_rk_norm + 0.4 * away_rec_tds_rk_norm)) * 100,
    
    home_receiving = (home_receiving_raw + home_receiving_rawrk) / 2,
    away_receiving = (away_receiving_raw + away_receiving_rawrk) / 2,
    
    # team defensive abilities
    home_defense_raw = (0.2 * home_def_int_norm + 0.15 * home_force_fumb_norm + 0.3 * home_def_p6_norm
                    + 0.3 * home_sacks_norm + 0.15 * home_safeties_norm) * 100,
    
    home_defense_rawrk = (1 - abs(0.2 * home_def_int_rk_norm + 0.15 * home_force_fumb_rk_norm + 0.3 * home_def_p6_rk_norm
                                  + 0.3 * home_sacks_rk_norm + 0.15 * home_safeties_rk_norm)) * 100,
    
    away_defense_raw = (0.2 * away_def_int_norm + 0.15 * away_force_fumb_norm + 0.3 * away_def_p6_norm
                    + 0.3 * away_sacks_norm + 0.15 * away_safeties_norm) * 100,
    
    away_defense_rawrk = (1 - abs(0.2 * away_def_int_rk_norm + 0.15 * away_force_fumb_rk_norm + 0.3 * away_def_p6_rk_norm
                                  + 0.3 * away_sacks_rk_norm + 0.15 * away_safeties_rk_norm)) * 100,
    
    home_defense = (home_defense_raw + home_defense_rawrk) / 2,
    away_defense = (away_defense_raw + away_defense_rawrk) / 2,
    
    # team kicking abilities
    home_kicking_raw = (0.5 * home_fg_prob_norm + 0.2 * home_total_fg_yards_norm + 0.3 * home_avg_fg_yards_norm) * 100,
    home_kicking_rawrk = (1 - abs(0.5 * home_fg_prob_rk_norm + 0.2 * home_total_fg_yards_rk_norm + 0.3 * home_avg_fg_yards_rk_norm)) * 100,
    
    away_kicking_raw = (0.5 * away_fg_prob_norm + 0.2 * away_total_fg_yards_norm + 0.3 * away_avg_fg_yards_norm) * 100,
    away_kicking_rawrk = (1 - abs(0.5 * away_fg_prob_rk_norm + 0.2 * away_total_fg_yards_rk_norm + 0.3 * away_avg_fg_yards_rk_norm)) * 100,
    
    home_kicking = (home_kicking_raw + home_kicking_rawrk) / 2,
    away_kicking = (away_kicking_raw + away_kicking_rawrk) / 2,
    
    # team overalls
    home_offense = 0.45 * home_teamiq + 0.35 * home_passing + 0.1 * home_rushing + 0.1 * home_receiving,
    away_offense = 0.45 * away_teamiq + 0.35 * away_passing + 0.1 * away_rushing + 0.1 * away_receiving,
    
    home_team_grade = 0.55 * home_offense + 0.45 * home_defense,
    away_team_grade = 0.55 * away_offense + 0.45 * away_defense,
    
    avg_home_grade = mean(home_team_grade, na.rm = TRUE),
    avg_away_grade = mean(away_team_grade, na.rm = TRUE),
  ) %>%
  ungroup()

cfb_viable <- cfb_teams %>%
  dplyr::select(
    game_id, week, season, home_team, away_team, home_score, away_score, home_team_win, away_team_win,
    home_team_grade, away_team_grade, home_offense, away_offense, home_defense, away_defense, avg_home_grade, avg_away_grade
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
  ) %>%
  drop_na()

points_pred_cfb <- cfb_teams %>%
  dplyr::select(
    game_id, week, season, home_team, away_team, home_score, away_score,
    home_team_grade, away_team_grade, home_offense, away_offense, home_defense, away_defense, avg_home_grade, avg_away_grade, avg_difference
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
    home_defense = (home_defense)^(2/3),
    away_defense = (away_defense)^(2/3),
    avg_home_grade = (avg_home_grade)^(2/3),
    avg_away_grade = avg_away_grade^(2/3),
    avg_difference = (avg_difference)^(2/3),
    .groups = "drop"
  )















    