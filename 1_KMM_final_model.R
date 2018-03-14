# KMM 2018
# 8 MAR 2018
# Mike Abla
#
# Kag MM Cont where there are two stages
# 1) Predicted Prob for every possible matchup in past tourneys (2014-2017)
# 2) Predicted Prob for every possible matchup before 2018 tourney
#
# This is stage 2 submission (final)


# set env -----------------------------------------------------------------

cat("\n load packages...\n")

pckgs <- c('data.table', 'dplyr', 'caret', 'ggplot2', 'rpart', 'rpart.plot', 'stringr', 'reshape', 'randomForest', 
           'gbm', 'xgboost', 'ROSE', 'pROC', 'gridExtra')
new_pckgs <- pckgs[!(pckgs %in% installed.packages()[, "Package"])]
if (length(new_pckgs))
  install.packages(new_pckgs, dependencies = TRUE, quiet = TRUE)
sapply(pckgs, require, character.only = TRUE)


# load models to use for predictions and prediction file ------------------

load("C:/Users/AblMi001/Desktop/Kaggle March Madness II/top2_kmm_models.RData") #gbm and log model
SampleSubmissionStage2 <- read.csv("SampleSubmissions/SampleSubmissionStage2.csv")


# load data ---------------------------------------------------------------

# model data (2018 season)
regularseason_detailed <- read.csv("./DataFiles - Stage2/RegularSeasonDetailedResults.csv") 
conference_tourney <- read.csv("./DataFiles - Stage2/ConferenceTourneyGames.csv")

# references (2018 season)
team_IDs <- read.csv("./DataFiles/Teams.csv")
seedteam_IDs <- read.csv("DataFiles - Stage2/NCAATourneySeeds.csv")
TeamConferences <- read.csv("./DataFiles/TeamConferences.csv")

# build regular season data set -------------------------------------------

wdiff_stats <- cbind(regularseason_detailed[, c("Season", "DayNum", "WTeamID")], 
                     as.data.frame(as.matrix(regularseason_detailed[,c(4,9:21)]) - as.matrix(regularseason_detailed[,c(6,22:34)]))) %>%
  dplyr::rename(TeamID = WTeamID) %>%
  dplyr::rename_at(vars(contains("W")), funs(str_replace(.,"^W", "D")))

ldiff_stats <- cbind(regularseason_detailed[, c("Season", "DayNum", "LTeamID")], 
                     as.data.frame(as.matrix(regularseason_detailed[,c(6,22:34)]) - as.matrix(regularseason_detailed[,c(4,9:21)]))) %>%
  dplyr::rename(TeamID = LTeamID) %>%
  dplyr::rename_at(vars(contains("L")), funs(str_replace(.,"^L", "D")))

win_stats <- regularseason_detailed %>%
  dplyr::select(Season, DayNum, starts_with('W')) %>%
  dplyr::rename_at(vars(contains("W")), funs(str_replace(.,"^W", ""))) %>%
  dplyr::mutate(Wins = 1,
                Losses = 0,
                HomeAway_Index = as.numeric(ifelse(Loc == 'H', 0, 
                                                   ifelse(Loc == 'N', 1, 
                                                          ifelse(Loc == 'A', 2,""))))) %>%
  left_join(wdiff_stats, by = c("Season", "DayNum", "TeamID"))

loss_stats <- regularseason_detailed %>%
  dplyr::select(Season, DayNum, starts_with('L'), WLoc) %>%
  dplyr::rename_at(vars(contains("L")), funs(str_replace(.,"^L", ""))) %>%
  dplyr::mutate(Wins = 0,
                Losses = 1,
                HomeAway_Index = as.numeric(ifelse(WLoc == 'H', -2, 
                                                   ifelse(WLoc == 'N', -1, 
                                                          ifelse(WLoc == 'A', 0,""))))) %>%
  dplyr::rename(Loc = WLoc) %>%
  left_join(ldiff_stats, by = c("Season", "DayNum", "TeamID"))

all_stats <- rbindlist(list(win_stats, loss_stats), use.names = T) %>%
  dplyr::mutate(Games = 1)

season_stats_mean <- all_stats %>%
  dplyr::group_by(Season, TeamID) %>%
  dplyr::summarise_at(vars(Score, FGM:Games), mean, na.rm = T) %>%
  dplyr::mutate_at(vars(Score, FGM:Games), funs(round(., 4)))
names(season_stats_mean)[-c(1:2)] <- paste0(names(season_stats_mean[,-c(1:2)]), ".mean")

season_stats_sum <- all_stats %>%
  dplyr::group_by(Season, TeamID) %>%
  dplyr::summarise_at(vars(Score, FGM:Games), sum, na.rm = T) %>%
  dplyr::mutate_at(vars(Score, FGM:Games), funs(round(., 1)))
names(season_stats_sum)[-c(1:2)] <- paste0(names(season_stats_sum[,-c(1:2)]), ".sum")

regular_df <- inner_join(season_stats_mean, season_stats_sum, by = c('Season', "TeamID")) %>%
  dplyr::rename(Games = Games.sum,
                Wins = Wins.sum,
                Losses = Losses.sum) %>%
  dplyr::select(-Games.mean) %>%
  dplyr::mutate(FGP.mean = FGM.sum/FGA.sum,
                FGP3.mean = FGM3.sum/FGA3.sum,
                FTP.mean = FTM.sum/FTA.sum,
                TA.mean = (Stl.sum - TO.sum)/Games)

rm(wdiff_stats, ldiff_stats, win_stats, loss_stats, season_stats_mean, season_stats_sum)  

# build conference tourney data -------------------------------------------

conf_tourney_df <- conference_tourney %>%
  dplyr::group_by(Season, ConfAbbrev, WTeamID) %>%
  dplyr::summarise(Wins_CT = n()) %>%
  dplyr::rename(TeamID = WTeamID) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ConfAbbrev)

# merge regular season and conf tourney data ------------------------------

regular_ct_df <- left_join(regular_df, conf_tourney_df, by = c("Season", "TeamID")) %>%
  dplyr::mutate(Wins_CT = ifelse(is.na(Wins_CT), 0, Wins_CT)) %>%
  dplyr::left_join(TeamConferences, by = c("Season", "TeamID")) %>%
  dplyr::mutate(p5 = as.factor(ifelse(ConfAbbrev %in% c('acc', 'big_ten', 'big_twelve', 'pac_ten', 'pac_twelve', 'sec'), 1, 0))) %>%
  dplyr::left_join(select(.data = team_IDs, TeamID, TeamName), by = c("TeamID")) %>%
  dplyr::select(Season, TeamID, TeamName, ConfAbbrev, p5, everything())

# build seeds -------------------------------------------------------------

seeds_df <- seedteam_IDs %>%
  dplyr::mutate(Seed_num = as.numeric(str_extract(Seed, "\\d+")))

tourney_seeds_df <- colsplit(as.character(SampleSubmissionStage2$ID),split =  "_", names = c("Season", "FirstTeam", "SecondTeam")) %>%
  cbind(SampleSubmissionStage2$ID) %>%
  dplyr::rename(ID = `SampleSubmissionStage2$ID`) %>%
  dplyr::select(ID, everything()) %>%
  dplyr::inner_join(select(.data = seeds_df, Season, Seed_num, TeamID), by = c("Season", "FirstTeam" = "TeamID")) %>%
  dplyr::left_join(select(.data = seeds_df, Season, Seed_num, TeamID), by = c("Season", "SecondTeam" = "TeamID")) %>%
  dplyr::rename(FirstTeam_Seed = Seed_num.x, SecondTeam_Seed = Seed_num.y) %>%
  dplyr::mutate(D.seed = FirstTeam_Seed - SecondTeam_Seed)

# build differences -------------------------------------------------------

conf_diff <- dplyr::left_join(select(.data = tourney_seeds_df, ID:SecondTeam), 
                              select(.data = regular_ct_df, Season:p5, Wins_CT), 
                              by = c("Season", "FirstTeam" = "TeamID")) %>%
  dplyr::rename_at(vars(TeamName:Wins_CT), funs(paste0("FirstTeam_", .))) %>%
  dplyr::left_join(select(.data = regular_ct_df, Season:p5, Wins_CT), 
                   by = c("Season", "SecondTeam" = "TeamID")) %>%
  dplyr::rename_at(vars(TeamName:Wins_CT), funs(paste0("SecondTeam_", .))) %>%
  dplyr::mutate(D.Wins_CT = FirstTeam_Wins_CT - SecondTeam_Wins_CT) %>%
  dplyr::select(-contains("_Wins_CT"))

regular_ct_diff <- dplyr::left_join(select(.data = tourney_seeds_df, Season:SecondTeam), 
                                    select(.data = regular_ct_df, Season:TeamID, Score.mean:TA.mean), 
                                    by = c("Season", "FirstTeam" = "TeamID")) %>%
  dplyr::rename_at(vars(Score.mean:TA.mean), funs(paste0("FirstTeam_", .))) %>%
  dplyr::left_join(select(.data = regular_ct_df, Season:TeamID, Score.mean:TA.mean), 
                   by = c("Season", "SecondTeam" = "TeamID")) %>%
  dplyr::rename_at(vars(Score.mean:TA.mean), funs(paste0("SecondTeam_", .))) 

#build two matrices and then subtract to get a new df
firstteam_df <- regular_ct_diff %>%
  dplyr::select(Season, contains("FirstTeam"))
secondteam_df <- regular_ct_diff %>%
  dplyr::select(Season, contains("SecondTeam"))
first_second_diff <- firstteam_df[,-c(1:2)] - secondteam_df[, -c(1:2)]
names(first_second_diff) <- gsub("FirstTeam_", "D.", names(first_second_diff))

final_df <- cbind(conf_diff, tourney_seeds_df[, c("FirstTeam_Seed", "SecondTeam_Seed", "D.seed") ], first_second_diff)

# apply predictions using already created models --------------------------

pred_gbm_1 <- predict(model_gbm.1, final_df, type = "prob") %>%
  cbind(final_df[, c("ID")]) %>%
  dplyr::rename(Pred = `1`,
                ID = `final_df[, c(\"ID\")]`) %>%
  dplyr::select(ID, Pred)

pred_log <- predict(model_log, 
                    dplyr::mutate(.data = final_df, FirstTeam_p5 = as.numeric(FirstTeam_p5), SecondTeam_p5 = as.numeric(SecondTeam_p5)) , type = "prob") %>%
  cbind(final_df[, c("ID")]) %>%
  dplyr::rename(Pred = `1`,
                ID = `final_df[, c(\"ID\")]`) %>%
  dplyr::select(ID, Pred)


# export ------------------------------------------------------------------

#gbm model
write.csv(pred_gbm_1, file = "Kaggle_MarchMad18_gbm.csv", row.names = F)

#log model
write.csv(pred_log, file = "Kaggle_MarchMad18_log.csv", row.names = F)
