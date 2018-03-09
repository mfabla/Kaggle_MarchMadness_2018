# KMM 2018
# 8 MAR 2018
# Mike Abla
#
# Kag MM Cont where there are two stages
# 1) Predicted Prob for every possible matchup in past tourneys (2014-2017)
# 2) Predicted Prob for every possible matchup before 2018 tourney


# set env -----------------------------------------------------------------

cat("\n load packages...\n")

pckgs <- c('data.table', 'dplyr', 'caret', 'ggplot2', 'rpart', 'rpart.plot', 'stringr', 'reshape', 'randomForest', 
          'gbm', 'xgboost', 'ROSE', 'pROC', 'gridExtra')
new_pckgs <- pckgs[!(pckgs %in% installed.packages()[, "Package"])]
if (length(new_pckgs))
  install.packages(new_pckgs, dependencies = TRUE, quiet = TRUE)
sapply(pckgs, require, character.only = TRUE)


# load data ---------------------------------------------------------------

# model data
regularseason_detailed <- read.csv("C:./DataFiles/RegularSeasonDetailedResults.csv")
tourney_detailed <- read.csv("./DataFiles/NCAATourneyDetailedResults.csv")
conference_tourney <- read.csv("./DataFiles/ConferenceTourneyGames.csv")

# references
final4_regions <- read.csv("./DataFiles/Seasons.csv")
team_IDs <- read.csv("./DataFiles/Teams.csv")
seedteam_IDs <- read.csv("./DataFiles/NCAATourneySeeds.csv")
final4_seed_matchups <- read.csv("./DataFiles/NCAATourneySlots.csv")
team_locations <- read.csv("GeoLocations/TeamGeog.csv")
tourney_locations <- read.csv("./GeoLocations/TourneyGeog.csv")

# sample submission
samplesubmission <- read.csv("SampleSubmissions/SampleSubmissionStage1.csv")


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
  dplyr::rename(TeamID = WTeamID,
                Conf = ConfAbbrev)
