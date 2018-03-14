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
team_IDs <- read.csv("./DataFiles/Teams.csv")
seedteam_IDs <- read.csv("./DataFiles/NCAATourneySeeds.csv")
TeamConferences <- read.csv("./DataFiles/TeamConferences.csv")
#final4_seed_matchups <- read.csv("./DataFiles/NCAATourneySlots.csv")
#team_locations <- read.csv("GeoLocations/TeamGeog.csv")
#tourney_locations <- read.csv("./GeoLocations/TourneyGeog.csv")
#final4_regions <- read.csv("./DataFiles/Seasons.csv")


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

tourney_df <- tourney_detailed %>%
  dplyr::select(Season:LTeamID, -WScore)

tourney_seeds_df <- tourney_df %>%
  dplyr::mutate(FirstTeam = ifelse(WTeamID > LTeamID, WTeamID, LTeamID),
         SecondTeam = ifelse(WTeamID > LTeamID, LTeamID, WTeamID),
         Result = ifelse(WTeamID == FirstTeam, 1,0)) %>%
  dplyr::select(-(DayNum:LTeamID)) %>%
  dplyr::left_join(select(.data = seeds_df, Season, Seed_num, TeamID), by = c("Season", "FirstTeam" = "TeamID")) %>%
  dplyr::left_join(select(.data = seeds_df, Season, Seed_num, TeamID), by = c("Season", "SecondTeam" = "TeamID")) %>%
  dplyr::rename(FirstTeam_Seed = Seed_num.x, SecondTeam_Seed = Seed_num.y) %>%
  dplyr::mutate(D.seed = FirstTeam_Seed - SecondTeam_Seed)


# build differences -------------------------------------------------------

conf_diff <- dplyr::left_join(select(.data = tourney_seeds_df, Season:SecondTeam), 
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

final_df <- cbind(conf_diff, tourney_seeds_df[, c("Result", "FirstTeam_Seed", "SecondTeam_Seed", "D.seed") ], first_second_diff)


# model data --------------------------------------------------------------

model_df <- final_df %>%
  dplyr::select(Result, everything(), -(Season:FirstTeam_TeamName), -SecondTeam_TeamName) %>%
  dplyr::mutate(Result = as.factor(Result),
                FirstTeam_p5 = as.numeric(FirstTeam_p5),
                SecondTeam_p5 = as.numeric(SecondTeam_p5))

set.seed(85)
intrain <- createDataPartition(model_df$Result, p = 0.70, list = F)

d.train <- model_df[intrain,]
d.test <- model_df[-intrain,]


# eda ---------------------------------------------------------------------

eda_dtree <- rpart::rpart(Result ~ ., d.train)
rpart.plot::rpart.plot(eda_dtree)
dt_top10vars <- eda_dtree$variable.importance[1:10]

eda_rf <- caret::train(Result ~ ., d.train, method = 'rf', allowParallel=TRUE)
plot(varImp(eda_rf), top = 40)
rf_vims <- data.frame(varImp(eda_rf)$importance)
rf_vims$Vars <- row.names(rf_vims)
rf_top40vars <- rf_vims[order(-rf_vims$Overall),][1:40,]

eda_rf_pred <- predict(eda_rf, d.test)
caret::confusionMatrix(eda_rf_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, eda_rf_pred)


# modeling ----------------------------------------------------------------

top_vars <- unique(c('Result', rf_top40vars$Vars))
d.train_topvars <- d.train[, top_vars] 

# RandomForest
trControl_rf =trainControl(method = "repeatedcv",number = 10)

model_rf.1 <- caret::train(Result ~ ., d.train, method = 'rf', trControl = trControl_rf, allowParallel = T)
plot(varImp(model_rf.1), top = 10)
model_rf.1_pred <- predict(model_rf.1, d.test)
caret::confusionMatrix(model_rf.1_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_rf.1_pred)

model_rf.2 <- caret::train(Result ~ ., d.train_topvars, method = 'rf', trControl = trControl_rf, allowParallel = T)
plot(varImp(model_rf.2), top = 10)
model_rf.2_pred <- predict(model_rf.2, d.test)
caret::confusionMatrix(model_rf.2_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_rf.2_pred)

# gbm
gbmGrid <- expand.grid(
  n.trees = c(200, 250, 300),
  shrinkage = c(0.0025, 0.001), #lambda
  interaction.depth = c(4,6, 8),
  n.minobsinnode = c(5)) #default
gbmtrcl <- trainControl(
  method = "repeatedcv",
  number = 10,
  verboseIter = F,
  allowParallel = T)

set.seed(1000)
model_gbm.1 <- caret::train(x = as.matrix(d.train[,-c(1:2,4)]), d.train$Result, 
                            trControl = gbmtrcl, tuneGrid = gbmGrid, method = 'gbm', metric = 'Accuracy')

model_gbm.1$bestTune
model_gbm.1$results %>% dplyr::arrange(desc(Kappa)) %>% head(.)

model_gbm.1_pred <- predict(model_gbm.1, d.test)
caret::confusionMatrix(model_gbm.1_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_gbm.1_pred)

set.seed(1000)
model_gbm.2 <- caret::train(x = as.matrix(d.train_topvars[,-c(1)]), d.train_topvars$Result, 
                            trControl = gbmtrcl, tuneGrid = gbmGrid, method = 'gbm', metric = 'Accuracy')

model_gbm.2$bestTune
model_gbm.2$results %>% dplyr::arrange(desc(Kappa)) %>% head(.)

model_gbm.2_pred <- predict(model_gbm.2, d.test)
caret::confusionMatrix(model_gbm.2_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_gbm.2_pred)

# svm
trControl_svm =trainControl(method = "repeatedcv",number = 10)

model_svm <- caret::train(Result ~ ., d.train, method = 'svmLinear', trControl = trControl_svm)
model_svm_pred <- predict(model_svm, d.test)
caret::confusionMatrix(model_svm_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_svm_pred)

# neural network
trControl_nn =trainControl(method = "repeatedcv",number = 10)
set.seed(39)
model_nn <- train(Result~., d.train, method = "nnet",  lineout = F, trace = F, trControl = trControl_nn)
plot(varImp(model_nn))
model_nn_pred <- predict(model_nn, d.test)
caret::confusionMatrix(model_nn_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_nn_pred)

# log model
trControl_log =trainControl(method = "repeatedcv",number = 10)
set.seed(39)
model_log <- train(Result ~., d.train, method = "glm", trControl = trControl_log)

model_log_pred <- predict(model_log, d.test)
caret::confusionMatrix(model_log_pred, d.test$Result)
ROSE::roc.curve(d.test$Result, model_log_pred)

# evaluate models ---------------------------------------------------------

modelvalues <- resamples(list(rf = model_rf.1,
                              gbm = model_gbm.1, 
                              svm = model_svm,
                              nn = model_nn,
                              log = model_log))

summary(modelvalues)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6323529 0.6534161 0.6642157 0.6817416 0.7137681 0.7647059    0
# gbm 0.6086957 0.6592072 0.7101449 0.6977293 0.7275880 0.7647059    0
# svm 0.5588235 0.6043798 0.6086957 0.6232810 0.6666667 0.6714286    0
# nn  0.5507246 0.6182864 0.6568627 0.6585422 0.7071429 0.7794118    0
# log 0.5588235 0.6738613 0.6934143 0.6816228 0.7044118 0.7391304    0
