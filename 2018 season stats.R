## To do: add rank columns


work_dir <- "C:/Users/eqa47693/Desktop/CFB/ssa_data/ssa_2019/data_2018"
setwd(work_dir)

library(tidyverse)
library(dplyr)


pbp <- read.csv("play.csv")
drive <- read.csv("drive.csv")
teams <- read.csv("team.csv")
conf <- read.csv("conference.csv")
reception <- read.csv("reception.csv")
game <- read.csv("game.csv")
team.game.stats <- read.csv("team-game-statistics.csv")
player <- read.csv("player.csv")
teams <- read.csv("team.csv")
conf <- read.csv("conference.csv")
rush <- read.csv("rush.csv")
rush.att <-  read.csv("rush-attempt.csv")
pass <- read.csv("pass.csv")
pass.att <-  read.csv("pass-attempt.csv")


#2 Merge Pass and Rush info with PBP dataset
## Rushing

rushing <- merge(pbp, rush, 
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all=TRUE)


##Add Passing

all.pbp <- merge(rushing, pass,
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all=TRUE)

#3 Merge team names with the main drive and play-by-play datasets

#PBP
#offense

all.plays.off <- merge(all.pbp, teams, by.x = "Offense.Team.Code", by.y = "Team.Code", all=TRUE)

#defense
all.plays <- merge(all.plays.off, teams, by.x = "Defense.Team.Code", by.y = "Team.Code", all=TRUE)

###CLEANING UP THE VARIABLE NAMES 

plays_clean <- all.plays %>%
  filter(Play.Type == "PASS" | Play.Type == "RUSH") %>% 
  replace(., is.na(.), 0) %>%
  rename(rush = Attempt.x,
         pass = Attempt.y,
         offense = Name.x, 
         defense = Name.y, 
         yardline = Spot,
         offense.conf = Conference.Code.x,
         defense.conf = Conference.Code.y,
         Rushing.Player.Code =  Player.Code) %>%
  mutate(yardsgained = Yards.x + Yards.y,
         touchdown = Touchdown.x + Touchdown.y,
         firstdown = X1st.Down.x + X1st.Down.y) %>%
  arrange(Game.Code, Play.Number) %>%
  select(Game.Code, offense, offense.conf, defense, defense.conf, Drive.Number, Play.Number, Clock, Down, 
         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
         Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code)

#Last thing - gotta merge the player IDs, so we have the Names. 

plays1 <-
  merge(plays_clean, player, by.x = "Passer.Player.Code", by.y = "Player.Code", all = TRUE)

plays12 <- plays1 %>%   select(Game.Code, offense, offense.conf, defense, defense.conf, Drive.Number, Play.Number, Clock, Down, 
                               Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                               Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast = Last.Name, passerfirst = First.Name)

plays2 <-
  merge(plays12, player, by.x = "Receiver.Player.Code", by.y = "Player.Code", all = TRUE) 

plays22 <- plays2 %>% select(Game.Code, offense, offense.conf, defense, defense.conf, Drive.Number, Play.Number, Clock, Down, 
                             Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                             Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast, passerfirst, receiverlast = Last.Name, receiverfirst = First.Name)

plays <-
  merge(plays22, player, by.x = "Rushing.Player.Code", by.y = "Player.Code", all = TRUE)

plays_2018 <- plays%>% select(Game.Code, offense, offense.conf, defense, defense.conf, Drive.Number, Play.Number, Clock, Down, 
                         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                         Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code,
                         passerlast, passerfirst, receiverlast, receiverfirst, rushlast = Last.Name, rushfirst = First.Name)





## Create new variables
plays_2018 <- plays_2018 %>%
  mutate(success = ifelse(((yardsgained >= (.5 * Distance)) & Down == 1) |
                            ((yardsgained >= (.7 * Distance)) & Down == 2) |
                            ((yardsgained >= Distance) & Down == 3) |
                            ((yardsgained >= Distance) & Down == 4), 1, 0),
         rz_play = ifelse((yardline <= 20), 1, 0),
         so_play = ifelse((yardline <=40 | touchdown == 1 ), 1, 0),
         stuffed_run = ifelse((Sack != 1 & rush == 1 & yardsgained <=0), 1, 0),
         opp_rate_run = ifelse((rush == 1 & yardsgained >= 4), 1, 0),
         exp_play = ifelse((yardsgained >= 13), 1, 0),
         short_rush_attempt = ifelse(Distance <= 2 & rush == 1, 1, 0),
         short_rush_success = ifelse(Distance <= 2 & rush == 1 & yardsgained >= Distance, 1, 0),
         std.down = ifelse(Down == 2 & Distance < 8, 1, 
                           ifelse(Down == 3 & Distance < 5, 1,
                                  ifelse(Down == 4 & Distance < 5, 1, 0))),
         pass.down = ifelse(Down == 2 & Distance > 8, 1, 
                            ifelse(Down == 3 & Distance > 5, 1, 
                                   ifelse(Down == 4 & Distance > 5, 1, 0)))
  )

## add drive variables
plays_2018 <- plays_2018 %>%
  group_by(offense, defense, Drive.Number) %>%
  mutate(
    so_drive = if_else(so_play == 1, 1, 0),
    td_drive = if_else(touchdown == 1, 1, 0),
    rz_drive = ifelse(rz_play == 1, 1, 0)
  ) %>% ungroup()


## fix sacks problem
plays_2018$rush <- ifelse(plays_2018$Sack == 1, 0, plays_2018$rush)
plays_2018$pass <- ifelse(plays_2018$Sack == 1, 1, plays_2018$pass)

## fix fumble success problem
plays_2018$success <- ifelse(plays_2018$Fumble.Lost == 1, 0, plays_2018$success)

## fix all season game drive problem
plays_2018 <- plays_2018 %>% unite(game_drive, Game.Code, Drive.Number, sep = "_", remove = FALSE)

## new all season offense stats
all_stats_off_2018 <- plays_2018 %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(offense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays_2018 = sum(rush + pass), 
    yards=sum(yardsgained),
    yardsperplay = mean(yardsgained),
    drives = n_distinct(game_drive),
    overall_sr = mean(success),
    pass.sr = mean(success[pass==1]),
    rush.sr = mean(success[rush==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuffed_run_rate = mean(stuffed_run[rush==1]),
    opp_rate = mean(opp_rate_run[rush==1]),  
    overall_exp_rate = mean(exp_play),
    exp_rate_rush = mean(exp_play[rush == 1]),
    exp_rate_pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    pass.down.rush.rte = sum(rush[pass.down==1]) / sum(pass.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    so_total = n_distinct(game_drive[so_drive == 1]),
    touchdown_total = sum(touchdown),
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    std.down.sr = mean(success[std.down == 1]),
    pass.down.sr = mean(success[pass.down == 1]),
  )%>% ungroup()

## season stats defense
all_stats_def_2018 <- plays_2018 %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(defense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays_2018 = sum(rush + pass), 
    yards=sum(yardsgained),
    yardsperplay = mean(yardsgained),
    drives = n_distinct(game_drive),
    overall_sr = mean(success),
    pass.sr = mean(success[pass==1]),
    rush.sr = mean(success[rush==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuffed_run_rate = mean(stuffed_run[rush==1]),
    opp_rate = mean(opp_rate_run[rush==1]),  
    overall_exp_rate = mean(exp_play),
    exp_rate_rush = mean(exp_play[rush == 1]),
    exp_rate_pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    pass.down.rush.rte = sum(rush[pass.down==1]) / sum(pass.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    so_total = n_distinct(game_drive[so_drive == 1]),
    touchdown_total = sum(touchdown),
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total
  ) %>% ungroup()


## mean ratings for all stats
avgs_2018 <- plays_2018 %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yardsgained),
    plays_2018 = sum(rush + pass), 
    yards=sum(yardsgained),
    yardsperplay = mean(yardsgained),
    drives = n_distinct(game_drive),
    overall_sr = mean(success),
    pass.sr = mean(success[pass==1]),
    rush.sr = mean(success[rush==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuffed_run_rate = mean(stuffed_run[rush==1]),
    opp_rate = mean(opp_rate_run[rush==1]),  
    overall_exp_rate = mean(exp_play),
    exp_rate_rush = mean(exp_play[rush == 1]),
    exp_rate_pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays_2018,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    pass.down.rush.rte = sum(rush[pass.down==1]) / sum(pass.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    so_total = n_distinct(game_drive[so_drive == 1]),
    touchdown_total = sum(touchdown),
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total
  ) %>% ungroup()




## add rank variables
all_stats_off_2018_rk <- all_stats_off_2018 %>%
    mutate(
      success.rte = 
      pass.success.rte = 
      rush.success.rte = 
      stuffed.run.rte = 
      opp_rte =   
      exp.rte = 
      exp.rte.rush = 
      exp.rte.pass = 
      redz.sr = 
      scor.opp.sr =
      short.rush.sr = 
      scoring.opps = 
      scor.opp.rte =  
      so_td_rate = 
    )  




##########################################
## individual team exports

osu_off_2018 <- all_stats_off_2018 %>%
    filter(offense == "Ohio State")

osu_def_2018 <- all_stats_def_2018 %>%
  filter(defense == "Ohio State")


write.csv(osu_off_2018, file = "osu_off_2018.csv")
write.csv(osu_def_2018, file = "osu_def_2018.csv")



uc_2018_off <- all_stats_off_2018 %>%
  filter(offense == "Cincinnati")

uc_2018_def <- all_stats_def_2018 %>%
  filter(defense == "Cincinnati")

