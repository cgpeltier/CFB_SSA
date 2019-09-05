## add rank columns

library(tidyverse)
library(dplyr)


## Create new variables
plays <- plays %>%
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
          short_rush_success = ifelse(Distance <= 2 & rush == 1 & yardsgained >= Distance, 1, 0)
    )

## add drive variables
plays <- plays %>%
  group_by(offense, defense, Drive.Number) %>%
  mutate(
    so_drive = if_else(so_play == 1, 1, 0),
    td_drive = if_else(touchdown == 1, 1, 0),
    rz_drive = ifelse(rz_play == 1, 1, 0)
  ) %>%
  ungroup()


## fix sacks problem
plays$rush <- ifelse(plays$Sack == 1, 0, plays$rush)
plays$pass <- ifelse(plays$Sack == 1, 1, plays$pass)

## fix fumble success problem
plays$success <- ifelse(plays$Fumble.Lost == 1, 0, plays$success)

## new box score stats
box.score.stats <- plays %>%
  group_by(offense, defense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
    yards = sum(yardsgained),
    yardsperplay = mean(yardsgained),
    drives = n_distinct(Drive.Number),
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
    stuffed_exp_ratio = stuffed_run_rate / exp_rate_rush,
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(Drive.Number[so_drive == 1]),
    touchdown_total = sum(touchdown),
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    so_td_rate2 = (sum(touchdown[so_play==1])/so_total),
    sr_1d = mean(success[Down == 1]),
    sr_2d = mean(success[Down == 2]),
    sr_3d = mean(success[Down == 3]),
    sr_4d = mean(success[Down == 4])
      ) %>%
    ungroup()

 

## new all season stats
plays<- plays %>% unite(game_drive, Game.Code, Drive.Number, sep = "_", remove = FALSE)

all.stats <- plays %>%
  group_by(offense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
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
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    so_total = n_distinct(game_drive[so_drive == 1]),
    touchdown_total = sum(touchdown),
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    so_td_rate2 = sum(touchdown[so_play==1])
  ) %>%
  ungroup()


uc_ucla <- box.score.stats %>%
    filter(offense == "Cincinnati" | defense == "Cincinnati")


    

