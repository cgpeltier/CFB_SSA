## This script will produce: individual game box score stats, std and passing downs box score stats,
## skill player box score stats, season stats, and season skill player stats. 

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(gt) # beautiful tables
library(ggthemes) # custom pre-built themes
library(teamcolors) # NFL team colors and logos


### SUCCESS DEFINITION AND GARBAGE TIME
plays.gs <- plays %>% mutate(success = ifelse(yardsgained >= .5* Distance & Down == 1, 1, 
                                             ifelse(yardsgained >= .7* Distance & Down == 2, 1,
                                                    ifelse(yardsgained >= Distance & Down == 3, 1,
                                                           ifelse(yardsgained>= Distance & Down ==4, 1, 0)))),
                            score.margin = Offense.Points - Defense.Points, 
                            garbage = ifelse(Period.Number == 2 & score.margin > 38, 1, 
                                             ifelse(Period.Number == 2 & score.margin < -38, 1,
                                                    ifelse(Period.Number == 3 & score.margin > 28, 1,
                                                           ifelse(Period.Number == 3 & score.margin < -28, 1,
                                                                  ifelse(Period.Number == 4 & score.margin > 22, 1,
                                                                         ifelse(Period.Number == 4 & score.margin < -22, 1, 0))))))) %>%
  filter(offense != "NA" & defense != "NA")


##Situational Plays
advanced.stats1 <- plays.gs %>%
  mutate(
    rz_play = ifelse((yardline <= 20 & Play.Type != "Kickoff"), 1, 0),
    so_play = ifelse((yardline <=40 & Play.Type != "Kickoff" | touchdown == 1 ), 1, 0),
    stuffed_run = ifelse((rush == 1 & yardsgained <=0), 1, 0),
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
advanced.stats2 <- advanced.stats1 %>%
  group_by(offense, defense, Drive.Number) %>%
  mutate(
    so_drive = if_else(so_play == 1, 1, 0),
    td_drive = if_else(touchdown == 1, 1, 0),
    rz_drive = ifelse(rz_play == 1, 1, 0),
  ) %>%
  ungroup()


## fix sacks problem
advanced.stats2$rush <- ifelse(advanced.stats2$Sack == 1, 0, advanced.stats2$rush)
advanced.stats2$pass <- ifelse(advanced.stats2$Sack == 1, 1, advanced.stats2$pass)
advanced.stats2$Play.Type <- ifelse(advanced.stats2$Sack == 1, "Sack", advanced.stats2$Play.Type)

## fix lost fumble problem
advanced.stats2$success <- ifelse(advanced.stats2$Fumble.Lost == 1, 0, advanced.stats2$success)

## add game_drive variable for season drive stats
advanced.stats2 <- advanced.stats2 %>% 
    unite(game_drive, Game.Code, Drive.Number, sep = "_", remove = FALSE)

## new box score stats
box.score.stats <- advanced.stats2 %>%
  group_by(offense, defense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
    yards = sum(yardsgained),
    drives = n_distinct(Drive.Number),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuff.rte = mean(stuffed_run[rush==1]),
    opp.rate = mean(opp_rate_run[rush==1]), 
    exp.rte = mean(exp_play),
    exp.rte.rush = mean(exp_play[rush == 1]),
    exp.rte.pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays,
    yds.to.go.3d = mean(Distance[Down==3]),
    redz.sr = mean(success[rz_play == 1]),
    scor.opp.sr = mean(success[so_play == 1]),
    short.rush.sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    scoring.opps = n_distinct(Drive.Number[so_drive == 1]),
    touchdowns = sum(touchdown),
    scor.opp.rte = scoring.opps / drives, 
    scor.opp.tdrte = touchdowns / scoring.opps,
    redzone.dr = n_distinct(game_drive[rz_drive == 1])/n_distinct(game_drive),
    redzone.td.rte = sum(touchdown[rz_play==1])/n_distinct(game_drive[rz_play==1]),
    sr_1d = mean(success[Down == 1]),
    sr_2d = mean(success[Down == 2]),
    sr_3d = mean(success[Down == 3]),
    sr_4d = mean(success[Down == 4]),
    std.down.sr = mean(success[std.down == 1]),
    pass.down.sr = mean(success[pass.down == 1]),
  ) %>% ungroup()

## new all season stats
season.stats <- advanced.stats2 %>%
  group_by(offense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
    yards = sum(yardsgained),
    drives = n_distinct(game_drive),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuff.rte = mean(stuffed_run[rush==1]),
    opp.rate = mean(opp_rate_run[rush==1]), 
    exp.rte = mean(exp_play),
    exp.rte.rush = mean(exp_play[rush == 1]),
    exp.rte.pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays,
    yds.to.go.3d = mean(Distance[Down==3]),
    redz.sr = mean(success[rz_play == 1]),
    scor.opp.sr = mean(success[so_play == 1]),
    short.rush.sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    scoring.opps = n_distinct(game_drive[so_drive == 1]),
    touchdowns = sum(touchdown),
    scor.opp.rte = scoring.opps / drives, 
    scor.opp.tdrte = touchdowns / scoring.opps,
    redzone.dr = n_distinct(game_drive[rz_drive == 1])/n_distinct(game_drive),
    redzone.td.rte = sum(touchdown[rz_play==1])/n_distinct(game_drive[rz_play==1]),
    sr_1d = mean(success[Down == 1]),
    sr_2d = mean(success[Down == 2]),
    sr_3d = mean(success[Down == 3]),
    sr_4d = mean(success[Down == 4]),
    std.down.sr = mean(success[std.down == 1]),
    pass.down.sr = mean(success[pass.down == 1]),
  ) %>% ungroup()


## standard down box score stats
std.down.stats <- advanced.stats2 %>%
    filter(std.down == 1) %>%
    group_by(offense, defense) %>%
    summarize(
        rush.rte = sum(rush)/sum(std.down),
        success.rte = mean(success),
        rush.sr = mean(success[rush==1]),
        pass.sr = mean(success[pass==1]),
        exp.rte = mean(exp_play),
        exp.rte.rush = mean(exp_play[rush == 1]),
        exp.rte.pass = mean(exp_play[pass == 1])
    ) %>% ungroup()


## passing down box score stats
pass.down.stats <- advanced.stats2 %>%
  filter(pass.down == 1) %>%
  group_by(offense, defense) %>%
  summarize(
    rush.rte = sum(rush)/sum(pass.down),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    exp.rte = mean(exp_play),
    exp.rte.rush = mean(exp_play[rush == 1]),
    exp.rte.pass = mean(exp_play[pass == 1])
  ) %>% ungroup()


## rusher box score stats
rb.stats <- advanced.stats2 %>%
  filter(rush == 1) %>%
  group_by(offense, defense, rushfirst, rushlast) %>%
  summarise(
    plays = n(),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rte = mean(exp_play),
    stuff.rte = mean(stuffed_run),
    opp.rate = mean(opp_rate_run),
    short.rush.sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  ) %>% ungroup()

## receiver box score stats
wr.stats <- advanced.stats2 %>%
  filter(pass == 1) %>%
  group_by(offense, defense, receiverfirst, receiverlast) %>%
  summarise(
    plays = n(),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()

## qb box score stats
qb.stats <- advanced.stats2 %>%
    filter(pass == 1) %>%
    group_by(offense, defense, passerfirst, passerlast) %>%
    summarise(
      plays = n(),
      yards = sum(yardsgained),
      success.rte = mean(success),
      exp.rate = mean(exp_play)
    ) %>% ungroup()

## rusher season stats
rb.season.stats <- advanced.stats2 %>%
  filter(rush == 1) %>%
  group_by(offense, rushfirst, rushlast) %>%
  summarise(
    plays = n(),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rte = mean(exp_play),
    stuff.rte = mean(stuffed_run),
    opp.rate = mean(opp_rate_run),
    short.rush.sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  ) %>% ungroup()

## receiver season stats
wr.season.stats <- advanced.stats2 %>%
  filter(pass == 1) %>%
  group_by(offense, receiverfirst, receiverlast) %>%
  summarise(
    plays = n(),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()

qb.season.stats <- advanced.stats2 %>%
  filter(pass == 1) %>%
  group_by(offense, passerfirst, passerlast) %>%
  summarise(
    plays = n(),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()

