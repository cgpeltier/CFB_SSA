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
                              ifelse(Down == 4 & Distance > 5, 1, 0))),
    qb.hurry = ifelse(is.na(hurrier.position), 0, 1)
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
advanced.stats2$rushlast <-as.character(advanced.stats2$rushlast) 
advanced.stats2$rushfirst <-as.character(advanced.stats2$rushfirst) 
advanced.stats2$passerlast <-as.character(advanced.stats2$passerlast) 
advanced.stats2$passerfirst <-as.character(advanced.stats2$passerfirst) 
advanced.stats2$Play.Type <- as.character(advanced.stats2$Play.Type)

advanced.stats2$rush <- ifelse(advanced.stats2$Sack == 1, 0, advanced.stats2$rush)
advanced.stats2$pass <- ifelse(advanced.stats2$Sack == 1, 1, advanced.stats2$pass)
advanced.stats2$Play.Type <- ifelse(advanced.stats2$Sack == 1, "Sack", advanced.stats2$Play.Type)
advanced.stats2$passerlast <- ifelse(advanced.stats2$Sack == 1, advanced.stats2$rushlast, advanced.stats2$passerlast)
advanced.stats2$passerfirst <- ifelse(advanced.stats2$Sack == 1, advanced.stats2$rushfirst, advanced.stats2$passerfirst)
advanced.stats2$rushlast <- ifelse(advanced.stats2$Sack == 1, NA, advanced.stats2$rushlast)
advanced.stats2$rushfirst <- ifelse(advanced.stats2$Sack == 1, NA, advanced.stats2$rushfirst)


## fix lost fumble problem
advanced.stats2$success <- ifelse(advanced.stats2$Fumble.Lost == 1, 0, advanced.stats2$success)


## add game_drive variable for season drive stats
advanced.stats2 <- advanced.stats2 %>% 
    unite(game_drive, Game.Code, Drive.Number, sep = "_", remove = FALSE)


## change assisted tfls/sacks to .5 
advanced.stats2$TFL.Assist <- ifelse(advanced.stats2$TFL.Assist == 1, .5, advanced.stats2$TFL.Assist)
advanced.stats2$Sack.Assist <- ifelse(advanced.stats2$Sack.Assist == 1, .5, advanced.stats2$Sack.Assist)


## new box score stats
box.score.stats <- advanced.stats2 %>%
  group_by(offense, defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = sum(rush + pass), 
    yards = sum(yardsgained),
    drives = n_distinct(Drive.Number),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuff.rte = mean(stuffed_run[rush==1]),
    opp.rte = mean(opp_rate_run[rush==1]), 
    exp.rte = mean(exp_play),
    exp.rte.rush = mean(exp_play[rush == 1]),
    exp.rte.pass = mean(exp_play[pass == 1]),
    rush.rte = sum(rush)/plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    pass.down.rush.rte = sum(rush[pass.down==1]) / sum(pass.down),
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
    havoc.rte = (sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                       Pass.Intercepted + Fumble + qb.hurry)) / sum(rush + pass)
  ) %>% ungroup()

## new all season stats - offense
season.stats.off <- advanced.stats2 %>%
  group_by(offense, offense.conf) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = sum(rush + pass), 
    yards = sum(yardsgained),
    drives = n_distinct(game_drive),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuff.rte = mean(stuffed_run[rush==1]),
    opp.rte = mean(opp_rate_run[rush==1]), 
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
    sr.1d = mean(success[Down == 1]),
    sr.2d = mean(success[Down == 2]),
    sr.3d = mean(success[Down == 3]),
    sr.4d = mean(success[Down == 4]),
    std.down.sr = mean(success[std.down == 1]),
    pass.down.sr = mean(success[pass.down == 1]),
    havoc.rte.allowed = (sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                       Pass.Intercepted + Fumble + qb.hurry)) / sum(rush + pass)
  ) %>% ungroup()

## add ranks - offense
season.stats.off <- season.stats.off %>%
  filter(offense.conf == 823 | offense.conf == 821 | offense.conf == 25354 | offense.conf == 827 |
           offense.conf == 24312 | offense.conf == 99001 | offense.conf == 875 | offense.conf == 5486 |
           offense.conf == 905 | offense.conf == 911 | offense.conf == 818) %>%
  mutate(
    ypp.rank = dense_rank(desc(ypp)),
    sr.rank = dense_rank(desc(success.rte)),
    rush.sr.rank = dense_rank(desc(rush.sr)),
    pass.sr.rank = dense_rank(desc(pass.sr)),
    ypp.rush.rank = dense_rank(desc(ypp.rush)),
    ypp.pass.rank = dense_rank(desc(ypp.pass)),
    stuff.rte.rank = dense_rank(stuff.rte),
    opp.rte.rank = dense_rank(desc(opp.rte)),
    exp.rte.rank = dense_rank(desc(exp.rte)),
    exp.rte.rush.rank = dense_rank(desc(exp.rte.rush)),
    exp.rte.pass.rank = dense_rank(desc(exp.rte.pass)),
    rush.rte.rank = dense_rank(desc(rush.rte)),
    yds.to.go.3d.rank = dense_rank(yds.to.go.3d),
    redz.sr.rank = dense_rank(desc(redz.sr)),
    scor.opp.sr.rank = dense_rank(desc(scor.opp.sr)),
    short.rush.sr.rank = dense_rank(desc(short.rush.sr)),
    scor.opp.rte.rank =  dense_rank(desc(scor.opp.rte)),
    scor.opp.tdrte.rank = dense_rank(desc(scor.opp.tdrte)),
    redzone.dr.rank = dense_rank(desc(redzone.dr)),
    redzone.td.rte.rank = dense_rank(desc(redzone.td.rte)),
    sr.1d.rank = dense_rank(desc(sr.1d)),
    sr.2d.rank = dense_rank(desc(sr.2d)),
    sr.3d.rank = dense_rank(desc(sr.3d)),
    sr.4d.rank = dense_rank(desc(sr.4d)),
    std.down.sr.rank = dense_rank(desc(std.down.sr)),
    pass.down.sr.rank = dense_rank(desc(pass.down.sr)),
    havoc.rte.allowed.rank = dense_rank(havoc.rte.allowed)
  )

col_order <- c("offense", "ypp", "ypp.rank", "plays", "yards", "drives", "success.rte", "sr.rank", "rush.sr", "rush.sr.rank", "pass.sr", "pass.sr.rank",
               "ypp.rush", "ypp.rush.rank", "ypp.pass", "ypp.pass.rank", "stuff.rte", "stuff.rte.rank",
               "opp.rte", "opp.rte.rank", "exp.rte", "exp.rte.rank", "exp.rte.rush", "exp.rte.rush.rank",
               "exp.rte.pass", "exp.rte.pass.rank", "rush.rte", "rush.rte.rank", "yds.to.go.3d", "yds.to.go.3d.rank",
               "redz.sr", "redz.sr.rank", "scor.opp.sr", "scor.opp.sr.rank", "short.rush.sr", "short.rush.sr.rank",
               "touchdowns", "scor.opp.rte", "scor.opp.rte.rank", "scor.opp.tdrte","scor.opp.tdrte.rank", "redzone.dr","redzone.dr.rank", "redzone.td.rte", "redzone.td.rte.rank",
               "sr.1d", "sr.1d.rank", "sr.2d", "sr.2d.rank", "sr.3d", "sr.3d.rank", "sr.4d","sr.4d.rank", 
               "std.down.sr", "std.down.sr.rank", "pass.down.sr", "pass.down.sr.rank", "havoc.rte.allowed", "havoc.rte.allowed.rank" )

season.stats.off  <- season.stats.off[, col_order] 
  



## all season stats - defense
season.stats.def <- advanced.stats2 %>%
  group_by(defense, defense.conf) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = sum(rush + pass), 
    yards = sum(yardsgained),
    drives = n_distinct(game_drive),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    ypp.rush = mean(yardsgained[rush==1]),
    ypp.pass = mean(yardsgained[pass==1]),
    stuff.rte = mean(stuffed_run[rush==1]),
    opp.rte = mean(opp_rate_run[rush==1]), 
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
    sr.1d = mean(success[Down == 1]),
    sr.2d = mean(success[Down == 2]),
    sr.3d = mean(success[Down == 3]),
    sr.4d = mean(success[Down == 4]),
    std.down.sr = mean(success[std.down == 1]),
    pass.down.sr = mean(success[pass.down == 1]),
    havoc.rte = (sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                       Pass.Intercepted + Fumble + qb.hurry)) / sum(rush + pass)
  ) %>% ungroup()


## add ranks - defense 
season.stats.def <- season.stats.def %>%
  filter(defense.conf == 823 | defense.conf == 821 | defense.conf == 25354 | defense.conf == 827 |
           defense.conf == 24312 | defense.conf == 99001 | defense.conf == 875 | defense.conf == 5486 |
           defense.conf == 905 | defense.conf == 911 | defense.conf == 818) %>%
  mutate(
    ypp.rank = dense_rank(ypp),
    sr.rank = dense_rank(success.rte),
    rush.sr.rank = dense_rank(rush.sr),
    pass.sr.rank = dense_rank(pass.sr),
    ypp.rush.rank = dense_rank(ypp.rush),
    ypp.pass.rank = dense_rank(ypp.pass),
    stuff.rte.rank = dense_rank(desc(stuff.rte)),
    opp.rte.rank = dense_rank(opp.rte),
    exp.rte.rank = dense_rank(exp.rte),
    exp.rte.rush.rank = dense_rank(exp.rte.rush),
    exp.rte.pass.rank = dense_rank(exp.rte.pass),
    rush.rte.rank = dense_rank(rush.rte),
    yds.to.go.3d.rank = dense_rank(desc(yds.to.go.3d)),
    redz.sr.rank = dense_rank(redz.sr),
    scor.opp.sr.rank = dense_rank(scor.opp.sr),
    short.rush.sr.rank = dense_rank(short.rush.sr),
    scor.opp.rte.rank =  dense_rank(scor.opp.rte),
    scor.opp.tdrte.rank = dense_rank(scor.opp.tdrte),
    redzone.dr.rank = dense_rank(redzone.dr),
    redzone.td.rte.rank = dense_rank(redzone.td.rte),
    sr.1d.rank = dense_rank(sr.1d),
    sr.2d.rank = dense_rank(sr.2d),
    sr.3d.rank = dense_rank(sr.3d),
    sr.4d.rank = dense_rank(sr.4d),
    std.down.sr.rank = dense_rank(std.down.sr),
    pass.down.sr.rank = dense_rank(pass.down.sr),
    havoc.rte.rank = dense_rank(desc(havoc.rte))
  )

col_order <- c("defense", "ypp", "ypp.rank", "plays", "yards", "drives", "success.rte", "sr.rank", "rush.sr", "rush.sr.rank", "pass.sr", "pass.sr.rank",
               "ypp.rush", "ypp.rush.rank", "ypp.pass", "ypp.pass.rank", "stuff.rte", "stuff.rte.rank",
               "opp.rte", "opp.rte.rank", "exp.rte", "exp.rte.rank", "exp.rte.rush", "exp.rte.rush.rank",
               "exp.rte.pass", "exp.rte.pass.rank", "rush.rte", "rush.rte.rank", "yds.to.go.3d", "yds.to.go.3d.rank",
               "redz.sr", "redz.sr.rank", "scor.opp.sr", "scor.opp.sr.rank", "short.rush.sr", "short.rush.sr.rank",
               "touchdowns", "scor.opp.rte", "scor.opp.rte.rank", "scor.opp.tdrte","scor.opp.tdrte.rank", "redzone.dr","redzone.dr.rank", "redzone.td.rte", "redzone.td.rte.rank",
               "sr.1d", "sr.1d.rank", "sr.2d", "sr.2d.rank", "sr.3d", "sr.3d.rank", "sr.4d","sr.4d.rank", 
               "std.down.sr", "std.down.sr.rank", "pass.down.sr", "pass.down.sr.rank", "havoc.rte", "havoc.rte.rank" )

season.stats.def  <- season.stats.def[, col_order] 

write.csv(season.stats.def, file = "season.stats.def.csv")

## standard down box score stats
std.down.stats <- advanced.stats2 %>%
    filter(std.down == 1 & (rush == 1 | pass == 1)) %>%
    group_by(offense, defense) %>%
    summarize(
        rush.rte = sum(rush)/sum(std.down),
        success.rte = mean(success),
        rush.sr = mean(success[rush==1]),
        pass.sr = mean(success[pass==1]),
        exp.rte = mean(exp_play),
        exp.rte.rush = mean(exp_play[rush == 1]),
        exp.rte.pass = mean(exp_play[pass == 1]),
        havoc.rte = (sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                           Pass.Intercepted + Fumble + qb.hurry)) / sum(rush + pass)
    ) %>% ungroup()


## passing down box score stats
pass.down.stats <- advanced.stats2 %>%
  filter(pass.down == 1 & (rush == 1 | pass == 1)) %>%
  group_by(offense, defense) %>%
  summarize(
    rush.rte = sum(rush)/sum(pass.down),
    success.rte = mean(success),
    rush.sr = mean(success[rush==1]),
    pass.sr = mean(success[pass==1]),
    exp.rte = mean(exp_play),
    exp.rte.rush = mean(exp_play[rush == 1]),
    exp.rte.pass = mean(exp_play[pass == 1]),
    havoc.rte = (sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                       Pass.Intercepted + Fumble + qb.hurry)) / sum(rush + pass)
  ) %>% ungroup()


## rusher box score stats
rb.stats <- advanced.stats2 %>%
  filter(rush == 1) %>%
  group_by(offense, defense, rushfirst, rushlast) %>%
  summarise(
    carries = sum(rush),
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
    pass.attempts = sum(pass),
    receptions = sum(Completion),
    drop.rte = mean(Dropped),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()


## qb box score stats
qb.stats <- advanced.stats2 %>%
    filter(pass == 1) %>%
    group_by(offense, defense, passerfirst, passerlast) %>%
    summarise(
      dropbacks = sum(pass),
      completions = sum(Completion),
      incompletions = sum(pass[Sack==0]) - sum(Completion),
      sacks = sum(Sack),
      yards = sum(yardsgained),
      completion.rte = mean(Completion[Sack==0]),
      success.rte = mean(success),
      exp.rate = mean(exp_play)
    ) %>% ungroup()

## defensive individual stats
defensive.stats <- advanced.stats2 %>%
  filter((rush == 1 | pass == 1) & is.na(tackler.first) == 0) %>%
  group_by(offense, defense, tackler.first, tackler.last) %>%
  summarise(
    total.havoc.plays = sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + Pass.Intercepted +
                              Fumble + qb.hurry),
    tackles.solo = sum(Tackle.Solo),
    tackles.assist = sum(Tackle.Assist), 
    sack = sum(Sack.Solo + Sack.Assist),
    tfls = sum(TFL.Solo + TFL.Assist),
    hurries = sum(qb.hurry),
    fumbles = sum(Fumble),
    interceptions = sum(Pass.Intercepted),
  ) 


## rusher season stats
rb.season.stats <- advanced.stats2 %>%
  filter(rush == 1) %>%
  group_by(offense, rushfirst, rushlast) %>%
  summarise(
    plays = sum(rush),
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
    pass.attempts = sum(pass),
    receptions = sum(Completion),
    drop.rte = mean(Dropped),
    yards = sum(yardsgained),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()

## qb season stats
qb.season.stats <- advanced.stats2 %>%
  filter(pass == 1) %>%
  group_by(offense, passerfirst, passerlast) %>%
  summarise(
    dropbacks = sum(pass),
    completions = sum(Completion),
    incompletions = sum(pass[Sack==0]) - sum(Completion),
    yards = sum(yardsgained),
    completion.rte = mean(Completion[Sack==0]),
    success.rte = mean(success),
    exp.rate = mean(exp_play)
  ) %>% ungroup()

## defensive season stats 
defensive.season.stats <- advanced.stats2 %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(defense, tackler.first, tackler.last) %>%
  summarise(
    total.havoc.plays = sum(TFL.Solo + TFL.Assist + Pass.Broken.Up + 
                              Pass.Intercepted + Fumble + qb.hurry),
    tackles.solo = sum(Tackle.Solo),
    tackles.assist = sum(Tackle.Assist), 
    sack = sum(Sack.Solo + Sack.Assist),
    tfls = sum(TFL.Solo + TFL.Assist),
    fumbles = sum(Fumble),
    interceptions = sum(Pass.Intercepted),
  ) 

