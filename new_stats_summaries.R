library(tidyverse)

## Create new variables
plays <- plays %>%
    mutate(success = ifelse(((yardsgained >= (.5 * Distance)) & Down == 1) |
                              ((yardsgained >= (.7 * Distance)) & Down == 2) |
                              ((yardsgained >= Distance) & Down == 3) |
                              ((yardsgained >= Distance) & Down == 4), 1, 0),
          rz_play = ifelse((yardline <= 20), 1, 0),
          so_play = ifelse((yardline <=40 | touchdown == 1 ), 1, 0),
          stuffed_run = ifelse((rush == 1 & yardsgained <=0), 1, 0),
          opp_rate_run = ifelse((rush == 1 & yardsgained >= 4), 1, 0),
          exp_play = ifelse((yardsgained >= 13), 1, 0),
          short_rush_attempt = ifelse(Distance <= 2 & rush == 1, 1, 0),
          short_rush_success = ifelse(Distance <= 2 & rush == 1 & yardsgained >= Distance, 1, 0)
    )


## new box score stats
box.score.stats<- plays %>%
  group_by(offense, defense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
    yards = sum(yardsgained),
    yardsperplay = mean(yards),
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
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  )

 

## new all season stats
all.stats <- plays %>%
  group_by(offense) %>%
  summarize(
    ypp = mean(yardsgained),
    plays = n(), 
    yards=sum(yardsgained),
    yardsperplay = mean(yards),
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
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  )



