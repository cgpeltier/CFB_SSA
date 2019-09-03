## rb advanced stats

rb_stats <- plays %>%
    filter(rush == 1) %>%
    group_by(offense, rushfirst, rushlast) %>%
    summarise(
      plays = n(),
      sr = mean(success),
      exp_rate = mean(exp_play),
      stuff_rate = mean(stuffed_run),
      opp_rate = mean(opp_rate_run),
      short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
    )

## wr advanced stats
wr_stats <- plays %>%
  filter(pass == 1) %>%
  group_by(offense, receiverfirst, receiverlast) %>%
  summarise(
    plays = n(),
    sr = mean(success),
    exp_rate = mean(exp_play)
  )




