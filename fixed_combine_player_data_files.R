##After you've installed R Studio, type "install.packages("tidyverse")" and let it run.
library(tidyverse)


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
tackle <- read.csv("tackle.csv")
hurry <- read.csv("qb-hurry.csv")
pass.defend <- read.csv("pass-defended.csv")
forced.fumble <- read.csv("fumble-forced.csv")
fumble <- read.csv("fumble.csv")

## fix duplicate tackles problem
tackle.dup <- tackle %>%
    group_by(Game.Code) %>%
    filter(duplicated(Play.Number)) %>%
    select(Game.Code, Play.Number, Tackler2.Player.Code = Tackler.Player.Code)

tackle <- tackle %>%
    group_by(Game.Code) %>%
    distinct(Play.Number, .keep_all = TRUE) %>%
    mutate(Tackler2.Player.Code = 0)

tackle2 <- merge(tackle, tackle.dup, by = c("Game.Code", "Play.Number"), all.x=TRUE) 

tackle2 <- tackle2 %>% 
    select(-Tackler2.Player.Code.x) %>%
    rename(Tackler2.Player.Code = Tackler2.Player.Code.y)


## next step
rushing <- merge(pbp, rush, 
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all.x=TRUE)
all.off <- merge(rushing, pass,
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all.x=TRUE)
all.plays.off <- merge(all.off, teams, by.x = "Offense.Team.Code", by.y = "Team.Code", all.x=TRUE)
all.offense.plays <- merge(all.plays.off, teams, by.x = "Defense.Team.Code", by.y = "Team.Code", all.x=TRUE)

tackling1 <- merge(tackle2, player, by.x = "Tackler.Player.Code", by.y = "Player.Code", all.x=TRUE)
tackling2 <- merge(tackling1, player, by.x = "Tackler2.Player.Code", by.y = "Player.Code", all.x=TRUE) %>%
    rename(tackler1.last = Last.Name.x, tackler1.first= First.Name.x, 
           tackler2.last = Last.Name.y, tackler2.first = First.Name.y,
           tackler.team.code = Team.Code.x) %>%
    select(-Team.Code.y)



tackling <- merge(tackling2, player, by.x = "Tackled.Player.Code", by.y = "Player.Code", all.x=TRUE) %>%
  select(Offense.Team.Code = Team.Code.y, Defense.Team.Code = tackler.team.code, tackler1.last, tackler1.first, 
         tackler2.last, tackler2.first, tackled.last = Last.Name, tackled.first = First.Name, tackler1.position = Position.x,tackler2.position = Position.y,
         tackled.position = Position.y, Game.Code, Play.Number, Tackle.Solo, Tackle.Assist, TFL.Solo, TFL.Assist, TFL.Yards, Sack.Solo, 
         Sack.Assist, Sack.Yards)

tackling.a <- merge(tackling, teams, by.x = "Offense.Team.Code", by.y = "Team.Code", all.x=TRUE) 
tackling.b <- merge(tackling.a, teams, by.x = "Defense.Team.Code", by.y = "Team.Code", all.x=TRUE) 
tackling <- tackling.b %>% rename(offense = Name.x, defense = Name.y) %>%
    select(-(c(Defense.Team.Code, Offense.Team.Code)))


hurries.def <- merge(hurry, player, by.x = "QB.Hurrier.Player.Code", by.y = "Player.Code", all.x=TRUE)
hurries <- merge(hurries.def, player, by.x = "QB.Hurried.Player.Code", by.y = "Player.Code", all.x=TRUE) %>% 
  select(Offense.Team.Code = Team.Code, Defense.Team.Code = Team.Code.x, hurrier.first = First.Name.x, 
         hurrier.last=Last.Name.x, hurried.last = Last.Name.y, hurried.first = First.Name.y, 
         hurrier.position=Position.x, hurried.position=Position.y, Game.Code, Play.Number)

pass.def <- merge(pass.defend, player, by.x = "Defender.Player.Code", by.y = "Player.Code", all.x=TRUE)
pd <- merge(pass.def, player, by.x = "Passer.Player.Code", by.y = "Player.Code", all.x=TRUE) %>%
  select(Offense.Team.Code = Team.Code, Defense.Team.Code = Team.Code.x, 
         pass.defender.last = Last.Name.x, pass.defender.first = First.Name.x,
         Game.Code, Play.Number, Pass.Intercepted, Pass.Broken.Up)

f.fumble <- merge(forced.fumble, player, by.x="Forcer.Player.Code", by.y = "Player.Code", all.x=TRUE)
forced.fum <- merge(f.fumble, player, by.x="Fumbler.Player.Code", by.y="Player.Code", all.x=TRUE) %>%
  select(Offense.Team.Code = Fumbler.Team.Code, Defense.Team.Code = Forcer.Team.Code,
         fumble.forced.last = Last.Name.x, fumble.forced.first = First.Name.x, fumbler.first = First.Name.y, 
         fumbler.last = Last.Name.y, Game.Code, Play.Number)

defense1 <- merge(all.offense.plays, hurries, by = c("Game.Code", "Play.Number"), all.x=TRUE)
defense2 <- merge(defense1, pd, by = c("Game.Code", "Play.Number"), all.x=TRUE) %>%
  rename(offense = Name.x, defense = Name.y) %>%
  select(-c(Offense.Team.Code.x, Defense.Team.Code.x, Offense.Team.Code.y, Defense.Team.Code.y))
defense3 <- merge(defense2, tackling, by = c("Game.Code", "Play.Number"), all.x=TRUE) 
  
all.plays <- merge(defense3, forced.fum, by=c("Game.Code", "Play.Number"), all.x=TRUE)

plays_clean <- all.plays %>%
  replace(., is.na(.), 0) %>%
  rename(rush = Attempt.x,
         pass = Attempt.y,
         offense = offense.x, 
         defense = defense.x, 
         rush.yards = Yards.x,
         pass.yards = Yards.y,
         yardline = Spot,
         offense.conf = Conference.Code.x.x,
         defense.conf = Conference.Code.y.x,
         Rushing.Player.Code =  Player.Code) %>%
  mutate(yardsgained = rush.yards + pass.yards,
         touchdown = Touchdown.x + Touchdown.y,
         firstdown = X1st.Down.x + X1st.Down.y) %>%
  arrange(Game.Code, Play.Number) %>%
  select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
         tackler1.last, tackler1.first, tackler2.last, tackler2.first, tackled.last, tackled.first, tackler1.position, tackled.position, Tackle.Solo, Tackle.Assist, TFL.Solo,
         TFL.Assist, Sack.Solo, Sack.Assist, Sack.Yards, hurrier.first, hurrier.last, hurrier.position, hurried.first, hurried.last, hurried.position, 
         pass.defender.last, pass.defender.first, Pass.Intercepted, Pass.Broken.Up, 
         fumble.forced.last, fumble.forced.first, fumbler.first, fumbler.last, 
         Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code)
plays1 <-
  merge(plays_clean, player, by.x = "Passer.Player.Code", by.y = "Player.Code", all = TRUE)

plays12 <- plays1 %>%   select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf,Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
                               Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                               Fumble.Lost, Safety, Completion, Dropped, Interception, 
                               tackler1.last, tackler1.first, tackler2.last, tackler2.first, tackled.last, tackled.first, tackler1.position, tackled.position, Tackle.Solo, Tackle.Assist, TFL.Solo,
                               TFL.Assist, Sack.Solo, Sack.Assist, Sack.Yards, hurrier.first, hurrier.last, hurrier.position, hurried.first, hurried.last, hurried.position, 
                               pass.defender.last, pass.defender.first, Pass.Intercepted, Pass.Broken.Up, 
                               fumble.forced.last, fumble.forced.first, fumbler.first, fumbler.last, 
                               Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast = Last.Name, passerfirst = First.Name)

plays2 <-
  merge(plays12, player, by.x = "Receiver.Player.Code", by.y = "Player.Code", all = TRUE) 

plays22 <- plays2 %>% select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
                             Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                             Fumble.Lost, Safety, Completion, Dropped, Interception, 
                             tackler1.last, tackler1.first, tackler2.last, tackler2.first, tackled.last, tackled.first, tackler1.position, tackled.position, Tackle.Solo, Tackle.Assist, TFL.Solo,
                             TFL.Assist, Sack.Solo, Sack.Assist, Sack.Yards, hurrier.first, hurrier.last, hurrier.position, hurried.first, hurried.last, hurried.position, 
                             pass.defender.last, pass.defender.first, Pass.Intercepted, Pass.Broken.Up, 
                             fumble.forced.last, fumble.forced.first, fumbler.first, fumbler.last,
                             Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast, passerfirst, receiverlast = Last.Name, receiverfirst = First.Name)

plays <-
  merge(plays22, player, by.x = "Rushing.Player.Code", by.y = "Player.Code", all = TRUE)

plays <- plays%>% select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number,  Drive.Number, Play.Number, Clock, Down, 
                         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                         Fumble.Lost, Safety, Completion, Dropped, Interception, 
                         tackler1.last, tackler1.first, tackler2.last, tackler2.first, tackled.last, tackled.first, tackler1.position, tackled.position, Tackle.Solo, Tackle.Assist, TFL.Solo,
                         TFL.Assist, Sack.Solo, Sack.Assist, Sack.Yards, hurrier.first, hurrier.last, hurrier.position, hurried.first, hurried.last, hurried.position, 
                         pass.defender.last, pass.defender.first, Pass.Intercepted, Pass.Broken.Up, 
                         fumble.forced.last, fumble.forced.first, fumbler.first, fumbler.last,  
                         Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code,
                         passerlast, passerfirst, receiverlast, receiverfirst, rushlast = Last.Name, rushfirst = First.Name)


## Add home team and game date
game <- merge(game, teams, by.x = "Home.Team.Code", by.y="Team.Code") %>% select(Game.Code, Date, Home.Team = Name)
plays <- merge(plays, game, by="Game.Code" )


write.csv(plays, file ="playbyplay2019.csv")