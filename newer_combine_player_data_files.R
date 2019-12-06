getwd()
work_dir <- "C:/Users/eqa47693/Desktop/CFB/ssa_data/ssa_2019"
setwd(work_dir)

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

rushing <- merge(pbp, rush, 
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all=TRUE)
all.off <- merge(rushing, pass,
                 by.x = c("Game.Code", "Play.Number", "Offense.Team.Code"),
                 by.y = c("Game.Code", "Play.Number", "Team.Code"), all=TRUE)
all.plays.off <- merge(all.off, teams, by.x = "Offense.Team.Code", by.y = "Team.Code", all=TRUE)

all.plays <- merge(all.plays.off, teams, by.x = "Defense.Team.Code", by.y = "Team.Code", all=TRUE)

plays_clean <- all.plays %>%
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
  select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
         Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code)
plays1 <- merge(plays_clean, player, by.x = "Passer.Player.Code", by.y = "Player.Code", all = TRUE)

plays12 <- plays1 %>%   select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf,Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
                               Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                               Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast = Last.Name, passerfirst = First.Name)

plays2 <- merge(plays12, player, by.x = "Receiver.Player.Code", by.y = "Player.Code", all = TRUE) 

plays22 <- plays2 %>% select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number, Drive.Number, Play.Number, Clock, Down, 
                             Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                             Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code, passerlast, passerfirst, receiverlast = Last.Name, receiverfirst = First.Name)

plays <- merge(plays22, player, by.x = "Rushing.Player.Code", by.y = "Player.Code", all = TRUE)

plays <- plays %>% select(Game.Code, offense, offense.conf, Offense.Points, defense, defense.conf, Defense.Points, Period.Number,  Drive.Number, Play.Number, Clock, Down, 
                         Distance, yardline, Play.Type, yardsgained, rush, pass, touchdown, firstdown, Sack, Fumble,
                         Fumble.Lost, Safety, Completion, Dropped, Interception, Passer.Player.Code, Receiver.Player.Code, Rushing.Player.Code,
                         passerlast, passerfirst, receiverlast, receiverfirst, rushlast = Last.Name, rushfirst = First.Name)



write.csv(plays, file ="playbyplay2019")