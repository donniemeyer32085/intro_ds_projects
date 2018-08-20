library(tidyverse)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/")

#Moneyball: The power of sports analytics
mb <- read_csv("moneyball.csv")
glimpse(mb)
mb$Team <- factor(mb$Team) 
mb$Playoffs <- factor(mb$Playoffs) 

#how many games do you need win to make playoffs, the A's claimed 95 games
mb %>% 
  ggplot(aes(x = W, y = Team, color = Playoffs)) +
  geom_jitter() + 
  geom_vline(xintercept=95)

#how many runs did they need to score to make playoffs, they claimed 135 more runs than there oppenenets 
#during the regular season
#we need data to include years before 2002

mb_2002 <- filter(mb, Year < 2002)
glimpse(mb_2002)

#build a lm model to dredict the differece between runs scored and runs allowed to predict wins
mb_2002 <- mb_2002 %>% 
  mutate(diff_runs = RS-RA) %>% 
  select(Team:RA, diff_runs, everything())

wins_runs <- lm(W ~ diff_runs, data = mb_2002)
summary(wins_runs)

#our model is W = 80.881375 + 0.105766 (diff_runs)
#we expected that 95 wins to make playoffs
#so the model must be greater than 95, therefor,  95 >= 80.881375 + 0.105766 (diff_runs) OR diff_runs = 133


#lets use linear regression to predict runs scored

runs_score <- lm(RS ~ OBP + SLG + BA, data = mb_2002)
summary(runs_score)

#we notice that a higher batting percentage is predicting a lower runs scored
#this is counterintuitive, check for correlation
cor(mb_2002$BA, mb_2002$SLG)
mb_2002 %>% ggplot(aes(BA, SLG)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
#we find a high degree of correltion between these two variables, lets remove BA
runs_score_2 <- lm(RS ~ OBP + SLG, data = mb_2002)
summary(runs_score_2)

#predict runs scored for 2001
RS_2001 <- -804.63 + 2737.77*0.339 + 1584.91*0.430
RS_2001
#RA predicted to be 622
#we can now predict wins

W = 80.881375 + 0.105766 * (RS_2001 - 622)
W

