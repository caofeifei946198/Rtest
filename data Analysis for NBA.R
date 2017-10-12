nba_shots <- read.csv('shot_logs.csv')
dim(nba_shots)#data dimmision
str(nba_shots)#data structure
library(tidyverse)#data analysis package
ggplot(nba_shots, aes(SHOT_DIST))+geom_histogram()#shot distance graph
ggplot(nba_shots, aes(CLOSE_DEF_DIST))+geom_histogram()+xlim(0,20)
ggplot(nba_shots, aes(SHOT_DIST, CLOSE_DEF_DIST
                      ))+geom_point(aes(color=factor(SHOT_RESULT)))
+geom_vline(xintercept=c(15,22), color='blue')
ggplot(nba_shots, aes(DRIBBLES,TOUCH_TIME))+
  geom_point(aes(color=factor(SHOT_RESULT)))+
  ylim(-10,20)
(home_avay <- nba_shots %>% group_by(LOCATION) 
  %>% summarise(PERCENTAGE=sum(FGM)/length(FGM)*100))
wins <- nba_shots %>% group_by(GAME_ID, LOCATION) %>% filter(W=='W', FGM==1)
ggplot(data = wins, aes(LOCATION, fill=factor(W)))+geom_bar()
#term important guys
first_quarter_guys <- nba_shots %>% filter(PERIOD==1,
            SHOT_DIST>5) %>% group_by(player_name) %>% 
  summarise(made=sum(FGM), points=sum(PTS),
            total_attempts = length(FGM),
            ave_distance=mean(SHOT_DIST)) %>% 
  mutate(percentage=made/total_attempts) %>% 
  arrange(desc(percentage)) %>% filter(total_attempts>200)
best_lst <- data.frame(first_quarter_guys)
best_lst
#best player
nba_shots %>% filter(SHOT_RESULT=='missed') %>%
  group_by(CLOSEST_DEFENDER) %>% summarise(GoodDefense = n()) %>%
  ungroup %>% arrange(desc(GoodDefense)) %>% head
#worst player
nba_shots %>% filter(SHOT_RESULT=='made') %>%
  group_by(CLOSEST_DEFENDER) %>% summarise(BadDefense = n()) %>%
  ungroup %>% arrange(desc(BadDefense)) %>% head
#feature select and filter
attach(nba_shots)
#filter touch_time < 0
nba_shots <- nba_shots[TOUCH_TIME > 0,]
#filter SHOT_RESULT
nba_shots <- nba_shots[,-14]
player_name <- as.character(player_name)
player_name <- factor(player_name)
#deal with game_clock var
GAME_CLOCK <- as.character(GAME_CLOCK)
GAME_CLOCK <- gsub(':', '.', GAME_CLOCK)
#the same with shot_colock
GAME_CLOCK <- as.numeric(GAME_CLOCK)

#select feature
nba_shots$Crucial_time <- 'No'
nba_shots$Crucial_time[nba_shots$GAME_CLOCK <= 1 &
                         nba_shots$PERIOD >= 4 & abs(nba_shots$FINAL_MARGIN)
                        <= 5] <- 'Yes'
nba_shots$Crucial_time <- factor(nba_shots$Crucial_time)
nba_shots$shot_type[nba_shots$DRIBBLES <= 1 & 
                      nba_shots$SHOT_DIST > 4] <- 'Catch and Shoot'
nba_shots$shot_type[nba_shots$DRIBBLES <= 1 & 
                      nba_shots$SHOT_DIST <= 4] <- 'Cut'
nba_shots$shot_type [nba_shots$DRIBBLES > 1 & 
                       nba_shots$SHOT_DIST <= 4] <- 'Drive'
nba_shots$shot_type [nba_shots$DRIBBLES > 4] <- 'ISO'
nba_shots$shot_type [nba_shots$DRIBBLES > 20] <- 'Long ISO'
nba_shots$shot_type [nba_shots$DRIBBLES <= 1 & 
                       nba_shots$PTS_TYPE == 3] <- 'Spot up Three'
nba_shots$shot_type <- factor(nba_shots$shot_type)
summary(nba_shots$shot_type)                       

#shot quality
nba_shots$shot_quality <- 'Open'
nba_shots$shot_quality[nba_shots$CLOSE_DEF_DIST <= 2] <- 'Tightly Contested'
nba_shots$shot_quality[nba_shots$CLOSE_DEF_DIST <= 3.5] <- 'Contested'
nba_shots$shot_quality[nba_shots$CLOSE_DEF_DIST > 6] <- 'Wide Open'
nba_shots$shot_quality <- factor(nba_shots$shot_quality)
summary(nba_shots$shot_quality)

#use rep to build empty list
OppPG <- rep(1, 473)
OppPPP <- rep(1, 473)
OppDist <- rep(1, 473)
ShotCont <- rep(1, 473)
RimDist <- rep(1, 473)
ShotsNear <- rep(1, 473)
#DefenderList <- nba_shots$DefenderLIst
FGM <- nba_shots$FGM
PTS <- nba_shots$PTS
CLOSE_DEF_DIST <- nba_shots$CLOSE_DEF_DIST
SHOT_DIST <- nba_shots$SHOT_DIST
CLOSEST_DEFENDER <- nba_shots$CLOSEST_DEFENDER
#defender consist 
for (i in 1:473) {
OppPG[i] = sum(FGM[CLOSEST_DEFENDER == DefenderList[i]]) / length(FGM[CLOSEST_DEFENDER == DefenderList[i]])
OppPPP[i] = sum(PTS[CLOSEST_DEFENDER == DefenderList[i]]) / length(FGM[CLOSEST_DEFENDER == DefenderList[i]])
OppDist[i] = sum(CLOSEST_DEF_DIST[CLOSEST_DEFENDER == DefenderList[i]]) / length(FGM[CLOSEST_DEFENDER == DefenderList[i]])
ShotsCont[i] = length(FGM[CLOSEST_DEFENDER == DefenderList[i]
                          & CLOSE_DEF_DIST <= 3.5])
RimDist[i] = sum(SHOT_DIST[CLOSEST_DEFENDER == DefenderList[i]])
}

#build nba_def
nba_def <- data.frame(DefenderList, OppPG, OppPPP, OppDist,
                      ShotCont, RimDist, ShotsNear)
summary(nba_def)

#better player defender 
ggplot(data = nba_def[ShotCont >= 150 & OppPG <=
                        .41,], aes(OppPG, OppPPP)
       ) + geom_point(colour = 'black', size = 2)+geom_point(
         aes(colour = DefenderList)) + geom_segment(method = 'lm',
                              se = FALSE) + geom_text(aes(label = DefenderList),
                                                      size = 1, vjust = 0, check_overlap = TRUE) + guides(
                    colour = FALSE) + labs(x = 'Opponent Field Goal Percentage',
                                           y = 'Opponent Points per Possesion')

#the correlation of playerDist and per score
ggplot(Eliterdef(OppDist, OppPPP)) + geom_point(colour = 'black', size = 2)+geom_point(
  aes(colour = DefList)) + geom_smooth(method = 'lm',
                                             se = FALSE) + geom_text(aes(label = DefenderList),
                                                                     size = 3, vjust = 0, check_overlap = TRUE) + guides(
                                                                       colour = FALSE) + labs(x = 'Opponent Field Goal Percentage',
                                                                                              y = 'Opponent Points per Possesion')

                                                      
  