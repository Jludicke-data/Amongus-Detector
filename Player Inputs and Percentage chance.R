##### Z-SCORE FUNCTION #####
conservative.score <- function(probability, missing_players, df){
  
  zscore <- (probability - mean(df$Probabilities[df$Missing.Players == missing_players], na.rm = T))/
               sd(df$Probabilities[df$Missing.Players == missing_players], na.rm = T)
  
  return(zscore)
}

##### Simulation Calculation Function #####
liberal.score <- function(probability, missing_players, df){
  
  #Finding NA's at current missing players
  num.na <- sum(is.na(df$Probabilities[df$Missing.Players == missing_players]))
  
  #Adding Less than Probability Value for players at current missing players
  less.p <- sum((df$Probabilities[(df$Player.Type == "Player") & 
                                    (df$Missing.Players == missing_players)] < probability), na.rm = T)
  
  #Standard Deviation for players at current missing players
  more.p <- sum((df$Probabilities[(df$Player.Type == "Player") & 
                                  (df$Missing.Players == missing_players)] >= probability), na.rm = T)
  
  #Caclulating the liberal score by dividing those more or equal to the value of probability at # players missing
  #by total population (including the dead). Use dead because they still make up a percentage chance of being
  #at that specific state by this time in the game
  sim_score <- more.p/(num.na + less.p + more.p)
  
  return(sim_score)
}
player.colors <- c()
for(i in 1:10){
  color <- readline(paste0("Enter the Color of Player: ", i, ": "))
  player.colors <- c(player.colors, color)
}
color.prob <- numeric(10)+0.1
input_df <- data.frame(player.colors, color.prob)
