##### Z-SCORE FUNCTION #####
conservative.score <- function(probability, missing_players, df){
  
  zscore <- (probability - mean(df$Probabilities[df$Missing.Players == missing_players], na.rm = T))/
               sd(df$Probabilities[df$Missing.Players == missing_players], na.rm = T)
  zscore <- pnorm(zscore) * 100
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
  sim_score <- sim_score * 100
  return(sim_score)
}

##### MAIN #####

#Setup

Player.Name <- c()
for(i in 1:10){
  color <- readline(paste0("Enter the Color or Name of Player: ", i, ": "))
  Player.Name <- c(Player.Name, color)
}
Population.Probability <- numeric(10)+0.1
input_df <- data.frame(Player.Name, Population.Probability, stringsAsFactors = F)

#Player Interactivity Section

cat(paste("Main Menu",
          "Type Meeting for reporting dead and assigning blame at meetings",
          "Type Check for a certain player's probability that they are the imposter",
          "Type Probability to check population probabilities for colors or names to input into the Check menu",
          "Type Exit if the game is over", sep = "\n"))

flag <- TRUE
while(flag == TRUE){
  print("Meeting, Check, Probability, Exit")
  choice <- readline("What would you like to do: ")
  
  if(choice == "Meeting" | choice == "meeting"){
    answer <- as.integer(readline("How many players are died between last meeting and this one? 
              Type Back to go back to the main menu: "))
    #Minus 3 because thats the max amount of players that can be in a game and not lose
    if((answer %in% c(1:(length(input_df[,1])-3))) == TRUE){
      
      eliminated_names <- c()
      for(i in 1:answer){
        eliminated <- readline("What is the Color/Name of one of the eliminated people: ")
        eliminated_names <- c(eliminated, eliminated_names)
      }
      
      counter <- 1
      color_counter <- 1
      meeting <- T
      
      while(meeting == T){
        
        if(input_df$Player.Name[color_counter] == eliminated_names[counter]){
          
          p.color1 <- readline(paste0("What player would you like to put a point against for the killing of ", 
                                      eliminated_names[counter], ": "))
          p.color2 <- readline(paste0("What other player would you like to put a point against for the killing of ", 
                                      eliminated_names[counter], ": "))
          
          prob_to_be_added <- input_df[color_counter, 2]/2
          #Take the colors/names players selected and add half the probability of the player eliminated to each
          input_df[,2] <- input_df[,2] + (((input_df$Player.Name == p.color1) | 
                                          (input_df$Player.Name == p.color2)) * prob_to_be_added)
          
          dimensions_new <- dim(input_df) - c(1,0)
          #Eliminate the row with the player that was killed in game and resize the dataframe
          input_df <- input_df[-color_counter,]
          counter <- counter + 1
          color_counter <- 0
          
          if(counter == (answer+1)){
            meeting <- F
          }
        }
        color_counter <- color_counter + 1
      }
      print("The population probabilities have been updated")
      print(input_df)

    }else{
      print("You have chosen back, or not chosen a valid number of missing players")
    }
  }
  
  else if(choice == "Check" | choice == "check"){
    print(input_df)
  }
  
  else if(choice == "Probability" | choice == "probability"){
    answer <- as.double(readline("Which population probability percentage would you like to check? 
              (type it exactly as shown in the Check table) or would you like to go back: "))
    
    if((0 < answer) & (answer < 1)){
      eliminated <- as.integer(readline("How many players are dead in the game: "))
      conservative <- conservative.score(answer, eliminated, amongus.df)
      #liberal <- liberal.score(answer, eliminated, amongus.df)
      cat(paste0("A conservative estimate of a player with this population probability at this stage
                   in the game has a ", conservative, "% chance of being an Imposter."))
      
      #print(paste0("A liberal estimate of a player with this population probability at this stage
      #             in the game has a ", liberal, "% chance of being an Imposter."))
    }else{
      print("Your answer wasn't valid or you chose to go back")
    }
  }
  else if(choice == "Exit" | choice == "exit"){
    flag <- FALSE
  }
  
}






















