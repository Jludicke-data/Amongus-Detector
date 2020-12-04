#FUNCTION 1
##### WORLD BUILDER #####
map <- function(dimensions, players){
  
  #dimensions: a vector of length 2 to make the map
  #players: how many players are playing (minimum 5)
  #always 2 imposters
  #player_positions_array: an easy way to update player positions rather than run through a matrix later
  
  if(players <= 4){
    break
    
  }else{
    #fixing numbers to normalize games and assign imposters easily
    players <- players - 2
    imposters <- 2
    world_space <- dimensions[1] * dimensions[2]
    world_vector <- c(numeric(world_space - players - imposters), numeric(players)+1, numeric(imposters)+2)
    
    #3D to return 1 easy tracking system
    world <- array(data = numeric(world_space), dim = c(dimensions[1], dimensions[2], 2))
    world[,,1] <- array(data = (sample(world_vector, world_space, replace = F)), dim = dimensions)

  }
  return(world)
}


#FUNCTION 2
##### RECORDING PROBABILITY AND CLOSEST INDIVIDUALS #####

prob <- function(positions_array, kill_coordinates){
  
    #Records the probability of the individual that died for later use.
    for(e in 1:length(positions_array[,1,1])){
      if(positions_array[e,,1][1] == kill_coordinates[1] && positions_array[e,,1][2] == kill_coordinates[2]){
        prob_to_be_added <- positions_array[e,,2]
        break
      }
    }
    #divided by 2 to be divided between two closest individuals and delete the row that the person was killed in
    prob_to_be_added <- prob_to_be_added/2
    dimensions_new <- dim(positions_array) - c(1,0,0)
    positions_array <- array(data = positions_array[-e,,], dim = c(dimensions_new))
    
    #matrix to subtract and perform distance equation on all players coords
    kill_coords_array <- array(data = c(numeric(length(positions_array[,,1])/2) + kill_coordinates[1], 
                                         numeric(length(positions_array[,,1])/2) + kill_coordinates[2]), 
                                dim = c(dimensions_new[1], dimensions_new[2]))

    #get the distance away from the kill coordinates and record the two closest people
    distance <- (kill_coords_array - positions_array[,,1])**2
    distance <- sqrt(distance[,1] + distance[,2])
    
    #Now find the two Lowest distances
    lowest <- 1000000
    second_lowest <- 1000000
    counter_index <- 1
    
    for(i in 1:length(distance)){
      
      if(distance[i] < lowest){
        second_lowest <- lowest
        lowest <- distance[i]
      }
      else if(distance[i] > lowest && distance[i] < second_lowest){
        second_lowest <- distance[i]
        counter_index <- i
      }
    }
    #Row numbers of probabilities to alter
    distance <- c(which.min(distance), counter_index)
    positions_array[distance[1],1,2] <- positions_array[distance[1],1,2] + prob_to_be_added[1]
    positions_array[distance[2],1,2] <- positions_array[distance[2],1,2] + prob_to_be_added[1]
    
    return(positions_array)
}




#FUNCTION 3
##### MOVEMENT OF INDIVIDUALS #####

#import the world and then update it with new positions

move <- function(positions_array, world){
  #Runs through the player positions array and updates the world information, this is player movement only, not imposter
  
  #####PLAYER MOVEMENT#####
  player_moves_correct <- FALSE
  while(player_moves_correct == FALSE){
    r_num <- 1
    c_num <- 1
    for(g in 1:((length(positions_array[,1,1])-2)*2)){
      
      #for if they x-coordinate is at the boundary we can adjust that one accordingly
      if(c_num == 1){
        if(positions_array[r_num,c_num,1] == 1 | positions_array[r_num,c_num,1] == length(world[,1,1])){
          
          if(positions_array[r_num,c_num,1] == 1){
            positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,0,1))
          }
          if(positions_array[r_num,c_num,1] == length(world[1,,1])){
            positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,-1,0))
          }
          
        }else{
          positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,-1,1))
        }
      }
      #For if the Y-coordinate is at the boundary we can adjust that one accordingly
      if(c_num == 2){
        
        if(positions_array[r_num,c_num,1] == 1 | positions_array[r_num,c_num,1] == length(world[1,,1])){
          
          if(positions_array[r_num,c_num,1] == 1){
            positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,0,1))
          }
          if(positions_array[r_num,c_num,1] == length(world[,1,1])){
            positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,-1,0))
          }
          
        }else{
          positions_array[r_num,c_num,1] <- positions_array[r_num,c_num,1] + round(runif(1,-1,1))
        }
      }
      
      if(c_num == length(positions_array[1,,1])){
        r_num <- r_num + 1
        c_num <- 0
      }
      c_num <- c_num + 1
    }
    
    ##### IMPOSTER MOVEMENT ######
    
    for(i in 1:2){
      #Takes a player position array dimensions minus both imposters and builds coordinates of the imposter in a new array
      imposter_move_array <- array(data = c(numeric(length(positions_array[,1,1])-2) + 
                                              positions_array[(length(positions_array[,1,1])-2+i),1,1],
                                            numeric(length(positions_array[,1,1])-2) + 
                                              positions_array[(length(positions_array[,1,1])-2+i),2,1]), 
                                   dim = c(length(positions_array[,1,1])-2, length(positions_array[1,,1])))
      
      distance <- (imposter_move_array - positions_array[1:(length(positions_array[,1,1])-2),,1])**2
      distance <- sqrt(distance[,1] + distance[,2])
      
      #Make a vector with element 1 being player closest to first imposter, and second being closest to second imposter
      
      if(i == 1){
        closest_players <- c(which.min(distance))
      }else{
        closest_players <- c(closest_players, which.min(distance))
      }
    }
    
    e <- 1
    for(e in 1:length(closest_players)){
      #Index for first, then second imposter in x and y positions and find out whether to move negatively or positively
      if(positions_array[closest_players[e],1,1] - positions_array[length(positions_array[,1,1])-2+e,1,1] < -1){
        move_x <- -1
      }
      else if(positions_array[closest_players[e],1,1] - positions_array[length(positions_array[,1,1])-2+e,1,1] > 1){
        move_x <- 1
      }else{
        move_x <- 0
      }
      if(positions_array[closest_players[e],2,1] - positions_array[length(positions_array[,1,1])-2+e,2,1] < -1){
        move_y <- -1
      }
      else if(positions_array[closest_players[e],2,1] - positions_array[length(positions_array[,1,1])-2+e,2,1] > 1){
        move_y <- 1
      }else{
        move_y <- 0
      }
      #now apply movement to the player positions graph
      positions_array[length(positions_array[,1,1])-2+e,1,1] <- 
        positions_array[length(positions_array[,1,1])-2+e,1,1] + move_x
      
      positions_array[length(positions_array[,1,1])-2+e,2,1] <- 
        positions_array[length(positions_array[,1,1])-2+e,2,1] + move_y
      
      if(positions_array[length(positions_array[,1,1])-1,1,1] == positions_array[length(positions_array[,1,1]),1,1] &&
         positions_array[length(positions_array[,1,1])-1,2,1] == positions_array[length(positions_array[,1,1]),2,1]){
        #Use this to prevent overlap of imposters and infinite looping, it will just make sure the imposter stays still
        #to prevent overlap
        positions_array[length(positions_array[,1,1])-2+e,1,1] <- 
          positions_array[length(positions_array[,1,1])-2+e,1,1] - move_x
        
        positions_array[length(positions_array[,1,1])-2+e,2,1] <- 
          positions_array[length(positions_array[,1,1])-2+e,2,1] - move_y
      }
    }
    player_moves_correct <- compare_player_spots(positions_array)
  }
  return(positions_array)
}

#FUNCTION 4
##### MAKES SURE 2 PLAYERS CANNOT LAND ON THE SAME SPACE #####
compare_player_spots <- function(positions_array){
  counter <- 1
  for(i in 1:(length(positions_array[,1,1])-1)){
    if(positions_array[i,1,1] == positions_array[counter+1,1,1] && positions_array[i,2,1] == positions_array[counter+1,2,1]){
      return(FALSE)
    }
    if(counter == 9){
      break
    }
    counter <- counter + 1 
  }
  return(TRUE)
}


#FUNCTION 5
##### UPDATES THE WORLD INFORMATION #####

update <- function(positions_array, world){
  #Reset the world to all 0's
  world <- array(data = numeric(length(world)), dim = dim(world))
  #Now Add the coordinates of the players
  
  for(p in 1:length(positions_array[,1,1])){
    current_player <- positions_array[p,,1]
    if(p <= (length(positions_array[,1,1])-2)){
      world[current_player[1],current_player[2],1] <- 1
    }else{
      world[current_player[1],current_player[2],1] <- 2
    }
    
  }
  return(world)
}



#FUNCTION 6
##### CHECK AROUND IMPOSTER FOR PLAYERS TO KILL #####

check_kill <- function(positions_array, world, storage_grid, storage_index){
  
  #minus 1 and 0 because last two positions in positions array are always them
  #always 2 imposters to check
  #Wraps world in NA's to not exceed bounds of scan
  #adjust the positions array to match with player scans
  
  count <- 1
  for(g in 1:2){
    world.pad <- rbind(NA, cbind(NA, world[,,1], NA), NA)
    positions_array.pad <- positions_array + 1
    imposter_check <- positions_array.pad[length(positions_array.pad[,1,1])-count,,1]
    #North, Northwest, West, Southwest, South, Southeast, East, Northeast
    surroundings <- c(world.pad[imposter_check[1]-1,imposter_check[2]],
                      world.pad[imposter_check[1]-1,imposter_check[2]-1],
                      world.pad[imposter_check[1],imposter_check[2]-1],
                      world.pad[imposter_check[1]+1,imposter_check[2]-1],
                      world.pad[imposter_check[1]+1,imposter_check[2]],
                      world.pad[imposter_check[1]+1,imposter_check[2]+1],
                      world.pad[imposter_check[1],imposter_check[2]+1],
                      world.pad[imposter_check[1]-1,imposter_check[2]+1])
    
    #Minus 1 to the array to fix padding issues when sending to other function
    
    array_surroundings <- t(array(data = c(imposter_check[1]-1,imposter_check[2],
                                         imposter_check[1]-1,imposter_check[2]-1,
                                         imposter_check[1],imposter_check[2]-1,
                                         imposter_check[1]+1,imposter_check[2]-1,
                                         imposter_check[1]+1,imposter_check[2],
                                         imposter_check[1]+1,imposter_check[2]+1,
                                         imposter_check[1],imposter_check[2]+1,
                                         imposter_check[1]-1,imposter_check[2]+1), dim = c(2,8))) - 1
    for(e in 1:length(surroundings)){
      if(is.na(surroundings[e]) == T){
        #This exists to stop an error where NA was put into if statement
      }
      else if(surroundings[e] == 1){
        player_killed <- array_surroundings[e,]
        #update the kills as go
        positions_array <- prob(positions_array, c(player_killed))
        world <- update(positions_array, world)
        storage_grid <- prob_storage(positions_array, storage_grid, storage_index)
        break
      }
    }
    #adjust to last imposter
    count <- count - 1
  }
  return(list(positions_array, storage_grid))
}


#FUNCTION 7
##### CHECK FOR ONLY IMPOSTERS LEFT#####
#Checks to see whether the game is over or not. When 2 imposters left, game is over.

is_over <- function(positions_array){
  
  if(length(positions_array[,1,1]) == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


#FUNCTION 8
##### STORE DATA FROM EACH KILL AND PROBABILITIES #####

prob_storage <- function(positions_array, storage_grid, storage_index){
  missing_players <- length(storage_grid[,1,1]) - length(positions_array[,1,2])
  for(i in 1:length(positions_array[,1,2])){
    storage_grid[positions_array[i,2,2], missing_players + 1, storage_index] <- positions_array[i,1,2]
  }
  return(storage_grid)
}


##### MAIN #####

#Set up a matrix to store all of our data for analysis. Pre-allocate and hard code to save memory and increase runtime
#9 columns for the amount of players remaining: Column 1 is 0 players missings, col 2 is 1 missing and so on until only 2 left
raw_data <- array(data = numeric(10*8*1000), dim = c(10,9,2000))

#MAIN LOOP FOR DATA COLLECTION
for(y in 1:2000){
  world <- map(c(10,10), 10)
  #-2 because 2 imposters
  player_positions_array <- array(data = numeric(2*(sum(world[,,1])-2)), dim = c(sum(world[,,1])-2, 2, 2))
  
  #Assigning initial Probability in 3rd dimension for all players (will all be the same regardless)
  player_positions_array[1:length(player_positions_array[,1,2]),1,2] <- numeric(length(player_positions_array[,1,2]))+
    (1/length(player_positions_array[,1,2]))
  player_positions_array[1:length(player_positions_array[,2,2]),2,2] <- numeric(length(player_positions_array[,2,2]))+
    c(1:length(player_positions_array[,2,2]))
  
  #Obtaining initial positions
  player_num <- 1
  imposter_num <- 1
  r_num <- 1
  c_num <- 1
  #Loop through the array and obtain initial coordinates of players. Each one will be unique by assigning a probability
  #Position plus probability will make that player unique
  for(i in 1:length(world[,,1])){
    
    if(world[r_num, c_num, 1] != 0){
      #make a way for imposters always to be in last two slots of player positions array for ease of recording
      if(world[r_num, c_num, 1] == 2){
        #puts imposter into last two slots
        player_positions_array[(length(player_positions_array[,1,1])-imposter_num),,1] <- c(r_num, c_num)
        imposter_num <- 0
      }else{
        player_positions_array[player_num,,1] <- c(r_num, c_num)
        player_num <- player_num + 1
      }
      
    }
    if(c_num == length(world[1,,1])){
      c_num <- 1
      r_num <- r_num + 1
      
    }else{
      c_num <- c_num + 1
    }
    
  }
  #Update Data before any killing occurs
  raw_data <- prob_storage(player_positions_array, raw_data, y)
  
  game_finished <- FALSE
  
  #Main Game Loop
  while(game_finished == FALSE){
    
    #Move and update
    player_positions_array <- move(player_positions_array, world)
    world <- update(player_positions_array, world)
    
    #Check for imposter ability to kill (1 per round), store the data and update
    positions_and_data <- check_kill(player_positions_array, world, raw_data, y)
    player_positions_array <- positions_and_data[[1]]
    raw_data <- positions_and_data[[2]]
    world <- update(player_positions_array, world)
    
    #Check to see if the game has converged for imposters
    game_finished <- is_over(player_positions_array)
  }
}

