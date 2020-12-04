##### DATA CLEANING #####

#REMOVING 0's FROM POLLUTING DATASET
raw_data[raw_data == 0] <- NA

#Sample Size
sum(is.na(raw_data[1,5,]))

#Creating Data Frame to Test data

#Extract Probability values to build the data frame and player type vector
p1.dat <- raw_data[1,,]
p2.dat <- raw_data[2,,]
p3.dat <- raw_data[3,,]
p4.dat <- raw_data[4,,]
p5.dat <- raw_data[5,,]
p6.dat <- raw_data[6,,]
p7.dat <- raw_data[7,,]
p8.dat <- raw_data[8,,]
p9.dat <- raw_data[9,,]
p10.dat <- raw_data[10,,]

#Build the Player Type Vector for the Data Frame
all_player_prob <- vector("character", length(p1.dat)*10)
all_player_prob[1:(length(p1.dat)*8)] <- "Player"
all_player_prob[((length(p1.dat)*8)+1):length(all_player_prob)] <- "Imposter"

#Assign Column names for the data frame and making the missing players vector
Probabilities <- c(p1.dat, p2.dat, p3.dat, p4.dat, p5.dat, p6.dat, p7.dat, p8.dat, p9.dat, p10.dat)
Player.Type <- all_player_prob
Missing.Players <- rep(0:8, (length(Probabilities)/length(raw_data[1,,1])))
Player.Number <- c(rep(1, length(p1.dat)), rep(2, length(p2.dat)), rep(3, length(p3.dat)), rep(4, length(p4.dat)),
                   rep(5, length(p5.dat)), rep(6, length(p6.dat)), rep(7, length(p7.dat)), rep(8, length(p8.dat)),
                   rep(9, length(p9.dat)), rep(10, length(p10.dat)))

#Building the Actual Data Frame
#Maybe make player #?
amongus.df <- data.frame(Player.Number, Player.Type, Missing.Players, Probabilities, stringsAsFactors = F)

#Generating Normal Distributions

x.axis.probabilities <- amongus.df$Probabilities[(amongus.df$Missing.Players == 4)]

Player_distribution <- dnorm(amongus.df$Probabilities[(amongus.df$Player.Type == "Player") & (amongus.df$Missing.Players == 4)], 
              mean = mean(amongus.df$Probabilities[(amongus.df$Player.Type == "Player") & (amongus.df$Missing.Players == 4)], na.rm = T), 
              sd = sd(amongus.df$Probabilities[(amongus.df$Player.Type == "Player") & (amongus.df$Missing.Players == 4)], na.rm = T))

Imposter_distribution <- dnorm(amongus.df$Probabilities[(amongus.df$Player.Type == "Imposter") & (amongus.df$Missing.Players == 4)],
                               mean = mean(amongus.df$Probabilities[(amongus.df$Player.Type == "Imposter") & (amongus.df$Missing.Players == 4)], na.rm = T),
                               sd = sd(amongus.df$Probabilities[(amongus.df$Player.Type == "Imposter") & (amongus.df$Missing.Players == 4)], na.rm = T))

#plot(amongus.df$Probabilities[amongus.df$Missing.Players == 4], test)
#amongus.df$Probabilities[(amongus.df$Player.Type == "Imposter") & (amongus.df$Missing.Players == 4)]


#Plots of Normal Distributions

player_plot <- plot(amongus.df$Probabilities[(amongus.df$Player.Type == "Player") & (amongus.df$Missing.Players == 4)], Player_distribution,
     main = "Player Distribution in Among Us", xlab = "Probability for Players at 6 Players Remaining", ylab = "Probability Denisty",
     type = "p", col = "red")

imposter_plot <- plot(amongus.df$Probabilities[(amongus.df$Player.Type == "Imposter") & (amongus.df$Missing.Players == 4)], Imposter_distribution,
     main = "Imposter Distribution in Among Us", xlab = "Probability for Imposters at 6 Players Remaining", ylab = "Probability Denisty",
     type = "p", col = "blue")


#initial T-test to test for differences in player and imposter means
t.test(amongus.df$Probabilities[amongus.df$Missing.Players == 4] ~ amongus.df$Player.Type[amongus.df$Missing.Players == 4])

#Look for z-scores

player_1_data <- raw_data[1,5,]
player_2_data <- raw_data[1:8,5,]
imposter_1_data <- raw_data[9,5,]
imposter_2_data <- raw_data[10,5,]


#intitial data viewing
hist(player_1_data, prob = T, breaks = seq(0, 0.45, 0.05), xlim = c(0,0.5), xlab = "Probability")
hist(player_2_data, prob = T, breaks = seq(0, 0.45, 0.05), xlim = c(0,0.5), xlab = "Probability")

hist(imposter_1_data, prob = T, breaks = seq(0, 0.45, 0.05), xlim = c(0,0.5), xlab = "Probability")
hist(imposter_2_data, prob = T, breaks = seq(0, 0.45, 0.05), xlim = c(0,0.5), xlab = "Probability")



#plot(Probabilities ~ Player.Type, data = amongus.df)
#summary(aov(Probabilities ~ Player.Type, data = amongus.df))
#summary(aov(Probabilities[amongus.df$Missing.Players == 4] ~ Player.Type[amongus.df$Missing.Players == 4], data = amongus.df))
#plot(Probabilities[amongus.df$Missing.Players == 4] ~ Player.Type[amongus.df$Missing.Players == 4], data = amongus.df)