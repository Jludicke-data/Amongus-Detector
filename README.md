# Amongus-Detector
A simulation/data-analysis project of detecting imposters at stages of the game in Among Us

This is a model of Among Us which strips the game down to the core components of movement, killing of players by imposters, and a method for keeping track of those who are most suspicious (in this program's case, the closest individuals to the kill).

The model has a number of restrictions to it, the main one being that it is only as good as the player who uses it. If your deductions in Among Us are usually good, you will find this program to be very useful. It assumes it receives all the "correct" information and is able to give you a probability out that your deduction is correct (That your deductions are based on evidence and not guesses).

There are 3 separate files to this program, two of which you need in order to use it alongside an Among Us game.

1. AMONG US.R is a file that simulates 2000 games of Among Us with 2 imposters until convergence (only two players left that are imposters). It provides the data that the player interface file uses to calculate probabilities. It is limited by the fact it always has two imposters. I plan to update this in the future, but for now it is very useful at detecting imposters at 6 players left in the game (a key point in the game).

2. Data Analysis Among Us.R is a file used to visualize and analyze the data collected in the simulation stage. I used this to experiment and test what would be the best way to caculate the chance that someone was an imposter at a certain stage of the game. It is not essential to run this file to make the overall program work.

3. Player Inputs and Percentage chance.R is the player interface file. It allows players to navigate a menu and keep track of player probability throughout the game. They can enter player color or names to make it easier for them as well. It is a bit buggy, but I plan to update it to eventually be able to take all inputs.
  Key Highlights:
    1. Meeting: Allows users to input those who died during the round and assign points to the two most suspicious players.
    2. Check: Prints a dataframe that shows population level probabilities and player names/colors to input into the probability checker.
    3. Probability: Checks a player's chance that they are the imposter based on the number of players missing and population probability percentage.
    4. Exit: Enter this when the game is over.
