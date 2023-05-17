library("ggplot2")
library("dplyr")

#TASK 1
#roll the die function

two.dice <- function() {
  dice <- sample(1:6, size=1) + sample(1:6, size=1)
  return(dice)
}

two.dice()
sims <- replicate(10000, two.dice())
table(sims)


#TASK 2
#ggplot2
library("ggplot2")
rolls <- replicate(10000, two.dice())
qplot(rolls, binwidth=1)

#TASK 3
#The dice on their own are fair, but their sums do not have equal probability

#TASKS 4 & 5
simulation <- function()
{
  unfair_die_roll <- sample (x  = 1:6, size = 1, replace = TRUE, prob = c(1/8, 1/8, 1/8, 1/8,1/8,3/8)) + sample(x  = 1:6, size = 1, replace = TRUE, prob = c(1/8, 1/8, 1/8, 1/8,1/8,3/8))
return(unfair_die_roll)
  }

simulation()



#ggplot2
rolls_2 <- replicate(10000, simulation())
qplot(rolls_2, binwidth=1)
rolls_2
#The dice are unfair because their sum is not normally distributed
#even though we have a fairly large number of simulations 

#TASK 6
#Probabilities of basic outcomes (fair die)

#Here we demonstrated that the probability of a sum of 2 and 7 

simulation_data=as.data.frame(sims)


values=list()

for (i in c(2:12))
{
  values[i]=count(filter(simulation_data,sims==i))/count(simulation_data)  
}


sum(simplify2array(values[2:12]))
values[2] # # Here we show the probability of the sum being equal to two
values[7] # Here we show the probability of the sum being equal to seven

#TASK 7
#If we were in a casino...
simulation_2 <- function()
{
fair_die_roll <- sample (x  = 1:6, size = 1, replace = TRUE, prob = NULL) + sample(x  = 1:6, size = 1, replace = TRUE, prob = NULL)
return(fair_die_roll)
}


#table
x <- replicate(1000,simulation_2())
table(x)


#ggplot2
qplot(x, binwidth=1)

#our casino simulation (the number of times we got the sum of 7)

casino <- function(x)
{
sim2_probs <- replicate(1000,simulation_2())
condition <- sum(sim2_probs == 7) 
return(condition)
}
casino() 




#Simulation 
gamblling=sample(c(1,-0.15),1000,prob = c(as.numeric(values[7]),1-as.numeric(values[7])),replace=TRUE)
qplot(gamblling)

wins=1000*as.numeric(values[7])*1-1000*(1-as.numeric(values[7]))*0.15
wins

#As showed earlier, the probability of the sum of 7 is approximately 0.167
#If played 1000 times, we would get the sum of 7, 167 times 
sumofseven <- (1000/100*16.7)
sumofseven
#We would win 167x 1 euro = 167 euros
winnings <- 167*1
winnings
#We would lose 833 times (1000-167=833)
losings <- 1000-167
losings
#Casino would get 15 cents for each of the 833 wins (124,95 euros)
casino_winnings <- 833*0.15
casino_winnings
#Our gain would be 167-124.95= 42,05 euros
gain <- 167-124.95
gain
#But when we subtract our original bet of 150e... we end up losing a total of 107.95 euros
total_loss <- 42.05-150
total_loss
#Conclusion: This is not a good deal for us

