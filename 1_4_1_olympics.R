# In the 200m dash finals in the Olympics, 8 runners 
# compete for 3 medals (order matters).
# In the 2012 Olympics, 3 of the 8 runners were from Jamaica and 
# the other 5 were from different countries. 
# The three medals were all won by Jamaica:
# (Usain Bolt, Yohan Blake, and Warren Weir).

library("gtools")

# The number of different ways can the 3 medals be distributed across 8 runners
medal_distribution <- permutations(8,3)
nrow(medal_distribution)

# The number of different ways the three medals 
# can be distributed among the 3 runners from Jamaica
from_jamaica <- permutations(3,3)
nrow(from_jamaica)

# The probability that all 3 medals are won by Jamaica
nrow(from_jamaica) / nrow(medal_distribution)


# A Monte Carlo simulation on this vector representing the countries of 
# the 8 runners in this race

# Set the seed to 1 before running the loop
set.seed(1)

# Repeat this simulation 10,000 times.
B <- 10000

# Vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

# For each iteration of the Monte Carlo simulation, within a replicate() loop, 
# select 3 runners representing the 3 medalists 
# and check whether they are all from Jamaica. 

result <- replicate(B, {
  # select 3 random winners
  winners <- sample(runners, 3)
  
  # determine if they are all from Jamaica
  (length(unique(winners)) == 1) & ("Jamaica" %in% winners)
})

# The probability that all the runners are from Jamaica
mean(result)


