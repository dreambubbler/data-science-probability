library(tidyverse)

sample_size <- 10000
test_mean <- 20.9
test_sd <- 5.7

set.seed(16, sample.kind = "Rounding")

act_scores <- rnorm(sample_size, test_mean, test_sd)

# determine the mean of the act_scores sample
mean(act_scores)

# determine the standard deviation of the act_scores sample
sd(act_scores)

# determine the number of perfect scores in the sample
perfect_scores <- act_scores >= 36
sum(perfect_scores)

# The probability of an ACT score greater than 30 in act_scores?
gt_30 <- act_scores > 30
count_gt_30 <- sum(gt_30)

prob_gt_30 <- count_gt_30 / sample_size
prob_gt_30

# The probability of an ACT score <= 10 in act_scores?
lt_10 <- act_scores <= 10
count_lt_10 <- sum(lt_10)

prob_lt_10 <- count_lt_10 / sample_size
prob_lt_10

# Set x equal to the sequence of integers 1 to 36. 
# Use dnorm to determine the value of the probability density function over x
# given a mean of 20.9 and standard deviation of 5.7;
# save the result as f_x. Plot x against f_x.

x <- seq(1,36)

f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x, type = 'l')


# Part 3
#  convert raw ACT scores to Z-scores 

z_score_scale <- scale(act_scores)
1 - mean(abs(z_score_scale) <= 2)

#  the probability of a Z-score greater than 2
z_gt_2 <- z_score_scale > 2
sum(z_gt_2) / 10000

# score value corresponds to 2 standard deviations above the mean (Z = 2)
mean(act_scores) + (2*sd(act_scores))

# the 97.5th percentile of act_scores
qnorm(0.975, mean = mean(act_scores), sd = sd(act_scores))

##
##
# Write a function that takes a value and produces the
# probability of an ACT score less than or equal to that value (the CDF).
# Apply this function to the range 1 to 36.

act_cdf <- function(ascore) {
 
  lessthan <- act_scores <= ascore
  count_lt <- sum(lessthan)
  
  prob_lt_cdf <- count_lt / sample_size
  prob_lt_cdf
}

applyrange <- sapply(x, act_cdf)

# list the values of score such that the probability of that score or lower
# is at least .95
which(applyrange >= 0.95)

# list the minimum index
min(which(applyrange >= 0.95))


# determine the expected 95th percentile, the value for which the probability of
# receiving that score or lower is 0.95, 
# given a mean score of 20.9 and standard deviation of 5.7.
qnorm(0.95, 20.9, 5.7)

# use quantile() to determine sample quantiles from the data.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

# Determine the percentile of a score of 26.
which(sample_quantiles >= 26)


# Make a QQ-plot graphing 
# sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
theoretical_quantiles <- qnorm(p, mean = 20.9, sd =  5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()


