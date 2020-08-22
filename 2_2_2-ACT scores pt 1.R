
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
