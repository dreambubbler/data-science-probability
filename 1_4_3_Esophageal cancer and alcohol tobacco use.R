

library(tidyverse)


head(esoph)
esoph_data <- esoph
str(esoph)


# Determine the number of groups in the study.
nrow(esoph_data)

# Determine the total number of cases in the study.
all_cases <- sum(esoph_data$ncases)
all_cases

# Determine the total number of controls in the study.
all_controls <- sum(esoph_data$ncontrols)
all_controls

# Determine the probability that a subject in the highest
# alcohol consumption group is a cancer case.

highest_alcgrp <- esoph_data %>% filter(alcgp == "120+")
highest_alcgrp
cases_in_highest_alcgrp <- sum(highest_alcgrp$ncases)
cases_in_highest_alcgrp

controls_in_highest_alcgrp <- sum(highest_alcgrp$ncontrols)
controls_in_highest_alcgrp

cases_in_highest_alcgrp / (cases_in_highest_alcgrp + controls_in_highest_alcgrp)

# Determine the probability that a subject in the lowest
# alcohol consumption group is a cancer case.
lowest_alcgrp <- esoph_data %>% filter(alcgp == "0-39g/day")
lowest_alcgrp
cases_in_lowest_alcgrp <- sum(lowest_alcgrp$ncases)
cases_in_lowest_alcgrp

controls_in_lowest_alcgrp <- sum(lowest_alcgrp$ncontrols)
controls_in_lowest_alcgrp

# Probability that a subject in the lowest alcohol consumption group is a cancer case.
cases_in_lowest_alcgrp / (cases_in_lowest_alcgrp + controls_in_lowest_alcgrp)


# Determine the probability that they smoke 10g or more a day,
# given that a person is a case.
min_tobgrp <- min(esoph_data$tobgp)
tobgrp_not_lowest_users <- esoph_data %>% filter(tobgp != as.character( min_tobgrp))
tobgrp_not_lowest_users

cases_not_lowest_users <- sum(tobgrp_not_lowest_users$ncases)
cases_not_lowest_users


# Probability that a cancer patient smokes 10g or more per day.
cases_not_lowest_users / (all_cases)

# What is the probability that a person smokes 10g or more a day
# given they are a control?
controls_not_lowest_users <- sum(tobgrp_not_lowest_users$ncontrols)
controls_not_lowest_users

controls_not_lowest_users / (all_controls)
