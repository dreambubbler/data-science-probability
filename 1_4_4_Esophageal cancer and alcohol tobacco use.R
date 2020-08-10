library(tidyverse)

esoph_data <- esoph

# Determine the number of groups in the study.
all_groups <- nrow(esoph_data)
all_groups

# Determine the total number of cases in the study.
all_cases <- sum(esoph_data$ncases)
all_cases

# Determine the total number of controls in the study.
all_controls <- sum(esoph_data$ncontrols)
all_controls

# The probability of being in the highest alcohol group, for cases.
max_alcgrp <- max(esoph_data$alcgp)
max_alcgrp

highest_alcgrp <- esoph_data %>% filter(alcgp == as.character(max_alcgrp))
highest_alcgrp
cases_in_highest_alcgrp <- sum(highest_alcgrp$ncases)
cases_in_highest_alcgrp
# Probability
p_hi_alcgrp <- cases_in_highest_alcgrp / all_cases
p_hi_alcgrp

# The probability of being in the highest tobacco group, for cases.
max_tobgrp <- max(esoph_data$tobgp)
max_tobgrp

highest_tobgrp <- esoph_data %>% filter(tobgp == as.character(max_tobgrp))
highest_tobgrp

cases_in_highest_tobgrp <- sum(highest_tobgrp$ncases)
cases_in_highest_tobgrp
# Probability
p_hi_tobgrp <- cases_in_highest_tobgrp / all_cases
p_hi_tobgrp


# Probability of being in the highest alcohol group and the highest tobacco group
in_highest_alc_and_tob <- highest_alcgrp %>% filter(tobgp == as.character(max_tobgrp))
in_highest_alc_and_tob
cases_in_both_highest_grps <- sum(in_highest_alc_and_tob$ncases)
cases_in_both_highest_grps
# Probability
p_both_hi_grp <- cases_in_both_highest_grps / all_cases
p_both_hi_grp

# The probability of being in the highest alcohol group or the highest tobacco group
p_hi_alc_or_hi_tob <- p_hi_alcgrp + p_hi_tobgrp - p_both_hi_grp
p_hi_alc_or_hi_tob

# The probability of being in the highest alcohol group, as a control
controls_in_higest_alcgrp <- sum(highest_alcgrp$ncontrols)
controls_in_higest_alcgrp
# Probability
p_cont_hi_alcgrp <- controls_in_higest_alcgrp / all_controls
p_cont_hi_alcgrp


# Determine how mant time cases are more likely than controls to be in the highest alcohol group
p_hi_alcgrp / p_cont_hi_alcgrp


# The probability of being in the highest tobacco group, as a control
controls_in_higest_tobgrp <- sum(highest_tobgrp$ncontrols)
controls_in_higest_tobgrp
# Probability
p_cont_hi_tabgrp <- controls_in_higest_tobgrp / all_controls
p_cont_hi_tabgrp


# Probability of being in the highest alcohol group and the highest tobacco group, as a control
cont_in_highest_alc_and_tob <- highest_alcgrp %>% filter(tobgp == as.character(max_tobgrp))
cont_in_highest_alc_and_tob
cont_cases_in_both_highest_grps <- sum(cont_in_highest_alc_and_tob$ncontrols)
cont_cases_in_both_highest_grps
# Probability
p_cont_both_hi_grp <- cont_cases_in_both_highest_grps / all_controls
p_cont_both_hi_grp


# The probability of being in the highest alcohol group or the highest tobacco group, as a control
p_cont_hi_alc_or_hi_tob <- p_cont_hi_alcgrp + p_cont_hi_tabgrp - p_cont_both_hi_grp
p_cont_hi_alc_or_hi_tob


# Determine how many times cases are more likely than controls to be in
# the highest alcohol group or the highest tobacco group
# Pr(A or B) = Pr(A) + Pr(B) - Pr(A and B)

#cases
p_cases_in_hialc_or_hitob <- p_hi_alcgrp + p_hi_tobgrp - p_both_hi_grp
p_cases_in_hialc_or_hitob

#controls
p_cont_in_hialc_or_hitob <- p_cont_hi_alcgrp + p_cont_hi_tabgrp - p_cont_both_hi_grp
p_cont_in_hialc_or_hitob

# More times likely
p_cases_in_hialc_or_hitob / p_cont_in_hialc_or_hitob