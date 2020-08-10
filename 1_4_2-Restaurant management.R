# A restaurant manager wants to advertise that his lunch special offers 
# enough choices to eat different meals every day of the year. 
# He doesn't think his current special actually allows that number of choices,
# but wants to change his special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. 
# He currently offers:
# a choice of 1 entree from a list of 6 options, 
# a choice of 2 different sides from a list of 6 options, and
# a choice of 1 drink from a list of 2 options.


library("gtools")

# Determine the number of meal combinations are possible with the current menu.
entree_total_options <- 6
side_total_options <- 6
drink_total_options <-2 

# entree choice
entree_choice <- combinations(entree_total_options, 1)
entree_choice_count <- nrow(entree_choice)
entree_choice_count
#side_choices
side_choices <- combinations(side_total_options, 2)
side_choices_count <- nrow(side_choices)
side_choices_count
#drink_choice
drink_choice <- combinations(drink_total_options, 1)
drink_choice_count <- nrow(drink_choice)
drink_choice_count

# Determine Meal combinations
meal_combintations <- entree_choice_count * side_choices_count * drink_choice_count
meal_combintations

# Assume the manager has one additional drink he could add to the special.
# The number of combinations possible with additional drink choice.
new_drink_choice_count <- drink_choice_count + 1
meal_combintations_with_extra_drink <- entree_choice_count * side_choices_count * new_drink_choice_count
meal_combintations_with_extra_drink

# Manager adds the extra drink. Also allows 3 sides to be chosen.
# Determine the meal combinations if customers can
# choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options
new_side_choices <- combinations(side_total_options, 3)
new_side_choices_count <- nrow(new_side_choices)
new_side_choices_count

new_meal_combinations <- new_drink_choice_count * new_side_choices_count * entree_choice_count
new_meal_combinations


# Manager wants to know how many entree choices
# he would have to offer in order to meet his goal.

# Function that takes a number of entree options, side options, and drink options
# along with the number of choices for each.
# Returns the number of meal combinations possible

meal_combinations_from_entree_choices <- function(x_entrees_total = 6, x_sides_total = 6, x_drinks_total = 3,
                                           entree_pick = 1, sides_pick = 2, drinks_pick = 1) {
  x_e_combinations <- nrow(combinations(x_entrees_total, entree_pick))
  x_s_combinations <- nrow(combinations(x_sides_total, sides_pick))
  x_d_combinations <- nrow(combinations(x_drinks_total, drinks_pick))
  
  x_e_combinations * x_s_combinations * x_d_combinations
}

entree_total_choices <- seq(1,12)
meal_cominations_vary_entree <- sapply(entree_total_choices, meal_combinations_from_entree_choices)
meal_cominations_vary_entree

# Function that takes a number of side choices and
# Returns the number of meal combinations possible
meal_combinations_from_side_choices <- function(x_sides_total = 6, x_entrees_total = 6, x_drinks_total = 3,
                                           entree_pick = 1, sides_pick = 2, drinks_pick = 1) {
  x_e_combinations <- nrow(combinations(x_entrees_total, entree_pick))
  x_s_combinations <- nrow(combinations(x_sides_total, sides_pick))
  x_d_combinations <- nrow(combinations(x_drinks_total, drinks_pick))
  
  x_e_combinations * x_s_combinations * x_d_combinations
}


# Determine the minimum number of side options required in order to generate
# more than 365 combinations from sides counts ranging from 2 to 12.
side_total_choices <- seq(2,12)
meal_combinations_vary_sides <- sapply(side_total_choices, meal_combinations_from_side_choices)

meal_combinations_vary_sides
