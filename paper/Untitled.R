Title:"Terminal Investment"

set.seed(42)  # For reproducibility (generates same numbers everytime you run the code)
n <- 100  # Number of observations
# FAKE SEAL DATA SET 
fakeseal <- data.frame(
  biological_age = runif(n, -15, 0),  # Biological age from -15 to 0, runif generates the numbers from the min and max values 
  pup_weight = rnorm(n, mean = 30, sd = 5) + runif(n, -10, 0)  # rnorm() generates numbers from a normal distribution 
)
fakeseal$pup_weight <- fakeseal$pup_weight + 0.7 * (-fakeseal$biological_age)
fakeseal$lactation_duration <- rnorm(n, mean =10, sd = 8) - 0.5 * (-fakeseal$biological_age)

#FOR PUP WEIGHT INCREASING AGE BIOLOGICAL AGE DOES
ggplot(fakeseal, aes(x = biological_age, y = pup_weight)) +
  geom_point(color = "darkblue", alpha = 0.6) +  # alpha is for the transparency of points (just aes stuff)
  geom_smooth(method = "lm", color = "red", se = FALSE) +   # standard errors bands showed or not
  labs(
    title = "Terminal Investment- pup weight",
    x = "Biological Age ",
    y = "Pup Weight at Birth"
  ) +
  theme_minimal()

#FOR LACTATION DURATION 
ggplot(fakeseal, aes(x = biological_age, y = lactation_duration)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Lactation Duration in near death",
    x = "Biological Age ",
    y = "Lactation Duration (days)"
  ) +
  theme_minimal()

#PUP WEIGHT AT WEAN FOR LAST 3 YEARS
set.seed(42)
lastyears <- c(-3, -2, -1)  # Adjusted to 3 values to match the pup_weight length
lastseals <- data.frame(
  individual = rep(c("Seal 1", "Seal 2", "Seal 3"), each = 3),
  biological_age = rep(lastyears, 3),  # Now works with 9 values
  pup_weight = c(
    30, 32, 35,  
    28, 29, 33,  
    27, 30, 40   
  )
)

#THREE UNIQUE MOMS WITH HEAVIER PUP LAST YEAR 
seal_list <- lapply(unique(lastseals$individual), function(indiv) {     #function(indiv) is to define a function
  ggplot(lastseals[lastseals$individual == indiv, ], aes(x = biological_age, y = pup_weight)) +
    geom_point(color = "blue", size = 6) +  # Scatter points for pup weight
    geom_line(group = 1, color = "red") +  # Connect points with lines
    geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +  # Line of best fit
    labs(
      title = paste(" ", indiv),
      x = "Biological Age ",
      y = "Weight at Weaning"
    ) +
    theme_minimal()
})
grid.arrange(grobs = seal_list, ncol = 1) #grobs is for graphical objects 

#6 Moms
selected_individuals <- unique(lastseals$individual)[1:6]
seal_list <- lapply(selected_individuals, function(indiv) {
  ggplot(lastseals[lastseals$individual == indiv, ], aes(x = biological_age, y = pup_weight)) +
    geom_point(color = "blue", size = 4) +  # Scatter points for pup weight
    geom_line(aes(group = 1), color = "red") +  # Connect points with lines
    geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +  # Line of best fit
    labs(
      title = paste("Individual:", indiv),
      x = "Biological Age",
      y = "Weight at Weaning"
    ) +
    theme_minimal()
})
grid.arrange(grobs = seal_list, ncol = 1)

