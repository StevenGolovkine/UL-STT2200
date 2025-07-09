library(tibble)
library(dplyr)

generate_noisy_data <- function(n = 100, noise_levels = c(0, 0.1, 0.3, 0.5)) {
  # x values (avoid 0 for log)
  x_vals <- seq(0.01, 0.99, length.out = n)
  
  # True function
  f <- function(x) 4 * x * (1 - x) * log(x) + 2
  
  # Generate data for each noise level
  data <- lapply(noise_levels, function(sigma) {
    y_true <- f(x_vals)
    y_noisy <- y_true + rnorm(n, mean = 0, sd = sqrt(sigma))
    
    tibble(
      x = x_vals,
      y = y_noisy,
      noise = sigma
    )
  }) %>% bind_rows()
  
  return(data)
}

# Example usage
set.seed(123)

noise_levels <- seq(0, 0.5, by=0.01)
df <- generate_noisy_data(noise_levels = noise_levels)
print(df)
write.csv(df, './data.csv')

