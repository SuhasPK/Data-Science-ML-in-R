# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(1234)

# Generate random data following a normal distribution
data <- rnorm(10000, mean = 0, sd = 1)

# Calculate mean and standard deviation
mu <- mean(data)
sigma <- sd(data)

# Create a data frame
df <- data.frame(x = data)

# Define custom x-axis labels
custom_labels <- expression(mu - 3 * sigma, mu - 2 * sigma, mu - sigma, mu, mu + sigma, mu + 2 * sigma, mu + 3 * sigma)

# Function to calculate normal density
normal_density <- function(x) dnorm(x, mean = mu, sd = sigma)

# Calculate the area under the curve between mu - sigma and mu + sigma
area_sigma <- integrate(normal_density, mu - sigma, mu + sigma)$value
area_2sigma <- integrate(normal_density, mu - 2 * sigma, mu + 2 * sigma)$value
area_3sigma <- integrate(normal_density, mu - 3 * sigma, mu + 3 * sigma)$value

# Calculate the area under the curve between mu - 2sigma and mu - sigma
area_green_left <- integrate(normal_density, mu - 2 * sigma, mu - sigma)$value

# Calculate the area under the curve between mu + sigma and mu + 2sigma
area_green_right <- integrate(normal_density, mu + sigma, mu + 2 * sigma)$value

# Calculate the area under the curve between mu - 3sigma and mu - 2sigma
area_pink_left <- integrate(normal_density, mu - 3 * sigma, mu - 2 * sigma)$value

# Calculate the area under the curve between mu + 2sigma and mu + 3sigma
area_pink_right <- integrate(normal_density, mu + 2 * sigma, mu + 3 * sigma)$value

# Calculate the area under the curve between mu - 3sigma and mu - 2sigma
area_orange_left <- integrate(normal_density, mu - 3 * sigma, mu - 2 * sigma)$value

# Calculate the area under the curve between mu + 2sigma and mu + 3sigma
area_orange_right <- integrate(normal_density, mu + 2 * sigma, mu + 3 * sigma)$value

# Plot the normal distribution curve with custom x-axis labels, vertical lines, and shaded areas
ggplot(df, aes(x = x)) +
  stat_function(fun = normal_density, color = "blue", size = 1) +
  geom_vline(xintercept = c(mu - sigma, mu + sigma), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(mu - 2 * sigma, mu + 2 * sigma), linetype = "dashed", color = "purple")+
  geom_vline(xintercept = c(mu - 3 * sigma, mu + 3 * sigma), linetype = "dashed", color = "black")+
  geom_ribbon(aes(ymax = normal_density(x), ymin = 0), fill = "yellow", alpha = 0.5) +
  geom_ribbon(aes(x = ifelse(x < mu - sigma & x > mu - 2 * sigma, x, mu - 2 * sigma),
                  ymax = ifelse(x < mu - sigma & x > mu - 2 * sigma, normal_density(x), 0),
                  ymin = 0), fill = "green", alpha = 0.3) +
  
  geom_ribbon(aes(x = ifelse(x < mu + 2 * sigma & x > mu + sigma, x, mu + sigma),
                  ymax = ifelse(x < mu + 2 * sigma & x > mu + sigma, normal_density(x), 0),
                  ymin = 0), fill = "green", alpha = 0.3) +
  
  geom_ribbon(aes(x = ifelse(x < mu - 2 * sigma & x > mu - 3 * sigma, x, mu - 3 * sigma),
                  ymax = ifelse(x < mu - 2 * sigma & x > mu - 3 * sigma, normal_density(x), 0),
                  ymin = 0), fill = "purple", alpha = 0.5) +
  
  geom_ribbon(aes(x = ifelse(x < mu + 3 * sigma & x > mu + 2 * sigma, x, mu + 2 * sigma),
                  ymax = ifelse(x < mu + 3 * sigma & x > mu + 2 * sigma, normal_density(x), 0),
                  ymin = 0), fill = "purple", alpha = 0.5) +
  ggtitle("Normal Distribution Curve") +
  xlab(expression("Number of " * sigma * " from mean")) +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(mu - 3 * sigma, mu - 2 * sigma, mu - sigma, mu, mu + sigma, mu + 2 * sigma, mu + 3 * sigma),
                     labels = custom_labels) +
  annotate("text", x = mu, y = c(0.1, 0.5, 1.0), 
           label = c(sprintf("Area(\u03C3) = %.4f", area_sigma),
                     sprintf("Area(2\u03C3) = %.4f", area_2sigma),
                     sprintf("Area(3\u03C3) = %.4f", area_3sigma)),
           vjust = 0.5, hjust = 0.5, size = 4, color = "black") 
ggsave("Normal_distribution.png", plot = last_plot(), width = 25, height = 18, units = "cm", dpi = 840)
