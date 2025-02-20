library(tidyverse)
library(dplyr)
library(pwr)
# install.packages("pwrss")
library(pwrss)

cone_data <- read_csv("/Users/hannahzonnevylle/Desktop/juas/2024 analysis/cone processing 24 - counts.csv")

ten_samples <- cone_data |>
  filter(!is.na(cone_data[[17]])) |> 
  select(-notes) |> 
  rename("s6_weight" = 16,
         "s6_count" = 17,
         "s7_weight" = 18,
         "s7_count" = 19,
         "s8_weight" = 20,
         "s8_count" = 21,
         "s9_weight" = 22,
         "s9_count" = 23,
         "s10_weight" = 24,
         "s10_count" = 25) |> 
  mutate(across(s1_weight:s10_count, as.numeric)) |> 
  mutate(s1 = s1_count/s1_weight,
         s2 = s2_count/s2_weight,
         s3 = s3_count/s3_weight,
         s4 = s4_count/s4_weight,
         s5 = s5_count/s5_weight,
         s6 = s6_count/s6_weight,
         s7 = s7_count/s7_weight,
         s8 = s8_count/s8_weight,
         s9 = s9_count/s9_weight,
         s10 = s10_count/s10_weight) |> 
  rowwise() |>  
  mutate(cone_sd = sd(c_across(s1:s10), na.rm = TRUE),
         cone_mean = mean(c_across(s1:s10), na.rm = TRUE),
         cone_se = cone_sd/sqrt(10)) |> 
  ungroup() 

ten_samples_clean <- ten_samples %>%
  mutate(tree_id = paste(site, tree, sep = "")) |> 
  pivot_longer(cols = c(s1:s10), names_to = "subsample", values_to = "count_per_weight") |> 
  select(date_collected, tree_id, subsample, count_per_weight, cone_mean, cone_sd, cone_se, everything())

#function to simulate subsampling
simulate_subsampling <- function(data, tree_id_col, max_reps = 1000, subsample_sizes = c(2, 3, 5, 7, 10)) {
  
  results <- data.frame()  #create empty df to store results
  
  #loop over unique tree id in the dataset
  for (i in unique(data[[tree_id_col]])) {
    
    #subset data for the current tree
    tree_data <- data %>% 
      filter(.data[[tree_id_col]] == i)
    
    #loop over different subsample sizes
    for (n in subsample_sizes) {
      
      #perform repeated sampling to estimate mean, sd, and se
      resampled_stats <- replicate(max_reps, {
        sample_vals <- sample(tree_data$count_per_weight, size = n, replace = FALSE)  #draw n subsamples
        c(mean = mean(sample_vals), sd = sd(sample_vals), se = sd(sample_vals) / sqrt(n))  #compute stats
      })
      
      #convert results to a dataframe
      temp_results <- data.frame(
        tree_id = i,  #store tree ID
        subsample_size = n,  #store subsample size 
        mean_est = mean(resampled_stats["mean", ]),  #compute average estimated mean
        sd_est = mean(resampled_stats["sd", ]),  #compute average estimated SD
        se_est = mean(resampled_stats["se", ])  #compute average estimated SE
      )
      
      #add results to the main results dataframe
      results <- rbind(results, temp_results)
    }
  }
  
  return(results)  #return the final results dataframe
}


#apply function to data 
simulation_results <- simulate_subsampling(
  data = ten_samples_clean, 
  tree_id_col = "tree_id"  
)
print(simulation_results)

#visualize how se changes based on sample size
ggplot(simulation_results, aes(x = subsample_size, y = se_est, group = tree_id, color = tree_id)) +
  geom_line() +
  geom_point() +
  labs(title = "effect of subsample size on SE", x = "subsample n", y = "estimated SE") +
  theme_minimal()

#visualize how sd changes
# ggplot(simulation_results, aes(x = subsample_size, y = sd_est, group = tree_id, color = tree_id)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "effect of subsample size on SD", x = "subsample n", y = "estimated SD") +
#   theme_minimal()

##WHY DOES SD GET LARGER WITH HIGER NUM OF SUBSAMPLES

ggplot(ten_samples_clean) +
  geom_boxplot(aes(y = count_per_weight, col = tree_id))


