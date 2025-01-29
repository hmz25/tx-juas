library(tidyverse)
library(dplyr)

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
  mutate(cone_sd = sd(c_across(s1:s10), na.rm = TRUE)) |> 
  ungroup()

five_samples <- cone_data |> 
  filter(is.na(cone_data[[17]])) |>   
  select(date_collected:s5_count) |> 
  mutate(across(s1_weight:s5_count, as.numeric)) |> 
  mutate(s1 = s1_count/s1_weight,
         s2 = s2_count/s2_weight,
         s3 = s3_count/s3_weight,
         s4 = s4_count/s4_weight,
         s5 = s5_count/s5_weight) |> 
  rowwise() |>  
  mutate(cone_sd = sd(c_across(s1:s5), na.rm = TRUE)) |> 
  ungroup()
