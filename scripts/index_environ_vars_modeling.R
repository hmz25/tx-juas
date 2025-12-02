#script to analyze reproductive output as a function of environmental variables 
#this script needs two data sources: 
  #1. index values from trees at each site as a proxy for reproductive output 
  #2. environmental variables downloaded from SMAP and precip, temp, etc from gridmet
#see [INSERT] script for downloading environmental variables
#see [INSERT] script for extracting repro index from tree canopies 

#load libraries
library(dplyr)
library(tidyverse)
library(ggpubr)
library(lme4)
library(car)
library(climwin)
library(ggplot2)
library(patchwork)
library(sjPlot)


# index data --------------------------------------------------------------


#load in index data

rg_index_df <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/rg_index_df.csv")

#summary stats for df 

# #first taking mean of each band
# site_mean_rgb_df <- rg_index_df %>% 
#   group_by(site, date, tree) %>% 
#   summarize(mean_r = mean(r),
#             mean_g = mean(g),
#             mean_b = mean(b),
#             canopy_area = sum(coverage_area)) 
# 
# #then calculating index
# site_mean_index_df <- site_mean_rgb_df %>% 
#   group_by(site, date, tree, canopy_area) %>% 
#   summarize(mean_rg_index = ((mean_r - mean_g)/(mean_r + mean_g))) %>%
#   mutate(date = as.character(date),
#          date = parse_datetime(date)) %>%
#   separate(date, into = c("year", "month", "day"), sep = "-")

#alternate approach = calculating index for each pixel then taking mean of index 
site_mean_index_df <- rg_index_df %>% 
  group_by(site, date, tree) %>% 
  summarize(mean_rg_index = mean(rg_index),
            canopy_area = sum(coverage_area)) %>% 
  mutate(date = as.character(date),
         date = parse_datetime(date)) %>% 
  separate(date, into = c("year", "month", "day"), sep = "-")

site_mean_index_df %>% 
  filter(site %in% "sonora",
         tree %in% 427)

#take random subset of 100 trees from each site for analyses
set.seed(123)
site_mean_index_df_sub <- site_mean_index_df %>% 
  group_by(site) %>%
  # count unique trees in each site
  mutate(n_trees = n_distinct(tree)) %>%
  # sample 100 tree IDs if more than 100 exist, else keep all
  group_modify(~ {
    if (n_distinct(.x$tree) > 100) {
      selected_trees <- sample(unique(.x$tree), 100)
      .x %>% filter(tree %in% selected_trees)
    } else {
      .x
    }
  }) %>%
  ungroup() %>%
  dplyr::select(-n_trees) # clean up temporary column
  
#for analysis, choose highest index value for each year 
max_index_dates <- site_mean_index_df_sub %>% 
  group_by(site, year) %>% 
  filter(mean_rg_index == max(mean_rg_index)) %>% 
  dplyr::select(site, year, day)

site_max_mean_index_df <- site_mean_index_df_sub %>% 
  inner_join(max_index_dates, by = c("site", "year", "day")) #%>% 
  # filter(!site %in% c("creek", "gun", "rocky", "windmill", "glimmer", "williamson")) #remove sites with only one year of data collection

#visualize how index changes for each year of data collection
ggplot(site_max_mean_index_df) +
  geom_boxplot(aes(x = site, y = mean_rg_index, fill = year)) + 
  theme_classic() +
  ylab("mean index") +
  theme(axis.text.x = element_text(angle = 90))

#filter out sites with only one year of data collection so we can use a paired t test to check significance

site_index_paired_df <- site_max_mean_index_df %>% 
  filter(!site %in% c("creek", "gun", "rocky", "windmill", "glimmer", "williamson", "goodguys")) #remove sites with only one year of data collection


# p <- ggplot(site_index_paired_df) +
#   geom_boxplot(aes(x = site, y = mean_rg_index, fill = year), varwidth = TRUE) + 
#   theme_classic() +
#   ylab("mean index") +
#   theme(axis.text.x = element_text(angle = 90)) 
# 
# p <- ggplot(site_index_paired_df) +
#   geom_boxplot(aes(x = site, y = mean_rg_index, fill = year), 
#                position = position_dodge2(width = NULL,
#                                          preserve = "total")) +
#   theme_classic() +
#   ylab("mean index") +
#   theme(axis.text.x = element_text(angle = 90)) 

p <- ggplot(site_index_paired_df) +
  geom_boxplot(aes(x = site, y = mean_rg_index, fill = year), 
               width = 0.4, notch = F) +
  theme_classic() +
  ylab("mean index") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Oranges")


ggsave("paired_boxplot.png", plot = p, width = 8, height = 6, units = "in")

#check normality 

ggplot(site_index_paired_df, aes(x = mean_rg_index, fill = year), alpha = 0.5) +
  geom_histogram()

ggqqplot(site_index_paired_df$mean_rg_index) #has heavy tails

site_year_means <- site_index_paired_df %>%
  group_by(site, year) %>%
  summarise(mean_value = mean(mean_rg_index, na.rm = TRUE), .groups = "drop")

index_wide_df <- site_year_means %>%
  pivot_wider(names_from = year, values_from = mean_value, names_prefix = "year_")

index_wide_df <- index_wide_df %>%
  mutate(diff = year_2025 - year_2024) %>% 
  drop_na(diff)

shapiro.test(index_wide_df$diff) #normally distributed 

#check equality of variances
bartlett.test(mean_rg_index ~ year, data = site_mean_index_df)
#homogeneity of variance is violated

#run t-test
# index_t_test <- t.test(index_wide_df$year_2025, index_wide_df$year_2024, paired = TRUE)
# print(index_t_test)

# t_test_df <- site_max_mean_index_df %>% 
#   mutate(year_col = paste0("year_", year)) %>% 
#   dplyr::select(site, tree, year_col, mean_rg_index, canopy_area) %>% 
#   pivot_wider(names_from = year_col, values_from = c(mean_rg_index, canopy_area)) 
#               
# index_t_test <- t.test(t_test_df$mean_rg_index_year_2024, t_test_df$mean_rg_index_year_2025, paired = TRUE)
# 
# pval <- signif(index_t_test$p.value, 3)

#run ANOVA
#null hypothesis is that 2024 index = 2025 index
anova <- aov(mean_rg_index ~ site * year, data = site_mean_index_df_sub)
summary(anova)
p_val_aov <- summary(anova)[[1]][["Pr(>F)"]][2]

# anova <- aov(mean_rg_index ~ site + year, data = site_mean_index_df)
# summary(anova)
# p_val_aov <- summary(anova)[[1]][["Pr(>F)"]][2]

#plot and add p-vals

ggplot(site_index_paired_df, aes(x = site, y = mean_rg_index, fill = year)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("mean index") +
  annotate("text", 
           x = Inf, 
           y = Inf, 
           label = paste0("p = ", pval), 
           hjust = 1.1, vjust = 2, size = 5) 

#plot with p-values

compare_means(mean_rg_index ~ year, data = site_index_paired_df, 
              group.by = "site", method = "t.test")

# p <- ggboxplot(site_index_paired_df, x = "site", y = "mean_rg_index",
#                fill = "year") 


  
p <- ggplot(site_index_paired_df, aes(x = site, y = mean_rg_index, fill = year)) +
  geom_boxplot(width = 0.4, notch = F) +
  theme_classic() +
  ylab("mean index") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "PRGn")
  
p_sig <- p + 
  stat_compare_means(aes(group = year), 
                       label = "p.signif", #"p.format" for numbers
                       size = 5,
                       vjust = -0.3) +
  ylab("mean index") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold")) + 
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))
    

ggsave("paired_boxplot.png", plot = p_sig, width = 8, height = 6, units = "in")

#generate df of p-vals for each site

# pvals_df <- site_index_paired_df %>%
#   group_by(site, tree, year) %>%
#   summarize(mean_index = mean(mean_rg_index), .groups = "drop") %>% 
#   pivot_wider(names_from = year, values_from = mean_index, names_prefix = "index_val_") %>%
#   group_by(site) %>%
#   summarize(p_value = t.test(index_val_2024, index_val_2025, paired = TRUE)$p.value)
# 
# print(pvals_df)

#subset site data to look at how trees varied year to year
index_sub_df <- site_index_df %>% 
  filter(site %in% "cathedral",
         tree %in% 0:9) 

index_sub_plot_df <- index_sub_df %>% 
  filter(date %in% c("20240112", "20250110")) %>%
  mutate(tree = as.numeric(tree),
         tree = (tree+1),
         tree = as.character(tree)) %>% 
  mutate(tree = fct_relevel(tree, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) %>% 
  mutate(date = ymd(date)) %>% 
  separate(date, into = c("year", "month", "day"), sep = "-")  

ggplot(index_sub_plot_df) +
  geom_boxplot(aes(x = tree, y = rg_index, fill = year), 
               width = 0.3, 
               notch = F,
               outlier.shape = NA) +
  theme_classic() +
  ylab("index") + 
  scale_fill_brewer(palette = "PRGn") + 
  ylim(-0.2,0.1) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold")) + 
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))

ggsave("tree_boxplot.png", width = 8, height = 6, units = "in")
  
  

# environmental data ------------------------------------------------------


#load in environ vars CSV
environ_vars_df <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/environ_vars_df.csv")
# unique(environ_vars_df$site)

# duplicates <- environ_vars_df |>
#   dplyr::summarise(n = dplyr::n(), .by = c(date, site, year, month, variable)) |>
#   dplyr::filter(n > 1L) 

#reformat environ vars data frame
environ_vars_wide <- environ_vars_df %>% 
  group_by(site, year, month, variable) %>% 
  summarize(mean_val = mean(values)) %>% 
  pivot_wider(names_from = variable, values_from = mean_val)  

# environ_vars_wide %>% 
#   filter(year %in% 2024) %>%
#   group_by(site, year) %>% 
#   summarize(mean_pr = mean(pr),
#             mean_tmmx = mean(tmmx),
#             mean_vpd = mean(vpd),
#             mean_smap = mean(smap)) %>% 
#   mutate(mean_tmmx = (mean_tmmx-273.15)) %>% 
#   write_csv(file = "site_environ_vars.csv")

#quick visualization of covariation
plot(environ_vars_wide[,4:7])

#reformatting date for climwin analysis
environ_vars_dmy <- environ_vars_df %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>% 
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day),
         date = sprintf("%02d/%02d/%04d", day, month, year))

# #climwin analysis to figure out what time of year is most impactful for reproductive output 
# clim_data <- environ_vars_dmy %>% 
#   mutate(site = as.factor(site))
# 
# biol_data <- site_index_mean_df %>% 
#   mutate(year = as.integer(year),
#          month = as.integer(month),
#          day = as.integer(day),
#          date = sprintf("%02d/%02d/%04d", day, month, year),
#          site = as.factor(site))
# 
# tmmx_wind <- slidingwin(xvar = list(tmmx = clim_data$tmmx), cdate = clim_data$date, bdate = biol_data$date, baseline = lm(mean_index ~ 1, data = biol_data), cinterval = "month", range = c(12, 2), type = "relative", stat = "mean", func = c("lin"), spatial = list(biol_data$site, clim_data$site))
# 
# #if climwin doesn't work, subset to spring months for environmental variables
environ_vars_sub_df <- environ_vars_df %>%
  filter(month %in% 3:6)

##MAKE NEW ENVIRON VARS SUB DF BASED ON CLIMWIN FOR DOWNSTREAM ANALYSIS




# pheno data --------------------------------------------------------------

#add phenological data to analysis

focal_pheno25 <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/focal_trees_2025.csv")

focal_pheno24 <- read_csv()

focal_pheno <- bind_rows(focal_pheno24, focal_pheno25)

match_site_name_pheno <- function(site_name) {
  case_when(
    site_name == "cath" ~ "cathedral",
    site_name == "cell" ~ "celltower",
    site_name == "fish" ~ "fisher",
    site_name == "sweet" ~ "sweeten",
    site_name == "rock" ~ "rocky",
    site_name == "wind" ~ "windmill",
    TRUE ~ site_name  # Keep unchanged if no match
  )
}

focal_pheno_clean <- focal_pheno %>%
  dplyr::select(date_time, site, percent_cones_open, x, y) %>%
  mutate(site = tolower(site)) %>%
  mutate(site = match_site_name_pheno(site))

misc_pheno <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/misc_trees_2025.csv")

misc_pheno_clean <- misc_pheno %>%
  dplyr::select(date_time, site, percent_cones_open, x, y) %>%
  mutate(site = tolower(site)) %>% 
  mutate(site = match_site_name_pheno(site))

pheno_df <- bind_rows(focal_pheno_clean, misc_pheno_clean) %>% 
  dplyr::filter(!is.na(percent_cones_open)) %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  dplyr::select(-time) %>% 
  mutate(site = gsub(" ", "", site))
#%>%
  #mutate(date = format(as.Date(date, format = "%m/%d/%Y"), "%d/%m/%Y")) #fixing formatting



# modeling ----------------------------------------------------------------


#run some models to see how index changes as a function of environ vars
environ_vars <- environ_vars_sub_df %>%
  group_by(site, year, variable) %>%
  summarize(mean_val = mean(values)) %>%
  pivot_wider(names_from = variable, values_from = mean_val) %>%
  mutate(site = gsub(" ", "", site))

# environ_vars <- environ_vars_df %>% 
#   group_by(site, year, variable) %>% 
#   summarize(mean_val = mean(values)) %>% 
#   pivot_wider(names_from = variable, values_from = mean_val) %>% 
#   mutate(site = gsub(" ", "", site))

environ_vars_lagged <- environ_vars %>% 
  mutate(year = year + 1) %>% 
  mutate(year = as.character(year))

#df leaving tree as the grouping variable for index
mod_df <- left_join(environ_vars_lagged, site_mean_index_df_sub, by = c("site", "year")) %>% 
  drop_na(mean_rg_index) %>% 
  group_by(site, year, tree) %>% 
  filter(mean_rg_index == max(mean_rg_index)) %>% 
  ungroup() %>% 
  mutate(tmmx = (tmmx-273.15), #converting to Celsius from Kalvin
         year = as.factor(year)) #turning year into a factor


# #mod_df_site_yr <- 
#   mod_df %>% 
#   group_by(site, year) %>% 
#   summarize(index_mean = mean(mean_rg_index),
#             index_median = median(mean_rg_index),
#             pr = mean(pr),
#             tmmx = mean(tmmx),
#             vpd = mean(vpd),
#             smap = mean(smap)) %>% 
#   ggplot(aes(x = smap, y = index_median, col = site, shape = year)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
# 
#   fit <- lm(index_mean ~ smap, data = mod_df_site_yr)
#   summary(fit)
  
  

# #df taking site mean of index
# mod_df <- left_join(environ_vars_lagged, site_mean_index_df, by = c("site", "year")) %>% 
#   drop_na(mean_rg_index) %>% 
#   group_by(site, year) %>% 
#   filter(mean_rg_index == max(mean_rg_index)) %>% 
#   ungroup() %>% 
#   mutate(tmmx = (tmmx-273.15), #converting to Celsius from Kalvin
#          year = as.factor(year)) #turning year into a factor

#model 1: tmmx

# p1 <- ggplot(mod_df, aes(x = tmmx, y = mean_rg_index)) +
#   geom_point(aes(color = site, shape = year)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "index vs temp") +
#   ylab("index") + 
#   theme_minimal()

tmmx_mod <- lmer(mean_rg_index ~ tmmx + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree), data = mod_df)

tmmx_mod_summary <- summary(tmmx_mod) #plot(m1)
tmmx_mod_slope <- round(tmmx_mod_summary$coefficients[2,1], 5)
tmmx_mod_intercept <- round(tmmx_mod_summary$coefficients[1,1], 5)
# tmmx_mod_slope_p <- round(tmmx_mod_summary$coefficients[2,5], 2)

tmmx_pred_plot <- sjPlot::plot_model(tmmx_mod, type = "pred", terms = "tmmx", margin = "empirical")

tmmx_plot <- tmmx_pred_plot +
  geom_point(aes(x = tmmx, y = mean_rg_index, col = site), alpha = 0.3, data = mod_df) +
  xlab("temperature (C)") + ylab("index") +
  ggthemes::theme_few() + scale_color_discrete() +
  ggtitle("temperature") +
  annotate("text", x = 29.5, y = 0.1, 
           label = paste0("y = ", tmmx_mod_slope, " * x + ", tmmx_mod_intercept),
           hjust = 1) + 
  theme(plot.title = element_text(size = 20))

#model 2: pr

# p2 <- ggplot(mod_df, aes(x = vpd, y = mean_rg_index)) +
#   geom_point(aes(color = site, shape = year)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "index vs vpd") + 
#   ylab("index") +
#   theme_minimal()

pr_mod <- lmer(mean_rg_index ~ pr + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree), data = mod_df)

pr_mod_summary <- summary(pr_mod) #plot(m1)
pr_mod_slope <- round(pr_mod_summary$coefficients[2,1], 5)
pr_mod_intercept <- round(pr_mod_summary$coefficients[1,1], 5)
# tmmx_mod_slope_p <- round(tmmx_mod_summary$coefficients[2,5], 2)

pr_pred_plot <- sjPlot::plot_model(pr_mod, type = "pred", terms = "pr", margin = "empirical")

pr_plot <- pr_pred_plot +
  geom_point(aes(x = pr, y = mean_rg_index, col = site), alpha = 0.3, data = mod_df) +
  xlab("precipitation (mm)") + ylab("index") +
  ggthemes::theme_few() + scale_color_discrete() +
  ggtitle("precipitation") +
  annotate("text", x = 3.5, y = 0.1, 
           label = paste0("y = ", pr_mod_slope, " * x + ", pr_mod_intercept),
           hjust = 1) + 
  theme(plot.title = element_text(size = 20))

# Plot 3: smap 

# p3 <- ggplot(mod_df, aes(x = smap, y = mean_rg_index)) +
#   geom_point(aes(color = site, shape = year)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "index vs soil moisture") +
#   ylab("index") + 
#   theme_minimal()

smap_mod <- lmer(mean_rg_index ~ smap + year + canopy_area + (1 | site) +
                 (1| site:year) + (1 | site:tree), data = mod_df)

smap_mod_summary <- summary(smap_mod) #plot(smap_mod)
smap_mod_slope <- round(smap_mod_summary$coefficients[2,1], 5)
smap_mod_intercept <- round(smap_mod_summary$coefficients[1,1], 5)
# tmmx_mod_slope_p <- round(tmmx_mod_summary$coefficients[2,5], 2)

smap_pred_plot <- sjPlot::plot_model(smap_mod, type = "pred", terms = "smap", margin = "empirical")

smap_plot <- smap_pred_plot +
  geom_point(aes(x = smap, y = mean_rg_index, col = site), alpha = 0.3, data = mod_df) +
  xlab("soil moisture (m3/m3)") + ylab("index") +
  ggthemes::theme_few() + scale_color_discrete() +
  ggtitle("soil moisture") +
  annotate("text", x = .28, y = 0.1, 
           label = paste0("y = ", smap_mod_slope, " * x + ", smap_mod_intercept),
           hjust = 1) + 
  theme(plot.title = element_text(size = 20))

# model 4: vpd 

# p4 <- ggplot(mod_df, aes(x = pr, y = mean_rg_index)) +
#   geom_point(aes(color = site, shape = year)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "index vs precip") +
#   ylab("index") + 
#   theme_minimal()

vpd_mod <- lmer(mean_rg_index ~ vpd + year + canopy_area + (1 | site) +
                 (1| site:year) + (1 | site:tree), data = mod_df)

vpd_mod_summary <- summary(vpd_mod) #plot(m1)
vpd_mod_slope <- round(vpd_mod_summary$coefficients[2,1], 5)
vpd_mod_intercept <- round(vpd_mod_summary$coefficients[1,1], 5)
# tmmx_mod_slope_p <- round(tmmx_mod_summary$coefficients[2,5], 2)

vpd_pred_plot <- sjPlot::plot_model(vpd_mod, type = "pred", terms = "vpd", margin = "empirical")

vpd_plot <- vpd_pred_plot +
  geom_point(aes(x = vpd, y = mean_rg_index, col = site), alpha = 0.3, data = mod_df) +
  xlab("vapor pressure deficit (kPA)") + ylab("index") +
  ggthemes::theme_few() + scale_color_discrete() +
  ggtitle("vapor pressure deficit") +
  annotate("text", x = 1.8, y = 0.1, 
           label = paste0("y = ", vpd_mod_slope, " * x + ", vpd_mod_intercept),
           hjust = 1) + 
  theme(plot.title = element_text(size = 20))

#combine plots
# (p1 + p2 + p3 + p4) + plot_layout(ncol = 2)
# 
# (p1 + p2 + p3 + p4) +
#   plot_layout(ncol = 4, guides = "collect") &
#   theme(legend.position = "right")

mod_plot <- (tmmx_plot + pr_plot + smap_plot + vpd_plot) + 
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "right")

ggsave("all_model_plot.png", plot = mod_plot)

# mod_df %>%
#   pivot_longer(cols = c("pr", "smap", "tmmx", "vpd"),
#                names_to = "env_var",
#                values_to = "val") %>%
#   ggplot(aes(x = val, y = mean_rg_index, col = site)) +
#   geom_point() +
#   facet_wrap(~env_var)

p_env_var <- ggplot(mod_df) +
  geom_point(aes(x = tmmx, y = mean_rg_index, col = site)) #+
# # facet_wrap(~year) # +
# #theme(axis.text.x = element_text(angle = 90))

#set up models 

#multiple linear regressions

#old code, when done on site level instead of tree level
# mod_mlr = lm(mean_rg_index ~ pr + smap + tmmx + vpd, data = mod_df)
# 
# summary(mod_mlr)

# #to add tree and site to model, need unique ID for trees
# mod_df$treeID <- interaction(mod_df$site, mod_df$tree) #create composite factor
# 
# mod_mlr = lm(mean_rg_index ~ pr + smap + tmmx + vpd + site + treeID, data = mod_df)
# 
# summary(mod_mlr)
# 
# mod_mlr = lm(mean_rg_index ~ pr + smap + vpd + site + treeID, data = mod_df)
# summary(mod_mlr)

# mod_pr <- lm(mean_rg_index ~ pr + year + site + treeID, data = mod_df)
# summary(mod_pr)
# 
# mod_tmmx <- lm(mean_rg_index ~ tmmx + year + site + treeID, data = mod_df)
# summary(mod_tmmx)
# 
# mod_vpd <- lm(mean_rg_index ~ vpd + year + site + treeID, data = mod_df)
# summary(mod_vpd)
# 
# mod_smap <- lm(mean_rg_index ~ smap + year + site + treeID, data = mod_df)
# summary(mod_smap)

#mixed effect model
mod_lmm = lmer(mean_rg_index ~ year + pr + tmmx + vpd + smap + canopy_area + (1 | site/tree),
               data = mod_df)

summary(mod_lmm)

ggplot(mod_df, aes(x = pr, y = mean_rg_index, col = site)) +
  geom_point() + 
  facet_wrap(~year)

# model selection ---------------------------------------------------------

#examine direct and interactive effects between environmental variables on reproductive output

formula_list = c(
  # 1 predictor
  "mean_rg_index ~ pr + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ tmmx + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ vpd + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  
  # 2 predictors
  "mean_rg_index ~ pr + tmmx + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ pr + vpd + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ pr + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ tmmx + vpd + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ tmmx + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ vpd + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  
  # 3 predictors
  "mean_rg_index ~ pr + tmmx + vpd + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ pr + tmmx + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ pr + vpd + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  "mean_rg_index ~ tmmx + vpd + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)",
  
  # full model
  "mean_rg_index ~ pr + tmmx + vpd + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree)"
)

# mod_lmm = lmer(mean_rg_index ~ year + pr + tmmx + vpd + smap + canopy_area + (1 | site/tree),
#                data = mod_df)

mod_test <- lmer(mean_rg_index ~ pr + tmmx + vpd + smap + year + canopy_area + (1 | site) +
                   (1| site:year) + (1 | site:tree), data = mod_df)

summary(mod_test)

# with(mod_df, addmargins(table(site,year)))
# 
# View(with(mod_df, addmargins(table(tree,site))))

#view vif
vif(lm(mean_rg_index ~ vpd + tmmx + smap + year + canopy_area, data = mod_df))

aic_list <- numeric()

for (i in seq_along(formula_list)) {
  reg_str <- as.formula(formula_list[i])
  res <- lmer(reg_str, data = mod_df, REML = FALSE)  # REML=FALSE for AIC comparison
  aic_list <- append(aic_list, AIC(res))
}

df_aic <- data.frame(
  model = formula_list,
  AIC = aic_list
)

df_aic$dAIC <- df_aic$AIC - min(df_aic$AIC)
print(df_aic)
model.sel(df_aic)

#full model is best, but the next best model is mean_rg_index ~ pr + tmmx + smap + year + canopy_area + (1 | site/tree) 
#has dAIC of 6.8, meaning that it is considerably different than the best model
#but still might want to consider it bc of how correlated temp and VPD are

sites_canopy_per_ha <- sites_df %>% 
  left_join(mod_df, by = "site") %>% 
  filter(year %in% 2025) %>% 
  group_by(site, area) %>% 
  summarize(total_canopy_area = sum(canopy_area)) %>% 
  filter(!site %in% "sweeten")

site_canopy_density_plot <- ggplot(sites_canopy_per_ha) +
  geom_point(aes(x = area, y = total_canopy_area, col = site)) +
  theme_classic() + 
  ylab("total canopy area (m2)") +
  xlab("total site area (ha)")

ggsave("site_canopy_density_plot.png", plot = site_canopy_density_plot)
