library(readr)
library(gcookbook)
library(ggthemes)
library(viridis)
library(gridExtra)
library(tidyverse)

#######
# T21 #
#######
sts <- readRDS("/Users/lkock/Desktop/STS/sts.RDS")
# Baseline model. Start 2013
baseline_seg <- read_delim("/Users/lkock/Desktop/Data/T21/segmented_prevalence_baseline.txt", 
                       ",", col_names = T)

# LK2 T21. Start 2013. Intervention start 2019. Reduction 16yo 0.4. Reduction uptake 16-24 0.3 
t21_seg_a <- read_delim("/Users/lkock/Desktop/Data/T21/segmented_prevalence_t21_0.4_0.3.txt", 
                           ",", col_names = T)

# LK2 T21. Start 2013. Intervention start 2019. Reduction 16yo 0.8. Reduction uptake 16-24 0.6
t21_seg_b <- read_delim("/Users/lkock/Desktop/Data/T21/segmented_prevalence_t21_0.8_0.6.txt", 
                        ",", col_names = T)

data_prep <- function(df, string){
  # Multiply each of the states by the weight for the subgroup
  df$ex <- df$`ex-smoker`*df$weight
  df$non <- df$`non-smoker`*df$weight
  df$quit <- df$quitter*df$weight
  df$smok <- df$smoker*df$weight
  
  # Sum together over the subgroups to get the group of interest https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
  df <- df %>% 
    group_by(year) %>%
    summarise(across(c(smok, weight), list(sum = sum)))
  
  # divide by the sum of weights of the minimal segments in the bigger segment. EX-SMOKER TEST
  df$smoker <- df$smok_sum/df$weight_sum
  
  df <- df %>% 
    group_by(year) %>%
    summarise(across(c(smoker, weight_sum)))
  
  df <- as.data.frame(df)
  
  df$subgroup <- c(string) 
  
  gather(df, State, Estimate, smoker, factor_key =  TRUE) %>%
    subset(State == "smoker")
}

# OVERALL #
# Baseline
baseline_overall <- baseline_seg

baseline_overall$smok <- baseline_overall$smoker*baseline_overall$weight

baseline_overall <- baseline_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

baseline_overall$smoker <- baseline_overall$smok_sum/baseline_overall$weight_sum

View(baseline_overall[,4]*100)

# T21A
t21_a_overall <- t21_seg_a

t21_a_overall$smok <- t21_a_overall$smoker*t21_a_overall$weight

t21_a_overall <- t21_a_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

t21_a_overall$smoker <- t21_a_overall$smok_sum/t21_a_overall$weight_sum

View(t21_a_overall[,4]*100)

# T21B
t21_b_overall <- t21_seg_b

t21_b_overall$smok <- t21_b_overall$smoker*t21_b_overall$weight

t21_b_overall <- t21_b_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

t21_b_overall$smoker <- t21_b_overall$smok_sum/t21_b_overall$weight_sum

View(t21_b_overall[,4]*100)



## SOCIAL GRADE ##

# Baseline series

# C2DE
df_c2de <- baseline_seg %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_abc1 <- baseline_seg %>%
  subset(sg == "AB" | sg == "C1")

df_c2de_long <- data_prep(df_c2de, "BASELINE C2DE")
df_abc1_long <- data_prep(df_abc1, "BASELINE ABC1")

# Tobacco 21 scenario A. Reduction 16yo 0.4. Reduction uptake 16-24 0.3 

df_t21_c2de <- t21_seg_a %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_t21_abc1 <- t21_seg_a %>%
  subset(sg == "AB" | sg == "C1")

df_t21_c2de_long <- data_prep(df_t21_c2de, "T21 C2DE")
df_t21_abc1_long <- data_prep(df_t21_abc1, "T21 ABC1")


df_merge_t21a <- rbind(df_c2de_long, df_t21_c2de_long, df_abc1_long, df_t21_abc1_long)
df_merge_t21a$Estimate <- df_merge_t21a$Estimate*100

sgz_colours <- c('hotpink', 'deeppink', 'deeppink3', 'deeppink4')
viridis_inferno <- c('orangered', 'deeppink3', 'darkorchid4', 'midnightblue')
viridis <- c('darkorchid4', 'cyan4', 'springgreen3', "greenyellow")
rocket <- c("midnightblue", "dodgerblue", "deeppink4", "deeppink")

t21a <- ggplot(data = df_merge_t21a, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("Scenario A") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 24, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2013.00, 2023.00))


# Tobacco 21 scenario B. Reduction 16yo 0.8. Reduction uptake 16-24 0.6 

df_t21_c2de <- t21_seg_b %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

df_t21_abc1 <- t21_seg_b %>%
  subset(sg == "AB" | sg == "C1")

df_t21_c2de_long <- data_prep(df_t21_c2de, "T21 C2DE")
df_t21_abc1_long <- data_prep(df_t21_abc1, "T21 ABC1")

df_merge_t21b <- rbind(df_c2de_long, df_t21_c2de_long, df_abc1_long, df_t21_abc1_long)
df_merge_t21b$Estimate <- df_merge_t21b$Estimate*100

t21b <- ggplot(data = df_merge_t21b, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("Scenario B") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 24, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = seq(2013.00, 2023.00))

ggarrange(t21a, t21b,
          common.legend = TRUE, widths = c(1, 1),
          ncol = 2, nrow = 1)


# Plotting change from baseline scenario 
Ab_change <- function(df1, df2, r1, r2, r3, r4, c1, string, string2, string3, string4) {
  w <- -(df1[r3, c1] - df1[r4, c1])
  x <- -(df1[r1, c1] - df1[r2, c1])
  y <- -(df2[r3, c1] - df2[r4, c1])
  z <- -(df2[r1, c1] - df2[r2, c1])
  
  A <- cbind(w, x)
  colnames(A) <- c(string, string2)
  B <- cbind(y, z)
  colnames(B) <- c(string3, string4)
  data <- cbind(A,B) 
  print(data)
}

absolute_change <- Ab_change(df_merge_t21a, df_merge_t21b, 120, 240, 360, 480, 5, "A_ABC1", "A_C2DE", "B_ABC1", "B_C2DE") %>%
  as.data.frame()

t21_long_absolute <- gather(absolute_change, Group, factor_key =  T)

scenario <- c("A", "A", "B", "B")
sgrade <- c("ABC1", "C2DE", "ABC1", "C2DE")
t21_long_absolute <- cbind(scenario, sgrade, t21_long_absolute) 
t21_long_absolute <- t21_long_absolute %>% 
  mutate_if(is.numeric, round, digits = 2)

absolute <- ggplot(t21_long_absolute, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A C2DE", "A ABC1", "B C2DE", "B ABC1")) +
  ylab("Absolute % change") + xlab("") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("", "", "", "")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_text(size = 14)) +
  ggtitle("")


# Relative inequality - relative difference between change from baseline in each group under each intervention scenario
ineq_rel <- function(df1, df2, r1, r2, r3, r4, c1, string, string2, string3, string4){
  w <- (1 - (df1[r3, c1]/df1[r4, c1]))*100
  x <- (1 - (df1[r1, c1]/df1[r2, c1]))*100
  y <- (1 - (df2[r3, c1]/df2[r4, c1]))*100
  z <- (1 - (df2[r1, c1]/df2[r2, c1]))*100
  
  A <- cbind(w, x)
  colnames(A) <- c(string, string2)
  B <- cbind(y, z)
  colnames(B) <- c(string3, string4)
  data <- cbind(A,B) 
  print(data)
}

rel_change <- ineq_rel(df_merge_t21a, df_merge_t21b, 120, 240, 360, 480, 5, "A ABC1", "A C2DE", "B ABC1", "B C2DE") %>% as.data.frame()

t21_rel_long <- gather(rel_change, Group, factor_key =  T)
t21_rel_long <- cbind(scenario, sgrade, t21_rel_long)
t21_rel_long <- t21_rel_long %>% 
  mutate_if(is.numeric, round, digits = 2)

relative <- ggplot(t21_rel_long, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A C2DE", "A ABC1", "B C2DE", "B ABC1")) +
  ylab("Relative % change") + xlab("Social grade") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("ABC1", "C2DE", "ABC1", "C2DE")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  ggtitle("")

grid.arrange(arrangeGrob(absolute, relative, nrow = 2),
            heights = c(15))



## REGION ##

# Baseline series

df_n <- baseline_seg %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

df_lon <- baseline_seg %>%
  subset(region == "LONDON")

df_n_long <- data_prep(df_n, "Baseline North")
df_lon_long <- data_prep(df_lon, "Baseline London")

# Tobacco 21 scenario A. Reduction 16yo 0.4. Reduction uptake 16-24 0.3 

df_t21_n <- t21_seg_a %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

df_t21_lon <- t21_seg_a %>%
  subset(region == "LONDON")

df_n_long_t21 <- data_prep(df_t21_n, "T21 North")
df_lon_long_t21 <- data_prep(df_t21_lon, "T21 London")


df_merge_t21a <- rbind(df_n_long, df_n_long_t21, df_lon_long, df_lon_long_t21)
df_merge_t21a$Estimate <- df_merge_t21a$Estimate*100

t21a <- ggplot(data = df_merge_t21a, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("Scenario A") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 25) +
  theme_clean(base_size = 24, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2013.00, 2023.00))


# Tobacco 21 scenario B. Reduction 16yo 0.8. Reduction uptake 16-24 0.6 

df_t21_n <- t21_seg_b %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

df_t21_lon <- t21_seg_b %>%
  subset(region == "LONDON")

df_n_long_t21 <- data_prep(df_t21_n, "T21 North")
df_lon_long_t21 <- data_prep(df_t21_lon, "T21 London")


df_merge_t21b <- rbind(df_n_long, df_n_long_t21, df_lon_long, df_lon_long_t21)
df_merge_t21b$Estimate <- df_merge_t21b$Estimate*100


t21b <- ggplot(data = df_merge_t21b, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("Scenario B") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 25) +
  theme_clean(base_size = 24, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2013.00, 2023.00))

ggarrange(t21a, t21b,
          common.legend = TRUE, widths = c(1, 1),
          ncol = 2, nrow = 1)



absolute_change <- Ab_change(df_merge_t21a, df_merge_t21b, 120, 240, 360, 480, 5, "A London", "A North", "B London", "B North") %>%
  as.data.frame()

t21_long_absolute <- gather(absolute_change, Group, factor_key =  T)

scenario <- c("A", "A", "B", "B")
region <- c("London", "North", "London", "North")
t21_long_absolute <- cbind(scenario, region, t21_long_absolute) 
t21_long_absolute <- t21_long_absolute %>% 
  mutate_if(is.numeric, round, digits = 2)

absolute_reg <- ggplot(t21_long_absolute, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A North", "A London", "B North", "B London")) +
  ylab("Absolute % change") + xlab("Social grade") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("London", "North", "London", "North")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.title.x = (element_blank()), 
        axis.text.x = element_blank(), axis.title = element_text(size = 14)) +
  ggtitle("")


# Relative change - relative change from baseline in each group under each intervention scenario

rel_change <- ineq_rel(df_merge_t21a, df_merge_t21b, 120, 240, 360, 480, 5, "A ABC1", "A C2DE", "B ABC1", "B C2DE") %>% as.data.frame()

t21_rel_long <- gather(rel_change, Group, factor_key =  T)
t21_rel_long <- cbind(scenario, region, t21_rel_long)
t21_rel_long <- t21_rel_long %>% 
  mutate_if(is.numeric, round, digits = 2)

relative_reg <- ggplot(t21_rel_long, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A N", "A LON", "B N", "B LON")) +
  ylab("Relative % change") + xlab("Region") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("London", "North East & North West", "London", "North East & North West")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  ggtitle("")

grid.arrange(arrangeGrob(absolute_reg, relative_reg, nrow = 2),
             heights = c(15))


# Little change in relative inequality..slight reduction possibly


