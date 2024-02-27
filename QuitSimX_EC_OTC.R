library(readr)
library(gcookbook)
library(ggthemes)
library(viridis)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)

####################
# EC push from OTC #
####################

# LK3 EC. Start 2015.. 
baseline_ec_seg <- read_delim("/Users/lkock/Desktop/Data/EC move/NRT OTC/segmented_prevalence_baseline.txt", 
                              ",", col_names = T)

# LK3 EC. Start 2015.Intervention start 2016. 0.33 moved from OTC 
ec_seg_0.33 <- read_delim("/Users/lkock/Desktop/Data/EC move/NRT OTC/segmented_prevalence_EC_OTC_0.33.txt", 
                          ",", col_names = T)

# LK3 EC. Start 2015.Intervention start 2016. 0.66 moved from OTC 
ec_seg_0.66 <- read_delim("/Users/lkock/Desktop/Data/EC move/NRT OTC/segmented_prevalence_EC_OTC_0.66.txt", 
                          ",", col_names = T)

# LK3 EC. Start 2015.Intervention start 2016. all moved from OTC 
ec_seg_all <- read_delim("/Users/lkock/Desktop/Data/EC move/NRT OTC/segmented_prevalence_EC_OTC_All.txt", 
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

## OVERALL ##
# Baseline
baseline_ec_overall <- baseline_ec_seg

baseline_ec_overall$smok <- baseline_ec_overall$smoker*baseline_ec_overall$weight

baseline_ec_overall <- baseline_ec_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

baseline_ec_overall$smoker <- baseline_ec_overall$smok_sum/baseline_ec_overall$weight_sum

View(baseline_ec_overall)
View(baseline_ec_overall[,4]*100)

# 0.33 moved 
# Baseline
baseline_ec33_overall <- ec_seg_0.33

baseline_ec33_overall$smok <- baseline_ec33_overall$smoker*baseline_ec33_overall$weight

baseline_ec33_overall <- baseline_ec33_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

baseline_ec33_overall$smoker <- baseline_ec33_overall$smok_sum/baseline_ec33_overall$weight_sum

View(baseline_ec33_overall)

# 0.66
baseline_ec66_overall <- ec_seg_0.66

baseline_ec66_overall$smok <- baseline_ec66_overall$smoker*baseline_ec66_overall$weight

baseline_ec66_overall <- baseline_ec66_overall %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

baseline_ec66_overall$smoker <- baseline_ec66_overall$smok_sum/baseline_ec66_overall$weight_sum

View(baseline_ec66_overall)

# All
baseline_ec_all_overall <- ec_seg_all

ec_seg_all$smok <- ec_seg_all$smoker*ec_seg_all$weight

ec_seg_all <- ec_seg_all %>% 
  group_by(year) %>%
  summarise(across(c(smok, weight), list(sum = sum)))

ec_seg_all$smoker <- ec_seg_all$smok_sum/ec_seg_all$weight_sum

View(ec_seg_all)




## SOCIAL GRADE ##

# Baseline series

# C2DE
df_c2de <- baseline_ec_seg %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_abc1 <- baseline_ec_seg %>%
  subset(sg == "AB" | sg == "C1")

df_c2de_long <- data_prep(df_c2de, "BASELINE C2DE")
df_abc1_long <- data_prep(df_abc1, "BASELINE ABC1")

# EC scenario A. 0.33 moved from OTC 

df_ec_c2de <- ec_seg_0.33 %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_ec_abc1 <- ec_seg_0.33 %>%
  subset(sg == "AB" | sg == "C1")

df_ec_c2de_long <- data_prep(df_ec_c2de, "EC C2DE")
df_ec_abc1_long <- data_prep(df_ec_abc1, "EC ABC1")


df_merge_ec_a <- rbind(df_c2de_long, df_ec_c2de_long, df_abc1_long, df_ec_abc1_long)
df_merge_ec_a$Estimate <- df_merge_ec_a$Estimate*100

sgz_colours <- c('hotpink', 'deeppink', 'deeppink3', 'deeppink4')
viridis_inferno <- c('orangered', 'deeppink3', 'darkorchid4', 'midnightblue')
viridis <- c('darkorchid4', 'cyan4', 'springgreen3', "greenyellow")
rocket <- c("midnightblue", "dodgerblue", "deeppink4", "deeppink")

ec_a <- ggplot(data = df_merge_ec_a, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("A) 1/3 moved to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))

# EC scenario B. 0.66 moved from OTC 

df_ec_c2de <- ec_seg_0.66 %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_ec_abc1 <- ec_seg_0.66 %>%
  subset(sg == "AB" | sg == "C1")

df_ec_c2de_long <- data_prep(df_ec_c2de, "EC C2DE")
df_ec_abc1_long <- data_prep(df_ec_abc1, "EC ABC1")

df_merge_ec_b <- rbind(df_c2de_long, df_ec_c2de_long, df_abc1_long, df_ec_abc1_long)
df_merge_ec_b$Estimate <- df_merge_ec_b$Estimate*100

ec_b <- ggplot(data = df_merge_ec_b, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("B) 2/3 moved to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))


# EC scenario C. All moved from OTC 

df_ec_c2de <- ec_seg_all %>%
  subset(sg == "C2" | sg == "D" | sg =="E")

# ABC1
df_ec_abc1 <- ec_seg_all %>%
  subset(sg == "AB" | sg == "C1")

df_ec_c2de_long <- data_prep(df_ec_c2de, "EC C2DE")
df_ec_abc1_long <- data_prep(df_ec_abc1, "EC ABC1")

df_merge_ec_c <- rbind(df_c2de_long, df_ec_c2de_long, df_abc1_long, df_ec_abc1_long)
df_merge_ec_c$Estimate <- df_merge_ec_c$Estimate*100

ec_c <- ggplot(data = df_merge_ec_c, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("C) All moved from OTC to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))

plot_figure <- ggarrange(ec_a, ec_b, ec_c,
                         common.legend = TRUE, widths = c(1, 1),
                         labels = c("", "", ""), hjust = -0.0, vjust = 1.0,
                         ncol = 1, nrow = 3)

ggsave("Figure_ec.png", plot_figure, width = 10, height = 20, dpi = 300)

getwd()

# Plotting absolute change from baseline scenario
Ab_change2 <- function(df1, df2, df3, r1, r2, r3, r4, c1, string, string2, string3, string4, string5, string6) {
  u <- -(df1[r3, c1] - df1[r4, c1])
  v <- -(df1[r1, c1] - df1[r2, c1])
  w <- -(df2[r3, c1] - df2[r4, c1])
  x <- -(df2[r1, c1] - df2[r2, c1])
  y <- -(df3[r3, c1] - df3[r4, c1])
  z <- -(df3[r1, c1] - df3[r2, c1])
  
  A <- cbind(u, v)
  colnames(A) <- c(string, string2)
  B <- cbind(w, x)
  colnames(B) <- c(string3, string4)
  C <- cbind(y, z)
  colnames(C) <- c(string5, string6)
  data <- cbind(A, B, C) 
  print(data)
}

absolute_change <- Ab_change2(df_merge_ec_a, df_merge_ec_b, df_merge_ec_c, 120, 240, 360, 480, 5, "A_ABC1", "A_C2DE", "B_ABC1", "B_C2DE", "C_ABC1", "C_C2DE") %>%
  as.data.frame()

ec_long_absolute <- gather(absolute_change, Group, factor_key =  T)

scenario <- c("A) 1/3 shift to EC", "A) 1/3 shift to EC", "B) 2/3 shift to EC", "B) 2/3 shift to EC", "C) All shift to EC", "C) All shift to EC")
sgrade <- c("ABC1", "C2DE", "ABC1", "C2DE", "ABC1", "C2DE")
ec_long_absolute <- cbind(scenario, sgrade, ec_long_absolute) 
ec_long_absolute <- ec_long_absolute %>% 
  mutate_if(is.numeric, round, digits = 2)

absolute <- ggplot(ec_long_absolute, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A C2DE", "A ABC1", "B C2DE", "B ABC1", "C C2DE", "C ABC1")) +
  ylab("Absolute % change") + xlab("Social grade") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("ABC1", "C2DE", "ABC1", "C2DE", "ABC1", "C2DE")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  ggtitle("")

# Relative inequality - relative difference between change from baseline in each group under each intervention scenario

ineq_rel2 <- function(df1, df2, df3, r1, r2, r3, r4, c1, string, string2, string3, string4, string5, string6){
  u <- (1 - (df1[r3, c1]/df1[r4, c1]))*100
  v <- (1 - (df1[r1, c1]/df1[r2, c1]))*100
  w <- (1 - (df2[r3, c1]/df2[r4, c1]))*100
  x <- (1 - (df2[r1, c1]/df2[r2, c1]))*100
  y <- (1 - (df3[r3, c1]/df3[r4, c1]))*100
  z <- (1 - (df3[r1, c1]/df3[r2, c1]))*100
  
  A <- cbind(u, v)
  colnames(A) <- c(string, string2)
  B <- cbind(w, x)
  colnames(B) <- c(string3, string4)
  C <- cbind(y, z)
  colnames(C) <- c(string5, string6)
  data <- cbind(A, B, C) 
  print(data)
}

rel_change <- ineq_rel2(df_merge_ec_a, df_merge_ec_b, df_merge_ec_c, 120, 240, 360, 480, 5, "A ABC1", "A C2DE", "B ABC1", "B C2DE", "C ABC1", "C C2DE") %>% as.data.frame()

ec_rel_long <- gather(rel_change, Group, factor_key =  T)
ec_rel_long <- cbind(scenario, sgrade, ec_rel_long)
ec_rel_long <- ec_rel_long %>% 
  mutate_if(is.numeric, round, digits = 2)

relative <- ggplot(ec_rel_long, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A C2DE", "A ABC1", "B C2DE", "B ABC1", "C ABC1", "C C2DE")) +
  ylab("Relative % change") + xlab("Social grade") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("ABC1", "C2DE", "ABC1", "C2DE", "ABC1", "C2DE")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  ggtitle("")

grid.arrange(arrangeGrob(absolute, relative, nrow = 2),
             heights = c(15))

# These are equity positive...



## REGION ##

# Baseline series

df_n <- baseline_ec_seg %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

df_lon <- baseline_ec_seg %>%
  subset(region == "LONDON")

df_n_long <- data_prep(df_n, "Baseline North East & North West")
df_lon_long <- data_prep(df_lon, "Baseline London")

# EC scenario A. 0.33 moved from OTC 

# NORTH
df_ec_n <- ec_seg_0.33 %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

# LONDON
df_ec_lon <- ec_seg_0.33 %>%
  subset(region == "LONDON")

df_ec_n_long <- data_prep(df_ec_n, "EC North East &  North West")
df_ec_lon_long <- data_prep(df_ec_lon, "EC London")

df_merge_ec_a <- rbind(df_n_long, df_ec_n_long, df_lon_long, df_ec_lon_long)
df_merge_ec_a$Estimate <- df_merge_ec_a$Estimate*100

reg_ec_a <- ggplot(data = df_merge_ec_a, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("A) 1/3 moved to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))

# EC scenario B. 0.66 moved from OTC 

df_ec_n <- ec_seg_0.66 %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

# ABC1
df_ec_lon <- ec_seg_0.66 %>%
  subset(region == "LONDON")

df_ec_n_long <- data_prep(df_ec_n, "EC North East &  North West")
df_ec_lon_long <- data_prep(df_ec_lon, "EC London")

df_merge_ec_b <- rbind(df_n_long, df_ec_n_long, df_lon_long, df_ec_lon_long)
df_merge_ec_b$Estimate <- df_merge_ec_b$Estimate*100

reg_ec_b <- ggplot(data = df_merge_ec_b, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("B) 2/3 moved to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))


# EC scenario C. All moved from OTC 

df_ec_n <- ec_seg_all %>%
  subset(region == "NORTH EAST" | region == "NORTH WEST")

# ABC1
df_ec_lon <- ec_seg_all %>%
  subset(region == "LONDON")

df_ec_n_long <- data_prep(df_ec_n, "EC North East & North West")
df_ec_lon_long <- data_prep(df_ec_lon, "EC London")

df_merge_ec_c <- rbind(df_n_long, df_ec_n_long, df_lon_long, df_ec_lon_long)
df_merge_ec_c$Estimate <- df_merge_ec_c$Estimate*100

reg_ec_c <- ggplot(data = df_merge_ec_c, aes(x = year, y = Estimate, color = subgroup)) +
  geom_line(aes(colour = subgroup), size = 1.3) +
  scale_colour_manual(name = "State", values = rocket) +
  ggtitle("C) All moved to e-cigarettes") + 
  xlab('Year') + ylab('Smoking prevalence (%)') +
  ylim(10, 30) +
  theme_clean(base_size = 18, base_family = "sans") +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_x_continuous(breaks = seq(2015.00, 2025.00))


plot_figure <- ggarrange(reg_ec_a, reg_ec_b, reg_ec_c,
                         common.legend = TRUE, widths = c(1, 1),
                         labels = c("", "", ""), hjust = -0.0, vjust = 1.0,
                         ncol = 1, nrow = 3)

ggsave("Figure_ec_region.png", plot_figure, width = 12, height = 20, dpi = 300)


absolute_change <- Ab_change2(df_merge_ec_a, df_merge_ec_b, df_merge_ec_c, 120, 240, 360, 480, 5, "A LONDON", "A NORTH EAST & NORTH WEST", 
                              "B LONDON", "B NORTH EAST & NORTH WEST", "C LONDON", "C NORTH EAST & NORTH WEST") %>% as.data.frame()

ec_long_absolute <- gather(absolute_change, Group, factor_key =  T)

scenario <- c("A) 1/3 shift to EC", "A) 1/3 shift to EC", "B) 2/3 shift to EC", "B) 2/3 shift to EC", "C) All shift to EC", "C) All shift to EC")
region <- c("London", "North", "London", "North", "London", "North")
ec_long_absolute <- cbind(scenario, sgrade, ec_long_absolute) 
ec_long_absolute <- ec_long_absolute %>% 
  mutate_if(is.numeric, round, digits = 2)

absolute <- ggplot(ec_long_absolute, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A London", "A North", "B London", "B North", "C London", "C North")) +
  ylab("Absolute % change") + xlab("Social grade") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("London", "North", "London", "North", "London", "North")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  ggtitle("")

# Relative inequality - relative difference between change from baseline in each group under each intervention scenario

rel_change <- ineq_rel2(df_merge_ec_a, df_merge_ec_b, df_merge_ec_c, 120, 240, 360, 480, 5, "A London", "A North East & North West", "B London", "B North East & North West",
                        "C London", "C North East & North West") %>% as.data.frame()

ec_rel_long <- gather(rel_change, Group, factor_key =  T)
ec_rel_long <- cbind(scenario, region, ec_rel_long)
ec_rel_long <- ec_rel_long %>% 
  mutate_if(is.numeric, round, digits = 2)

relative <- ggplot(ec_rel_long, aes(factor(Group), value, fill = Group)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  facet_wrap(~ scenario, scales = "free_x") +
  scale_fill_manual(name = "Social grade", values = c("deeppink4", "deeppink", "deeppink4", "deeppink", "deeppink4", "deeppink"), 
                    labels = c("A London", "A NE & NW", "B London", "B NE & NW", "C London", "C NE & NW")) +
  ylab("Relative % change") + xlab("Region") +
  geom_text(aes(label = value), vjust = 1.5) +
  scale_x_discrete(labels = c("London", "North East & North West", "London", "North East & North West", "London", "North East & North West")) +
  theme_clean() +
  theme(legend.position = "none", text = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  ggtitle("")

grid.arrange(arrangeGrob(absolute, relative, nrow = 2),
             heights = c(15))
