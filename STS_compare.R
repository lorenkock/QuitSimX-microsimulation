library(haven)
library(tigerstats)
library(tidyverse)
library(survey)
library(gtsummary)
library(DescTools)

sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")
names(sts)[names(sts)=="@weight0"] <- "weight0"
sts_eng <- sts %>% subset(gore < 10)

abc1 <- sts_eng %>% subset(randm == 0)
t(colPerc(xtabs(abc1$weight0 ~ abc1$cigsmok + abc1$xyear)))

c2de <- sts_eng %>% subset(randm == 1)
t(colPerc(xtabs(c2de$weight0 ~ c2de$cigsmok + c2de$xyear)))
