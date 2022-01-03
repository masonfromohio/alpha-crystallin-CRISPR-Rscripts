#This script produces a scatter plot of DAPI intensity across the lens to quantify 
#fiber cell denucleation. It is used in Figure 6.
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

#import data
DAPI <- read_excel("Fig 6 Nuclear Stain analysis.xlsx")

#Filter to separate 3 and 4 dpf
DAPI3<-filter(DAPI, Age == 3)
DAPI4<-filter(DAPI, Age == 4)

#Data plot
ggplot(data = DAPI3, mapping = aes(x = Circle, y = Mean, color = Genotype, shape = Genotype, linetype = Genotype)) +
  geom_smooth(show.legend = FALSE)+
    geom_jitter(width = 0.1, show.legend = FALSE, size=2)+
  scale_x_continuous(breaks=seq(0,10,2))+
  labs(x="Lens Radius", y="DAPI Intensity")+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("3dpf DAPI.jpg", width = 5, height = 3.5, units = "in", dpi = 300)

#Run an independent t-test, AKA a Welch test
#need to filter data by circle number
#source for stats scripts: https://www.datanovia.com/en/lessons/t-test-in-r/#demo-data-1
#source for Mann-Whitney test: https://www.statology.org/mann-whitney-u-test-r/

#enter each circle to filter before running Mann-Whitney U test
#required as not all groups have normal distribution found with the Shapiro test
Circle<-filter(DAPI3, Circle == 5)

#tests for normalcy
Circle %>%
  group_by(Genotype) %>%
  shapiro_test(Mean)

#Data not normal, so used Mann-Whitney U Test
Circle %>% wilcox_test(Mean~Genotype)

#If normal you could use t-test
stat.test <- Circle %>%
  t_test(Mean~Genotype) %>%
  add_significance()
stat.test




