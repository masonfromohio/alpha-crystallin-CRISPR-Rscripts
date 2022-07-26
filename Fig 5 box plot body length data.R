#Figure 5 box plots for body length and lens size analysis

#install libraries
library(dplyr)
library(ggplot2)
library(ggfortify)

#import data
library(readxl)
Lens_size <- read_excel("Fig 5 lens diameter and body size data.xlsx")

#Change the age between 3 and 4 to produce plots for each age
#There are three blocks of script below for body length, lens diameter and ratio of the two

#This block makes plots of body length 
Lens_size <- Lens_size %>% filter(age == 3)
size_plot<-ggplot(Lens_size, aes(x=gene, y=body)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  labs(x=NULL, y=NULL)+
  ylim(2.5, 4)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
    scale_x_discrete(limits=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"),
                   labels=c("wild type 1", "wild type 2", "cryaa-/-", "cryaba-/-", "cryabb-/-"))
size_plot

size_plot+ggsave("3dpf body.tiff", width = 6, height = 3.5, units = "in", dpi = 300)

#Run Statistics with Tukey Honest Significant Difference (HSD) post test adjusted p-values for body length
Stat_test <- lm(body ~ gene,
                data = Lens_size)
autoplot(Stat_test, smooth.colour = NA)
anova(Stat_test)
summary(Stat_test)

TukeyHSD(aov(body ~ gene,data = Lens_size))

#This block plots and analyzes lens diameter
size_plot<-ggplot(Lens_size, aes(x=gene, y=lens)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  labs(x=NULL, y=NULL)+
  ylim(80, 140)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_x_discrete(limits=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"),
                   labels=c("wild type 1", "wild type 2", "cryaa-/-", "cryaba-/-", "cryabb-/-"))
size_plot

size_plot+ggsave("3dpf lens.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#Run Statistics with Tukey Honest Significant Difference (HSD) post test adjusted p-values
Stat_test <- lm(lens ~ gene,
                data = Lens_size)
autoplot(Stat_test, smooth.colour = NA)
anova(Stat_test)
summary(Stat_test)

TukeyHSD(aov(lens ~ gene,data = Lens_size))

#This block plots and analyzes the ration of body length to lens diameter
size_plot<-ggplot(Lens_size, aes(x=gene, y=ratio)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  labs(x=NULL, y=NULL)+
  ylim(30, 50)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_x_discrete(limits=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype_1", "Wildtype_2", "cryaa", "cryaba", "cryabb"),
                   labels=c("wild type 1", "wild type 2", "cryaa-/-", "cryaba-/-", "cryabb-/-"))
size_plot

size_plot+ggsave("3dpf ratio.tiff", width = 6, height = 3.5, units = "in", dpi = 300)

#Run Statistics with Tukey Honest Significant Difference (HSD) post test adjusted p-values
Stat_test <- lm(ratio ~ gene,
                data = Lens_size)
autoplot(Stat_test, smooth.colour = NA)
anova(Stat_test)
summary(Stat_test)

TukeyHSD(aov(body ~ gene,data = Lens_size))


