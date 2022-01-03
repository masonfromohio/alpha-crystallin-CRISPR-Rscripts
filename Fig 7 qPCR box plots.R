#Box plot of qPCR Cq values

#install libraries
library(dplyr)
library(ggplot2)
library(ggfortify)
library(readxl)


#import data
qPCR_data <- read_excel("Fig 7 qPCR data.xlsx")

#filter data for each gene
cryaa<-filter(qPCR_data, Gene == "cryaa")
cryaba<-filter(qPCR_data, Gene == "cryaba")
cryabb<-filter(qPCR_data, Gene == "cryabb")

#cryaa expression plot
ggplot(cryaa, aes(x=Sample, y=Delta_Ct)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  scale_y_reverse( lim=c(15,0))+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
        scale_x_discrete(limits=c("Wildtype", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype", "cryaa", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "cryaba-/-", "cryabb-/-"))

ggsave("cryaa qpcr plot.jpg", width = 6, height = 3, units = "in", dpi = 300)

#cryaba expression plot
ggplot(cryaba, aes(x=Sample, y=Delta_Ct)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  scale_y_reverse( lim=c(15,0))+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
        scale_x_discrete(limits=c("Wildtype", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype", "cryaa", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "cryaba-/-", "cryabb-/-"))

ggsave("cryaba qpcr plot.jpg", width = 6, height = 3, units = "in", dpi = 300)

#cryabb expression plot
ggplot(cryabb, aes(x=Sample, y=Delta_Ct)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.075, color="grey")+
  scale_y_reverse( lim=c(15,0))+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(face="bold", size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
        scale_x_discrete(limits=c("Wildtype", "cryaa", "cryaba", "cryabb"), 
                   breaks=c("Wildtype", "cryaa", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "cryaba-/-", "cryabb-/-"))

ggsave("cryabb qpcr plot.jpg", width = 6, height = 3, units = "in", dpi = 300)


#Run Statistics with Tukey Honest Significant Difference (HSD) post test adjusted p-values
#cryaa
aA_exp<-lm(Delta_Ct ~ Sample,
           data = cryaa)
autoplot(aA_exp, smooth.colour = NA)
anova(aA_exp)
summary(aA_exp)

TukeyHSD(aov(Delta_Ct ~ Sample,
             data = cryaa))
#cryaba
aBa_exp<-lm(Delta_Ct ~ Sample,
            data = cryaba)
autoplot(aBa_exp, smooth.colour = NA)
anova(aBa_exp)
summary(aBa_exp)

TukeyHSD(aov(Delta_Ct ~ Sample,
             data = cryaba))

#cryabb
aBb_exp<-lm(Delta_Ct ~ Sample,
            data = cryabb)
autoplot(aBb_exp, smooth.colour = NA)
anova(aBb_exp)
summary(aBb_exp)

TukeyHSD(aov(Delta_Ct ~ Sample,
             data = cryabb))
