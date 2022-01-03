#Figure 4 column graphs of phenotype proportions

library(dplyr)
library(ggplot2)
library(ggfortify)
library(readxl)

#The two Fig 4 Excel files include raw data and tidied proportion data for this script
lens_defects <- read_excel("Fig 4 Proportion data.xlsx")

#Filter defects into separate data sets
rough<-filter(lens_defects, Defect == "Rough")
pits<-filter(lens_defects, Defect == "Pits")
peripheral<-filter(lens_defects, Defect == "Peripheral")
disorganized<-filter(lens_defects, Defect == "Disorganized")
severe<-filter(lens_defects, Defect == "Severe")
any<-filter(lens_defects, Defect == "Any")

#rough graph
ggplot(rough, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,50)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                             breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                             labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")
        
ggsave("Rough.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#pits graph
ggplot(pits, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,50)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                   breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")

ggsave("Pits.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#peripheral graph
ggplot(peripheral, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,50)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                   breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")

ggsave("Peripheral.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#disorganized graph
ggplot(disorganized, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,50)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                   breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")

ggsave("Disorganized.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#severe graph
ggplot(severe, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,50)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                   breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")

ggsave("Severe.jpg", width = 6, height = 3.5, units = "in", dpi = 300)

#any graph
ggplot(any, aes(x = Genotype, y = Proportion, fill= factor (Age)))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x=NULL, y=NULL)+
  ylim(0,80)+
  scale_fill_manual(values=c("grey", "white"))+
  scale_x_discrete(limits=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"), 
                   breaks=c("wildtype", "cryaa", "pgRNA", "cryaba", "cryabb"),
                   labels=c("Wildtype", "cryaa-/-", "pgRNA", "cryaba-/-", "cryabb-/-"))+
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.title.y  = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        legend.position="none")

ggsave("Any.jpg", width = 6, height = 3.5, units = "in", dpi = 300)