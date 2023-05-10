getwd()
setwd("C:/Users/monic/OneDrive/Desktop/Tesis")

library("tidyverse")
library("caret")
library("tidymodels")
library("themis")
library("rpart.plot")
library("ranger")
library("ggpubr")
library("gglasso")
library("ggplot2")


dat <- read_csv("Heart-Data2.csv")
dat[sapply(dat, is.character)] <- lapply(dat[sapply(dat, is.character)],
                                         as.factor)
head(dat)
colnames(dat)
levels(dat$Race) <- c("American Indian/\nAlaskan Native", "Asian", "Black", "Hispanic", 
                      "Other", "White")


#Function to calculate probabilities
probs <- function(df){
  hy <- which(df[,1] == 'Yes')
  hh <- df[hy,]$n
  phav <- hh[2]/sum(hh)
  
  hn <- which(df[,1] == 'No')
  hh <- df[hn,]$n
  pno <- hh[2]/sum(hh)
  c(phav,pno)
}
df <- dat %>% count(DiffWalking,HeartDisease)#Change variables
probs(df)

#Percentage Plots
h <- dat %>% count(Race,HeartDisease) 
h <- h %>% group_by(Race) %>% summarise(percent = f(n))

#Change h for each variable for each plot
prace <- ggplot(data = h, aes(x = Race, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("Percent")+ggtitle("Percent of Individuals with Heart Disease for each Race")
page <- ggplot(data = h, aes(x = as.factor(AgeCategory),y = percent))+
  geom_col()+theme_minimal()+
  xlab("Age Category")+
  ylab("Percent")+ggtitle("Percent of Individuals with Heart Disease for Age")
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete()+scale_y_continuous(labels = percent)+
psex <- ggplot(data = h, aes(x = Sex, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("Percent")+ggtitle("Percent of Individuals with Heart Disease")
pasthm <- ggplot(data = h, aes(x = Asthma, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("")
pdw <- ggplot(data = h, aes(x = DiffWalking, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("")
pstroke <- ggplot(data = h, aes(x = Stroke, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("")
pdiab <-  ggplot(data = h, aes(x = Diabetic, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("")
psmoke <- ggplot(data = h, aes(x = Smoking, y=percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent) +ylab("Percent")
pdiab <- ggplot(data = dat, aes(x = Diabetic, fill = HeartDisease))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")
pkd <- ggplot(data = h, aes(x = KidneyDisease, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = percent)+
  ylab("Percent")
psc <- ggplot(data = h, aes(x = SkinCancer, y = percent))+
  geom_col()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_y_continuous(labels = percent)+
  ylab("")
#Do nor change h for Sleep
psleep <- ggplot(data = dat, aes(x = SleepTime, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))

#Graficos
pp <- ggarrange(pkd,psc,pasthm,pdw,pstroke,pdiab ,ncol = 6, 
                common.legend = T,legend = "bottom")

annotate_figure(pp, 
                top = text_grob("Percentage of Individuals with Heart Disease by Health Conditions", 
                size = 14))
ggarrange(prace,                                                 # First row with scatter plot
          ggarrange(page, psex, ncol = 2, labels = c("B", "C"),
                    common.legend = T,legend = "none"), # Second row with box and dot plots
          nrow = 2, 
          labels = "A" ,common.legend = T,legend = "bottom"                                       # Labels of the scatter plot
) 

ggarrange(psleep,
          ncol = 1,
          ggarrange(psmoke, palco,pact, ncol = 3,
                    labels = c("B", "C", "D"),
                    common.legend = T,legend = "bottom"), 
          labels = c("A", ""))


#Odds CI
library("epitools")
#Change M for each variable
M <- table(Var = dat$PhysicalActivity,HD = dat$HeartDisease)
oddsratio(M)$measure










