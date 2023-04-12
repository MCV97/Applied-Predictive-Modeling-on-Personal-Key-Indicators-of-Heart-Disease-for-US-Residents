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



dat %>% count(MentalHealth,HeartDisease)

dat %>% count(Asthma,HeartDisease) 

probs <- function(df){
  hy <- which(df[,1] == 'Yes')
  hh <- df[hy,]$n
  phav <- hh[2]/sum(hh)
  
  hn <- which(df[,1] == 'No')
  hh <- df[hn,]$n
  pno <- hh[2]/sum(hh)
  c(phav,pno)
}
h <- dat %>% count(PhysicalActivity,HeartDisease) 
probs(h)




prace <- ggplot(data = dat, aes(x = Race, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma)


psex <- ggplot(data = dat, aes(x = Sex, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")

psmoke <- ggplot(data = dat, aes(x = Smoking, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

pdiab <- ggplot(data = dat, aes(x = Diabetic, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")

ggplot(data = dat, aes(x = GenHealth, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

pasthm <- ggplot(data = dat, aes(x = Asthma, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")

pstroke <- ggplot(data = dat, aes(x = Stroke, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")

pkd <- ggplot(data = dat, aes(x = KidneyDisease, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

psc <- ggplot(data = dat, aes(x = SkinCancer, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +ylab("")

page <- ggplot(data = dat, aes(x = as.factor(AgeCategory), fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete() +
  scale_y_continuous(labels = comma) +xlab("AgeCategory")

pdw <- ggplot(data = dat, aes(x = DiffWalking, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 


ggplot(data = dat, aes(x = BMI, color  = HeartDisease))+
  geom_bar(show_guide=F)+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

ggplot(data = dat, aes(x = BMI, fill  = HeartDisease))+
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 


 ggplot(data = dat, aes(x = SleepTime, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

 psleep <- ggplot(data = dat, aes(x = SleepTime, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))
 
 psleep <- ggplot(data = dat, aes(x = SleepTime, fill = HeartDisease))+
   geom_boxplot()+theme_minimal()+
   theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
   theme(axis.text.y=element_blank())

ggplot(data = dat, aes(x = AgeCategory, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

#Que se ve mejor, geombar o geompoly?
ggplot(data = dat, aes(x = AgeCategory, fill  = HeartDisease))+
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

ggplot(data = dat, aes(x = PhysicalHealth, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_y_continuous(labels = comma) 

ggplot(data = dat, aes(x = MentalHealth, fill  = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  scale_y_continuous(labels = comma) 


ggplot(data = dat, aes(x = PhysicalHealth, fill  = HeartDisease))+
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

ggplot(data = dat, aes(x = MentalHealth, color  = HeartDisease))+
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

ggplot(data = dat, aes(x = MentalHealth, fill  = HeartDisease))+
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+
  theme(axis.text.y=element_blank())+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 

palco <-ggplot(data = dat, aes(x = AlcoholDrinking, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) + ylab("")




pact <- ggplot(data = dat, aes(x = PhysicalActivity, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) + ylab("")


#Age Category lineal graficas
ggplot(data = dat, aes(x = AgeCategory, fill = HeartDisease))+
  geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) 
#Linear in AgeCat=Yes?: Answer no.
jj <- as.data.frame(table(dat$AgeCategory, dat$HeartDisease))
jj <- jj[14:26,]
Age <- jj$Var1
freq <- jj$Freq
df <- data.frame(Age,freq)
str(df)
df$Age <- as.numeric(df$Age)
df$freq <- as.numeric(df$freq)
dim(df)
model <- lm(data = df, log(freq)~ Age)
summary(model)
cc <- as.numeric(coef(model))
ff <- function(x){exp(cc[1] + cc[2]*as.numeric(Age))}
df$fit <- ff(as.numeric(Age))
ggplot(df, aes(x = Age, y = freq))+geom_point()+theme_bw()+
  geom_line(aes(x = Age, y = fit), color="red")



#DISEASES
#Esto pa la tabla NO SIRVE
asth <- dat$Asthma
kd <- dat$KidneyDisease
sc <- dat$SkinCancer
diab <- dat$Diabetic
strok <- dat$Stroke
c1 <- c(asth,kd,sc,diab,strok)
c2 <- rep(c("Asthma", "KidneyDisease", "SkinCancer", "Diabetic", "Stroke"),
          each = 319795)
df <- data.frame(c1,c2)
table(df)#sacar porcentaje o probabilidad
#veruficar lo que es myocardial infartion (las dos que se modelan)
#dado que la persona tenga asma, se modela la probabilida que 
#la persona tenga o desarrolle HD???
#####################
dff <- df[which(df$c1 == "Yes"),]
ggplot(data = df, aes(x = c2,fill = c1))+geom_bar()+theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5))+ 
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma)

#Graficos
ggarrange(pkd,psc,pasthm,pdw,pstroke,pdiab ,ncol = 6, 
          common.legend = T,legend = "bottom")

ggarrange(prace,                                                 # First row with scatter plot
          ggarrange(page, psex, ncol = 2, labels = c("B", "C"),
                    common.legend = T,legend = "none"), # Second row with box and dot plots
          nrow = 2, 
          labels = "A" ,common.legend = T,legend = "bottom"                                       # Labels of the scatter plot
) 

ggarrange(psleep,
          ncol = 1,
          common.legend = T,legend = "none", 
          ggarrange(psmoke, palco,pact, ncol = 3,
          labels = c("B", "C", "D"),
          common.legend = T,legend = "bottom"), 
          labels = c("A", ""))


?chisq.test
chisq.test(dat$HeartDisease, dat$KidneyDisease)
colnames(dat)


HD_up_rec <- recipe(HeartDisease ~., data = dat) %>%
  step_upsample(HeartDisease) %>% prep()
up  <- HD_up_rec %>%
    bake(new_data = NULL) 
Upsampling <- up 


HD_smote_rec <- recipe(HeartDisease ~., data = dat) %>%
  step_smote(HeartDisease)  %>% prep()
smotee  <- HD_smote_rec %>%
  bake(new_data = NULL) 
SMOTE <- smotee 

HD_down_rec <- recipe(HeartDisease ~., data = dat) %>%
  step_downsample(HeartDisease)  %>% prep()
down  <- HD_down_rec %>%
  bake(new_data = NULL) 
Downsampling <- down 

Upsampling$Method <- "Upsampling"
SMOTE$Method <- "SMOTE"
Downsampling$Method <- "Downsampling"

df <- rbind(Upsampling,SMOTE,Downsampling)

ggplot(data = df, aes(x = HeartDisease, fill = Method))+
  geom_bar(position = position_dodge())+theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(palette="Pastel 1")


