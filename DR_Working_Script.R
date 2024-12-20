DRdata = read.csv("PercentCanopy.csv", header=TRUE)

boxplot(DRdata$X.canopy ~ DRdata$Species, 
        xlab = "Species", 
        ylab = "Percentage Height within canopy")
stripchart(DRdata$X.canopy ~ DRdata$Species, vertical = TRUE, 
           method = "jitter", 
            pch = 19, add = TRUE, col="black")

stripchart(chickwts$weight ~ chickwts$feed, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(chickwts$feed)))

fixeddata = read.csv("OnlyInterestingSpecies.csv", header=T)

boxplot(fixeddata$X.canopy ~ fixeddata$Species, 
        xlab = "Species", 
        ylab = "Percentage Height within canopy")
stripchart(fixeddata$X.canopy ~ fixeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")
par(mfrow=c(1,1))
boxplot(fixeddata$Canopy.height ~ fixeddata$Species, 
        xlab = "Species", 
        ylab = "Canopy height")
stripchart(fixeddata$Canopy.height ~ fixeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")

boxplot(fixeddata$Bird.Height ~ fixeddata$Species, 
        xlab = "Species", 
        ylab = "Bird height")
stripchart(fixeddata$Bird.Height ~ fixeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")
CMWAFB = subset(fixeddata$Foraging.Behavior, fixeddata$Species=="CMWA")
barplot(table(CMWAFB))

par(mfrow=c(2,2))
barplot(table(fixeddata$Foraging.Behavior, fixeddata$Species=="BBTO"))
BBTOFB = subset(fixeddata$Foraging.Behavior, fixeddata$Species=="BBTO")
barplot(table(BBTOFB))
YRWAFB = subset(fixeddata$Foraging.Behavior, fixeddata$Species=="YRWA")
barplot(table(YRWAFB))

NOPAFB = subset(fixeddata$Foraging.Behavior, fixeddata$Species=="NOPA")
barplot(table(NOPAFB))
BWVIFB = subset(fixeddata$Foraging.Behavior, fixeddata$Species=="BWVI")

barplot(table(BWVIFB))


boxplot(table(fixeddata$Foraging.Behavior ~ fixeddata$Species, fixeddata$Species=="YRWA"))
barplot(fixeddata$Foraging.Behavior, fixeddata$Species=="YRWA")
barplot(fixeddata$Foraging.Behavior, fixeddata$Species=="NOPA")
barplot(fixeddata$Foraging.Behavior, fixeddata$Species=="CMWA")


boxplot(fixeddata$X..canopy ~ fixeddata$Species, 
        xlab = "Species", 
        ylab = "Percentage Height within canopy", )
stripchart(fixeddata$X.canopy ~ fixeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")



####NEW STUFF-->>>>

editeddata = read.csv("EditedDRdata.csv", header = T)

View(editeddata)
X.Canopy = editeddata$Bird.Height/editeddata$Canopy.height


#BIRD HEIGHT AS A PERCENT OF CANOPY HEIGHT
boxplot(X.Canopy ~ editeddata$Species, 
        xlab = "Species", 
        ylab = "Percentage Height Within Canopy")
stripchart(X.Canopy ~ editeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")

#RAW BIRD HEIGHT
boxplot(editeddata$Bird.Height ~ editeddata$Species, 
        xlab = "Species", 
        ylab = "Bird Height (m)")
stripchart(editeddata$Bird.Height ~ editeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")

#CANOPY HEIGHT
boxplot(editeddata$Canopy.height ~ editeddata$Species, 
        xlab = "Species", 
        ylab = "Canopy Height at Attack Location")
stripchart(editeddata$Canopy.height ~ editeddata$Species, vertical = TRUE, 
           method = "jitter", 
           pch = 19, add = TRUE, col="black")

hist(editeddata$Canopy.height)



library(dplyr)

temp <- editeddata %>% group_by(Species, Foraging.Behavior) %>% mutate(n=n())
temp <- as.data.frame(temp)
temp <- temp[,c(8, 15, 36)]
temp <- temp[!duplicated(temp[,1:3]),]

Stacked = read.csv("FINAL.csv", header=F)

library(ggplot2)
# library
library(ggplot2)


# create a dataset
Species <- c(rep("AMRE" , 8) , rep("BAWW" , 8) , rep("BBTO" , 8) , rep("BWVI" , 8) , rep("CMWA" , 8) , rep("NOPA" , 8) , rep("OVEN" , 8) , rep("PAWA" , 8) , rep("PRAW" , 8) , rep("YRWA" , 8) )
Behavior <- rep(c("C" , "D" , "F", "G", "H", "K", "P", "S") , 10)
Count <- abs(Stacked$V3)
data <- data.frame(Species,Behavior,Count)

# Stacked + percent
ggplot(data, aes(fill=Behavior, y=Count, x=Species)) + 
  geom_bar(position="fill", stat="identity")




install.packages(ggplot2)

library(dplyr)

temp <- editeddata %>% group_by(Species, Foraging.Location) %>% mutate(n=n())
temp <- as.data.frame(temp)
temp <- temp[,c(8, 14, 36)]
temp <- temp[!duplicated(temp[,1:3]),]

Location = read.csv("V2.csv", header=F)

library(ggplot2)
# library
library(ggplot2)


# create a dataset
Species <- c(rep("AMRE" , 14) , rep("BAWW" , 14) , rep("BBTO" , 14) , rep("BWVI" , 14) , rep("CMWA" , 14) , rep("NOPA" , 14) , rep("OVEN" , 14) , rep("PAWA" , 14) , rep("PRAW" , 14) , rep("YRWA" , 14) )
Foraging_Location <- rep(c("B" , "D" , "F", "G", "L", "OF", "OL", "S", "SW", "T", "UF", "UL", "W", "Y") , 10)
Count <- abs(Location$V3)
data <- data.frame(Species,Foraging_Location,Count)

install.packages("viridisLite")
library("viridisLite")

# Stacked + percent
ggplot(data, aes(fill=Foraging_Location, y=Count, x=Species)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=c('darkblue', 'sienna4', 'deepskyblue', 'darkgreen', 'darkorchid1', 'deepskyblue2', 'darkorchid', 'blue', 'gray100', 'gray0', 'deepskyblue4', 'darkorchid4', 'red', 'pink'))



dat <- data.frame(x = rnorm(10000), y = rnorm(10000))

ggplot(dat, aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_gradientn(colours = viridis(256, option = "D"))



library(dplyr)

temp <- editeddata %>% group_by(Species, Flock.) %>% mutate(n=n())
temp <- as.data.frame(temp)
temp <- temp[,c(8, 16, 36)]
temp <- temp[!duplicated(temp[,1:3]),]

Flock = read.csv("Flock.csv", header=F)

library(ggplot2)
# library
library(ggplot2)


# create a dataset
Species <- c(rep("AMRE" , 2) , rep("BAWW" , 2) , rep("BBTO" , 2) , rep("BWVI" , 2) , rep("CMWA" , 2) , rep("NOPA" , 2) , rep("OVEN" , 2) , rep("PAWA" , 2) , rep("PRAW" , 2) , rep("YRWA" , 2) )
Flock <- rep(c("N", "Y") , 10)
Count <- abs(Flock$V3)
data <- data.frame(Species,Flock,Count)

# Stacked + percent
ggplot(data, aes(fill=Flock, y=Count, x=Species)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c('red', 'blue'))


flockpercentage = read.csv("FlockYes.csv", header = F)
barplot(flockpercentage$V2 ~ flockpercentage$V1, xlab = 'Species', ylab = 'Percentage in Probable Flock')









####ANOVVAAAAAA



install.packages(c("ggpubr", "tidyverse", "broom", "AICcmodavg"))


library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

behavior_data = read.csv("FINAL.csv", header=F)

view


summary(behavior_data)

Speciesstuff = lm(X..Height.in.Canopy~Species, data=Mar_3_data)
anova(Speciesstuff)
library(emmeans)

#Shows pairwise significance in canopy height b/w species
emmeans(Speciesstuff, pairwise~Species)



behaviorstuff = lm(X..Height.in.Canopy~Foraging.Behavior, data=Mar_3_data)
anova(behaviorstuff)


two = lm(Foraging.Behavior~Species+Foraging.Location, data=Mar_3_data)
anova(two)

#differences in canopy height between foraging locations controlled by species
emmeans(two, pairwise~Foraging_Location)

#differences canopy height between species controlled by foraging lcoation
emmeans(two, pairwise~Species)


table(Mar_3_data$Foraging.Location)


#Shows pairwise significance in canopy height b/w foraging behaviors
emmeans(behaviorstuff, pairwise~Foraging.Behavior)






one.way <- aov(behavior_data$V3 ~ behavior_data$V2, data = behavior_data)
summary(one.way)

log-linear


Mar_3_data <- read.csv("Mar_3_data.csv", header = T, na.strings = " ")

table(Mar_3_data$Foraging.Behavior, Mar_3_data$Species)
round(prop.table(table(Mar_3_data$Foraging.Behavior, Mar_3_data$Species), m=2), 2)

#null hypothesis is no association between species and Foraging behavior
chisq.test(table(Mar_3_data$Foraging.Behavior, Mar_3_data$Species))

chisq.test(table(Mar_3_data$Foraging.Behavior, Mar_3_data$Species), simulate.p.value=T)

#comparing two species against each other
chisq.test(table(Mar_3_data$Foraging.Behavior[Mar_3_data$Species %in% c("AMRE", "BAWW")], Mar_3_data$Species[Mar_3_data$Species %in% c("AMRE", "BAWW")]), simulate.p.value=T)


fisher.test(table(Mar_3_data$Foraging.Behavior, Mar_3_data$Species))







###FOR MARCH 7 DATA
Mar_7_data <- read.csv("Mar_7.csv", header = T, na.strings = " ")

table(Mar_7_data$Foraging.Behavior, Mar_7_data$Species)
round(prop.table(table(Mar_7_data$Foraging.Behavior, Mar_7_data$Species), m=2), 2)

#null hypothesis is no association between species and Foraging behavior
chisq.test(table(Mar_7_data$Foraging.Behavior, Mar_7_data$Species))

chisq.test(table(Mar_7_data$Foraging.Behavior, Mar_7_data$Species), simulate.p.value=T)

#comparing two species against each other
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "AMRE")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "AMRE")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "BAWW")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "BAWW")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "BBTO")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "BBTO")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "BWVI")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "BWVI")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "CMWA")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "CMWA")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("NOPA", "NOPA")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "NOPA")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("OVEN", "OVEN")], Mar_7_data$Species[Mar_7_data$Species %in% c("NOPA", "OVEN")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("OVEN", "PAWA")], Mar_7_data$Species[Mar_7_data$Species %in% c("OVEN", "PAWA")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("PAWA", "PRAW")], Mar_7_data$Species[Mar_7_data$Species %in% c("PAWA", "PRAW")]), simulate.p.value=T)
chisq.test(table(Mar_7_data$Foraging.Behavior[Mar_7_data$Species %in% c("PRAW", "YRWA")], Mar_7_data$Species[Mar_7_data$Species %in% c("PRAW", "YRWA")]), simulate.p.value=T)

chisq.test(table(Mar_7_data$Combined.Location, Mar_7_data$Species)


fisher.test(table(Mar_7_data$Combined.Location, Mar_7_data$Species), simulate.p.value=T)















ggplot(Mar_3_data, aes(Species, X..Height.in.Canopy))+
  geom_boxplot()+
  geom_jitter(width = .1, alpha = .5)


height_by_behavior = read.csv("Height_by_behavior.csv", header = T)

ggplot(height_by_behavior, aes(Foraging.Behavior, X..Height.in.Canopy))+
  geom_boxplot()+
  geom_jitter(width = .1, alpha = .5)+
  facet_wrap(~Species)


ggplot(Mar_3_data, aes(Foraging.Behavior, X..Height.in.Canopy))+
  geom_boxplot()+
  geom_jitter(width = .1, alpha = .5)










### stacked bar chart


species = c(rep("YRWA" , 3) , rep("NOPA" , 3) , rep("CMWA" , 3) , rep("PRAW" , 3) )
FB = editeddata$Foraging.Behavior
value = count  

frame = data.frame(editeddata$Species, editeddata$Foraging.Behavior)
frequency = editeddata$Foraging.Behavior ~ editeddata$Species
ggplot(frame, aes(fill=editeddata$Foraging.Behavior, x=editeddata$Species, y=frequency))
geom_bar(position="dodge", stat="identity")
 

 library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

barplot(editeddata$Foraging.Behavior ~ editeddata$Species, editeddata$species="YRWA")
barplot


# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

data = data.frame(editeddata$Species, editeddata$Foraging.Behavior)
# create a dataset




ggplot(data, aes(fill=condition, y=Count, x=Species)) 
  geom_bar(position="dodge", stat="identity")


specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Grouped
condition <- rep(c("F" , "G" , "S") , 4)
ggplot(data, aes(y=editeddata$Foraging.Behavior, x=editeddata$Species))
geom_bar(position="dodge")



tapply(editeddata$Foraging.Behavior, editeddata$Species)
view(tapply)
### % flycatching

num

flycatching = editeddata$Foraging.Behavior==F/

### % gleaning















