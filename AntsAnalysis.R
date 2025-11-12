#opening file!
setwd("~/Desktop/Ants/Ants R")


##ANT ANALYSIS

ants <- read.csv("ANTSREAL.csv", stringsAsFactors = TRUE)

#renaming all the vars lol oops
names(ants)[names(ants) == "d15N..permil..vs.AIR."] <- "delta15"
names(ants)[names(ants) == "d13C..permil..vs.VPDB."] <- "delta13"
names(ants)[names(ants) == "wt..N"] <- "wtN"
names(ants)[names(ants) == "wt..C"] <- "wtC"
names(ants)[names(ants) == "Trail.ID"] <- "Trail ID"

#creating dataset with just the ants that have carbon 13 values for them
#carbonants <- ants[!ants$Notes=="loaded huge, too big for d13C",]

#dataset dropping ant species that we only have one of
#ants2 <- ants[-c(3:5,7),]

#and carbon dataset from ants2
#carbonants2 <- ants2[!ants$Notes=="loaded huge, too big for d13C",]

#just Lily's/lower trail (most ants, and largest range)
lower <- ants[ants$Trail.ID=="Ballena",]

#making the plots!
library(ggplot2)

##delta15 by trail, boxplot
ants$`Trail ID` <- factor(ants$`Trail ID` , levels=c("La Hoya", "Ojo de Agua", "Ballena"))
ggplot(ants, aes(x=`Trail ID`, y=delta15, fill=`Trail ID`)) + geom_boxplot() + ylim(0,10.5) +  
  scale_fill_manual(values = c("La Hoya" = '#03bb38ff', "Ojo de Agua" = '#629dffff', "Ballena" = '#f8766cff')) + 
  labs(x=" ", y=expression(delta*"15N")) + theme(panel.background=element_rect(fill='#d77e4850'))

##by species, boxplot
#ggplot(ants, aes(x=Species.ID, y=delta15, fill=Species.ID)) + geom_boxplot() +  labs(title="Delta 15 N of Ants, Separated by Species", x="Species ID", y="d15N (permil, vs AIR)")

##taking out species we only have one of (even though Da and Me, which I kept, only have 2 observations each)
#ggplot(ants2, aes(x=Species.ID, y=delta15, fill=Species.ID)) +geom_boxplot() + labs(title="Delta 15 N of Ants, Separated by Species", x="Species ID", y="d15N (permil, vs AIR)", subtitle="With species Ck, Cs, Ob, and Si removed")

##delta 13 by trail, boxplot
#ggplot(ants, aes(x=Trail.ID, y=delta13, fill=Trail.ID, show.legend=FALSE)) + geom_boxplot() +  labs(title="Delta 13 C of Ants, Separated by Trail", x="Trail ID", y="d13C (permil, vs VPDB)", subtitle="L = Lower Guanica, O = Other/Middle Trail, U = Upper Guanica")

##by species, boxplot
#ggplot(ants, aes(x=Species.ID, y=delta13, fill=Species.ID)) + geom_boxplot() + labs(title="Delta 13 C of Ants, Separated by Species", x="Species ID", y="d13C (permil, vs VPDB)")

##just lower trail, ant sampling location vs delta 15 (colored by species)
#ggplot(lower, aes(x=Original.Ant.ID, y=delta15, color=Species.ID)) + geom_point() + labs(title="Delta N 15 vs Sample Location (Lower Trail Only)", subtitle = "Colored by species, no visible pattern") + theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

##delta15 versus delta13 (of ants), modelled after the Roeder and Kaspari paper
ggplot(ants, aes(x=delta13, y=delta15)) + geom_point() + theme_linedraw() + 
  labs(y= "delta15N permil", x="delta13C permil", title="Variation Between Ant Individuals (delta15N and delta13C)", subtitle="r^2=0.04, LOBF: y=3.15146-0.1336x") +
  geom_smooth(method="lm", se=FALSE, color="red")
ggplot(ants, aes(x=delta13, y=delta15, color=`Trail ID`)) + geom_point() + theme_linedraw() + 
  labs(y="delta15N permil", x="delta13C permil", title="Variation Between Ant Individuals (delta15N and delta13C)")
model1 <- lm(delta15 ~ delta13, data=ants)
summary(model1)

#dataframe with TP and ant IDS
anttrophiclevel <- data.frame(ants$Original.Ant.ID,TP)

#the equation
delta15Nbase <- 1.0855
changeinN <- 3.4
TP <- 1 + (ants$delta15 - delta15Nbase)/changeinN

#trophic position vs deltaN15, colored by trail
ants3 <- data.frame(ants, anttrophiclevel)
ggplot(ants3, aes(x=TP, y=Trail.ID, color=Trail.ID)) +geom_point()
ggplot(ants3, aes(x=TP, y=Trail.ID, fill=Trail.ID)) +geom_boxplot()

#histogram of ant trophic position
ggplot(anttrophiclevel, aes(x=TP)) + geom_histogram(binwidth=0.2, color="white", fill="black") + theme_linedraw() + 
  xlim (1.5,4)  + labs(x = "Trophic Position", y="Number of Observations")


#lines of best fit for each trail
upper <- ants[ants$`Trail ID`=="La Hoya",]
other <- ants[ants$`Trail ID`=="Ojo de Agua",]
modellower <- lm(delta15 ~ delta13, data=lower)
modelupper <- lm(delta15 ~ delta13, data=upper)
modelother <- lm(delta15 ~ delta13, data=other)
summary(modellower)
summary(modelupper)
summary(modelother)

#scatterplot with line of best fit
ggplot(ants, aes(x=delta13, y=delta15, color=`Trail ID`)) + geom_point() + theme_linedraw() + 
  labs(y="delta15N permil", x="delta13C permil") +
  geom_abline(slope=0.1181, intercept=8.7726, color="red") +geom_abline(slope=0.5205, intercept=20.2838, color="green") + 
  geom_abline(slope=-0.2386,intercept=0.6820, color="cornflowerblue")



##SOLENOPSIS PICEA
sp <- ants[ants$Species.ID=="Sp",]
ggplot(sp, aes(x=delta13, y=delta15, color=`Trail ID`)) + geom_point() + 
  labs(title="Solenopsis picea", x="deltaC13", y="deltaN15")
ggplot(sp, aes(y=delta15, x=`Trail ID`, fill=`Trail ID`)) + geom_boxplot() + 
  labs(y= expression(delta*"15N"), x=" ") + 
  scale_fill_manual(values = c("La Hoya" = '#03bb38ff', "Ojo de Agua" = '#629dffff', "Ballena" = '#f8766cff')) +
  theme(panel.background=element_rect(fill='#d77e4850'))
model2 <- lm(delta15 ~ Trail.ID, data=sp)

#same species, nitrogen by different parts of trail (Pl)
pl <- ants[ants$Species.ID=="Pl",]
##pl$Original.Ant.ID <- as.numeric(pl$Original.Ant.ID)
factororiginal <- as.factor(pl$Original.Ant.ID)
ggplot(pl, aes(x=Original.Ant.ID, y=delta15)) + geom_point() + geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Paratrechina longicornis along lower trail")

##LEAF ANALYSIS

leaf <- read.csv("LEAF.csv", stringsAsFactors = TRUE)
names(leaf)[names(leaf) == "d15N..permil..vs.AIR."] <- "delta15"
names(leaf)[names(leaf) == "d13C..permil..vs.VPDB."] <- "delta13"
names(leaf)[names(leaf) == "Trail"] <- "Trail ID"


#quick summary stats
leafdeltansumm <- summary(leaf$d15N..permil..vs.AIR.)
leafdeltansumm
leafwtnsumm <- summary(leaf$wt..N)
leafwtnsumm

#histogram/density plot/scatterplot with delta n 
ggplot(leaf, aes(x=d15N..permil..vs.AIR.)) + geom_histogram()
ggplot(leaf, aes(x=d15N..permil..vs.AIR.)) + geom_density()
ggplot(leaf, aes(x=Original.ID, y=d15N..permil..vs.AIR.)) + geom_point()

#delta15 by trail boxplot
leaf$`Trail ID` <- factor(leaf$`Trail ID` , levels=c("La Hoya", "Ojo de Agua", "Ballena"))
ggplot(leaf, aes(y=delta15, x=`Trail ID`, fill=`Trail ID`)) + geom_boxplot() + 
  labs(y=expression(delta*"15N"), x=("")) + ylim(0,10.5) +
  scale_fill_manual(values = c("La Hoya" = '#03bb38ff', "Ojo de Agua" = '#629dffff', "Ballena" = '#f8766cff')) +
  theme(panel.background=element_rect(fill='#d1e7cbff'))
  
  
#ant ggplot (for reference)
ggplot(ants, aes(x=`Trail ID`, y=delta15, fill=`Trail ID`)) + geom_boxplot() + ylim(0,10.5) +  
  scale_fill_manual(values = c("La Hoya" = '#03bb38ff', "Ojo de Agua" = '#629dffff', "Ballena" = '#f8766cff')) + 
  labs(x=" ", y=expression(delta*"15N")) + theme(panel.background=element_rect(fill='#d77e4850'))

#with weight
ggplot(leaf, aes(x=wt..N)) + geom_histogram()
ggplot(leaf, aes(x=wt..N)) + geom_density()
ggplot(leaf, aes(x=Original.ID, y=wt..N)) + geom_point()


#plotting leaf and ant nitrogen together

allnitrogen <- read.csv("ants+plants.csv", stringsAsFactors = TRUE)

ggplot(allnitrogen, aes(y=delta15all, x=Species, fill=Species)) + geom_boxplot() + ylim(0,10.5) + labs(y="deltaN15 (permil)")



##leaf averages by trail
library(dplyr)
leaf$delta15 <- as.numeric(leaf$delta15)
summary_stats <- leaf %>%
  group_by(`Trail ID`) %>%
  summarize(
    count = n(),
    mean_value = mean(delta15),
    sum_value = sum(delta15),
    min_value = min(delta15),
    max_value = max(delta15),
    sd_value = sd(delta15)
  )
print(summary_stats)

mean_by_group <- leaf %>%
  group_by(Trail) %>%
  summarize(mean_value = mean(d15N..permil..vs.AIR.))
print(mean_by_group)

##STANDARDIZING BY TRAIL DELTA N

#splitting lower into 2
upper <- ants[ants$`Trail ID`=="La Hoya",]
other <- ants[ants$`Trail ID`=="Ojo de Agua",]
lower1 <- read.csv("lower1.csv", stringsAsFactors = TRUE)
lower2 <- read.csv("lower2.csv", stringsAsFactors = TRUE)
names(lower1)[names(lower1) == "d15N..permil..vs.AIR."] <- "delta15"
names(lower2)[names(lower2) == "d15N..permil..vs.AIR."] <- "delta15"
names(lower1)[names(lower1) == "d13C..permil..vs.VPDB."] <- "delta13"
names(lower1)[names(lower1) == "wt..N"] <- "wtN"
names(lower1)[names(lower1) == "wt..C"] <- "wtC"
names(lower1)[names(lower1) == "Trail.ID"] <- "Trail ID"
names(lower2)[names(lower2) == "d13C..permil..vs.VPDB."] <- "delta13"
names(lower2)[names(lower2) == "wt..N"] <- "wtN"
names(lower2)[names(lower2) == "wt..C"] <- "wtC"
names(lower2)[names(lower2) == "Trail.ID"] <- "Trail ID"

#the equation
upper15Nbase <- 1.9067
other15Nbase <- 1.6167
lower115Nbase <--0.545
lower215Nbase <- 0.6875
changeinN <- 3.4
TPupper <- 1 + (upper$delta15 - upper15Nbase)/changeinN
TPother <- 1 + (other$delta15 - other15Nbase)/changeinN
TPlower1 <- 1 + (lower1$delta15 - lower115Nbase)/changeinN
TPlower2 <- 1 + (lower2$delta15 - lower215Nbase)/changeinN

#making dfs
TPudf <- data.frame(upper, TPupper)
TPodf <- data.frame(other, TPother)
TPl1df <- data.frame(lower1, TPlower1)
TPl2df <- data.frame(lower2, TPlower2)

names(TPudf)[names(TPudf) == "TPupper"] <- "TP"
names(TPodf)[names(TPodf) == "TPother"] <- "TP"
names(TPl1df)[names(TPl1df) == "TPlower1"] <- "TP"
names(TPl2df)[names(TPl2df) == "TPlower2"] <- "TP"


#dataframe with all TP and ant IDS (from diff trails)
library(dplyr)
TPall <- rbind(TPudf, TPodf, TPl1df, TPl2df)
names(TPall)[names(TPall) == "Species.ID"] <- "Species ID"

#boxplot by species
ggplot(TPall, aes(x=Species.ID, y=TP, fill=Species.ID)) + geom_boxplot() + labs(y="Trophic Position", x="Species")
ggplot(TPall, aes(x=delta15, y=TP, color=Species.ID)) + geom_point() 

#sp trophic position
sptp <- TPall[TPall$`Species ID`=="Sp",]
names(sptp)[names(sptp) == "Trail.ID"] <- "Trail ID"
ggplot(sptp, aes(y=TP, x=`Trail ID`, fill=`Trail ID`)) + geom_boxplot() + labs(y= "Trophic Position", x=" ") +
  scale_fill_manual(values = c("La Hoya" = '#03bb38ff', "Ojo de Agua" = '#629dffff', "Ballena" = '#f8766cff'))
  

#trophic positions with base N standardized by trail
ggplot(TPall, aes(x=TP)) + geom_histogram(binwidth=0.2, color="white", fill="black") + theme_linedraw() + 
  xlim (1.5,4)  + labs(x = "Trophic Position", y="Number of Observations")
ggplot(TPall, aes(x=TP, fill=`Species ID`)) + geom_histogram(binwidth=0.2) + theme_linedraw() + 
  xlim (1.5,4)  + labs(x = "Trophic Position", y="Number of Observations")

#trophic position vs deltaN15, colored by trail
ants3 <- data.frame(ants, anttrophiclevel)
ggplot(ants3, aes(x=TP, y=Trail.ID, color=Trail.ID)) +geom_point()
ggplot(ants3, aes(x=TP, y=Trail.ID, fill=Trail.ID)) +geom_boxplot()


##RUNNING THE ANOVA
#all nitrogen
aovmodel <- aov(ants3$delta15 ~ ants$`Trail ID`, data=ants3)
summary(aovmodel)
TukeyHSD(aovmodel)

#and tp
aovmodel2 <- aov(TPall$TP ~ TPall$Trail.ID, data=TPall)
summary(aovmodel2)
TukeyHSD(aovmodel2)

#sp nitrogen
ants3 <- data.frame(ants, anttrophiclevel)
sp3 <- ants3[ants$Species.ID=="Sp",]
aovmodel3 <- aov(sp3$delta15 ~ sp3$Trail.ID, data=sp3)
summary(aovmodel3)
TukeyHSD(aovmodel3)
#and tp 
aovmodel4 <- aov(sptp$TP ~ sptp$Trail.ID, data=sptp)
summary(aovmodel4)
TukeyHSD(aovmodel4)


##PAPER PLOTS

setwd("~/Desktop/Ants/Ants R")

#getting all the data and sets
ants <- read.csv("ANTSREAL.csv", stringsAsFactors = TRUE)
names(ants)[names(ants) == "d15N..permil..vs.AIR."] <- "delta15"
names(ants)[names(ants) == "d13C..permil..vs.VPDB."] <- "delta13"
names(ants)[names(ants) == "wt..N"] <- "wtN"
names(ants)[names(ants) == "wt..C"] <- "wtC"
names(ants)[names(ants) == "Trail.ID"] <- "Trail ID"
lower <- ants[ants$Trail.ID=="Ballena",]
ants$`Trail ID` <- factor(ants$`Trail ID` , levels=c("La Hoya", "Ojo de Agua", "Ballena"))
sp <- ants[ants$Species.ID=="Sp",]
leaf <- read.csv("LEAF.csv", stringsAsFactors = TRUE)
names(leaf)[names(leaf) == "d15N..permil..vs.AIR."] <- "delta15"
names(leaf)[names(leaf) == "d13C..permil..vs.VPDB."] <- "delta13"
names(leaf)[names(leaf) == "Trail"] <- "Trail ID"

upper <- ants[ants$`Trail ID`=="La Hoya",]
other <- ants[ants$`Trail ID`=="Ojo de Agua",]
lower1 <- read.csv("lower1.csv", stringsAsFactors = TRUE)
lower2 <- read.csv("lower2.csv", stringsAsFactors = TRUE)
names(lower1)[names(lower1) == "d15N..permil..vs.AIR."] <- "delta15"
names(lower2)[names(lower2) == "d15N..permil..vs.AIR."] <- "delta15"
names(lower1)[names(lower1) == "d13C..permil..vs.VPDB."] <- "delta13"
names(lower1)[names(lower1) == "wt..N"] <- "wtN"
names(lower1)[names(lower1) == "wt..C"] <- "wtC"
names(lower1)[names(lower1) == "Trail.ID"] <- "Trail ID"
names(lower2)[names(lower2) == "d13C..permil..vs.VPDB."] <- "delta13"
names(lower2)[names(lower2) == "wt..N"] <- "wtN"
names(lower2)[names(lower2) == "wt..C"] <- "wtC"
names(lower2)[names(lower2) == "Trail.ID"] <- "Trail ID"
upper15Nbase <- 1.9067
other15Nbase <- 1.6167
lower115Nbase <--0.545
lower215Nbase <- 0.6875
changeinN <- 3.4
TPupper <- 1 + (upper$delta15 - upper15Nbase)/changeinN
TPother <- 1 + (other$delta15 - other15Nbase)/changeinN
TPlower1 <- 1 + (lower1$delta15 - lower115Nbase)/changeinN
TPlower2 <- 1 + (lower2$delta15 - lower215Nbase)/changeinN
TPudf <- data.frame(upper, TPupper)
TPodf <- data.frame(other, TPother)
TPl1df <- data.frame(lower1, TPlower1)
TPl2df <- data.frame(lower2, TPlower2)
names(TPudf)[names(TPudf) == "TPupper"] <- "TP"
names(TPodf)[names(TPodf) == "TPother"] <- "TP"
names(TPl1df)[names(TPl1df) == "TPlower1"] <- "TP"
names(TPl2df)[names(TPl2df) == "TPlower2"] <- "TP"
library(dplyr)
TPall <- rbind(TPudf, TPodf, TPl1df, TPl2df)
names(TPall)[names(TPall) == "Species.ID"] <- "Species ID"
sptp <- TPall[TPall$`Species ID`=="Sp",]
ants$`Trail ID` <- factor(ants$`Trail ID` , levels=c("La Hoya", "Ojo de Agua", "Ballena"))
leaf$`Trail ID` <- factor(leaf$`Trail ID` , levels=c("La Hoya", "Ojo de Agua", "Ballena"))
TPall$`Species ID`[7] <- "Sp"

#graph1: leaf nitrogen levels
library(ggplot2)
ggplot(leaf, aes(y=delta15, x=`Trail ID`)) + geom_boxplot() + 
  labs(y=expression(delta*"15N"), x=("")) + ylim(0,10.5)

#graph2: ant nitrogen levels
ggplot(ants, aes(x=`Trail ID`, y=delta15)) + geom_boxplot() + ylim(0,10.5) +  
  labs(x=" ", y=expression(delta*"15N")) 

#graph3: trophic position by species
#geom_histogram_pattern(binwidth=0.2, aes(pattern=`Species ID`)) pattern_angle = `Species ID`
TPall$`Species ID` <- factor(TPall$`Species ID`)
ggplot(TPall, aes(x=TP)) + geom_histogram_pattern(binwidth=0.2,
  aes(pattern = `Species ID`,fill = `Species ID`)) + theme_bw()
library(ggpattern)
ggplot(TPall, aes(x=TP, fill=`Species ID`)) + 
  geom_histogram_pattern(binwidth=0.2, color="black", pattern_fill="black",pattern_spacing=0.05, aes(pattern=`Species ID`)) +
                           scale_fill_grey(start=0, end=1) + theme_light() + labs(x="Trophic Position", y="Number of Observations")

#graphs4and5: sp
#tp
ggplot(sptp, aes(y=TP, x=`Trail ID`)) + geom_boxplot() + labs(y= "Trophic Position", x=" ") 
#nitrogen
ggplot(sp, aes(y=delta15, x=`Trail ID`)) + geom_boxplot() + 
  labs(y= expression(delta*"15N"), x=" ") 

#ants$Species.ID[7] <- "Sp"

TPall$Trail.ID <- as.factor(TPall$Trail.ID)
TPall$delta15 <- as.numeric(TPall$delta15)
summary_stats <- TPall %>%
  group_by(Trail.ID) %>%
  summarize(
    count = n(),
    mean_value = mean(delta15),
    sum_value = sum(delta15),
    min_value = min(delta15),
    max_value = max(delta15),
    sd_value = sd(delta15)
  )
print(summary_stats)

get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

summary_stats <- sptp %>%
  group_by(Trail.ID) %>%
  summarise(
    count = n(),
    mean_value = mean(delta15, na.rm = TRUE),
    sum_value = sum(delta15, na.rm = TRUE),
    min_value = min(delta15, na.rm = TRUE),
    max_value = max(delta15, na.rm = TRUE),
    sd_value = sd(delta15, na.rm = TRUE),
  )

# Print the results
print(summary_stats)


