library(readxl)
library(lmerTest)
library(car)
library(zoo)
library(beanplot)
library(emmeans)
library(lme4)


######spikelet number per head######
#####CONTROL#####

####################################################
######Rep 1 ########################################
####################################################
spike <- read_excel("harvesting rep 1.xlsx", sheet = 3)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)

par(mfrow = c(1,3))
par(mar = c(5,5,2,2))
boxplot(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), main = "Rep 1")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])


hiline<-spike[spike$Line =="Hiline",]
hilAgg<- aggregate(hiline$Spike, by = list(hiline$HeatStress, hiline$BL, hiline$Plant, hiline$Spike), FUN= length)

par(mfrow = c(1,3))
par(mar = c(5,5,2,2))
boxplot(hilAgg[hilAgg$Group.2 == "C",5] ~ hilAgg[hilAgg$Group.2 == "C",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), main = "Rep 1")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)


####################################################
######Rep 2 ########################################
####################################################

spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 2.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)
par(mar = c(5,5,2,2))

boxplot(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), main = "Rep 2")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])


####################################################
######Rep 3 ########################################
####################################################

spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 3.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)
par(mar = c(5,5,2,2))

boxplot(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), , main = "Rep 3")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "C",5] ~ vidaAgg[vidaAgg$Group.2 == "C",1])



#####SEED TREATMENT#####

####################################################
######Rep 1 ########################################
####################################################
spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 1.xlsx", sheet = 3)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)

par(mfrow = c(1,3))
par(mar = c(5,5,2,2))
boxplot(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), main = "Rep 1")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])


####################################################
######Rep 2 ########################################
####################################################

spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 2.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)
par(mar = c(5,5,2,2))

boxplot(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), main = "Rep 2")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])




####################################################
######Rep 3 ########################################
####################################################

spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 3.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")


###simpler Vida
head(spike)
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$Spike, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= length)
par(mar = c(5,5,2,2))

boxplot(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1], xlab = "heat stress",
        ylab = "spikelet number per head", col = c("blue", "orange"), 
        cex.lab=2, names.arg = 2, las = 2, xaxt="n", yaxt="n",
        ylim = c(0,25), , main = "Rep 3")
axis(1, at=c(1,2), labels=c("22 degrees", "29 degrees"), cex.axis=1.7)
axis(2, at=c(0:25), labels=0:25, cex.axis=1, las = 2)



leveneTest(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])
wilcox.test(vidaAgg[vidaAgg$Group.2 == "S",5] ~ vidaAgg[vidaAgg$Group.2 == "S",1])


####################################################
##############################################
####################################################

#####fertility--grain per floret######
####fertility rep1####
spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 1.xlsx", sheet = 3)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")

spike$fert <- spike$GrainNo./spike$Floret
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$fert, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= mean)
head(vidaAgg)
names(vidaAgg) <- c("Heat", "BL", "Plant", "Spike", "Fertility")
vidaAgg$Plant <- as.factor(vidaAgg$Plant)
vidaAgg$Spike <- as.factor(vidaAgg$Spike)

par(mfrow = c(1,1))
boxplot(vidaAgg$Fertility ~ vidaAgg$Heat + vidaAgg$BL, col = c("blue", "orange"))
mod0 <- lmer(log(Fertility+0.01) ~ BL * Heat + (1|Spike/Plant), data = vidaAgg )
plot(mod0)
Anova(mod0)
eem <- emmeans(mod0, c("Heat", "BL"))
plot(eem)

####fertility rep2####
spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 2.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")

spike$fert <- spike$GrainNo./spike$Floret
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$fert, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= mean)
head(vidaAgg)
names(vidaAgg) <- c("Heat", "BL", "Plant", "Spike", "Fertility")
vidaAgg$Plant <- as.factor(vidaAgg$Plant)
vidaAgg$Spike <- as.factor(vidaAgg$Spike)

par(mfrow = c(1,1))
boxplot(vidaAgg$Fertility ~ vidaAgg$Heat + vidaAgg$BL, col = c("blue", "orange"))
mod0 <- lmer(log(Fertility+0.01) ~ BL * Heat + (1|Spike/Plant), data = vidaAgg )
plot(mod0)
Anova(mod0)
eem <- emmeans(mod0, c("Heat", "BL"))
plot(eem)

####fertility rep3####
spike <- read_excel(path = "/Users/jenniferlachowiec/Dropbox/Montana State/projects/Aish/harvesting rep 3.xlsx", sheet = 2)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)

spike$single <- paste(spike$Line, spike$BL, spike$HeatStress, spike$Plant, spike$Spike, sep = "_")

spike$fert <- spike$GrainNo./spike$Floret
vida <- spike[spike$Line =="Vida",]
vidaAgg<- aggregate(vida$fert, by = list(vida$HeatStress, vida$BL, vida$Plant, vida$Spike), FUN= mean)
head(vidaAgg)
names(vidaAgg) <- c("Heat", "BL", "Plant", "Spike", "Fertility")
vidaAgg$Plant <- as.factor(vidaAgg$Plant)
vidaAgg$Spike <- as.factor(vidaAgg$Spike)

par(mfrow = c(1,1))
boxplot(vidaAgg$Fertility ~ vidaAgg$Heat + vidaAgg$BL, col = c("blue", "orange"))
mod0 <- lmer(log(Fertility+0.01) ~ BL * Heat + (1|Spike/Plant), data = vidaAgg )
plot(mod0)
Anova(mod0)
eem <- emmeans(mod0, c("Heat", "BL"))
plot(eem)
