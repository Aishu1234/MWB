library(ggplot2)

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard error
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x[[col]])), na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

spike <- read_excel("combined reps.xlsx", sheet = 1)
head(spike)

try1 <- read_excel("combined reps.xlsx", sheet = 3)

spike1<-rbind(grainNo1,grainNo2, grainNo3)
names(spike1) <- c("Rep","Line", "BL", "Chamber", "HS", "Plant", "Spike", "GrainNo")

spike1$Line<-factor(spike1$Line)
spike1$BL<-factor(spike1$BL)
spike1$HS<-factor(spike1$HS)
spike1$Rep<-factor(spike1$Rep)


spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)
spike$Rep <- as.factor(spike$Rep)

####Grain number per spike
###Vida only
vida1 <- newdata15[newdata15$Line =="Vida",]


#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
vidaAgg<- aggregate(vida1$GrainNo, by = list(vida1$Rep, vida1$HS, vida1$BL), FUN= mean)
names(vidaAgg) <- c("Rep", "HeatStress", "BL", "GrainNo.")

df2 <- data_summary(vidaAgg, varname="GrainNo.", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=GrainNo., group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=GrainNo.-se, ymax=GrainNo.+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic()  +
  ylab("Grain number per spike") + xlab("Stress")


###hiline only
hil1 <- newdata15[newdata15$Line =="Hiline",]



#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
hilAgg<- aggregate(hil1$GrainNo, by = list(hil1$Rep, hil1$HS, hil1$BL), FUN= mean)
names(hilAgg) <- c("Rep", "HeatStress", "BL", "GrainNo.")

df2 <- data_summary(hilAgg, varname="GrainNo.", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=GrainNo., group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=GrainNo.-se, ymax=GrainNo.+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic()  +
  ylab("Grain number per spike") + xlab("Stress")


###mth only
mth1 <- newdata15[newdata15$Line =="MTH",]



#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
mthAgg<- aggregate(mth1$GrainNo, by = list(mth1$Rep, mth1$HS, mth1$BL), FUN= mean)
names(mthAgg) <- c("Rep", "HeatStress", "BL", "GrainNo.")

df2 <- data_summary(mthAgg, varname="GrainNo.", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=GrainNo., group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=GrainNo.-se, ymax=GrainNo.+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic()  +
  ylab("Grain number per spike") + xlab("Stress")



##########Spike Length

spike1 <- read_excel("combined reps.xlsx", sheet = 4)

spike1$Chamber <- as.factor(spike1$Chamber)
spike1$Plant <- as.factor(spike1$Plant)
spike1$Rep <- as.factor(spike1$Rep)

###Vida only
vida <- spike1[spike1$Line =="Vida",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
vidaAgg<- aggregate(vida$SpikeLength, by = list(vida$Rep, vida$HeatStress, vida$BL), FUN= mean, na.rm=T)
names(vidaAgg) <- c("Rep", "HeatStress", "BL", "MeanSpikeLength")

df2 <- data_summary(vidaAgg, varname="MeanSpikeLength", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanSpikeLength, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanSpikeLength-se, ymax=MeanSpikeLength+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Spike Length (cm)") + xlab("Stress")

###Hiline only
hil <- spike1[spike1$Line =="Hiline",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
hilAgg<- aggregate(hil$SpikeLength, by = list(hil$Rep, hil$HeatStress, hil$BL), FUN= mean, na.rm=T)
names(hilAgg) <- c("Rep", "HeatStress", "BL", "MeanSpikeLength")

df2 <- data_summary(hilAgg, varname="MeanSpikeLength", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanSpikeLength, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanSpikeLength-se, ymax=MeanSpikeLength+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Spike Length (cm)") + xlab("Stress")

###MTH only
mth <- spike1[spike1$Line =="MTH",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
mthAgg<- aggregate(mth $SpikeLength, by = list(mth $Rep, mth $HeatStress, mth $BL), FUN= mean, na.rm=T)
names(mthAgg) <- c("Rep", "HeatStress", "BL", "MeanSpikeLength")

df2 <- data_summary(mthAgg, varname="MeanSpikeLength", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanSpikeLength, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanSpikeLength-se, ymax=MeanSpikeLength+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Spike Length (cm)") + xlab("Stress")




#####Plant height

###Vida only
vida <- spike[spike$Variety =="Vida" & spike$BL == "C",]
vida <- spike[spike$Variety =="Vida" & spike$BL == "C",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
vidaAgg<- aggregate(vida$PlantHeight, by = list(vida$Rep, vida$Stress, vida$BL), FUN= mean)
names(vidaAgg) <- c("Rep", "HeatStress", "BL", "MeanPlantHeight")

df2 <- data_summary(vidaAgg, varname="MeanPlantHeight", 
                    groupnames=c("HeatStress", "BL"))
df2
# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanPlantHeight, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanPlantHeight-se, ymax=MeanPlantHeight+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Plant Height (cm)") + xlab("Stress")

###Hiline only
hil <- spike[spike$Variety =="Hiline",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
hilAgg<- aggregate(hil$PlantHeight, by = list(hil$Rep, hil$HeatStress, hil$BL), FUN= mean)
names(hilAgg) <- c("Rep", "HeatStress", "BL", "MeanPlantHeight")

df2 <- data_summary(hilAgg, varname="MeanPlantHeight", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanPlantHeight, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanPlantHeight-se, ymax=MeanPlantHeight+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Plant Height (cm)") + xlab("Stress")

###MTH only
mth <- spike[spike$Variety =="MTH",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
mthAgg<- aggregate(mth $PlantHeight, by = list(mth $Rep, mth $HeatStress, mth $BL), FUN= mean)
names(mthAgg) <- c("Rep", "HeatStress", "BL", "MeanPlantHeight")

df2 <- data_summary(mthAgg, varname="MeanPlantHeight", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=MeanPlantHeight, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=MeanPlantHeight-se, ymax=MeanPlantHeight+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("Mean Plant Height (cm)") + xlab("Stress")



#######

#####1000 grain weight

#####STANDARD ERROR OF THE MEAN used in intplotarray######
###Vida
df2 <- data_summary(nd1, varname="tGW", 
                    groupnames=c("HS", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HS, y=tGW, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=tGW-se, ymax=tGW+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("1000 grain weight (g)") + xlab("Stress")

#####STANDARD ERROR OF THE MEAN used in intplotarray######
##hiline
df2 <- data_summary(nd2, varname="tGW", 
                    groupnames=c("HS", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HS, y=tGW, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=tGW-se, ymax=tGW+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("1000 grain weight (g)") + xlab("Stress")

#####STANDARD ERROR OF THE MEAN used in intplotarray######
###mth
df2 <- data_summary(nd3, varname="tGW", 
                    groupnames=c("HS", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HS, y=tGW, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=tGW-se, ymax=tGW+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() +
  ylab("1000 grain weight (g)") + xlab("Stress")
