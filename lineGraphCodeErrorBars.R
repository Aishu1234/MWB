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

spike <- read_excel(path = "combined reps.xlsx", sheet = 3)
head(spike)



spike1<-rbind(grainNo1,grainNo2, grainNo3)
names(spike1) <- c("Rep","Line", "BL", "Chamber", "HS", "Plant", "Spike", "GrainNo")

spike1$Line<-factor(spike1$Line)
spike1$BL<-factor(spike1$BL)
spike1$HS<-factor(spike1$HS)
spike1$Rep<-factor(spike1$Rep)


spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)
spike$Rep <- as.factor(spike$Rep)


###Vida only
vida1 <- spike[spike$Variety =="Vida",]

c <- newdata8[newdata8$Line == "Vida"& newdata8$BL == "C",]
s<- newdata8[newdata8$Line == "Vida"& newdata8$BL == "S",]
newdata111<-rbind(c,s)

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
vidaAgg<- aggregate(newdata111$GrainNo, by = list(newdata111$Rep, newdata111$HS, newdata111$BL), FUN= mean)
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
  theme_classic() 

###Hiline only
hil <- spike[spike$Line =="Hiline",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
hilAgg<- aggregate(hil$SpikeLength, by = list(hil$Rep, hil$HeatStress, hil$BL), FUN= mean)
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
  theme_classic() 

###MTH only
mth <- spike[spike$Line =="MTH",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
mthAgg<- aggregate(mth $SpikeLength, by = list(mth $Rep, mth $HeatStress, mth $BL), FUN= mean)
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
  theme_classic() 


#######
#####STANDARD ERROR OF THE MEAN used in intplotarray######
df2 <- data_summary(vida1, varname="tGW", 
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
  theme_classic() 

#####STANDARD ERROR OF THE MEAN used in intplotarray######
df2 <- data_summary(hil, varname="tGW", 
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
  theme_classic()

#####STANDARD ERROR OF THE MEAN used in intplotarray######
df2 <- data_summary(mth, varname="tGW", 
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
  theme_classic()
