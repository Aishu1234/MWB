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

spike <- read_excel(combined reps.xlsx", sheet = 3)
head(spike)

spike$Chamber <- as.factor(spike$Chamber)
spike$Plant <- as.factor(spike$Plant)
spike$Rep <- as. factor(spike$Rep)


###Vida only
vida <- spike[spike$Line =="Vida",]

#####STANDARD ERROR OF THE MEAN ERROR BARS FOR THREE REPLICATES#####
vidaAgg<- aggregate(vida$SpikeLength, by = list(vida$Rep, vida$HeatStress, vida$BL), FUN= mean)
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
  theme_classic() 

#####STANDARD ERROR OF THE MEAN used in intplotarray######
df2 <- data_summary(vida, varname="SpikeLength", 
                    groupnames=c("HeatStress", "BL"))
df2

# Default bar plot
p<- ggplot(df2, aes(x=HeatStress, y=SpikeLength, group=BL, color=BL)) + 
  geom_line(position=position_dodge(0.08)) +
  geom_point( position=position_dodge(0.08))+
  geom_errorbar(aes(ymin=SpikeLength-se, ymax=SpikeLength+se), width=.2,
                position=position_dodge(0.08))
# Finished line plot
p+
  theme_classic() 
