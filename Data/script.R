rm(list = ls())
cat("\014")  
library(readr)
library(ggplot2)

#load in Data
E1 = c("E1R1B12", "E1R1B25", "E1R3B12", "E1R3B25", "E1R5B12", "E1R5B25")
E1data = list()
for (j in seq(1,6))
{
  string = E1[j]
  assign(string, list())
  for (i in seq(0,9))
  {
    data <- read_delim(paste("~/Documents/Year2/Cognitive Robotics/Didabots/Data/E1/",string,"/Data/",string,"_0",toString(i),".txt", sep=""), 
                       " ", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
    assign(string, (append(eval(as.name(string)), list(data), after=length(eval(as.name(string))))))
  }
  E1data[[j]] <- eval(as.name(string))
}

#data in colums is:
#nrdatapoints, avgHeapSize, nrOfHeaps, percentageInHeap, boxesMoved

#plot a,b,c from original paper
boxVec12 = c(1,3,5)
boxVec12avgHeapSize = NULL
boxVec12nrOfHeaps = NULL
boxVec12percentageInHeap = NULL
for(box12 in boxVec12)
{
  for(test in seq(1,10))
  {
    lastDataPoint <- nrow(E1data[[box12]][[test]])
    boxVec12avgHeapSize <- append(boxVec12avgHeapSize,unlist(E1data[[box12]][[test]][lastDataPoint,][2]))
    boxVec12nrOfHeaps <- append(boxVec12nrOfHeaps,unlist(E1data[[box12]][[test]][lastDataPoint,][3]))
    boxVec12percentageInHeap <- append(boxVec12percentageInHeap,unlist(E1data[[box12]][[test]][lastDataPoint,][4]))
  }
}
#print(append(boxVec12percentageInHeap,unlist(E1data[[1]][[1]][14,][4])))

boxVec12nrOfHeaps <- as.data.frame(boxVec12nrOfHeaps)
boxVec12percentageInHeap <- as.data.frame(boxVec12percentageInHeap)
boxVec12avgHeapSize <- as.data.frame(boxVec12avgHeapSize)
label12 <- as.data.frame(rep("12 boxes",nrow(boxVec12nrOfHeaps)))
dataFrame12 = merge(merge(merge(boxVec12nrOfHeaps, boxVec12percentageInHeap), boxVec12avgHeapSize),label12)
colnames(dataFrame12) <- c("numberOfHeaps", "percentageInHeap", "avgHeapSize","label")

boxVec25 = c(2,4,6)
boxVec25avgHeapSize = NULL
boxVec25nrOfHeaps = NULL
boxVec25percentageInHeap = NULL
for(box25 in boxVec25)
{
  for(test in seq(1,10))
  {
    lastDataPoint <- nrow(E1data[[box25]][[test]])
    boxVec25avgHeapSize <- append(boxVec25avgHeapSize,unlist(E1data[[box25]][[test]][lastDataPoint,][2]))
    boxVec25nrOfHeaps <- append(boxVec25nrOfHeaps,unlist(E1data[[box25]][[test]][lastDataPoint,][3]))
    boxVec25percentageInHeap <- append(boxVec25percentageInHeap,unlist(E1data[[box25]][[test]][lastDataPoint,][4]))
  }
}
boxVec25nrOfHeaps <- as.data.frame(boxVec25nrOfHeaps)
boxVec25percentageInHeap <- as.data.frame(boxVec25percentageInHeap)
boxVec25avgHeapSize <- as.data.frame(boxVec25avgHeapSize)
label25 <- as.data.frame(rep("25 boxes",nrow(boxVec25nrOfHeaps)))
dataFrame25 = merge(merge(merge(boxVec25nrOfHeaps, boxVec25percentageInHeap), boxVec25avgHeapSize), label25)
colnames(dataFrame25) <- c("numberOfHeaps", "percentageInHeap", "avgHeapSize","label")

dataFrame = rbind(dataFrame12, dataFrame25)
ggplot(dataFrame, aes(numberOfHeaps)) + geom_bar(aes(fill = label), position = "dodge")
ggplot(dataFrame, aes(percentageInHeap)) + geom_bar(aes(fill = label), position = "dodge")
ggplot(dataFrame, aes(x = avgHeapSize, y = numberOfHeaps)) + geom_point(aes(fill = label), position = "dodge")


#Robot against avgSize
# c("E1R1B12", "E1R1B25", "E1R3B12", "E1R3B25", "E1R5B12", "E1R5B25")
#first 10, 1 robot
#second 10, 3 robot
#third 10, 5 robot
RobotVsAvgSize12 <- list()
Robotnr <- list()
for(i in c(1,3,5)){
  for(test in seq(1,10)){
    lastDataPoint <- nrow(E1data[[i]][[test]])
    RobotVsAvgSize12 <- append(RobotVsAvgSize12,unlist(E1data[[i]][[test]][lastDataPoint,][2]))
    Robotnr <- append(Robotnr,i)
  }
}
RobotVsAvgSize12 <- as.data.frame(unlist(RobotVsAvgSize12))
Robotnr <- as.data.frame(unlist(Robotnr))
RobotVsAvgSize12 <- merge(RobotVsAvgSize12, Robotnr)
colnames(RobotVsAvgSize12) <- c("avgHeapSize", "robotNr")
ggplot(RobotVsAvgSize12, aes(avgHeapSize)) + geom_bar(aes(fill = robotNr), position = "dodge")

RobotVsAvgSize25 <- list()
Robotnr <- list()
for(i in c(1,3,5)){
  for(test in seq(1,10)){
    lastDataPoint <- nrow(E1data[[i]][[test]])
    RobotVsAvgSize25 <- append(RobotVsAvgSize25,unlist(E1data[[i]][[test]][lastDataPoint,][2]))
    Robotnr <- append(Robotnr,i)
  }
}
RobotVsAvgSize25 <- as.data.frame(unlist(RobotVsAvgSize25))
Robotnr <- as.data.frame(unlist(Robotnr))
RobotVsAvgSize25 <- merge(RobotVsAvgSize25, Robotnr)
colnames(RobotVsAvgSize25) <- c("avgHeapSize", "robotNr")
ggplot(RobotVsAvgSize25, aes(avgHeapSize)) + geom_bar(aes(fill = robotNr), position = "dodge")

#E2