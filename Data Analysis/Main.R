##
#
# CC-BY Lonni Besancon et al. 
# See https://osf.io/4nrma/
##

#Check that "likert" is installed
if(!require(likert)){
  install.packages("likert")
  library(likert)
}

source("HelperFunctionsCIs.R")
source("HelperFunctionsPlot.R")



#Reading csv file
df <- read.csv("../Data/MockUp.csv")


# Renaming of columns to easier labels
colnames(df) <- c("Timestamp","Experience","LikertPreregistrationKnowledge","LikertPreregistrationNumbers","Knowledge Of RRs","LikertRRHelpful","RRHelpful","LikertRRReviewerWilling","RRReviewerWilling","LikertRRAuthorWilling","RRAuthorWilling","LikertRRInterested","RRInterested")

#Replace all Likert values by numerical values
df[df == "Very reluctant" ] <- 1
df[df == "Not helpful at all"] <- 1
df[df == "Reluctant" ] <- 2
df[df == "Somewhat helpful"] <- 2
df[df == "Neutral"] <- 3
df[df == "Helpful"] <- 3
df[df == "Eager"] <- 4
df[df == "Very helpful"] <- 4
df[df == "Very eager"] <- 5
df[df == "Extremely helpful"] <- 5



#Create the factors for Likert Plots
dfLikertRR <- df[,c("LikertRRHelpful","LikertRRReviewerWilling","LikertRRAuthorWilling","LikertRRInterested")]
dfLikertRR <- lapply(dfLikertRR, factor, levels= c(1:5), ordered = TRUE)
dfLikertRR <- as.data.frame(dfLikertRR)

dfExperience <- df[c(2)]
dfExperience <- lapply(dfExperience, factor, levels= c("< 5 years", "5 to 10 years", "> 10 years" ), ordered = TRUE)
dfExperience <- as.data.frame(dfExperience)


#RRData <- df[,substr(names(df), 1,10) == 'RR']

p <- likert(dfLikertRR)
plot(p)






############### Confidence Intervals plotting ###############
# We have two main hypothesis that we want to test
# 1/ Is the attitude towards RRs influenced by the number of years of experience?
# 2/ Is the attitude towards RRs influenced by the number of already submitted pre-registration?

indexOfLikertData <- 2

#From now on we need to make sure that all values are numerical
for (i in indexOfLikertData:indexOfLikertData+3){
  dfLikertRR[,i] <- as.numeric(dfLikertRR[,i])
}

#There are only 4 Likert Items that we want to compare accross experience levels or experience with pre-regs
nbLikertItems <- 4


### Analysis #1: Is the attitude towards RRs influenced by the number of years of experience?

dfLikertRR <- df[,c("Experience","LikertPreregistrationNumbers","LikertRRHelpful","LikertRRReviewerWilling","LikertRRAuthorWilling","LikertRRInterested")]

youngResearcher <- dfLikertRR[dfLikertRR$Experience == "< 5 years",]
midResearcher <- dfLikertRR[dfLikertRR$Experience == "5 to 10 years",]
seniorResearcher <- dfLikertRR[dfLikertRR$Experience == "> 10 years",]

youngData <- data.frame()
youngData <- bootstrapMeanCI(youngResearcher[,indexOfLikertData])
midData <- data.frame()
midData = bootstrapMeanCI(midResearcher[,3])
seniorData <- data.frame()
seniorData = bootstrapMeanCI(seniorResearcher[,3])
analysisdata <- c()
question <- c()

for (i in 3:5){
  youngData <- rbind(youngData,(bootstrapMeanCI(youngResearcher[,i])))
  midData <- rbind(midData,(bootstrapMeanCI(midResearcher[,i])))
  seniorData <- rbind(seniorData,(bootstrapMeanCI(seniorResearcher[,i])))
}

questionList <- c("LikertRRHelpful","LikertRRReviewerWilling","LikertRRAuthorWilling","LikertRRInterested")

for(i in 1:nbLikertItems){
  question <- questionList[i]
  analysisdata$question = c(analysisdata$question,question,question,question)
  analysisdata$name = c(analysisdata$name,"Senior","Middle","Young")
  analysisdata$pointEstimate = c(analysisdata$pointEstimate,seniorData[i,1],midData[i,1],youngData[i,1])
  analysisdata$ci.max = c(analysisdata$ci.max,seniorData[i,3],midData[i,3], youngData[i,3])
  analysisdata$ci.min = c(analysisdata$ci.min,seniorData[i,2],midData[i,2], youngData[i,2])
}

dataToPrint <- data.frame(factor(analysisdata$name),factor(analysisdata$question),analysisdata$pointEstimate, analysisdata$ci.max, analysisdata$ci.min)
colnames(dataToPrint) <- c("legend_factor", "y_axis_items","measure", "lowerBound_CI", "upperBound_CI")
dataToPrint$y_axis_items <- factor(dataToPrint$y_axis_items, levels = c("LikertRRHelpful", "LikertRRReviewerWilling", "LikertRRAuthorWilling", "LikertRRInterested"))
dataToPrint$legend_factor <- factor(dataToPrint$legend_factor, levels = c("Senior", "Middle", "Young"))
plotCI(dataToPrint, xlab="Questions", ylab="", ymax=5)

figureName <- "../Figures/ExperienceLevels.pdf"


### Analysis #2: Is the attitude towards RRs influenced by the number of already submitted pre-registration?

dfLikertRR <- df[,c("LikertPreregistrationNumbers","LikertRRHelpful","LikertRRReviewerWilling","LikertRRAuthorWilling","LikertRRInterested")]

noPreregs <- dfLikertRR[dfLikertRR$LikertPreregistrationNumbers == "0 pre-registration",]
fewPreregs <- dfLikertRR[dfLikertRR$LikertPreregistrationNumbers == "1 to 5 pre-registrations",]
somePreregs <- dfLikertRR[dfLikertRR$LikertPreregistrationNumbers == "5 to 10 pre-registrations",]
manyPreregs <- dfLikertRR[dfLikertRR$LikertPreregistrationNumbers == "> 10  pre-registrations",]

noPreregData <- data.frame()
noPreregData <- bootstrapMeanCI(noPreregs[,2])
fewPreregsData <- data.frame()
fewPreregsData = bootstrapMeanCI(fewPreregs[,2])
somePreregsData <- data.frame()
somePreregsData = bootstrapMeanCI(somePreregs[,2])
manyPreregsData <- data.frame()
manyPreregsData = bootstrapMeanCI(manyPreregs[,2])
analysisdata <- c()
question <- c()

for (i in 3:5){
  youngData <- rbind(youngData,(bootstrapMeanCI(youngResearcher[,i])))
  midData <- rbind(midData,(bootstrapMeanCI(midResearcher[,i])))
  seniorData <- rbind(seniorData,(bootstrapMeanCI(seniorResearcher[,i])))
}

questionList <- c("LikertRRHelpful","LikertRRReviewerWilling","LikertRRAuthorWilling","LikertRRInterested")

for(i in 1:nbLikertItems){
  question <- questionList[i]
  analysisdata$question = c(analysisdata$question,question,question,question)
  analysisdata$name = c(analysisdata$name,"Senior","Middle","Young")
  analysisdata$pointEstimate = c(analysisdata$pointEstimate,seniorData[i,1],midData[i,1],youngData[i,1])
  analysisdata$ci.max = c(analysisdata$ci.max,seniorData[i,3],midData[i,3], youngData[i,3])
  analysisdata$ci.min = c(analysisdata$ci.min,seniorData[i,2],midData[i,2], youngData[i,2])
}

dataToPrint <- data.frame(factor(analysisdata$name),factor(analysisdata$question),analysisdata$pointEstimate, analysisdata$ci.max, analysisdata$ci.min)
colnames(dataToPrint) <- c("legend_factor", "y_axis_items","measure", "lowerBound_CI", "upperBound_CI")
dataToPrint$y_axis_items <- factor(dataToPrint$y_axis_items, levels = c("LikertRRHelpful", "LikertRRReviewerWilling", "LikertRRAuthorWilling", "LikertRRInterested"))
dataToPrint$legend_factor <- factor(dataToPrint$legend_factor, levels = c("Senior", "Middle", "Young"))
plotCI(dataToPrint, xlab="Questions", ylab="", ymax=5)

figureName <- "../Figures/ExperienceLevels.pdf"






analysisdata$question = c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4")
analysisdata$name = c("G1","G2","G3","G1","G2","G3","G1","G2","G3","G1","G2","G3")
analysisdata$pointEstimate = c(1,2,3,1,2,3,1,2,3,1,2,3)
analysisdata$ci.max = c(2,3,4,2,3,4,2,3,4,2,3,4)
analysisdata$ci.min = c(0,1,2,0,1,2,0,1,2,0,1,2)

dataToPrint <- data.frame(factor(analysisdata$name),factor(analysisdata$question),analysisdata$pointEstimate, analysisdata$ci.max, analysisdata$ci.min)
colnames(dataToPrint) <- c("legend_factor", "y_axis_items","measure", "lowerBound_CI", "upperBound_CI")
dataToPrint$y_axis_items <- factor(dataToPrint$y_axis_items, levels = c("Q4", "Q3", "Q2", "Q1"))
dataToPrint$legend_factor <- factor(dataToPrint$legend_factor, levels = c("G3", "G1", "G2"))
plotCI(dataToPrint, xlab="Questions", ylab="", ymax=5) 
