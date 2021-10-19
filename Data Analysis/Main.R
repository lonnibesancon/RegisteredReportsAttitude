################################################################
# Main.R
# Code for the quantitative data analysis of the submission
# "Publishing Visualization Studies as Registered Reports: Expected Benefits and Researchers’ Attitudes"
# CC-BY Lonni Besançon et al., 2021
# See https://osf.io/4nrma/
################################################################

rm(list=ls())

#Check that "likert" is installed
if(!require(likert)){
  install.packages("likert")
  library(likert)
}

if(!require(vctrs)){
  install.packages("vctrs")
  library(vctrs)
}

source("HelperFunctionsCIs.R")
source("HelperFunctionsPlot.R")


#Reading csv file
df <- read.csv("../Data/Responses.tsv", sep = '\t',quote = "", stringsAsFactors = F)

# Renaming of columns to easier labels
colnames(df) <- c("Timestamp","Acknowledge","Experience","PreregKnowledge","PreregNumbers","RRKnowledge","RRKnowledgeComparison","Q1 Usefulness of early review","Q1 comments","Q2 Willingness to review","Q2 comments","Q3 Willingness to submit","Q3 comments","Q4 Interest to have RRs","Q4 comments")

############### Summary descriptive numbers to characterize our sample ###############
vec_count(df$Experience)
vec_count(df$PreregKnowledge)
vec_count(df$PreregNumbers)
vec_count(df$RRKnowledge)


#Replace all Likert values by numerical values which is needed for the rest of the analysis
df[df == "Very reluctant" ] <- 1
df[df == "Not helpful at all"] <- 1
df[df == "Not interested at all" ] <- 1
df[df == "Reluctant" ] <- 2
df[df == "Somewhat helpful"] <- 2
df[df == "Rather not interested"] <- 2
df[df == "Neutral"] <- 3
df[df == "Helpful"] <- 3
df[df == "Eager"] <- 4
df[df == "Very helpful"] <- 4
df[df == "Somehow interested"] <- 4
df[df == "Very eager"] <- 5
df[df == "Extremely helpful"] <- 5
df[df == "Very interested"] <- 5


############### Likert Plots ###############



#Create the factors for Likert Plots
dfLikertRR <- df[,c("Q1 Usefulness of early review","Q2 Willingness to review","Q3 Willingness to submit","Q4 Interest to have RRs")]
dfLikertRR <- lapply(dfLikertRR, factor, levels= c(1:5), ordered = TRUE)
dfLikertRR <- as.data.frame(dfLikertRR)
dfLikertRR <- dfLikertRR[,4:1]

#We create and save the plot as an image
p <- (likert(dfLikertRR))
figureName <- "../Figures/LikertPlots.pdf"
plot(p,group.order = c("Q1.Usefulness.of.early.review", "Q2.Willingness.to.review", "Q3.Willingness.to.submit", "Q4.Interest.to.have.RRs"))
ggsave(figureName)



############### Confidence Intervals plotting ###############
# We have two main hypothesis that we want to test
# 1/ Is the attitude towards RRs influenced by the number of years of experience?
# 2/ Is the attitude towards RRs influenced by the number of already submitted pre-registration?


dfLikertRR <- df[,c("Experience","PreregNumbers","Q1 Usefulness of early review","Q2 Willingness to review","Q3 Willingness to submit","Q4 Interest to have RRs")]
indexOfLikertData <- 3

#From now on we need to make sure that all values are numerical
for (i in indexOfLikertData:(indexOfLikertData+3)){
  dfLikertRR[,i] <- as.numeric(dfLikertRR[,i])
}


#There are only 4 Likert Items that we want to compare accross experience levels or experience with pre-regs
nbLikertItems <- 4


### Analysis #1: Is the attitude towards RRs influenced by the number of years of experience?

youngResearcher <- dfLikertRR[dfLikertRR$Experience == "< 5 years",]
midResearcher <- dfLikertRR[dfLikertRR$Experience == "5 to 10 years",]
seniorResearcher <- dfLikertRR[dfLikertRR$Experience == "> 10 years",]

youngData <- data.frame()
youngData <- bootstrapMeanCI(youngResearcher[,indexOfLikertData])
midData <- data.frame()
midData = bootstrapMeanCI(midResearcher[,indexOfLikertData])
seniorData <- data.frame()
seniorData = bootstrapMeanCI(seniorResearcher[,indexOfLikertData])
analysisdata <- c()
question <- c()

for (i in (indexOfLikertData+1):6){
  youngData <- rbind(youngData,(bootstrapMeanCI(youngResearcher[,i])))
  midData <- rbind(midData,(bootstrapMeanCI(midResearcher[,i])))
  seniorData <- rbind(seniorData,(bootstrapMeanCI(seniorResearcher[,i])))
}

questionList <- c("Q1 Usefulness of early review","Q2 Willingness to review","Q3 Willingness to submit","Q4 Interest to have RRs")

for(i in 1:nbLikertItems){
  question <- questionList[i]
  analysisdata$question = c(analysisdata$question,question,question,question)
  analysisdata$name = c(analysisdata$name,"> 10 years in the field","5 to 10 years in the field","< 5 years in the field")
  analysisdata$pointEstimate = c(analysisdata$pointEstimate,seniorData[i,1],midData[i,1],youngData[i,1])
  analysisdata$ci.max = c(analysisdata$ci.max,seniorData[i,3],midData[i,3], youngData[i,3])
  analysisdata$ci.min = c(analysisdata$ci.min,seniorData[i,2],midData[i,2], youngData[i,2])
}

dataToPrint <- data.frame(factor(analysisdata$name),factor(analysisdata$question),analysisdata$pointEstimate, analysisdata$ci.max, analysisdata$ci.min)
colnames(dataToPrint) <- c("legend_factor", "y_axis_items","measure", "lowerBound_CI", "upperBound_CI")
dataToPrint$y_axis_items <- factor(dataToPrint$y_axis_items, levels = rev(c("Q1 Usefulness of early review", "Q2 Willingness to review", "Q3 Willingness to submit", "Q4 Interest to have RRs")))
dataToPrint$legend_factor <- factor(dataToPrint$legend_factor, levels = c("> 10 years in the field","5 to 10 years in the field","< 5 years in the field"))
plotCI(dataToPrint, xlab="Questions", ylab="", ymax=5)

figureName <- "../Figures/ExperienceLevels.pdf"
ggsave(figureName)


### Analysis #2: Is the attitude towards RRs influenced by the number of already submitted pre-registration?


noPreregs <- dfLikertRR[dfLikertRR$PreregNumbers == "0 pre-registration",]
fewPreregs <- dfLikertRR[dfLikertRR$PreregNumbers == "1 to 5 pre-registrations",]
manyPreregs <- dfLikertRR[dfLikertRR$PreregNumbers == "> 5 pre-registrations",]

noPreregData <- data.frame()
noPreregData <- bootstrapMeanCI(noPreregs[,indexOfLikertData])
fewPreregsData <- data.frame()
fewPreregsData = bootstrapMeanCI(fewPreregs[,indexOfLikertData])
manyPreregsData <- data.frame()
manyPreregsData = bootstrapMeanCI(manyPreregs[,indexOfLikertData])
analysisdata <- c()
question <- c()

for (i in (indexOfLikertData+1):6){
  noPreregData <- rbind(noPreregData,(bootstrapMeanCI(noPreregs[,i])))
  fewPreregsData <- rbind(fewPreregsData,(bootstrapMeanCI(fewPreregs[,i])))
  manyPreregsData <- rbind(manyPreregsData,(bootstrapMeanCI(manyPreregs[,i])))
}

questionList <- c("Q1 Usefulness of early review","Q2 Willingness to review","Q3 Willingness to submit","Q4 Interest to have RRs")

for(i in 1:nbLikertItems){
  question <- questionList[i]
  analysisdata$question = c(analysisdata$question,question,question,question)
  analysisdata$name = c(analysisdata$name,"Submitted 6+ pre-registrations","Submitted 1 to 5 pre-registrations","No pre-registration submitted")
  analysisdata$pointEstimate = c(analysisdata$pointEstimate,manyPreregsData[i,1],fewPreregsData[i,1],noPreregData[i,1])
  analysisdata$ci.max = c(analysisdata$ci.max,manyPreregsData[i,3], fewPreregsData[i,3],noPreregData[i,3])
  analysisdata$ci.min = c(analysisdata$ci.min,manyPreregsData[i,2], fewPreregsData[i,2],noPreregData[i,2])
}

dataToPrint <- data.frame(factor(analysisdata$name),factor(analysisdata$question),analysisdata$pointEstimate, analysisdata$ci.max, analysisdata$ci.min)
colnames(dataToPrint) <- c("legend_factor", "y_axis_items","measure", "lowerBound_CI", "upperBound_CI")
dataToPrint$y_axis_items <- factor(dataToPrint$y_axis_items, levels = rev(c("Q1 Usefulness of early review", "Q2 Willingness to review", "Q3 Willingness to submit", "Q4 Interest to have RRs")))
dataToPrint$legend_factor <- factor(dataToPrint$legend_factor, levels = c("Submitted 6+ pre-registrations", "Submitted 1 to 5 pre-registrations","No pre-registration submitted"))
plotCI(dataToPrint, xlab="Questions", ylab="", ymax=5)


figureName <- "../Figures/PreregsLevels.pdf"
ggsave(figureName)


############### Descriptive stats bit on the knowledge about Preregs and RR as well as experience ###############
# Generates a sentence to copy paste directly in the paper

##' Function that generates the descriptive statistics that we will report in the paper
##' @param input_df the dataframe with a count for each possible value
##' @param nb_participants the number of total participants to generate the percentage
##' @return a string that can be put in the paper
generate_string_from_count <- function(input_df,nb_participants){
  size <- nrow(input_df)
  print(size)
  return_string <- ""
  nb_participants <- strtoi(nb_participants)
  for(i in 1:size){
    answer_string <- input_df[i,1]
    answer_value <- strtoi(input_df[i,2])
    percentage <- round((answer_value/nb_participants*100),1)
    return_string <- paste0(return_string," ``",answer_string,"'' (x ",answer_value,", ",percentage,"%),")
  }
  return_string <- substr(return_string,1,nchar(return_string)-1)
  return_string <- paste0(return_string,".")
  return (return_string)

}

nb_participants <- nrow(df)
Prereg_knowledge_count <- vec_count(df$PreregKnowledge)
RR_knowledge_count <- vec_count(df$RRKnowledge)
RR_knowledge_comparison_count <- vec_count(df$RRKnowledgeComparison)
nb_preregs_count <- vec_count(df$PreregNumbers)
experience_count <- vec_count(df$Experience)

print(paste0(nb_participants," participants responded to our survey."))
print(generate_string_from_count(experience_count,nb_participants))
print(generate_string_from_count(RR_knowledge_count,nb_participants))
print(generate_string_from_count(RR_knowledge_comparison_count,nb_participants))
print(generate_string_from_count(Prereg_knowledge_count,nb_participants))


