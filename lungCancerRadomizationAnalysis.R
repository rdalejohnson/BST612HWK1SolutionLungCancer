#Lung Cancer data set, descriptive statistics, Summer 2020, BST612Q
# Cell Type,Treatment,Prior,Age,KPS

#install.packages("pastecs")
library(pastecs)
library(summarytools)




lungCancer = read.csv("Lung_Cancer.csv")

freq(lungCancer$Cell.Type)
freq(lungCancer$Treatment)
freq(lungCancer$Prior)
freq(lungCancer$Age)
freq(lungCancer$KPS)

head(lungCancer)

str(lungCancer)

# Complete rows
complete.cases(lungCancer)

#  report position of NAs
which(is.na(lungCancer$Cell.Type))
which(is.na(lungCancer$Treatment))
which(is.na(lungCancer$Prior))
which(is.na(lungCancer$Age))
which(is.na(lungCancer$KPS))



summary(lungCancer)

min(lungCancer$Age)
max(lungCancer$Age)
range(lungCancer$Age)
mean(lungCancer$Age)
mean(lungCancer$Age, na.rm = TRUE)
median(lungCancer$Age)
sd(lungCancer$Age)


#age outliers
#https://www.r-bloggers.com/how-to-remove-outliers-in-r/
outlierAges <- boxplot(lungCancer$Age, plot=FALSE)$out
x<-lungCancer
x<- x[-which(lungCancer$Age %in% outlierAges),]
min(x$Age)
max(x$Age)
range(x$Age)
mean(x$Age)
mean(x$Age, na.rm = TRUE)
median(x$Age)
sd(x$Age)

boxplot(lungCancer$KPS, plot=FALSE)$out
min(lungCancer$KPS)
max(lungCancer$KPS)
range(lungCancer$KPS)
mean(lungCancer$KPS)
mean(lungCancer$KPS, na.rm = TRUE)
median(lungCancer$KPS)
sd(lungCancer$Age)


by(lungCancer, lungCancer$Treatment, summary)
stat.desc(lungCancer)

############################################
###### CHI SQUARE ###############
###########################################

#lungCancer$Cell.Type[lungCancer$Cell.Type==5] <- NA
#lungCancer2<-lungCancer[(lungCancer$Cell.Type!=5),]
#lungCancer <- subset(lungCancer,lungCancer$Cell.Type!=5)
#library(dplyr)
#
#lungCancer2 <- filter(lungCancer, lungCancer$Cell.Type!=5)

#lungCancer  <- lungCancer[-118,]

by(lungCancer$Treatment, lungCancer$Cell.Type, summary)
xx <- table(lungCancer$Treatment, lungCancer$Cell.Type)
#colnames(xx)

xx[, 2:5]

chisq.test(xx[, 2:5])

test <- fisher.test(xx)
test

by(lungCancer$Treatment, lungCancer$Prior, summary)
table(lungCancer$Treatment, lungCancer$Prior)
chisq.test(table(lungCancer$Treatment, lungCancer$Prior))




