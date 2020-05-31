#Lung Cancer data set, descriptive statistics, Summer 2020, BST612Q
# Cell Type,Treatment,Prior,Age,KPS

#install.packages("pastecs")
library(pastecs)
library(summarytools)
library(tidyverse)
library(psych)
library(e1071)
library(lsr)


# Create the function.
# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


lungCancer = read.csv("Lung_Cancer.csv")

freq(lungCancer$Cell.Type)
freq(lungCancer$Treatment)
freq(lungCancer$Prior)
freq(lungCancer$Age)
freq(lungCancer$KPS)

getmode(lungCancer$Age)
skewness(lungCancer$Age, na.rm = FALSE)
histo <- hist(lungCancer$Age)

table(cut(lungCancer$Age, breaks=seq(0, 100, 10)))

h = hist(lungCancer$Age) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)



h = hist(lungCancer$KPS) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)

table(cut(lungCancer$KPS, breaks=seq(0, 100, 10)))
skewness(lungCancer$KPS, na.rm = FALSE)




# xxx <- table(   subset(lungCancer,lungCancer$Age > 2)$Age)
# 
# cbind(xxx,prop.table(xxx))



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



object = lungCancer %>% group_by(Treatment) %>% summarise(mean = mean(Age), sd = sd(Age), n = n())




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

chiCellType <- chisq.test(xx[, 2:5])

#
phi(xx[, 2:5])

cramersV(xx[,2:5])



test <- fisher.test(xx)
test

by(lungCancer$Treatment, lungCancer$Prior, summary)
table(lungCancer$Treatment, lungCancer$Prior)
chiPriorDx <-chisq.test(table(lungCancer$Treatment, lungCancer$Prior))

phi(table(lungCancer$Treatment, lungCancer$Prior))
cramersV(table(lungCancer$Treatment, lungCancer$Prior))


library(car)

#http://www.sthda.com/english/wiki/f-test-compare-two-variances-in-r

res.ftest <- var.test(Age ~ Treatment, data = lungCancer)
res.ftest$p.value

bartlett.test(Age ~ Treatment, data=lungCancer)
lvtest <- leveneTest(Age ~ Treatment, data=lungCancer)
fligner.test(Age ~ Treatment, data = lungCancer)
plot(Age ~ Treatment, data = lungCancer)
t.test (Age ~ Treatment , var.equal=FALSE, data = lungCancer)
lungCancer %>% group_by(Treatment) %>% summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), 
                                                    median = median(Age, na.rm=TRUE) , min=min(Age, na.rm=TRUE),
                                                    max=max(Age, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Age)))

shapiro.test(lungCancer$Age)
qqnorm(lungCancer$Age)
qqline(lungCancer$Age, col = "red")
outlierAges <- boxplot(lungCancer$Age, plot=FALSE)$out
x<-lungCancer
x<- x[-which(lungCancer$Age %in% outlierAges),]

res.ftest <- var.test(Age ~ Treatment, data = x)
res.ftest$p.value

bartlett.test(Age ~ Treatment, data=x)
leveneTest(Age ~ Treatment, data=x)
fligner.test(Age ~ Treatment, data = x)
plot(Age ~ Treatment, data = x)
t.test (Age ~ Treatment , var.equal=TRUE, data = x)
x %>% group_by(Treatment) %>% summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), 
                                                 median = median(Age, na.rm=TRUE) , min=min(Age, na.rm=TRUE),
                                                 max=max(Age, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Age)))


################### KPS ********************

shapiro.test(lungCancer$KPS)
qqnorm(lungCancer$KPS)
qqline(lungCancer$KPS, col = "red")

res.ftest <- var.test(KPS ~ Treatment, data = lungCancer)
res.ftest$p.value

bartlett.test(KPS ~ Treatment, data=lungCancer)
leveneTest(KPS ~ Treatment, data=lungCancer)
fligner.test(KPS ~ Treatment, data = lungCancer)

#plot(KPS ~ Treatment, data = lungCancer)

t.test (KPS ~ Treatment , var.equal=TRUE, data = lungCancer)


kpsss <- lungCancer %>% group_by(Treatment) %>% summarise(mean = mean(KPS, na.rm=TRUE), sd = sd(KPS, na.rm=TRUE), 
                                                 median = median(KPS, na.rm=TRUE) , min=min(KPS, na.rm=TRUE),
                                                 max=max(KPS, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(KPS)))



#https://statistics.laerd.com/r-tutorials/independent-samples-t-test-using-r-excel-and-rstudio-3.php

