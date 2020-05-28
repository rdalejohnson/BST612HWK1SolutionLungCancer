#Lung Cancer data set, descriptive statistics, Summer 2020, BST612Q
# Cell Type,Treatment,Prior,Age,KPS

#install.packages("pastecs")
library(pastecs)



lungCancer = read.csv("Lung_Cancer.csv")

head(lungCancer)

str(lungCancer)

summary(lungCancer)

min(lungCancer$Age)
max(lungCancer$Age)
range(lungCancer$Age)
mean(lungCancer$Age)
mean(lungCancer$Age, na.rm = TRUE)
median(lungCancer$Age)


min(lungCancer$KPS)
max(lungCancer$KPS)
range(lungCancer$KPS)
mean(lungCancer$KPS)
mean(lungCancer$KPS, na.rm = TRUE)
median(lungCancer$KPS)

by(lungCancer, lungCancer$Treatment, summary)
stat.desc(lungCancer)


