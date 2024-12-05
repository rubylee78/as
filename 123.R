setwd("C:\\Users\\USER\\Desktop")
bank<-read.csv("bank.csv")
bank<-bank[,-c(10,11,13,14)]
str(bank)

library(infotheo)
bank$age<-discretize(bank$age,"equalwidth",3)
bank$age<-as.factor(bank$age$X)

bank$duration<-discretize(bank$duration,"equalfreq",3)
bank$duration<-as.factor(bank$duration$X)

bank$balance<-discretize(bank$balance,"equalfreq",3)
bank$balance<-as.factor(bank$balance$X)

bank$job<-as.factor(bank$job)
bank$marital<-as.factor(bank$marital)
bank$education<-as.factor(bank$education)
bank$default<-as.factor(bank$default)
bank$housing<-as.factor(bank$housing)
bank$loan<-as.factor(bank$loan)
bank$contact<-as.factor(bank$contact)
bank$previous<-as.factor(bank$previous)
bank$poutcome<-as.factor(bank$poutcome)
bank$y<-as.factor(bank$y)

require(arules)
rule<-apriori(bank,parameter = list(supp=0.1,conf=0.7),
              appearance = list(rhs=c("y=yes","y=no")))
sort.rule<-sort(rule,by="support")
subset.matrix<-as.matrix(is.subset(x=sort.rule,y=sort.rule))
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
sort.rule<-sort.rule[!redundant]
sort.rule<-as(sort.rule,"data.frame")
write.csv(sort.rule,"1.csv")

bankNo=subset(bank,y=="no")
bankYes=subset(bank,y=="yes")
set.seed(20241205)
n<-nrow(bankNo)
sindex<-sample(n,521)
newbankNo=bankNo[sindex,]
newbank<-rbind(bankNo,bankYes)
require(arules)
rule<-apriori(newbank,parameter = list(supp=0.1,conf=0.7),
              appearance = list(rhs=c("y=yes","y=no")))
sort.rule<-sort(rule,by="support")
subset.matrix<-as.matrix(is.subset(x=sort.rule,y=sort.rule))
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
sort.rule<-sort.rule[!redundant]
sort.rule<-as(sort.rule,"data.frame")
write.csv(sort.rule,"bank-rule.csv")
