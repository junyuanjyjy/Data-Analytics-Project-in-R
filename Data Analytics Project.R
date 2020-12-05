library(magrittr)
library(dplyr) 
library(corrplot)
library(corrgram)
library(tidyr)
library(caTools)
library(ggplot2)
library(caret)
library(MASS)
library(class)
library(ISLR)
library(MASS)
library(tree)


#PART1 starts
ewcs=read.table("EWCS_2016.csv",sep=",",header=TRUE)
ewcs[,][ewcs[, ,] == -999] <- NA
kk=complete.cases(ewcs)
ewcs=ewcs[kk,]
pca = prcomp(ewcs, scale = TRUE)
pca.var = pca$sdev^2
pcavar.per = round(pca.var/sum(pca.var)*100,1)
#biplot
names(pca)
biplot(pca, scale = 0)
#scree plot
barplot(pcavar.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

#95% confidence interval ellipse Principal Component Analysis Score Plot
ewcs2 <- cbind(ewcs, pca$x[,1:2])
ewcs2$Q2a <- factor(ewcs2$Q2a, levels = c(1,2),labels = c("Male","Female"))
AgeCat = cut(ewcs2$Q2b, breaks=c(0,20,40,60,80,100),labels=c("0-20","20-40","40-60","60-80","80-100"))
ewcs2 <- cbind(ewcs2, AgeCat)
ggplot(ewcs2, aes(PC1, PC2, col = Q2a, fill = Q2a)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
ggplot(ewcs2, aes(PC1, PC2, col = AgeCat, fill = AgeCat)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
#PART1 ends


#Part 2 starts
school1=read.table("student-mat.csv",sep=";",header=TRUE)
school2=read.table("student-por.csv",sep=";",header=TRUE)
schools=merge(school1,school2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
sch1 = subset(school1, select = -c(G1,G2))
sch2 = subset(school2, select = -c(G1,G2))
school = subset(schools, select = -c(G1.x,G2.x,G1.y,G2.y) )



#Histogram of final grades
school %>%
  gather(`G3.x`, `G3.y`, key="course", value="grade") %>%
  ggplot() +
  geom_bar(aes(x=grade, fill=course), position="dodge") + 
  ggtitle("Distribution of final grades in Math and Portuguese courses") +
  scale_fill_discrete(name = "Course", labels = c("Math", "Portuguese"))


#Correlation matrix of Maths dataset

num.cols <- sapply(sch1, is.numeric)
cor.data <- cor(sch1[,num.cols])
corrplot::corrplot(cor.data, method='number')

#Correlation matrix of Portugese dataset

num.cols <- sapply(sch2, is.numeric)
cor.data <- cor(sch2[,num.cols])
corrplot::corrplot(cor.data, method='number')


# MLR full model - Maths dataset
set.seed(1)
sample <- sample.split(sch1, SplitRatio = 0.7)
train <- subset(sch1,sample==TRUE)
test <- subset(sch1,sample==FALSE)
lm <- lm(G3~., data=train)
summary(lm)
par(mfrow=c(2,2))
plot(lm)
G3.pred <- predict(lm, test)
summary(G3.pred)
MSE = mean((G3.pred-test$G3)^2)
MSE
G3.pred2 <- predict(lm, sch2)
MSE2 = mean((G3.pred2-sch2$G3)^2)
MSE2

# MLR partial model - Maths dataset
set.seed(1)
sample <- sample.split(sch1, SplitRatio = 0.7)
train <- subset(sch1,sample==TRUE)
test <- subset(sch1,sample==FALSE)
lm <- lm(G3~failures+romantic+absences, data=train)
summary(lm)
G3.pred <- predict(lm, test)
summary(G3.pred)
MSE = mean((G3.pred-test$G3)^2)
MSE
G3.pred2 <- predict(lm, sch2)
MSE2 = mean((G3.pred2-sch2$G3)^2)
MSE2


# MLR full model - Port dataset
set.seed(1)
sample <- sample.split(sch2, SplitRatio = 0.7)
train <- subset(sch2,sample==TRUE)
test <- subset(sch2,sample==FALSE)
lm <- lm(G3~., data=train)
summary(lm)
par(mfrow=c(2,2))
plot(lm)
G3.pred <- predict(lm, test)
summary(G3.pred)
MSE = mean((G3.pred-test$G3)^2)
MSE
G3.pred2 <- predict(lm, sch1)
MSE2 = mean((G3.pred2-sch1$G3)^2)
MSE2

# MLR partial model - Port dataset
set.seed(1)
sample <- sample.split(sch2, SplitRatio = 0.7)
train <- subset(sch2,sample==TRUE)
test <- subset(sch2,sample==FALSE)
lm <- lm(G3~school+failures+schoolsup+higher, data=train)
summary(lm)
par(mfrow=c(2,2))
G3.pred <- predict(lm, test)
summary(G3.pred)
MSE = mean((G3.pred-test$G3)^2)
MSE
G3.pred2 <- predict(lm, sch1)
MSE2 = mean((G3.pred2-sch1$G3)^2)
MSE2
#Part 2 ends

#Part 3 starts
bank=read.table("bank.csv",sep=";",header=TRUE)
bank1 = subset(bank, select = -c(duration))


#Correlation matrix 
num.cols <- sapply(bank, is.numeric)
cor.data <- cor(bank[,num.cols])
corrplot::corrplot(cor.data, method='number')

#Logistic Regression - Full Training Set
set.seed(1)
glm.fits=glm(y~., data=bank1,family=binomial)
summary(glm.fits)
prob = predict(glm.fits,type="response")
pred=ifelse(prob>0.5,"yes","no")
attach(bank1)
table(pred,y)
mean(pred==y)

#Logistic Regression - 70% training Set 30% Test Set Full Model
set.seed(1)
sample <- sample.split(bank1, SplitRatio = 0.7)
train <- subset(bank1,sample==TRUE)
test <- subset(bank1,sample==FALSE)
glm.fits=glm(y~., data=train,family=binomial)
summary(glm.fits)
prob = predict(glm.fits,test,type="response")
pred=ifelse(prob>0.5,"yes","no")
table(pred,test$y)
mean(pred==test$y)

#Logistic regression - 70% training Set 30% Test Set Partial Model
set.seed(1)
glm.fits=glm(y~month+campaign+contact+poutcome, data=train,family=binomial)
summary(glm.fits)
prob = predict(glm.fits,test,type="response")
pred=ifelse(prob>0.5,"yes","no")
table(pred,test$y)
mean(pred==test$y)


#Linear Discriminant Analysis (LDA) classifier 
set.seed(1)
lda.fit=lda(y~month+campaign+contact+poutcome, data=train)
pred=predict(lda.fit,test)
table(pred$class,test$y)
mean(pred$class==test$y)

#Quadratic Discriminant Analysis (QDA) classifier 
set.seed(1)
qda.fit=qda(y~month+campaign+contact+poutcome, data=train)
pred=predict(qda.fit,test)
table(pred$class,test$y)
mean(pred$class==test$y)


# K-Nearest Neighbours (LDA) classifier
set.seed(1)
train.X = cbind(train$job,train$marital,train$loan,train$contact,train$poutcome)
test.X = cbind(test$job,test$marital,test$loan,test$contact,test$poutcome)
knn.pred = knn(train.X, test.X, train$y, k = 1)
mean(knn.pred == test$y)
knn.pred = knn(train.X, test.X, train$y, k = 10)
mean(knn.pred == test$y)
knn.pred = knn(train.X, test.X, train$y, k = 100)
mean(knn.pred == test$y)

# Classification Tree
set.seed(1)
tree = tree(y~job+marital+loan+contact+poutcome,train)
tree.pred=predict(tree,test,type="class")
table(tree.pred,test$y)
mean(tree.pred==test$y)
#Pruning of Classification Tree
set.seed(1)
cv=cv.tree(tree,FUN=prune.misclass)
prune =prune.misclass (tree,best=2)
treepred=predict(prune,test,type="class")
table(treepred,test$y)
mean(treepred==test$y)
#Part 3 ends 