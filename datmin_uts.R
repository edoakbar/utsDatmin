library(readxl)

wine <- read.csv(file.choose(),header = TRUE)
str(wine)

n <- round(nrow(wine)*0.75);n
set.seed(123);samp=sample(1:nrow(wine),n)

#memisahkan
train=wine[samp,]
test=wine[-samp,]

#mengambil nama variabel
namVar<-names(wine[,1:14])

#gabungan string
f <- paste(namVar,collapse=' + ')
f <- paste('Diagnosis ~',f)

# Convert to formula
f<-as.formula(f);f

#train neural net
library(neuralnet)
nn <- neuralnet(f,train,hidden = 10)

plot(nn)

#testing dg data test
pred1 <- compute(nn,test[1:9])
pred1.r <- round(pred1$net.result)

#confusion matrix
library(caret)
confusionMatrix(pred1.r,test$Diagnosis)