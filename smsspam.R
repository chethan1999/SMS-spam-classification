library(tm)
library(wordcloud)
library(e1071)
library(gmodels)
library(SnowballC)
spam <- read.csv("sms_spam.csv")
spam$type <- factor(spam$type)
table(spam$type)

spam_messages <- subset(spam,type=="spam")
ham_messages <- subset(spam, type=="ham")
wordcloud(spam_messages$text, max.words = 100, scale = c(3,0.5))

#wordcloud(ham_messages$text, max.words = 100, scale = c(3,0.5))

corpus <- VCorpus(VectorSource(spam$text)) 
dtm <- DocumentTermMatrix(corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

trainLabels <-spam[1:4169,]$type
testLabels <- spam[4170:5559,]$type
prop.table(table(trainLabels))

prop.table(table(testLabels))


dtmTrain <- dtm[1:4169,]
dtmTest <- dtm[4170:5559,]

freqWords <- findFreqTerms(dtmTrain,5)
freqTrain <- dtmTrain[,freqWords]
freqTest <- dtmTest[,freqWords]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
train <- apply(freqTrain, MARGIN = 2,
               convert_counts)
test <- apply(freqTest, MARGIN = 2,
              convert_counts)



classifier <- naiveBayes(train, trainLabels)
classifier[2]$tables$call

testPredict <- predict(classifier, test)
CrossTable(testPredict, testLabels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted',''))

plot(testPredict,type="o",col="blue",xlab="Type of message",ylab="No of messages",main="SMS spam classification")

