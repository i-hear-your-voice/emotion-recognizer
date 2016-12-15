library(tuneR)
library(audio)
library(base)
library(e1071)

# set path of voice files
path <- "/Users/hyeonjin/workspace/r/emotion-recognizer/voice"
setwd(path)

# read the list of voice files
wav.filename <- list.files()

# get label (emotion)
wav.split <- as.list(sapply(wav.filename, function(x) strsplit(x, "_") ))
wav.emotion <- as.vector(sapply(wav.split, function(x) x[2]))

# get mono wav objects
wav.obj <- sapply(wav.filename, function(x) mono(readWave(x), which = c("left", "right", "both")))

# get mfcc data
wav.mfcc <- sapply(wav.obj, function(x) melfcc(x))
wav.mfcc.mean <- sapply(wav.mfcc, function(x) apply(x, 2, mean)) 

wav.filename[1]
wav.emotion[1] 
wav.mfcc.mean[, 1]
class(wav.mfcc.mean[, 1])
wav.mfcc.mean <- data.frame(t(wav.mfcc.mean))

#
wav.emotion
wav.emotion.ang <- wav.emotion == "ang"
wav.emotion.flat<- wav.emotion == "flat"
wav.emotion.hap <- wav.emotion == "hap"
wav.emotion.sad <- wav.emotion == "sad"
wav.emotion.fear<- wav.emotion == "fear"
wav.emotion.sup <- wav.emotion == "sup"

wav.emotion.ang
wav.emotion.flat
wav.emotion.hap
wav.emotion.sad
wav.emotion.fear
wav.emotion.sup

ind<-sample(2, length(wav.emotion), replace=T, prob=c(0.7, 0.3))
ind
train.data <- cbind(emotion=wav.emotion[ind==1], wav.mfcc.mean[ind==1,])
test.data  <- cbind(emotion=wav.emotion[ind==2], wav.mfcc.mean[ind==2,])
train.data
test.data
myFormula<-emotion~.
wav.naiveBayes <-naiveBayes(myFormula, data = train.data)
wav.naiveBayes.pred <- predict(wav.naiveBayes, test.data)
table(Predictions= wav.naiveBayes.pred)
