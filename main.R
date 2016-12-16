library(tuneR)
library(audio)
library(e1071)
library(kernlab)

normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}

jitter.absolute <- function (wave) {
  jitter=0
  for (idx in 1:(length(wave@left)-1)) {
    jitter = jitter + abs(wave@left[idx]-wave@left[idx+1])
  }
  jitter = jitter/(length(wave@left)-1)
  jitter
}

jitter.relative <- function (wave) {
  jitter.ab <- jitter.absolute(wave)
  
  denominator = 0
  for (idx in 1:(length(wave@left))) {
    denominator = denominator + abs(wave@left[idx])
  }
  denominator = denominator/(length(wave@left))
  
  jitter.ab / denominator
}


# set path of voice files
path <- "/Users/hyeonjin/workspace/r/emotion-recognizer/voice"
#path <- "/Users/hyeonjin/Downloads/rml/all_lang"
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
wav.mfcc.mean <- data.frame(mfcc=t(wav.mfcc.mean))

# add jitter
wav.mfcc.mean$jitter.ab <- sapply(wav.obj, jitter.absolute)
wav.mfcc.mean$jitter.re <- sapply(wav.obj, jitter.relative)

# normalize
wav.mfcc.mean <- as.data.frame(lapply(wav.mfcc.mean, normalize))

tat <- function (idx) {
  # train and test
  #idx<-sample(length(wav.emotion), 0.8 * length(wav.emotion), prob=NULL)
  train.data <- cbind(emotion=wav.emotion[-idx], wav.mfcc.mean[-idx,])
  test.data  <- cbind(emotion=wav.emotion[idx],  wav.mfcc.mean[idx,])
  f <- emotion ~ .^2
  wav.ksvm <- ksvm(f, data = train.data, prob.model = T)
  wav.ksvm.pred.prob <- predict(wav.ksvm, test.data, type = "p")
  #wav.ksvm.pred <- predict(wav.ksvm, test.data)
  wav.ksvm.pred <- colnames(wav.ksvm.pred.prob)[which(wav.ksvm.pred.prob == max(wav.ksvm.pred.prob))]
  #sum(as.vector(wav.ksvm.pred) == test.data[[1]]) / nrow(test.data)
  pie(wav.ksvm.pred.prob, labels=colnames(wav.ksvm.pred.prob))
  c(levels(test.data[,1]), wav.ksvm.pred)
}

tat(  19  )

?ksvm
?predict
?pie
# k.s.v.m (f, data=train.data)

#sum(diag(table(wav.ksvm.pred, test.data[[1]]))) / sum(table(wav.ksvm.pred, test.data[[1]]))

