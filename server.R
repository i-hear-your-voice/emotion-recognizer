library(shiny)
library(tuneR)
library(audio)
library(e1071)
library(kernlab)

# functions

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

path.model.file = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.model.RData"
path.max.file   = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.max.RData"
path.min.file   = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.min.RData"

#train.waves <- function (train.data.path) {
if (!file.exists(path.model.file)) {
  
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
  wav.mfcc.mean <- data.frame(mfcc=t(wav.mfcc.mean))
  
  # add jitter
  wav.mfcc.mean$jitter.ab <- sapply(wav.obj, jitter.absolute)
  wav.mfcc.mean$jitter.re <- sapply(wav.obj, jitter.relative)
  
  wav.mfcc.mean.max <- sapply(wav.mfcc.mean, max)
  wav.mfcc.mean.min <- sapply(wav.mfcc.mean, min)
  saveRDS(wav.mfcc.mean.max, path.max.file)
  saveRDS(wav.mfcc.mean.min, path.min.file)
  
  # normalize
  wav.mfcc.mean <- as.data.frame(lapply(wav.mfcc.mean, normalize))
  
  # train
  train.data <- cbind(emotion=wav.emotion, wav.mfcc.mean)
  f <- emotion ~ .^2
  wav.ksvm <- ksvm(f, data = train.data, prob.model = T)
  saveRDS(wav.ksvm, path.model.file)
  
} else {
  wav.ksvm <- readRDS(path.model.file)
  wav.mfcc.mean.max <- readRDS(path.max.file)
  wav.mfcc.mean.min <- readRDS(path.min.file)
}
  

#if (!file.exists(path.model.file)) {
#  training <- train.waves("/Users/hyeonjin/workspace/r/emotion-recognizer/voice")
#  ihvy.model = training[1]
#  mfcc.max = training[2]
#  mfcc.min = training[3]
#  saveRDS(ihvy.model, path.model.file)
#} else {
#  ihvy.model = readRDS(path.model.file)
#}
  
  
# returns [class, pie]
recognize.emotion <- function (test.file.path) {
  
  ###test
  #test.file.path = "/Users/hyeonjin/workspace/r/emotion-recognizer/voice/wp_ang_08.wav"
  ###
  
  model = wav.ksvm
  
  test.wav.obj = mono(readWave(test.file.path), which = c("left", "right", "both"))
  
  # get mfcc data
  test.wav <- melfcc(test.wav.obj)
  test.wav <- apply(test.wav, 2, mean)
  test.wav.mean <- data.frame(mfcc=t(test.wav))
  
  # add jitter
  test.wav.mean$jitter.ab <- jitter.absolute(test.wav.obj)
  test.wav.mean$jitter.re <- jitter.relative(test.wav.obj)
  
  # normalize
  test.wav.mean <- ((test.wav.mean - wav.mfcc.mean.min) / (wav.mfcc.mean.max - wav.mfcc.mean.min))
  
  wav.ksvm.pred.prob <- predict(model, test.wav.mean, type = "p")
  #wav.ksvm.pred <- predict(wav.ksvm, test.data)
  
  wav.ksvm.pred.prob
}

#test
recognize.emotion("/Users/hyeonjin/workspace/r/emotion-recognizer/voice/wp_ang_08.wav")[2]

### Shiny Server
shinyServer(function(input, output) {
  
  result <- reactive({
    inFile <- input$file.wav
    recognize.emotion(inFile$datapath)
  })
  
  output$txt.result <- renderPrint({
    inFile <- input$file.wav
    
    if (is.null(inFile)) {
      return(as.symbol("Please select audio file"))
    }
    
    wav.ksvm.pred.prob <- result()
    cls <- colnames(wav.ksvm.pred.prob)[which(wav.ksvm.pred.prob == max(wav.ksvm.pred.prob))]
    cls
  })
  
  output$pie.result <- renderPlot({
    inFile <- input$file.wav
    
    if (is.null(inFile)) {
      return(pie(c(1)))
    }
    
    wav.ksvm.pred.prob <- result()
    pie <- pie(wav.ksvm.pred.prob, labels=colnames(wav.ksvm.pred.prob))
    pie
  })
})
