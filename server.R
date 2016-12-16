library(shiny)
library(tuneR)
library(audio)
library(e1071)
library(kernlab)

# self
path <- "/Users/hyeonjin/workspace/r/emotion-recognizer/voice"
path.model.file = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.model.RData"
path.max.file   = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.max.RData"
path.min.file   = "/Users/hyeonjin/workspace/r/emotion-recognizer/model/emrec.min.RData"


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


if (file.exists(path.model.file)) {
 
  # load data if its file exists
  wav.ksvm <- readRDS(path.model.file)
  wav.feature.max <- readRDS(path.max.file)
  wav.feature.min <- readRDS(path.min.file)

} else {
 
  # set path of voice files
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
  wav.feature <- sapply(wav.mfcc, function(x) apply(x, 2, mean)) 
  wav.feature <- data.frame(mfcc=t(wav.feature))
  
  # add jitter
  wav.feature$jitter.ab <- sapply(wav.obj, jitter.absolute)
  wav.feature$jitter.re <- sapply(wav.obj, jitter.relative)

  # set max and min of feature before normalize
  wav.feature.max <- sapply(wav.feature, max)
  wav.feature.min <- sapply(wav.feature, min)
  saveRDS(wav.feature.max, path.max.file)
  saveRDS(wav.feature.min, path.min.file)
  
  # normalize
  wav.feature <- as.data.frame(lapply(wav.feature, normalize))
  
  # train
  train.data <- cbind(emotion=wav.emotion, wav.feature)
  f <- emotion ~ (mfcc.x1 + mfcc.x3 + mfcc.x4 + mfcc.x6 + jitter.ab)^2
  wav.ksvm <- ksvm(f, data = train.data, prob.model = T)
  saveRDS(wav.ksvm, path.model.file)
  
}  
 

# returns probabilities
recognize.emotion <- function (test.file.path) {
  
  ###test
  #test.file.path = "/Users/hyeonjin/workspace/r/emotion-recognizer/voice/wp_ang_08.wav"
  ###
  
  model = wav.ksvm
  
  test.wav.obj = mono(readWave(test.file.path), which = c("left", "right", "both"))
  
  # get mfcc data
  test.wav <- melfcc(test.wav.obj)
  test.wav <- apply(test.wav, 2, mean)
  test.wav.feature <- data.frame(mfcc=t(test.wav))
  
  # add jitter
  test.wav.feature$jitter.ab <- jitter.absolute(test.wav.obj)
  test.wav.feature$jitter.re <- jitter.relative(test.wav.obj)
  
  # normalize
  test.wav.feature <- ((test.wav.feature - wav.feature.min) / (wav.feature.max - wav.feature.min))
  
  test.wav.pred.prob <- predict(model, test.wav.feature, type = "p")
  #wav.pred <- predict(wav.ksvm, test.data)
  
  test.wav.pred.prob
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
    
    wav.pred.prob <- result()
    cls <- colnames(wav.pred.prob)[which(wav.pred.prob == max(wav.pred.prob))]
    if (cls == "sup") as.symbol("Surprised")
    else if (cls == "hap") as.symbol("Happy")
    else if (cls == "ang") as.symbol("Angry")
    else if (cls == "fear") as.symbol("Fear")
    else if (cls == "flat") as.symbol("Flat")
    else if (cls == "sad") as.symbol("Sad")
    else as.symbol(cls)
  })
  
  output$pie.result <- renderPlot({
    inFile <- input$file.wav
    
    if (is.null(inFile)) {
      return(pie(c(1)))
    }
    
    wav.pred.prob <- result()
    pie <- pie(wav.pred.prob, labels=colnames(wav.pred.prob))
    pie
  })
})
