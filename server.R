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

train.waves <- function (train.data.path) {
  
  
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
  
  # train
  train.data <- cbind(emotion=wav.emotion, wav.mfcc.mean)
  f <- emotion ~ .^2
  wav.ksvm <- ksvm(f, data = train.data, prob.model = T)
  
  wav.ksvm
}

# returns [class, pie]
recognize.emotion <- function (model, test.file.path) {
  test.file.path = "/Users/hyeonjin/workspace/r/emotion-recognizer/voice/wp_flat_14.wav"
  test.wav.obj = mono(readWave(test.file.path), which = c("left", "right", "both"))
  
  # get mfcc data
  test.wav <- melfcc(test.wav.obj)
  test.wav <- apply(test.wav, 2, mean)
  test.wav.mean <- data.frame(mfcc=t(test.wav))
  
  # add jitter
  test.wav.mean$jitter.ab <- jitter.absolute(test.wav.obj)
  test.wav.mean$jitter.re <- jitter.relative(test.wav.obj)
  
  test.file.path = "/Users/hyeonjin/workspace/r/emotion-recognizer/voice/wp_flat_14.wav"
  test.wav.obj = mono(readWave(test.file.path), which = c("left", "right", "both"))
  
  # get mfcc data
  test.wav <- melfcc(test.wav.obj)
  test.wav <- apply(test.wav, 2, mean)
  test.wav.mean <- data.frame(mfcc=t(test.wav))
  
  # add jitter
  test.wav.mean$jitter.ab <- jitter.absolute(test.wav.obj)
  test.wav.mean$jitter.re <- jitter.relative(test.wav.obj)
  
  
  wav.ksvm.pred.prob <- predict(model, test.wav.mean, type = "p")
  #wav.ksvm.pred <- predict(wav.ksvm, test.data)
  wav.ksvm.pred <- colnames(wav.ksvm.pred.prob)[which(wav.ksvm.pred.prob == max(wav.ksvm.pred.prob))]
  #sum(as.vector(wav.ksvm.pred) == test.data[[1]]) / nrow(test.data)
  pie(wav.ksvm.pred.prob, labels=colnames(wav.ksvm.pred.prob))
  #  c(levels(test.data[,1]), wav.ksvm.pred)
  
  wav.ksvm.pred.prob <- predict(model, test.wav.mean, type = "p")
  #wav.ksvm.pred <- predict(wav.ksvm, test.data)
  cls <- wav.ksvm.pred <- colnames(wav.ksvm.pred.prob)[which(wav.ksvm.pred.prob == max(wav.ksvm.pred.prob))]
  #sum(as.vector(wav.ksvm.pred) == test.data[[1]]) / nrow(test.data)
  pie <- pie(wav.ksvm.pred.prob, labels=colnames(wav.ksvm.pred.prob))
#  c(levels(test.data[,1]), wav.ksvm.pred)
  c(cls, pie)
}

### Shiny Server

shinyServer(function(input, output) {
  
  if (!exists("ihvy.model")) {
    ihvy.model = train.waves("/Users/hyeonjin/workspace/r/emotion-recognizer/voice")
  }
  
  result <- reactive({
    
  })
  
  output$txt.result <- renderPrint({
    
    inFile <- input$file.wav
    
    if (is.null(inFile)) {
      return(as.symbol("Please select audio file"))
    }
    
    return(recognize.emotion(ihvy.model, inFile$datapath))
  })
  
  output$pie.result <- renderPlot({
    pie(c(1,2,3))
#    hist(faithful$eruptions,
#         probability = TRUE,
#         breaks = as.numeric(input$n_breaks),
#         xlab = "Duration (minutes)",
#         main = "Geyser eruption duration")
  
#    if (input$individual_obs) {
#      rug(faithful$eruptions)
#    }
    
#    if (input$density) {
#      dens <- density(faithful$eruptions,
#                      adjust = input$bw_adjust)
#      lines(dens, col = "blue")
#    }
    
  })
})

