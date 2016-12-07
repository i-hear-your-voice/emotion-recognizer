library(tuneR)
library(audio)
library(base)

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

## wav.filename[1]
## wav.emotion[1] 
## wav.mfcc.mean[, 1]
