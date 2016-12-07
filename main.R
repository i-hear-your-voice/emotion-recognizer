library(tuneR)
library(audio)
library(base)

path <- "/Users/hyeonjin/workspace/r/voice/speech_1206"

setwd(path)
wav.filename <- list.files()
wav.split <- as.list(sapply(wav.filename, function(x) strsplit(x, "_") ))
wav.emotion <- as.vector(sapply(wav.split, function(x) x[2]))
wav.filename
wav.emotion

# wav.obj: mono wave files
wav.obj <- sapply(wav.filename, function(x) mono(readWave(x), which = c("left", "right", "both")))
wav.mfcc <- sapply(wav.obj, function(x) melfcc(x))
wav.mfcc.mean <- sapply(wav.mfcc, function(x) apply(x, 2, mean)) 

wav.filename[1] # 1번째 오디오파일의 이름
wav.emotion[1] # 1번째 오디오파일의 감정
wav.mfcc.mean[, 1] # 1번째 오디오파일의 MFCC 1~12차수 (평균)
