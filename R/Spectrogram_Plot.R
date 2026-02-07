library(tuneR)
library(seewave)

setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

wavf <- readWave("Pine_TABR_20210609_231859_000_fixedSR.wav")
w <- mono(wavf, "left")


png("ImgFinal/Pine_call_1.png", width = 1500, height = 500)

spectro( w, f = w@samp.rate, wl = 512, ovlp = 75, flim = c(25, 40), tlim = c(1,2), osc = T,
         collevels = seq(-100, 0, 10), palette = temp.colors, noisereduction = 2)

dev.off()

png("ImgFinal/Pine_call_2.png", width = 500, height = 500)

spectro( w, f = w@samp.rate, wl = 512, ovlp = 75, flim = c(25, 40), tlim = c(1.19,1.22), osc = T,
         collevels = seq(-100, 0, 10), palette = temp.colors, noisereduction = 2)

dev.off()
