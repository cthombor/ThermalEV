# q for Hadley

install.packages("reprex")
library(reprex)

library(tidyverse)
library(zoo)
library(xts)

tb <- tibble(sample_value = runif(100),
             sample_time = make_date() + seq(1:100))

tb[40, 1] <- NA
w <- which(is.na(tb$sample_value)) # location(s) of missing sample(s)
wend <- c(w, length(tb) + 1) # append a 0-length "ghost segment"

EMA_parameter <- 0.25

tbts <- as.xts(tb)
for (i in seq(length(w))) {
  # apply filter to all non-ghost segments
  tbts[wend[i]:(wend[i + 1] - 1)] <-
    stats::filter(tbts[wend[i]:(wend[i + 1] - 1)] * EMA_parameter,
                  1. - EMA_parameter,
                  method = "recursive",
                  init = 0.)
}
plot(tbts)
