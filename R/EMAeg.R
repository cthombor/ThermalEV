# q for Hadley: is there an efficient way to do recursive filtering in the
# tidyverse?  I suspect not, as it'd be clumsy and still not be very general.
# Columns must be converted to vectors, and then you'd do an unvectorisable
# per-element tail-recursion within each group. Easiest to do this by lurching
# into xts... unless the recursive filter is outside the scope of what xts can
# handle i.e. a time-series of dim-1 samples, not a timeseries on samples from
# multiple channels, and not a time series with irregular or missing samples.

# Maybe it's best to do all signal processing in Matlab, and to push its results
# into the tidyverse only if you want to do some non-trivial statistical
# analysis?  Too late for me now!  I'm pretty sure the limitations of recursive
# filtering in xts won't severely cripple my modelling.

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
