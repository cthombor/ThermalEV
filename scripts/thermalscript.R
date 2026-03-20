# hacks and fiddles

# experimenting with Sys.timezone() in xts(), maybe a good way to mute timezone
# warning messages in xts?  Deep waters here, because the Win32 runtime doesn't
# support the *nix-style client-server distinction... so a remote R server's
# behaviour might be surprising with respect to its default TZ for xts...
# Sys.setenv(TZ = Sys.timezone()) # avoid warnings?
# options(xts_check_TZ = FALSE) # disable warnings?

# cheesy avoidance of lm() in plot_gids:
pd <- pd |> arrange(soc)
if (is.null(slope_start))
  slope_start = 0
i1 <- which.min(pd$soc >= slope_start)
x1 <- pd$gids_scaled[i1]
y1 <- pd$soc[i1]
i2 <- which.max(pd$soc)
x2 <- pd$gids_scaled[i2]
y2 <- pd$soc[i2]
soc_slope <- (y2 - y1) / (x2 - x1)
soc_intercept <- y2 - x2 * soc_slope
print(paste("SOC/scaled_gid =", round(soc_slope, 4),
            "; SOC intercept =", round(soc_intercept, 2)))


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
