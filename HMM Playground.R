# Cyclic HMM

library(depmixS4)
library(tidyverse)



# Generate sample data with a periodic pattern
set.seed(123)
period = 24
days = 4
sine <- rep(sin(seq(0,  2*pi, length.out = period)), days)
plot(sine, type = 'l')
lomb::lsp(sine, type ='period')$peak.at

bi <- rep(sin(seq(0, 2*pi, length.out = period)) - cos(seq(0, 2*pi, length.out = period/2)), days)
plot(bi, type = 'l')
l = lomb::lsp(bi, type ='period')
pracma::findpeaks(l$power, sortstr = TRUE)
a = acf(bi, lag.max = 50)
pracma::findpeaks(as.vector(a$acf), sortstr = TRUE)

#test IND
test_ind = circadiandynamics::read_trikinetics_2()
test_ind = circadiandynamics::downsample_time_series_2(test_ind)
test_ind = test_ind$`IND 32`
lomb::lsp(test_ind$value, type ='period')$peak.at




random = rnorm(period*days)
plot(random, type = 'l')
lomb::lsp(random, type ='period')$peak.at

df = data.frame(sine, bi, random, lag = 1:length(sine))


#Awake, Asleep
states_2 = c(0.9, 0.1,
             0.1, 0.9)


# Asleep, Increasing activity, Peak, Decreasing activity
states_4 = c(0.9, 0.1, 0, 0,
            0, 0.9, 0.1, 0,
            0, 0, 0.9, 0.1,
            0.1, 0, 0, 0.9)


# Asleep, Increasing activity, Peak, Decreasing activity
states_8 = c(0.9, 0.1, 0, 0, 0, 0, 0, 0,
             0, 0.9, 0.1, 0, 0, 0, 0, 0,
             0, 0, 0.9, 0.1, 0, 0, 0, 0,
             0, 0, 0, 0.9, 0.1, 0, 0, 0,
             0, 0, 0, 0, 0.9, 0.1, 0, 0,
             0, 0, 0, 0, 0, 0.9, 0.1, 0,
             0, 0, 0, 0, 0, 0, 0.9, 0.1,
             0.1, 0, 0, 0, 0, 0, 0, 0.9)



hmm = depmix(bi~rep(1:24, 4), data= df, family = gaussian(), nstates = 2, trstart = states_2)


summary(hmm, 'transition')
model = fit(hmm)

summary(model, 'transition')

AIC(model)
BIC(model)

df$states = viterbi(model)$state
lomb::lsp(df$states, type ='period')$peak.at

lmtest::grangertest(df$bi, df$states, 2)



ggplot(data = df, aes(x = lag, y = bi, color = factor(states))) +
  geom_vline(xintercept = c(seq(0, 24*days, by = 24)), lty = 2) +
  geom_point() +
  geom_line(aes(group = 'None'))


# Identify periodic patterns
periods <- c()
states = df$states
for (i in 2:length(states)) {
  if (states[i] != states[i-1] & states[i] == 1) {
    periods <- c(periods, i-1)
  }
}
median(diff(periods))

# Plot data and periodic pattern
plot(sine, type = "l")
abline(v = seq(0, n, by = 24), lty = 2, col = "red")


#Identify distance between peaks of different states
peaks = pracma::findpeaks(sine, nups = 1, ndowns = 0)[,2]
s = df$states[peaks]
peak_peds = c()
for (i in 2:length(peaks)) {
  if (s[i] != s[i-1]) {
    print(peaks[i])
    peak_peds <- c(peak_peds, peaks[i])
  } else {
    peak_peds <- c(peak_peds, peaks[i])
  }
}
diff(peak_peds)
median(diff(peak_peds))

#average phase by state and peak
v = seq(0, 24*days - 1, by = 24)
peak_phase = c()
for (i in unique(s)) {
  poi = which(s == i)
  peak_phase = c(peak_phase, mean(peaks[poi] - v))
}
peak_phase

