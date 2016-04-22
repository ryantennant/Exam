install.packages('deSolve')
library(deSolve)

log.growth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - (N / K)^theta)
    return(list(dN.dt))
  })
}

p.t <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
y0 <- c('N' = 0.01)

p.g <- c('r' = 0.28, 'K' = .75, 'theta' = 1.25)
p.p <- c('r' = 0.15, 'K' = 1.0, 'theta' = 1.0)

sim.t <- ode(y = y0, times = t, func = log.growth, parms = p.t, method = 'lsoda')
sim.g <- ode(y = y0, times = t, func = log.growth, parms = p.g, method = 'lsoda')
sim.p <- ode(y = y0, times = t, func = log.growth, parms = p.p, method = 'lsoda')

sim.t <- as.data.frame(sim.t)
sim.g <- as.data.frame(sim.g)
sim.p <- as.data.frame(sim.p)
?plot
sim.t$deriv <- c(diff(sim.t$N), NA)
plot(deriv ~ N, data = sim.t, type = 'l', col = 'red', xlim = c(0, 1), 
     ylim = c(0, .1), bty = 'l', xlab = 'Abundance', ylab = 'Pop. Growth Rate')

sim.g$deriv <- c(diff(sim.g$N), NA)
points(deriv ~ N, data = sim.g, type = 'l', col = 'purple')

sim.p$deriv <- c(diff(sim.p$N), NA)
points(deriv ~ N, data = sim.p, type = 'l', col = 'pink')
max.growths <- c(max(sim.t$deriv, na.rm = TRUE),
                 max(sim.g$deriv, na.rm = TRUE),
                 max(sim.p$deriv, na.rm = TRUE))

max.Ns <- c(sim.t$N[which(sim.t$deriv == max(sim.t$deriv, na.rm = TRUE))],
            sim.g$N[which(sim.g$deriv == max(sim.g$deriv, na.rm = TRUE))],
            sim.p$N[which(sim.p$deriv == max(sim.p$deriv, na.rm = TRUE))])


