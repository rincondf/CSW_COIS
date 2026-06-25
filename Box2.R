# Overlap between CSW and canola

library(sequential.pops)

###RUN OVERLAP FIRST

set.seed(6789)

ref_traj <- function(t) {
  dgamma(t, shape = coef(mod_CSW1)[1], 
         scale = coef(mod_CSW1)[2])*2460.993
}




ref_counts <- ref_traj(c(80, 85, 100, 117, 141, 155))

pop80 <- ref_counts * 0.8
count80 <- matrix(NA, 4, 6)

for(i in 1:6){
  count80[, i] <- rnbinom(4, mu = pop80[i], size = 20)
}



pop40 <- ref_counts * 0.4
count40 <- matrix(NA, 4, 6)

for(i in 1:6){
  count40[, i] <- rnbinom(4, mu = pop40[i], size = 20)
}




pop110 <- ref_counts * 1.1
count110 <- matrix(NA, 4, 6)

for(i in 1:6){
  count110[, i] <- rnbinom(4, mu = pop110[i], size = 20)
}


test_dyn80 <- stbp_composite(data = count80,
                             greater_than = TRUE,
                             hypothesis = ref_counts,
                             density_func = "negative binomial",
                             overdispersion = 15,
                             prior = 0.5,
                             lower_bnd = 0,
                             upper_bnd = Inf,
                             lower_criterion = 0,
                             upper_criterion = 1)

test_dyn40 <- stbp_composite(data = count40,
                             greater_than = TRUE,
                             hypothesis = ref_counts,
                             density_func = "negative binomial",
                             overdispersion = 15,
                             prior = 0.5,
                             lower_bnd = 0,
                             upper_bnd = Inf,
                             lower_criterion = 0,
                             upper_criterion = 1)

test_dyn110 <- stbp_composite(data = count110,
                              greater_than = TRUE,
                              hypothesis = ref_counts,
                              density_func = "negative binomial",
                              overdispersion = 15,
                              prior = 0.5,
                              lower_bnd = 0,
                              upper_bnd = Inf,
                              lower_criterion = 0,
                              upper_criterion = 1)


#########

par(mfrow = c(2, 1))

par(mar = c(3, 7, 3, 2))
plot(seq(15, 350), ref_traj(seq(15, 350)), 
     xlim = c(10, 350), cex.axis = 2, yaxt = "n", type = "l",
     lwd = 2, xlab = "", cex.lab = 2, ylab = "", ylim = c(0, 35),
     col ="brown")

mtext("Number of weevils", side = 2, cex = 2, line = 4)
axis(side = 2, at = seq(0, 40, 5), cex.axis = 2, las = 2)

abline(h = 30, lty = 2, col = "grey30", lwd = 1.8)

polygon(c((which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49 + 100), 
          (which.min(SPLCanA) + 49 + 100)), c(0, 35, 35, 0), col = App1col, border = NA)

arrows((which.min(SPLCanA) + 49), 35, (which.min(SPLCanA) + 49), -1, 
       col = "gold4", lwd = 2, length = 0.1, xpd = NA)


points(c(80, 85, 100, 117, 141, 155), ref_counts, lwd = 2, col = "brown")
points(c(80, 85, 100, 117, 141, 155), colMeans(count80), lwd = 2, type = "o")
points(c(80, 85, 100, 117, 141, 155), colMeans(count40), lwd = 2, type = "o", lty = 2)
points(c(80, 85, 100, 117, 141, 155), colMeans(count110), lwd = 2, type = "o", lty = 3)

mtext("A", side = 3, cex = 2, at = -45, line = 1)

par(mar = c(5, 7, 1, 2))
plot(c(78, 80, 85, 100, 117, 141, 155), test_dyn80@probabilities, lwd = 2, type = "o", ylim = c(0, 1), 
     cex.axis = 2, yaxt = "n", xaxt = "n", xlim = c(75, 170),
     xlab = "Cabbage seedpod weevil degree days", cex.lab = 2, ylab = "")

mtext("Probability of N > EIL", side = 2, cex = 2, line = 4.3)
axis(side = 2, at = seq(0, 1, 0.2), cex.axis = 2, las = 2)
axis(side = 1, at = seq(80, 175, 10), labels = seq(80, 175, 10), cex.axis = 2)

points(c(78, 80, 85, 100, 117, 141, 155), test_dyn40@probabilities, lwd = 2, lty = 2, type = "o")
points(c(78, 80, 85, 100, 117, 141, 155), test_dyn110@probabilities, lwd = 2, lty = 3, type = "o")

points(78, 0.5, lwd = 2, pch = 17, cex = 2)

text(160, test_dyn40@probabilities[7] + 0.02, paste(round(test_dyn40@probabilities[7], digits = 1)),
     cex = 1.8)

text(162, test_dyn80@probabilities[7], paste(round(test_dyn80@probabilities[7], digits = 2)),
     cex = 1.8)

text(162, test_dyn110@probabilities[7], paste(round(test_dyn110@probabilities[7], digits = 2)),
     cex = 1.8)

arrows((which.min(SPLCanA) + 49), 1, (which.min(SPLCanA) + 49), -0.01, 
       col = "gold4", lwd = 2, length = 0.1, xpd = NA)

mtext("B", side = 3, cex = 2, at = 60, line = 1)
