par(mar = c(5, 7, 2, 2))
plot(seq(15, 350), dgamma(seq(15, 350), shape = coef(mod_CSW1)[1], scale = coef(mod_CSW1)[2])*2460.993, 
     xlim = c(10, 350), cex.axis = 2, yaxt = "n", type = "l",
     lwd = 2, xlab = "Cabbage seedpod weevil degree days", cex.lab = 2, ylab = "", ylim = c(0, 35))

mtext("Number of weevils", side = 2, cex = 2, line = 4)
axis(side = 2, at = seq(0, 35, 5), cex.axis = 2, las = 2)

abline(h = 30, lty = 2, col = "grey30", lwd = 1.8)

polygon(c((which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49 + 100), 
          (which.min(SPLCanA) + 49 + 100)), c(0, 35, 35, 0), col = App1col, border = NA)

arrows((which.min(SPLCanA) + 49), 35, (which.min(SPLCanA) + 49), -1, 
       col = "gold4", lwd = 2, length = 0.1, xpd = NA)

library(sequential.pops)

