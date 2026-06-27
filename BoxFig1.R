### BOX FIGURE 1

par(mfrow = c(2, 2))

# A

par(mar = c(5, 6, 6, 3))

plot(seq(200, 700), pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                           scale = coef(mod_pods)[2]), xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(200, 700), ylim = c(0, 1), yaxt = "n", xaxt = "n")


mtext("A", side = 3, cex = 2, at = 165, line = 3)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")

axis(side = 2, at = seq(0, 1, 0.2), cex.axis = 2, las = 1)

polygon(c(seq(200, 700), 700), c(pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                                        scale = coef(mod_pods)[2]), 0), col = Cancol, border = NA)

title(ylab = "Proportion", cex.lab = 2, line = 4)

par(new = TRUE)
plot(seq(50, 400), dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                          scale = coef(mod_CSW1)[2])*50, 
     ylab = "", cex.lab = 2, type = "l", xlab = "",
     cex.axis = 2, lwd  = 2, ylim = c(0, 1), xlim = c(50, 400), yaxt = "n",
     xaxt = "n")

axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
polygon(seq(50, 400), dgamma(seq(50, 400), shape = coef(mod_CSW1)[1], 
                             scale = coef(mod_CSW1)[2])*50, col = CSWcol, border = NA)
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

####

polygon(c((which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49), (which.min(SPLCanA) + 49 + 100), 
          (which.min(SPLCanA) + 49 + 100)), c(0, 1, 1, 0), col = App1col, border = NA)


arrows((which.min(SPLCanA) + 49), 1.04, (which.min(SPLCanA) + 49), -0.04, 
       col = "gold4", lwd = 2, code = 3, length = 0.1, xpd = NA)






# C

par(mar = c(5, 6, 6, 6))

plot(seq(200, 700), pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                           scale = coef(mod_pods)[2]), xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(200, 700), ylim = c(0, 1), yaxt = "n", xaxt = "n")


mtext("C", side = 3, cex = 2, at = 165, line = 3)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")

axis(side = 4, at = seq(0, 1, 0.2), cex.axis = 2, las = 1, col.axis = "darkgreen")

polygon(c(seq(200, 700), 700), c(pgamma(seq(200, 700), shape = coef(mod_pods)[1], 
                                        scale = coef(mod_pods)[2]), 0), col = Cancol, border = NA)

mtext("Proportion", side = 4, cex = 2, line = 4.5, col = "darkgreen")


par(new = TRUE)

plot(seq(50, 398), sam_canola(th = 30)[[1]], 
     ylab = "", cex.lab = 2, type = "l", xlab = "",
     cex.axis = 2, lwd  = 2, ylim = c(0, 100), xlim = c(50, 400), yaxt = "n",
     xaxt = "n")

axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
axis(side = 2, at = seq(0, 100, 20), cex.axis = 2, col.axis = "brown")
polygon(seq(50, 398), sam_canola(th = 30)[[1]], col = CSWcol, border = NA)

abline(h = 30, lty = 3, lwd = 2, col = "grey40")

mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")
mtext("Population density", side = 2, cex = 2, line = 3.5, col = "brown")

arrows(78+50, 47, 78+50, 39, lwd = 2, length = 0.1, col = "gold4")
arrows(130+50, 90, 130+50, 82, lwd = 2, length = 0.1, col = "gold4")











# B

par(mar = c(5, 7, 7, 4))
plot(f, SPLCanA, xlab = "", 
     ylab = "", cex.lab = 2, type = "l",
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 40), yaxt = "n",
     xaxt = "n")

mtext("B", side = 3, cex = 2, at = 15, line = 3)

axis(side = 2, at = seq(0, 40, 10), cex.axis = 2, las = 1)
title(ylab = "Accumulated yield loss (%)", cex.lab = 2, line = 4.5)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

abline(h = SPLCanA[which.min(SPLCanA)], lty = 2)

abline(v = which.min(SPLCanA) + 49, lty = 2)











# D


par(mar = c(5, 7, 7, 4))
plot(seq(50, 399), sam_canola(th  = 30)[[2]], xlab = "", 
     ylab = "", cex.lab = 2, type = "l",
     cex.axis = 2, lwd  = 2, xlim = c(50, 400), ylim = c(0, 25), yaxt = "n",
     xaxt = "n")

mtext("D", side = 3, cex = 2, at = 15, line = 3)

polygon(c(seq(50, 399), 399), c(sam_canola(th  = 30)[[2]], 0), col = App2col, border = NA)

axis(side = 2, at = seq(0, 30, 5), cex.axis = 2, las = 1)
title(ylab = "Cumulative yield loss (%)", cex.lab = 2, line = 4.5)

mtext("Canola growing degree days", side = 1, cex = 2, line = 3, col = "darkgreen")
axis(side = 1, at = seq(50, 400, 70), labels = seq(200, 700, 100), cex.axis = 2, col.axis = "darkgreen")


axis(side = 3, at = seq(50, 400, 50), cex.axis = 2, col.axis = "brown")
mtext("Cabbage seedpod weevil degree days", side = 3, cex = 2, line = 3, col = "brown")

abline(h = sam_canola(th  = 30)[[2]][350], lty = 2)


arrows(78+50, 6, 78+50, 3, lwd = 2, length = 0.1, col = "gold4")
arrows(130+50, 7, 130+50, 4, lwd = 2, length = 0.1, col = "gold4")