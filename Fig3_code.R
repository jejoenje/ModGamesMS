### Land distribution examples:
library(GMSE)

m1 = gmse(time_max = 1, land_ownership = T, stakeholders = 6, ownership_var = 0, plotting = FALSE)
m2 = gmse(time_max = 1, land_ownership = T, stakeholders = 6, ownership_var = 0.25, plotting = FALSE)
m3 = gmse(time_max = 1, land_ownership = T, stakeholders = 6, ownership_var = 0.5, plotting = FALSE)



l1 = m1$land[[1]][,,3]
l2 = m2$land[[1]][,,3]
l3 = m3$land[[1]][,,3]

jpeg("fig3.jpg", width = 1000, height = 350)

par(mfrow = c(1,3))
par(mar = c(2,2,2,2))
par(oma = c(0,0,2,0))
par(xpd = TRUE)
image(l1, xaxt = "n", yaxt = "n")
mtext("(a)",side = 3, cex=1.5, line = 2)
image(l2, xaxt = "n", yaxt = "n")
mtext("(b)",side = 3, cex=1.5, line = 2)
image(l3, xaxt = "n", yaxt = "n")
mtext("(c)",side = 3, cex=1.5, line = 2)

dev.off()

