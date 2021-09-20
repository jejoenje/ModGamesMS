source("analysis.R")

par(mfrow =c(1,3))
XLAB = "Time step"
CEX.AXIS = 1.5
CEX.LAB = 1.5
CEX.MAIN = 1.5
CEX.MTEXT = 1.75
LCOL1 = "#525252"
LCOL2 = "red"   #d73027
LWD1 = 1.2
LWD2 = 1.5
par(oma= c(5,5,5,0))
par(mar= c(1,1,1,1))
otype = c(0,0.25,0.5)
for(i in 1:3) {
  plot(gdata_run$t, gdata_run$res, type = "n", ylab = "True population size", 
       xlab = XLAB, cex.axis = CEX.AXIS, cex.lab = CEX.LAB, cex.main = CEX.MAIN, yaxt = "n")
  mtext(bquote(o[v] ~ "=" ~ .(otype[i])), side=3, line =1.5, cex = CEX.MTEXT)
  if(i==1) { axis(2, cex.axis = CEX.AXIS) } else {axis(2, labels = FALSE)}
  tapply(gdata_run$res[gdata_run$ownership_var==otype[i] & gdata_run$extinct==0], 
         gdata_run$id[gdata_run$ownership_var==otype[i] & gdata_run$extinct==0],
         function(x) lines(x, col = LCOL1, lwd = LWD1))
  tapply(gdata_run$res[gdata_run$ownership_var==otype[i] & gdata_run$extinct==1], 
         gdata_run$id[gdata_run$ownership_var==otype[i] & gdata_run$extinct==1],
         function(x) lines(x, col = LCOL2, lwd = LWD2))
}
mtext("Time step", side = 1, outer = T, line = 2.5, cex = CEX.MTEXT)
mtext("True population size", side = 2, outer = T, line = 2.5, cex =CEX.MTEXT)