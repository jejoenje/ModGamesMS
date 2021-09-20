source("analysis.R")

### Calculate summary statistic for given cost type over time steps:
cost_summary = function(dat, ov, cost, type) {
  
  if(!(cost %in% names(dat))) stop("'cost' specified not present in 'dat'.")
  if(!(type %in% c("mean","median","q25","q75","q025","q975"))) stop("invalid 'type' specified .")
  
  d = dat[(dat$ownership_var==ov & dat$t>5),c("t",cost)]
  d[,cost] = (d[,cost]-10)*10
  d$t = factor(d$t)
  
  if(type == "mean") return(tapply(d[,cost],d$t,function(x) mean(x, na.rm=T)))
  if(type == "median") return(tapply(d[,cost],d$t,function(x) median(x, na.rm=T)))
  if(type == "q25") return(tapply(d[,cost],d$t,function(x) quantile(x, probs = 0.25, na.rm=T)))
  if(type == "q75") return(tapply(d[,cost],d$t,function(x) quantile(x, probs = 0.75, na.rm=T)))
  if(type == "q025") return(tapply(d[,cost],d$t,function(x) quantile(x, probs = 0.025, na.rm=T)))
  if(type == "q975") return(tapply(d[,cost],d$t,function(x) quantile(x, probs = 0.975, na.rm=T)))
  
}

par(mfrow =c(2,3))
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

ptype = c(rep("cull_cost",3), rep("scare_cost",3))
otype = rep(c(0,0.25,0.5),2)
aytype = rep(c(TRUE,FALSE,FALSE),2)
axtype = c(rep(FALSE,3),rep(TRUE,3))
polycols = rbind(cbind(rep("#e6550d",3),rep("#fdae6b",3)),cbind(rep("#a6bddb",3),rep("#2b8cbe",3)))

for(i in 1:6) {
  plot(c(1,1), xlim = c(1,25), ylim = c(0,1000), 
       type = "n", ylab = "", xlab = XLAB, cex.axis = CEX.AXIS, cex.lab = CEX.LAB, cex.main = CEX.MAIN, yaxt = "n", xaxt = "n")
  if(aytype[i]) { axis(2, labels = TRUE, cex.axis = CEX.AXIS) } else { axis(2, labels = FALSE) }
  if(axtype[i]) { axis(1, labels = TRUE, cex.axis = CEX.AXIS) } else { axis(1, labels = FALSE) }
  if(i==1) mtext("Hunting cost", side = 2, line = 3, cex = CEX.MTEXT)
  if(i==4) mtext("Scaring cost", side = 2, line = 3, cex = CEX.MTEXT)
  
  if(i<4) { mtext(bquote(o[v] ~ "=" ~ .(otype[i])), side=3, line =1.5, cex = CEX.MTEXT) }
  polygon(c(rev(6:24),6:24), 
          c(rev(cost_summary(gdata_run, ov=otype[i], cost=ptype[i], type="q025")[1:19]),
            cost_summary(gdata_run, ov=otype[i], cost=ptype[i],type="q975")[1:19]), col = polycols[i,1], border = NA)
  polygon(c(rev(6:24),6:24), 
          c(rev(cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q25")[1:19]),
            cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q75")[1:19]), col = polycols[i,2], border = NA)
  
  lines(6:24, cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="median")[1:19], lwd = 2)
  lines(6:24, cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q25")[1:19], lwd = 1)
  lines(6:24, cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q75")[1:19], lwd = 1)
  lines(6:24, cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q025")[1:19], lwd = 0.75)
  lines(6:24, cost_summary(gdata_run, ov = otype[i], cost=ptype[i],type="q975")[1:19], lwd = 0.75)
  
}
mtext("Time step", side = 1, outer = T, line = 2.5, cex = CEX.MTEXT)