library(plyr)

### Illustrative data analysis
run = read.csv("data/af_run.csv")
# Remove some weird extraneous cols from run:
run = run[,!grepl("X",names(run))]

### Clear up dates:
run$startTime = strptime(run$startTime,"%Y-%m-%d %H:%M:%S")
run$endTime = strptime(run$endTime,"%Y-%m-%d %H:%M:%S")


gdata = read.csv("data/af_run_gdata.csv")
scores = read.csv("data/af_run_scores.csv")
run_par = read.csv("data/af_run_par.csv")

run$player = factor(run$player)

### Restrict data to trial period only:
start_time = strptime("2021-07-16 23:59:59","%Y-%m-%d %H:%M:%S")
run = run[strptime(run$startTime,"%Y-%m-%d %H:%M:%S")>start_time,]
run = droplevels(run)

### Total number of sessions, no check of "validity":
total_all = nrow(run); total_all
nlevels(run$player)

### Exclude runs where there are no steps (i.e. "invalid", that is session cancelled because of impromper name entered, or browser closed straight away):
run$steps = scores$steps[match(run$id, scores$id)]
run = run[!is.na(run$steps),]
run = droplevels(run)

### Exclude runs that are only six time steps, as this implies no actions taken:
run = run[run$steps>6,]

### Total number of sessions with at least some player interaction:
nrow(run)
total_valid = nrow(run); total_valid
### Of this many unique player names:
total_valid_unames = nlevels(run$player); total_valid_unames

### Match some data:
#### scores
#### pop start
#### pop end
#### ownership_var
#### remove_pr

### Scores:
run$score_a = scores$mean_res[match(run$id, scores$id)]
run$score_y = scores$mean_yield[match(run$id, scores$id)]

### Initial population size (n0, at t = 5):
gdata5 = gdata[gdata$t == 5,]
run$n0 = gdata5$res[match(run$id, gdata5$id)]

### End population size (nT):
maxT = ddply(gdata, .(id), summarise, maxT = max(t))
gdata$maxT = maxT$maxT[match(gdata$id, maxT$id)]
nT = ddply(gdata, .(id), summarise, nT = res[t==maxT])
#gdata$nT = nT$nT[match(gdata$id, nT$id)]
run$nT = nT$nT[match(run$id, nT$id)]

### Derived population trend, and trend/time step:
run$r = (run$nT-run$n0)/run$n0
run$rt = run$r/run$steps

### Varying parameters:
run$ownership_var = run_par$ownership_var[match(run$id, run_par$id)]
run$remove_pr = run_par$remove_pr[match(run$id, run_par$id)]
run$stakeholders = run_par$stakeholders[match(run$id, run_par$id)]

### Extract only gdata for relevant runs:
gdata_run = gdata[gdata$id %in% run$id,]
                  
### There may unfortunately be some series > maxT, because of the problem referenced here:
###  https://github.com/jejoenje/GMSEGAME/issues/41 
maxT = ddply(gdata_run, .(id), summarise, maxT = max(t))
maxT[maxT$maxT>25,]
### Therefore, for the purposes of this summary, just drop all gdata_run records t>25:
gdata_run = gdata_run[!(gdata_run$t>25),]

### Match ownership_var to gdata_run:
gdata_run$ownership_var = run$ownership_var[match(gdata_run$id, run$id)]
gdata_run$extinct = run$extinct[match(gdata_run$id, run$id)]

######
### Summary stats:

### Start and end dates of data:
start_date = format(min(run$startTime),"%d %B %Y"); start_date
end_date = format(max(run$startTime),"%d %B %Y"); end_date

### Number of sessions played:
n_sessions = nrow(run); n_sessions

### Number of unique player-names in this sample:
n_players = nlevels(factor(run$player)); n_players

### Number/percentages of sessions with each ownership_var:
# 0
as.numeric(names(table(run$ownership_var)[1])) 
as.numeric(table(run$ownership_var)[1])
round(as.numeric(table(run$ownership_var)[1])/n_sessions,2)
# 0.25
as.numeric(names(table(run$ownership_var)[2])) 
as.numeric(table(run$ownership_var)[2])
round(as.numeric(table(run$ownership_var)[2])/n_sessions,2)
# 0.5
as.numeric(names(table(run$ownership_var)[3])) 
as.numeric(table(run$ownership_var)[3])
round(as.numeric(table(run$ownership_var)[3])/n_sessions,2)

### Numbers of runs extinct per ownership_var:
extinct_ov = table(run$extinct, run$ownership_var); extinct_ov
### Proportions:
extinct_ov_p = prop.table(extinct_ov,2)

# Reverse row order so extinction is bottom one in plot:
extinct_ov_p = as.matrix(rbind(extinct_ov_p[2,],extinct_ov_p[1,]))
# bp = barplot(extinct_ov_p, col = c("#d73027", "#e0f3f8"), space = 0.1, cex.names = 1.25, cex.lab = 1.25, cex.axis = 1.25,
#         xlab = "Land ownership variability",
#         ylab = "Proportion extinct/not extinct")
bp = barplot(extinct_ov_p[1,], col = c("#d73027"), space = 0.1, cex.names = 1.25, cex.lab = 1.25, cex.axis = 1.25, ylim = c(0,0.6),
        xlab = "Land ownership variability",
        ylab = "Proportion extinct")
text(bp[1],y = 0.55, paste("n =",as.numeric(table(run$ownership_var)[1])), cex = 1.25)
text(bp[2],y = 0.55, paste("n =",as.numeric(table(run$ownership_var)[2])), cex = 1.25)
text(bp[3],y = 0.55, paste("n =",as.numeric(table(run$ownership_var)[3])), cex = 1.25)

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
plot(gdata_run$t, gdata_run$res, type = "n", ylab = "True population size", xlab = XLAB, cex.axis = CEX.AXIS, cex.lab = CEX.LAB, cex.main = CEX.MAIN)
mtext("ownership_var = 0", side=3, line =1.5, cex = CEX.MTEXT)
tapply(gdata_run$res[gdata_run$ownership_var==0 & gdata_run$extinct==0], gdata_run$id[gdata_run$ownership_var==0 & gdata_run$extinct==0], 
       function(x) lines(x, col = LCOL1, lwd = LWD1))
tapply(gdata_run$res[gdata_run$ownership_var==0 & gdata_run$extinct==1], gdata_run$id[gdata_run$ownership_var==0 & gdata_run$extinct==1], 
       function(x) lines(x, col = LCOL2, lwd = LWD2))
par(mar= c(1,1,1,1))
plot(gdata_run$t, gdata_run$res, type = "n", yaxt="n", ylab = "", xlab = XLAB, cex.axis = CEX.AXIS, cex.lab = CEX.LAB)
axis(side = 2, labels = FALSE)
mtext("ownership_var = 0.25", side=3, line =1.5, cex = CEX.MTEXT)
tapply(gdata_run$res[gdata_run$ownership_var==0.25 & gdata_run$extinct==0], gdata_run$id[gdata_run$ownership_var==0.25 & gdata_run$extinct==0], 
       function(x) lines(x, col = LCOL1, lwd = LWD1))
tapply(gdata_run$res[gdata_run$ownership_var==0.25 & gdata_run$extinct==1], gdata_run$id[gdata_run$ownership_var==0.25 & gdata_run$extinct==1], 
       function(x) lines(x, col = LCOL2, lwd = LWD2))
par(mar= c(1,1,1,1))
plot(gdata_run$t, gdata_run$res, type = "n", yaxt = "n", ylab = "", xlab = XLAB, cex.axis = CEX.AXIS, cex.lab = CEX.LAB)
axis(side = 2, labels = FALSE)
mtext("ownership_var = 0.5", side=3, line =1.5, cex = CEX.MTEXT)
tapply(gdata_run$res[gdata_run$ownership_var==0.5 & gdata_run$extinct==0], gdata_run$id[gdata_run$ownership_var==0.5 & gdata_run$extinct==0], 
       function(x) lines(x, col = LCOL1, lwd = LWD1))
tapply(gdata_run$res[gdata_run$ownership_var==0.5 & gdata_run$extinct==1], gdata_run$id[gdata_run$ownership_var==0.5 & gdata_run$extinct==1], 
       function(x) lines(x, col = LCOL2, lwd = LWD2))
mtext("Time step", side = 1, outer = T, line = 2.5, cex = CEX.MTEXT)
mtext("True population size", side = 2, outer = T, line = 2.5, cex =CEX.MTEXT)

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

