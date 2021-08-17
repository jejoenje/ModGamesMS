### Illustrative data analysis
run = read.csv("data/af_run.csv")
# Remove some weird extraneous cols from run:
run = run[,!grepl("X",names(run))]
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

### Extract only gdata for relevant runs:
gdata_run = gdata[gdata$id %in% run$id,
                  
### There may unfortunately be some series > maxT, because of the problem referenced here:
###  https://github.com/jejoenje/GMSEGAME/issues/41 
maxT = ddply(gdata_run, .(id), summarise, maxT = max(t))
maxT[maxT$maxT>25,]
### Therefore, for the purposes of this summary, just drop all gdata_run records t>25:
gdata_run = gdata_run[!(gdata_run$t>25),]

plot(gdata_run$t, gdata_run$res, type = "n")
tapply(gdata_run$res, gdata_run$id, function(x) lines(x))
