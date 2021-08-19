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

### Play duration summaries:
mean_duration = round(as.numeric(mean(difftime(run$endTime, run$startTime, units = "mins"))),1)
min_duration = round(as.numeric(min(difftime(run$endTime, run$startTime, units = "mins"))),1)
max_duration = round(as.numeric(max(difftime(run$endTime, run$startTime, units = "mins"))),1)

### Number of sessions played:
n_sessions = nrow(run); n_sessions

### Number of unique player-names in this sample:
n_players = nlevels(factor(run$player)); n_players

### Number/percentages of sessions with each ownership_var:
# 0
as.numeric(names(table(run$ownership_var)[1])) 
n_ov0 = as.numeric(table(run$ownership_var)[1]); n_ov0
p_ov0 = round(as.numeric(table(run$ownership_var)[1])/n_sessions,2); p_ov0
# 0.25
as.numeric(names(table(run$ownership_var)[2])) 
n_ov025 = as.numeric(table(run$ownership_var)[2]); n_ov025
p_ov025 = round(as.numeric(table(run$ownership_var)[2])/n_sessions,2); p_ov025
# 0.5
as.numeric(names(table(run$ownership_var)[3])) 
n_ov050 = as.numeric(table(run$ownership_var)[3]); n_ov050
p_ov050 = round(as.numeric(table(run$ownership_var)[3])/n_sessions,2); p_ov050

### Numbers of runs extinct per ownership_var:
extinct_ov = table(run$extinct, run$ownership_var); extinct_ov
### Proportions:
extinct_ov_p = prop.table(extinct_ov,2)
# Reverse row order so extinction is bottom one in plot:
extinct_ov_p = as.matrix(rbind(extinct_ov_p[2,],extinct_ov_p[1,]))


