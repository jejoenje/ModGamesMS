run = read.csv("data/af_run.csv", header = T)
run = run[run$player!="jmTest",]
run = run[run$player!="JEROEN",]
run = run[run$player!="jm",]
run = run[run$player!="jmparatest",]
run = run[run$player!="jeroentest",]
run = run[run$player!="referralTest",]
run = run[run$player!="referralTest2",]

run$PLAYER = factor(run$player)
levels(run$PLAYER) = paste0("player",1:nlevels(run$PLAYER))
run$player = NULL
names(run)[names(run)=="PLAYER"] = "player"

write.csv(run, "data/af_run.csv")
  

