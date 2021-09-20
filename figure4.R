source("analysis.R")

#jpeg("fig1.jpeg",width = 800, height = 400)
extinct_ov = table(run$extinct, run$ownership_var)
extinct_ov_p = prop.table(extinct_ov, 2)
extinct_ov_p = as.matrix(rbind(extinct_ov_p[2,],extinct_ov_p[1,]))

run_stk = table(run$extinct, run$stakeholders)
extinct_s = cbind(rowSums(matrix(run_stk,2,9)[,1:3]), rowSums(matrix(run_stk,2,9)[,4:6]), rowSums(matrix(run_stk,2,9)[,7:9]))
extinct_s_p = prop.table(extinct_s, 2)
extinct_s_p = as.matrix(rbind(extinct_s_p[2,],extinct_s_p[1,]))

par(mfrow=c(1,2))
par(oma = c(5,5,1,1))
par(mar = c(1,1,1,1))
bp = barplot(extinct_ov_p[1,], col = c("#d73027"), space = 0.1, cex.names = 1.5, cex.lab = 1.5, cex.axis = 1.5, ylim = c(0,0.5),
             xlab = "Land ownership variability",
             ylab = "Proportion extinct")
text(bp[1],y = 0.05, paste("n =",as.numeric(table(run$ownership_var)[1])), cex = 1.5)
text(bp[2],y = 0.05, paste("n =",as.numeric(table(run$ownership_var)[2])), cex = 1.5)
text(bp[3],y = 0.05, paste("n =",as.numeric(table(run$ownership_var)[3])), cex = 1.5)
text("(a)",x=bp[1],y=0.475,cex=1.5)
mtext("Land ownership variability", 1, line = 3, cex = 1.5)

bp2 = barplot(extinct_s_p[1,], col = c("#d73027"), space = 0.1, cex.names = 1.5, cex.lab = 1.5, cex.axis = 1.5, ylim = c(0,0.5), yaxt="n",
              names = c("4-6","7-9","10-12"),
              xlab = "Number of farmers",
              ylab = "Proportion extinct")
axis(2,labels=FALSE)
text(bp2[1],y = 0.05, paste("n =",sum(as.matrix(table(run$stakeholders))[1:3])), cex = 1.5)
text(bp2[2],y = 0.05, paste("n =",sum(as.matrix(table(run$stakeholders))[4:6])), cex = 1.5)
text(bp2[3],y = 0.05, paste("n =",sum(as.matrix(table(run$stakeholders))[7:9])), cex = 1.5)
text("(b)",x=bp2[1],y=0.475,cex=1.5)
mtext("No. of farmers", 1, line = 3, cex = 1.5)
mtext("Proportion extinct", 2, line = 2.5, cex = 1.5, outer = T)
#dev.off()