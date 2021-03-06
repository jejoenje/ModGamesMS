rm(list=ls())
library(GMSE)
library(parallel)
library(doParallel)
library(foreach)
library(scales)

### Paras for A&F as of Sept 2021:

GMSE_PARAS = expand.grid(
  K = 5,                                #
  LAND_OWNERSHIP = TRUE,                #
  STAKEHOLDERS  = c(4:12),              #
  MANAGER_BUDGET  = 1000,               #
  MANAGE_TARGET  = 2000,                #
  OBSERVE_TYPE  = 0,                    #
  RES_MOVE_OBS  = TRUE,                 
  RES_DEATH_K  = 5000,                  #
  LAMBDA = 0.3,                         #
  RES_DEATH_TYPE  = 3,                  #
  REMOVE_PR = runif(1, 0.05, 0.2),      #
  USER_BUDGET = 1500,                   #
  CULLING = TRUE,                       #
  SCARING = TRUE,                       #
  TEND_CROPS = TRUE,                    #
  TEND_CROP_YLD = 0.3,                  #
  LAND_DIM_1 = 100,                     #
  LAND_DIM_2 = 100,                     #
  RESOURCE_INI = 1000,                  #
  TIME_MAX = 20,                        #
  PUBLIC_LAND = 0,                      #
  OWNERSHIP_VAR = c(0,0.25,0.5),        #
  USR_BUDGET_RNG = 0                    #
)

# SIMS = 100
# OUT = list()
# 
# 
# for(k in 1:nrow(GMSE_PARAS)) {
#   #cat(sprintf("\n"), file = "foreach_log.txt", append = TRUE)
#   cat(sprintf("\n%s - parameter set %d / %s.. \n\n", Sys.time(), k, nrow(GMSE_PARAS)), file = "sim_dat/foreach_log.txt", append = TRUE)
#   cat(sprintf("\n%s - parameter set %d / %s..", Sys.time(), k, nrow(GMSE_PARAS)))
#   
#   cl = makeCluster(6,outfile="foreach_log.txt")
#   registerDoParallel(cl = cl)
#  
#   results <- foreach(i=1:SIMS, .export=c('gmse'), .packages=c('GMSE')) %dopar% {
#     gmse(time_max = GMSE_PARAS[k,"K"] + GMSE_PARAS[k,"TIME_MAX"], 
#          land_dim_1 = GMSE_PARAS[k,"LAND_DIM_1"],
#          land_dim_2 = GMSE_PARAS[k,"LAND_DIM_2"],
#          land_ownership = GMSE_PARAS[k,"LAND_OWNERSHIP"],
#          ownership_var = GMSE_PARAS[k,"OWNERSHIP_VAR"],
#          public_land = GMSE_PARAS[k,"PUBLIC_LAND"],
#          stakeholders = GMSE_PARAS[k,"STAKEHOLDERS"],
#          user_budget = GMSE_PARAS[k,"USER_BUDGET"],
#          usr_budget_rng = GMSE_PARAS[k,"USR_BUDGET_RNG"],
#          manager_budget = GMSE_PARAS[k,"MANAGER_BUDGET"],
#          manage_target = GMSE_PARAS[k,"MANAGE_TARGET"],
#          culling = GMSE_PARAS[k,"CULLING"],
#          scaring = GMSE_PARAS[k,"SCARING"],
#          tend_crops = GMSE_PARAS[k,"TEND_CROPS"],
#          tend_crop_yld = GMSE_PARAS[k,"TEND_CROP_YLD"],
#          RESOURCE_ini = GMSE_PARAS[k,"RESOURCE_INI"],
#          lambda = GMSE_PARAS[k,"LAMBDA"],
#          res_death_K = GMSE_PARAS[k,"RES_DEATH_K"],
#          res_death_type = GMSE_PARAS[k,"RES_DEATH_TYPE"],
#          remove_pr = GMSE_PARAS[k,"REMOVE_PR"],
#          observe_type = GMSE_PARAS[k,"OBSERVE_TYPE"],
#          res_move_obs = GMSE_PARAS[k,"RES_MOVE_OBS"],
#          plotting = FALSE
#     )
#   }
#   
#   res_summary = list(data = lapply(results, gmse_summary))
#   
#   ### Get and save parameters with output list:
#   pars = as.list(as.matrix(GMSE_PARAS[k,]))
#   names(pars) = names(GMSE_PARAS)
#   res_summary$paras = pars
#   
#   rm(results)
#   
#   OUT[[k]] = res_summary
#   
#   gc()
#   
#   stopCluster(cl)
# }
# 
# saveRDS(OUT, "sim_dat/OUT.Rds")

source("helpers.R")

OUT = readRDS("sim_dat/OUT.Rds")

### Reconstitute parameters 
paras = get_set_pars(OUT)
paras$ext_prop = NA
paras$mean_r = NA
for(i in 1:nrow(paras)) {
  which_dat = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == paras[i,"OWNERSHIP_VAR"] & x$paras$STAKEHOLDERS == paras[i,"STAKEHOLDERS"] ))
  dat = OUT[which_dat][[1]]
  
  ### Prop of sims extinct:
  # Number of years for each sim in para comb:
  yrs_i = unlist(lapply(dat$data, function(x) nrow(x$resources)))
  ext_prop_i = sum(yrs_i != 25)/length(yrs_i)
  paras$ext_prop[i] = ext_prop_i
  
  ### Mean pop growth rate:
  n0_i = as.vector(unlist(lapply(dat$data, function(x) x$resources[6,2])))
  nT_i = unlist(lapply(dat$data, function(x) tail(x$resources[,2],1)))
  r_i = (nT_i-n0_i)/n0_i
  paras$mean_r[i] =  mean(r_i) 
  
}

### Plot trajectories per OWNERSHIP_VAR:


###### THIS CURRENTLY PLOTS ACROSS ALL STAKEHOLDER NUMBERS:
ov0 = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0 & x$paras$STAKEHOLDERS == 12 ))
ovM = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0.25 & x$paras$STAKEHOLDERS == 12 ))
ovH = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0.5 & x$paras$STAKEHOLDERS == 12 ))

###### WOULD BE NICE TO DO ACROSS ALL STAKEHOLDER NO'S THOUGH...:
# ov0 = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0 ))
# ovM = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0.25 ))
# ovH = unlist(lapply(OUT, function(x) x$paras$OWNERSHIP_VAR == 0.5 ))
##### BUT THIS NEEDS ADAPTING THE FOLLOWING TO LOOP ACROSS NESTED LISTS:

ov0_dat = OUT[ov0][[1]]$data
ovM_dat = OUT[ovM][[1]]$data
ovH_dat = OUT[ovH][[1]]$data
ylo1 = min(unlist(lapply(ov0_dat, function(x) min(x$resources[,2]))))
ylo2 = min(unlist(lapply(ovM_dat, function(x) min(x$resources[,2]))))
ylo3 = min(unlist(lapply(ovH_dat, function(x) min(x$resources[,2]))))
yhi1 = max(unlist(lapply(ov0_dat, function(x) max(x$resources[,2]))))
yhi2 = max(unlist(lapply(ovM_dat, function(x) max(x$resources[,2]))))
yhi3 = max(unlist(lapply(ovH_dat, function(x) max(x$resources[,2]))))
ylo = floor(min(c(ylo1,ylo2,ylo3))*0.95)
yhi = ceiling(max(c(yhi1,yhi2,yhi3))*1.05)
par(mfrow=c(1,3))
plot(1:25,1:25,type="n",ylim=c(ylo,yhi), xlab = "Time step", ylab = "True population size", cex.lab = 1.5, cex.axis = 1.5)
mtext(bquote(o[v] ~ "=" ~ .(0)), side=3, line =1.5, cex = 1.75)
lapply(ov0_dat, function(x) lines(x$resources[,2], col = alpha("black",0.5),lwd = 1.2))
plot(1:25,1:25,type="n",ylim=c(ylo,yhi), xlab = "Time step", ylab = "True population size", cex.lab = 1.5, cex.axis = 1.5)
mtext(bquote(o[v] ~ "=" ~ .(0.25)), side=3, line =1.5, cex = 1.75)
lapply(ovM_dat, function(x) lines(x$resources[,2], col = alpha("black",0.5), lwd = 1.2))
plot(1:25,1:25,type="n",ylim=c(ylo,yhi), xlab = "Time step", ylab = "True population size", cex.lab = 1.5, cex.axis = 1.5)
mtext(bquote(o[v] ~ "=" ~ .(0.5)), side=3, line =1.5, cex = 1.75)
lapply(ovH_dat, function(x) lines(x$resources[,2], col = alpha("black",0.5), lwd = 1.2))
