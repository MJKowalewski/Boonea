#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#
# COMPARATIVE ANALYSES OF SIZE-FREQUENCY DISTRIBUTION OF BOONEA IMPRESSA
#
# Anayses for Cannarozzi, N. and Kowalewski, M. (revised, Sep 2019, PLoS ONE)
#
# written by Michal Kowalewski, University of Florida, kowalewski@ufl.edu
#
# Last updated: September 23, 2019
#
# Script tested and executed in R version 3.6.1, R Studio 1.2.1335
#
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------

#------ ENTER DATA ------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))								# remove all preexisting objects (optional)
source(file='mydist3.R')							# custom function (required)
recent <- read.csv("2017 Boonea.csv")					# enter recent data
fossil <- read.csv("2017 BooneaArch.csv")					# enter fossil data
archl <- c('789N801E', 'W83S2', '784N811E')				# labels of archeological samples
recentC <- recent[which(recent[,2]>=1.6),]				# correct recent data (1.6mm mesh size)
months <- c(7:12,1:6)								# month sequence for plotting
month.l <- c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
             'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')		# month labels

# --- pdf defintions
fontFamily <- 'Helvetica'
pdf.options(paper="special", onefile=TRUE, family=fontFamily,
            pointsize=10, encoding="ISOLatin1.enc", height=5, width=10)
outPDF <- T # set to TRUE to output pdf files with figures

#--- SFDs DESCRIPTIVE ANALYSIS ------------------------------------------------------------------------

#--------------------------------------------
############# FIG S2 - SFDs
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_S2_', my.time.stamp, '.pdf', sep=''), height=10, width=7)
op <- par(mfcol=c(12,3),mar=c(0,0,0,0), omi=c(0.6,0.6,0.3,0.3))
for (j in 1:3) {
 k <- 0
 for (i in months) {
  if (j == 1) {
   k <- k + 1
   if (i>=7) {a <- which(recent[,4]==2006 & recent[,3]==i); yr=2006}
   if (i<7) {a <- which(recent[,4]==2007 & recent[,3]==i); yr=2007}
   hist(recent[a,2], xlab='', ylab='', axes=F, breaks=seq(0,7,0.1), 
        border=NA, col='black', main='')
    abline(v=median(recent[,2]), lwd=2, col='skyblue')
    points(median(recent[a,2]), 0, cex=2, pch=17, col='red1')
    mtext(side=3, line=-2, adj=0.98, paste(month.l[k],yr), col='black', cex=0.65)
    mtext(side=3, line=-3, adj=0.98, paste('n =',length(recent[a,2])), col='red4', cex=0.65)
    if (i==6) {axis(1); mtext(side=1, line=3, 'shell height [mm]', cex=0.9)}
    if (i==12) mtext(side=2, line=2, 'number of specimens', cex=0.9)
    if (i==7) mtext(side=3, line=0.7, 'Sampling Year 1 (2006-2007)', cex=0.75)
    box()
  }
  if (j == 2) {
   k <- k + 1
   if (i>=7) {a <- which(recent[,4]==2007 & recent[,3]==i); yr <- 2007}
   if (i<7) {a <- which(recent[,4]==2008 & recent[,3]==i); yr <- 2008}
   hist(recent[a,2], xlab='', ylab='', axes=F, breaks=seq(0,7,0.1), 
        border=NA, col='black', main='')
    abline(v=median(recent[,2]), lwd=2, col='skyblue')
    points(median(recent[a,2]), 0, cex=2, pch=17, col='red1')
    mtext(side=3, line=-2, adj=0.98, paste(month.l[k], yr), col='black', cex=0.65)
    mtext(side=3, line=-3, adj=0.98, paste('n =',length(recent[a,2])), col='red4', cex=0.65)
    if (i==6) {axis(1); mtext(side=1, line=3, 'shell height [mm]', cex=0.9)}
    if (i==7) mtext(side=3, line=0.7, 'Sampling Year 2 (2007-2008)', cex=0.75)
    box()
    }
  if (j==3) {
   k <- k + 1
   a <- which(recent[,3]==i)
   hist(recent[a,2], xlab='', ylab='', axes=F, breaks=seq(0,7,0.1),
        border=NA, col='black', main='')
    abline(v=median(recent[,2]), lwd=2, col='skyblue')
    points(median(recent[a,2]), 0, cex=2, pch=17, col='red1')
    mtext(side=3, line=-2, adj=0.98, month.l[k], col='black', cex=0.65)
    mtext(side=3, line=-3, adj=0.98, paste('n =',length(recent[a,2])), col='red4', cex=0.65)
    if (i==6) {axis(1); mtext(side=1, line=3, 'shell height [mm]', cex=0.9)}
    if (i==7) mtext(side=3, line=0.7, 'Pooled Data (2006-2008)', cex=0.75)
    box()
  }
 }
}
par(op)
if(outPDF) dev.off()

#--------------------------------------------
############# FIG. S3 -- consistency of monthly data across years
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_S3_', my.time.stamp, '.pdf', sep=''), height=5, width=7)
sfd1 <- tapply(recent[recent[,4]==2006,2], recent[recent[,4]==2006,3], median)
n1 <- tapply(recent[recent[,4]==2006,2], recent[recent[,4]==2006,3], length)
sfd2 <- tapply(recent[recent[,4]==2007,2], recent[recent[,4]==2007,3], median)
n2 <- tapply(recent[recent[,4]==2007,2], recent[recent[,4]==2007,3], length)
sfd3 <- tapply(recent[recent[,4]==2008,2], recent[recent[,4]==2008,3], median)
n3 <- tapply(recent[recent[,4]==2008,2], recent[recent[,4]==2008,3], length)
plot(1:12, sfd2[months], cex=0.2*n2[months]^(1/2), ylim=c(0,7), 
     xlab='month', ylab='median shell height [mm]', axes=F)
 axis(2, las=1)
 axis(1, at=1:12, labels=month.l)
 points(1:12, sfd3[months], col='blue', cex=0.2*n3[months]^(1/2))
 points(1:6, sfd1, col='orange', cex=0.2*n1^(1/2))
 legend('topleft', pch=21, col=c('orange', 'black', 'blue'), c('2006', '2007', '2008'))
 box()
 if(outPDF) dev.off()
 
#--------------------------------------------
############# FIG. 2 -- SFDs (pooled R-SFDs and A-SFDs)
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_2_', my.time.stamp, '.pdf', sep=''), height=10, width=7)
op <- par(mfcol=c(15,1),mar=c(0,0,0,0), omi=c(0.6,0.6,0.3,0.3))
k <- 0
  for (i in months) {
   k <- k + 1
   a <- which(recent[,3]==i)
   hist(recent[a,2], xlab='', ylab='', axes=F, breaks=seq(0,7,0.1), 
        border=NA, col='black', main='')
    abline(v=median(recent[,2]), lwd=2, col='skyblue')
    abline(v=median(fossil[,2]), lwd=2, col='orange')
    points(median(recent[a,2]), 0, cex=2, pch=17, col='red1')
    mtext(side=3, line=-1.5, adj=0.98, month.l[k], col='black', cex=0.65)
    mtext(side=3, line=-2.5, adj=0.98, paste('n =',length(recent[a,2])), col='red4', cex=0.65)
    if (i==2) mtext(side=2, line=2, 'number of specimens', cex=0.9)
    box()
  }
 for (j in 1:3) {
  x1 <- fossil[fossil[,1]==j,2]
  hist(x1, xlab='', ylab='', axes=F, breaks=seq(0,7,0.1), 
       border=NA, col='gray', main='')
  abline(v=median(recent[,2]), lwd=2, col='skyblue')
  abline(v=median(fossil[,2]), lwd=2, col='orange')
  points(median(x1), 0, cex=2, pch=17, col='red1')
  mtext(side=3, line=-1.5, adj=0.98, archl[j], col='black', cex=0.65)
  mtext(side=3, line=-2.5, adj=0.98, paste('n =',length(x1)), col='red4', cex=0.65)
  if (j==3) {axis(1); mtext(side=1, line=3, 'shell height [mm]', cex=0.9)}
  box()
  }
par(op)
if(outPDF) dev.off()

#--------------------------------------------
############# Dispersion/central tendency statisics are not diagnostic (not included in the paper)
desc.F <- function(x) c(ST.DEV=sd(x), IQR=quantile(x,prob=0.75)-quantile(x,prob=0.25), 
                         MEAN=mean(x), MEDIAN=median(x), n=length(x))
recE <- NULL
for (i in 1:12) {
 recE <- rbind(recE, desc.F(recent[which(recent[,3]==i),2]))
 }
fossE <- NULL
for (i in 1:3) {
 fossE <- rbind(fossE, desc.F(recent[which(fossil[,1]==i),2]))
 }
descR <- rbind(recE,fossE)
rownames(descR) <- c(1:12, archl)
descR

#--------------------------------------------
############# summary of small size classes in modern samples
rbind(minimum=min(recent[,2]), 
       'percent < 0.5 mm'=round(100*length(which(recent[,2] < 0.5))/length(recent[,2]),3),
      'percent < 1 mm'=round(100*length(which(recent[,2] < 1))/length(recent[,2]),3),
      'percent < 1.6 mm'=round(100*length(which(recent[,2] < 1.6))/length(recent[,2]),3))

#--------------------------------------------
############# summary of small size classes in archeological samples
rbind(minimum=min(fossil[,2]), 
      'percent < 2.5 mm'=round(100*length(which(fossil[,2] < 2.5))/length(fossil[,2]),3),
      'percent < 3 mm'=round(100*length(which(fossil[,2] < 3))/length(fossil[,2]),3),
      'percent < 3.5 mm'=round(100*length(which(fossil[,2] < 3.5))/length(fossil[,2]),3))

#--------------------------------------------
############# FIG. S4 -- ogives (pooled R-SFDs and A-SFDs)
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_S4_', my.time.stamp, '.pdf', sep=''), height=5, width=7)
my.cols <- c('yellow3', 'orange', 'red1', 'red3', 'brown', 'skyblue', 'blue2', 
             'darkblue', 'green1', 'green2', 'green3', 'yellow1')  
bins <- seq(0, 7, 0.1) 
k <- 0
plot(bins, rep(0,length(bins)), main="", type='n', ylim=c(0,1), xlim=range(bins),
     xlab='shell height [mm]', ylab='cumulative relative frequency', las=1)
for (i in months) {
 k <- k + 1
 x1 <- recent[recent[,3]==i,2]
 x2 <- table(cut(x1, bins, right=FALSE)) 
 x3 <- c(0, cumsum(x2))/sum(x2)
 points(bins, x3, type='l', col=my.cols[k], lwd=2)
}
for (j in 1:3) {
 x1 <- fossil[fossil[,1]==j,2]
 x2 <- table(cut(x1, bins, right=FALSE)) 
 x3 <- c(0, cumsum(x2))/sum(x2)
 points(bins, x3, type='l', col='gray', lwd=4)
}
legend('bottomright', col=c(my.cols,'gray'), lwd=c(rep(2,12),4), c(month.l, 'A-SFD'), cex=0.8)
if (outPDF) dev.off()

#-----------------------------------------------
############# Figure S4B -- ogives (pooled R-SFDs trimmed and A-SFDs)
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_S4B_', my.time.stamp, '.pdf', sep=''), height=5, width=7)
my.cols <- c('yellow3', 'orange', 'red1', 'red3', 'brown', 'skyblue', 'blue2', 
             'darkblue', 'green1', 'green2', 'green3', 'yellow1')  
bins <- seq(0, 7, 0.1) 
k <- 0
plot(bins, rep(0,length(bins)), main="", type='n', ylim=c(0,1), xlim=range(bins),
     xlab='shell height [mm]', ylab='cumulative relative frequency', las=1)
for (i in months) {
 k <- k + 1
 x1 <- recentC[recentC[,3]==i,2]
 x2 <- table(cut(x1, bins, right=FALSE)) 
 x3 <- c(0, cumsum(x2))/sum(x2)
 points(bins, x3, type='l', col=my.cols[k], lwd=2)
}
for (j in 1:3) {
 x1 <- fossil[fossil[,1]==j,2]
 x2 <- table(cut(x1, bins, right=FALSE)) 
 x3 <- c(0, cumsum(x2))/sum(x2)
 points(bins, x3, type='l', col='gray', lwd=4)
}
legend('bottomright', col=c(my.cols,'gray'), lwd=c(rep(2,12),4), c(month.l, 'A-SFD'), cex=0.8)
if (outPDF) dev.off()



#------ MODELS AND SIMULATIONS ------------------------------------------------------------------------
#------ PARAMETER VALUES ------------------------------------------------------------------------------
# NOTE: 100 iterations takes 10-20 minutes on a fast laptop (use simulation outputs generated previously)
# If you insist on generating your own models, set RUN.M <- TRUE below

choose <- 1									# select the fossil sample to be evaluated
times <- 2000								# number of iterations for null model
TRIM <- FALSE								# trim modern data (remove <1.6mm)
RUN.M <- FALSE								# set to TRUE to run simulations


#------ DATA, PARAMETER PROCESSING, AND FUNCTIONS ------------------------------------------------------
 fsam <- fossil[which(fossil[,1]==choose),2]			# selects a fossil sample		
 obsn <- length(fsam)							# finds n (sample size) for the selected fossil sample
(info <- rbind('n'=obsn, 'iterations'=times, 'sample'=choose)) 	# prints simulation summary
 size <- recent[,2]							# vector of size variable (modern data)
 month <- recent[,3]							# vector of month variable (modern data)
if(TRIM) size <- recentC[,2]						# vector of size variable (modern data trimmed)
if(TRIM) month <- recentC[,3]						# vectcor of month variable (modern data trimmed)
bfun <- function(x) {sample(size[which(month %in% x)], obsn, replace=T)}
bfun2 <- function(x,y) D <- D.STAT.F(x,y) 						 # using custom bare-bone function
D.STAT.F <- function(q1,q2) {
   q4 <- c(q1, q2)
   max(abs(cumsum(ifelse(order(q4) <= length(q1), 1/length(q1), -1/length(q2)))))
}

#------ List of all possible month combinations (144 models)-------------------------------------------
m <- vector('list',144)
for (i in 1:144) {
  d <- ceiling(i/12)
  if (i%%12==0) {
   if (d==1) ms <- i
   if (d>1)  ms <- c(12, 1:(d-1))
   }
  if (i%%12!=0) {
   if (i%%12 + d < 13) ms <- (i%%12):(i%%12+d-1)
   if (i%%12 + d == 13) ms <- (i%%12):12
   if (i%%12 + d > 13) ms <- c((i%%12):12,1:(i%%12+d-13))
   }
   m[[i]] <- ms
}
m <- m[-(134:144)]	# remove last 11 models (redundant) (m contains 133 unique models)
                        # NOTE: m is a list of all unique consecutive month combinations

#------ SIMULATION 1 (ACTUAL SAMPLE) ----------------------------------------------------------------
if(RUN.M) {
asm <- NULL
for (n in 1:times) {
    models <- sapply(m,bfun)
    out1 <- sapply(data.frame(models), bfun2, y=sample(fsam,replace=T))
    asm <- rbind(asm, cbind(n, which(out1==min(out1))))
   print(n)
  }
 arch.sam1 <- data.frame(asm)
 write.csv(arch.sam1, '???????.csv')
}

#------ SIMULATION 1 (NULL MODELS) -------------------------------------------------------------------
if(RUN.M) {
tm <- NULL
for (i in 1:133) {
  for (n in 1:times) {
    models <- sapply(m,bfun)
    out1 <- sapply(data.frame(models), bfun2, y=bfun(unlist(m[i])))
    tm <- rbind(tm, cbind(i, which(out1==min(out1))))
  }
print(paste(i, 'out of 133 month sets completed'))
}
 null.mT.3 <- cbind(tm[,2],0,tm[,1])
 write.csv(null.mT.3, '??????.csv')
}

#---------- SIMULATION OUTPUT ANALYSES -----------------------------------------------------------------
A1 <- read.csv('archsample1.csv')[,-1]	# simulation data for archeological sample 1 (all recent data)
A2 <- read.csv('archsample2.csv')[,-1]	# simulation data for archeological sample 2 (all recent data)
A3 <- read.csv('archsample3.csv')[,-1]	# simulation data for archeological sample 3 (all recent data)
A1T <- read.csv('archsample1T.csv')[,-1]	# simulation data for archeological sample 1 (trim recent data)
A2T <- read.csv('archsample2T.csv')[,-1]	# simulation data for archeological sample 2 (trim recent data)
A3T <- read.csv('archsample3T.csv')[,-1]	# simulation data for archeological sample 3 (trim recent data)
NM1 <- read.csv('nullmodel1.csv')[,-1]	# simulation data for 133 models (first model run, sample 1)
NM2 <- read.csv('nullmodel2.csv')[,-1]	# simulation data for 133 models (first model run, sample 2)
NM3 <- read.csv('nullmodel3.csv')[,-1]	# simulation data for 133 models (first model run, sample 3)
null01 <- read.csv('2017 model1.csv')[-1] # simulation data for 133 models (second model run, sample 1)
null02 <- read.csv('2017 model2.csv')[-1] # simulation data for 133 models (second model run, sample 2)
null03 <- read.csv('2017 model3.csv')[-1] # simulation data for 133 models (second model run, sample 3)
null01T <- read.csv('2017 model1 trim.csv')[-1] # simulation data for 133 models (trimmed, sample 1)
null02T <- read.csv('2017 model2 trim.csv')[-1] # simulation data for 133 models (trimmed, sample 2)
null03T <- read.csv('2017 model3 trim.csv')[-1] # simulation data for 133 models (trimmed, sample 3)

#--- choose model sets for the subsequent analysis
MOD.CH <- 1		# choose: 1 - first model run, 2 - second model run, 3 - trimmed model run

 if (MOD.CH==1) {
  as <- list(A1, A2, A3)				# set of archeological sample simulations (raw data)
  null1 <-  list(NM1, NM2, NM3)			# set of null simulations (raw data, first run)
  }
 if (MOD.CH==2) {
  as <- list(A1, A2, A3)				# set of archeological sample simulations (raw data)
  null1 <-  list(null01, null02, null03)		# set of null simulations (raw data, first run)
  }
 if (MOD.CH==3) {
   as <- list(A1T, A2T, A3T)				# set of archeological sample simulations (trimmed data)
   null1 <-  list(null01T, null02T, null03T)	# set of trimmed model simulations selected for the final analysis
  }

#--------------------------------------------------------------------------------------
# Figure S5 -- visualization of null models using one example (models for n of sample 1)
if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_S5_', my.time.stamp, '.pdf', sep=''), height=5, width=7)
archsample <- 3	# choose sample
outMON <- NULL
for (mn in 1:133) {
 tMON<- sort(table(factor(null1[[archsample]][null1[[archsample]][,3]==mn,1],1:133)),decreasing=T)
 tMONp <- tMON[1:133]/sum(tMON)
 if (length(which(tMON==max(tMON)))==1) {
  if (names(tMON)[1]==mn) outMON <- rbind(outMON, c(1,1,mn,tMONp[1:133],names(tMON)[1:133]))
  if (names(tMON)[1]!=mn) outMON <- rbind(outMON, c(which(names(tMON)==mn),1,mn,tMONp[1:133],names(tMON)[1:133]))
 }
 if (length(which(tMON==max(tMON)))>1) outMON <- rbind(outMON, c(which(names(tMON)==mn),0,mn,tMONp[1:133],names(tMON)[1:133]))
}
topD <- as.numeric(outMON[,1])
all_mod <- sapply(data.frame(outMON[,137:269]), function(x) as.numeric(as.character(x)))
mycol.F <- colorRampPalette(c("yellow3", "forestgreen")) # this function creates a function
mycol.F2 <- colorRampPalette(c("red3", "orange"))	# this function creates a function
mycols <- c(mycol.F2(67), mycol.F(66)) # apply this new function to define number of colors
plot(1:133,rep(0.5,133), type='n', ylim=c(0,1), xlim=c(1,133), las=1,
     xlab='actual month sets', ylab='proportion of matched month sets')
 for (k in 1:133) {
  kkk <- cumsum(as.numeric(outMON[k,4:136]))
  mycols2 <- mycols[all_mod[k,]]
  mycols2[topD[k]] <- 'black'
  for (L in 1:133) {
  if (L==1) lines(c(k,k), c(0,kkk[L]), lwd=4, col=mycols2[L], lend=2)
  if (L>1) lines(c(k,k), c(kkk[L-1],kkk[L]), lwd=4, col=mycols2[L], lend=2)
}
}
text(seq(2,132,2), 1.025, rep(c('F','A','J','A','O','D'),11), cex=0.6,
     col=mycols[seq(2,132,2)], xpd=T)
if (outPDF) dev.off()

#--------------------------------------------------------
# Figure 4 (Fig. S7, S8) -- Evaluation of all null models 
sim.m <- 'Bray'		# choose: 'Sorenson' or 'Jaccard' or 'Bray'
outk2 <- NULL
for (i in 1:3) {
 t1<- table(factor(as[[i]][,2],1:133))
 outk<- NULL
 for (kk in 1:133) {
 if (length(null1[[i]][null1[[i]][,3]==kk,1])>0) {
 t2<- table(factor(null1[[i]][null1[[i]][,3]==kk,1],1:133))
 t1r<- t1/sum(t1)			# use relative values
 t2r<- t2/sum(t2)			# use relative values
 BC<- 1-sum(abs(t1r-t2r))/(sum(t2r)+sum(t2r))	# Bray-Curtis Similarity
 JAC <- mydist(t1,t2)[13]				# Jaccard
 SOR <- mydist(t1,t2)[14]				# Sorenson
 if (sim.m=='Bray') SM <- BC
 if (sim.m=='Jaccard') SM <- JAC
 if (sim.m=='Sorenson') SM <- SOR
 outk<- rbind(outk,cbind(SM,kk)) # choose measure of similarity
  }
 }
 outk2[[i]] <- outk
}

maxFit <- NULL; modelB <- NULL; monthB <- NULL; stMON <- NULL; outk3 <- NULL
report.models <- NULL

if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_4_', my.time.stamp, '.pdf', sep=''), height=7, width=7)

op <- par(mfrow=c(3,1), mar=c(1.5,4.5,2.5,0.5), oma=c(3.2,0,1,0))
for (z in 1:3) {
 outk <- outk2[[z]]
 bestfit<- outk
 minDbest<- max(bestfit[,1])	# if you use dissimilarity (change to "min")
 monthDbest<- bestfit[which(bestfit[,1]==minDbest),2]
 smonth<- monthDbest-12*floor(monthDbest/12)
 smonth[smonth==0] <- 12
 if (length(monthDbest)==1) {
 maxFit[z] <- minDbest; monthB[z] <- ceiling(monthDbest/12); stMON[z] <- smonth; modelB[z] <- monthDbest
 }
 if (length(monthDbest)>1) {
 maxFit[z] <- minDbest
 monthB[z] <- toString(ceiling(monthDbest/12))
 stMON[z] <- toString(smonth)
 modelB[z] <- toString(monthDbest)
 }
report.models <- rbind(report.models, cbind(z, minDbest, ceiling(monthDbest/12), smonth, monthDbest))

plot(bestfit[,2],bestfit[,1],type="n", pch=16, col="black",
     ylim=c(0,1),xlab="", axes=F, ylab="")
 abline(v=seq(1,137,12),lwd=1.5, col="darkgray", lty=1)
 text(seq(7,144,12),1,1:12, cex=1, col="darkgray")
 points(bestfit[,2],bestfit[,1],type="o", pch=21, col="#003464", bg='white', cex=1.2, lwd=1.15)
 mtext(side=3, line=1.2, adj=0.95, paste("harvest length =",monthB[z], " months"),cex=0.7)
 mtext(side=3, line=1.2, adj=0.4, paste("harvest start month =",stMON[z]),cex=0.7)
 mtext(side=3,line=1.2, adj=0.02, paste("sample =", archl[z]),cex=0.7)
 mtext(side=3, line=0.2, adj=0.02,paste("number of iterations =",times),cex=0.7)
 mtext(side=3, line=0.2, adj=0.4, paste("similarity =",round(minDbest,3)),cex=0.7)
 mtext(side=3, line=0.2, adj=0.95, paste("similarity measure =",sim.m),cex=0.7)
 mtext(side=2, line=3, sim.m,cex=1)
 if (z==3) {
   axis(side=1, padj=1.5)
   mtext(side=1, line=3, 'null models (month sets)',cex=1)
   }
 axis(2, las=1)
 text(seq(2,132,2),rep(-0.12,12), rep(c("F","A","J","A","O","D"),12),
      xpd=T, cex=0.7, col='darkgray')
 box()
}
par(op)
if (outPDF) dev.off()

# step to generate final evaluation for various models (deactivated)
if(FALSE) {
  report.models <- data.frame(report.models,"Bray","run 1")
  colnames(report.models) <- c('sample', 'highest similarity', 'duration', 'start month',
                             'month set', 'similarity index', 'dataset')
  report.models
  write.csv(report.models, 'rM13.csv')
}

#------- SUMMARY OF ALL MODEL RESULTS (FIG SX)
# upload summary files from final anayses of similarity
sum.E <- rbind(read.csv('rTM1.csv'), read.csv('rTM2.csv'), read.csv('rTM3.csv'),
               read.csv('rM21.csv'), read.csv('rM22.csv'), read.csv('rM23.csv'),
               read.csv('rM11.csv'), read.csv('rM12.csv'), read.csv('rM13.csv'))
stV <- sum.E[,5]
stV[stV>6] <- stV[stV>6]-12
stV2 <- stV+sum.E[,4]
spaceM <- c(1:4,7:10,13:15, 21:23,26:28,31:33, 39:41,44:46,49:51)
seg.col <- c(rep(c('black','gray','green3','green3'), 2), rep(c('black','gray','green3'), length(spaceM)/3-2))

if (outPDF) my.time.stamp <- format(Sys.time(), "%m%d%Y_%H%M%S")
if(outPDF) pdf(paste('Fig_SX_', my.time.stamp, '.pdf', sep=''), height=7, width=7)

op <- par(mai=c(0.8,0.4,0.4,1.2), xpd=F)
plot(0, 0, xlim=c(-2,9), ylim=c(1,52), type='n', axes=F,
     ylab='', xlab='month', cex.lab=1.3)
 rect(-3,-3,0.7,55,col='brown1', border=NA)
 rect(0.7,-3,3.7,55,col='lightblue1', border=NA)
 rect(3.7,-3,6.7,55,col='lightgreen', border=NA)
 rect(6.7,-3,10,55,col='lightyellow', border=NA)
 segments(stV, spaceM,stV2,spaceM, col=seg.col, lwd=2)
 text(-2.2,c(50,32,14), "B")
 text(-2.2,c(45,27,8.5), "J")
 text(-2.2,c(40,22,2.5), "S")
 text(9.2,c(45,27,8.5), 1:3, cex=1.5)
 abline(h=c(18, 36), lty=3)
 axis(1, at=seq(-2,9,1), cex.axis=0.8, las=2,
      labels=c('OCT','NOV','DEC','JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG', 'SEP'))
 mtext(side=2, 'estimate', line=0.5, cex=1.3)
 mtext(side=3, line=0.05, 'FALL                    WINTER                     SPRING                     SUMMER')
 box()
 legend(9.6, 55, col=c('black','gray','green3'), lwd=2, archl, xpd=T, bty='n', cex=0.9)
 legend(9.2, 47, c('B - Bray Curtis', 'J - Jaccard', 'S - Sorenson'), cex=0.9, xpd=T, bty='n')
 legend(9.2, 39, c('1 - model run 1', '2 - model run 2', '3 - trimmed model'), cex=0.9, xpd=T, bty='n')
par(op)

if (outPDF) dev.off()

#----------------------------------------------------------
# FIGURE 3 (BEST FIT SIMULATIONS FOR ARCHEOLOGICAL SAMPLES)
labelX<- c("JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN")
mybluecolors<- c("white","#00FFCC","#00EFCC","#00EECC","#00DECC","#00DDCC","#00CDCC","#00CCCC","#00BCCC","#00BBCC","#00AACC","#0099CC",
                 "#0088CC","#0087CC","#0077CC","#0076CC","#0066CC","#0065CC","#0055CC","#0054CC","#0044CC","#0043CC","#0033CC","#0032CC",
                 "#0022CC","#0021CC","#0011CC","#0011BB","#0011AB","#0010AA","#001099","#001088","#001077","#001066","#001055","#001044",
			"#001033","#001022","#001011","black","black","black","black","black","black")
fontFamily <- 'Helvetica'
pdf.options(paper="special", onefile=TRUE, family=fontFamily,
             pointsize=9, encoding="ISOLatin1.enc", height=5, width=7.125)
pdf("BooneaCORRECTEDFIGURE1.pdf")

op <- par(mfcol=c(3,2), mar=c(5,4,0.5,0.5))

for (pp in 1:3) {
 plot(NA, xlim=range(1:12),
               ylim=range(1:12),xlab="Starting Month of Harvest",ylab="Length of Harvesting [Months]",
               frame=FALSE,axes=F,xaxs="i",yaxs="i")
 ddd <- as[[pp]][,2]
 g1<- NULL
 for (kk in 1:length(ddd)) {
  a<- rep(ddd[kk],floor((ddd[kk]-1)/12))
  if (length(a)>0) {
  b<- a+c(1:length(a))
  d<- 12+12*(floor((ddd[kk]-1)/12))
  b[b>d]<- b[b>d]-12
  g1<- c(g1,b)
  }
 }
 g1<- c(ddd,g1)
 g4<- factor(g1,1:144)
 g5<- matrix(table(g4),12,12)
 g6<- rbind(g5[7:12,],g5[1:6,])
 colbins<- round(seq(0,max(g6),max(g6)/39))
 # colbins <- 1:length(colbins)	# !!!! clumsy fix if there is an error in filled countour

 .filled.contour(x=1:12, y=1:12, z=g6,
               levels=colbins,
               col=mybluecolors)
  axis(1, 1:12, label=labelX, tcl=-0.5, las=2, cex.axis=0.7)
  axis(2, 1:12, label=1:12, tcl=-0.5, las=1, cex=0.7)
  mtext(side=3, col='black', paste('archeological sample', archl[pp]), adj=0.05, cex=0.5, line=-1) 
  box()
}

for (ppp in 1:3) {
 plot(NA, xlim=range(1:12),
               ylim=range(1:12),xlab="Starting Month of Harvest",ylab="",
               frame=FALSE,axes=F,xaxs="i",yaxs="i")
 ddd<- null1[[ppp]][null1[[ppp]][,3] == modelB[ppp],1]
 g1<- NULL
 for (kk in 1:length(ddd)) {
  a<- rep(ddd[kk],floor((ddd[kk]-1)/12))
  if (length(a)>0) {
  b<- a+c(1:length(a))
  d<- 12+12*(floor((ddd[kk]-1)/12))
  b[b>d]<- b[b>d]-12
  g1<- c(g1,b)
  }
 }
 g1<- c(ddd,g1)
 g4<- factor(g1,1:144)
 g5<- matrix(table(g4),12,12)
 g7<- rbind(g5[7:12,],g5[1:6,])
 colbins2<- round(seq(0,max(g7),max(g7)/39))

 .filled.contour(x=1:12, y=1:12, z=g7,
               levels=colbins2,
               col=mybluecolors)
  axis(1, 1:12, label=labelX, tcl=0.5, las=2, cex.axis=0.7)
  axis(2, 1:12, label=1:12, tcl=-0.5, las=1, cex=0.7)
  mtext(side=3, col='blue', paste('iterations =', times), adj=0.05, cex=0.5, line=-1) 
  mtext(side=3, col='blue', paste('Sorenson Index =', round(maxFit[ppp],3)), adj=0.7, cex=0.5, line=-1) 
  mtext(side=3, col='blue', paste("harvest length =",monthB[ppp], " months"), adj=0.05, cex=0.5, line=-2)
  mtext(side=3, col='blue', paste("start month =",stMON[ppp]), adj=0.7, line=-2, cex=0.5)
  box()
}
par(op)
dev.off()

##------------ Figure SX (using model for sample 1)
pdf.options(paper="special", onefile=TRUE, family=fontFamily,
             pointsize=9, encoding="ISOLatin1.enc", height=7.125, width=7.125)
pdf("BooneaCORRECTEDFIGURE2.pdf")
choose.model <- 1
month.order <- c(133, (121:132)[c(7:12,1:6)], (109:120)[c(7:12,1:6)],
   (97:108)[c(7:12,1:6)], (85:96)[c(7:12,1:6)], (73:84)[c(7:12,1:6)],
   (61:72)[c(7:12,1:6)], (49:60)[c(7:12,1:6)], (37:48)[c(7:12,1:6)],
   (25:36)[c(7:12,1:6)], (13:24)[c(7:12,1:6)], (1:12)[c(7:12,1:6)])
labelX2 <- labelX[c(7:12,1:6)]
op <- par(mfrow=c(12,12), mar=c(0,0,0,0), omi=c(0.8,0.8,0.1,0.1))
# for (ppp in c(121:133,109:120,97:108,85:96,73:84,61:72,49:60,37:48,25:36,13:24,1:12)) {
 for (ppp in month.order) {

 if (ppp < 133) {
 plot(NA, xlim=range(1:12),
               ylim=range(1:12),xlab="",ylab="",
               frame=FALSE,axes=F,xaxs="i",yaxs="i")
 ddd<- null1[[choose.model]][null1[[choose.model]][,3] == ppp,1]
 g1<- NULL
 for (kk in 1:length(ddd)) {
  a<- rep(ddd[kk],floor((ddd[kk]-1)/12))
  if (length(a)>0) {
  b<- a+c(1:length(a))
  d<- 12+12*(floor((ddd[kk]-1)/12))
  b[b>d]<- b[b>d]-12
  g1<- c(g1,b)
  }
 }
 g1<- c(ddd,g1)
 g4<- factor(g1,1:144)
 g5<- matrix(table(g4),12,12)
 g7<- rbind(g5[7:12,],g5[1:6,])
 colbins2<- round(seq(0,max(g7),max(g7)/39))

 .filled.contour(x=1:12, y=1:12, z=g7,
               levels=colbins2,
               col=mybluecolors)
if (ppp<13)  axis(1, 6, label=labelX2[ppp], tcl=-0.3, las=1, cex.axis=0.8)
if (ppp<13)  axis(1, 6, label='DEC', padj=-2.5, tcl=-0.3, las=1, cex.axis=0.5)
if (ppp %in% seq(7,133,12)) axis(2, 6, ceiling(ppp/12), tcl=-0.5, las=1, cex=0.7)
  box()
 }
 if (ppp == 133) {
 for (pppp in 1:12) {
 plot(NA, xlim=range(1:12),
               ylim=range(1:12),xlab="",ylab="",
               frame=FALSE,axes=F,xaxs="i",yaxs="i")
 ddd<- null1[[choose.model]][null1[[choose.model]][,3] == ppp,1]
 g1<- NULL
 for (kk in 1:length(ddd)) {
  a<- rep(ddd[kk],floor((ddd[kk]-1)/12))
  if (length(a)>0) {
  b<- a+c(1:length(a))
  d<- 12+12*(floor((ddd[kk]-1)/12))
  b[b>d]<- b[b>d]-12
  g1<- c(g1,b)
  }
 }
 g1<- c(ddd,g1)
 g4<- factor(g1,1:144)
 g5<- matrix(table(g4),12,12)
 g7<- rbind(g5[7:12,],g5[1:6,])
 colbins2<- round(seq(0,max(g7),max(g7)/39))

 .filled.contour(x=1:12, y=1:12, z=g7,
               levels=colbins2,
               col=mybluecolors)
  if (pppp==1) axis(2, 6, 12, tcl=-0.5, las=1, cex=0.7)
  box()
 }
 }
}
mtext(side=1, 'Starting Month of Harvest', line=3, outer=T)
mtext(side=2, 'Duration of Harves [Months]', line=3, outer=T)
par(op)
dev.off()
#-------------------------------------------------------------------------------------------------
# PLOT ANIIMATED SIMULATIONS FOR ALL NULL MODELS
#-------------------------------------------------------------------------------------------------
null2 <- NULL
for (i in 134:144) {
 add <- null1[which(null1[,3]==133),]
 add[,3] <- i
 null2 <- rbind(null2, add)
 }
null3 <- rbind(null1,null2)

null2 <- NULL
for (i in 134:144) {
 add <- null1[[1]][which(null1[[1]][,3]==133),]
 add[,3] <- i
 null2 <- rbind(null2, add)
 }
null3 <- rbind(null1[[1]],null2)

library(animation)
ani.options(interval=.3)
saveGIF({
for (i in seq(1,144,1))
{
ddd<- null3[which(null3[,3]==i),1]
g1<- NULL
for (kk in 1:length(ddd)) {
a<- rep(ddd[kk],floor((ddd[kk]-1)/12))
if (length(a)>0) {
b<- a+c(1:length(a))
d<- 12+12*(floor((ddd[kk]-1)/12))
b[b>d]<- b[b>d]-12
g1<- c(g1,b)
}
}
g1<- c(ddd,g1)
g4<- factor(g1,1:144)
g5<- matrix(table(g4),12,12)
nmonths<- which(apply(g5,2,sum)==max(apply(g5,2,sum)))
pmonths<- max(apply(g5,2,sum))/sum(g5)
month<- which(apply(g5,1,sum)==max(apply(g5,1,sum)))
pmonth<- max(apply(g5,1,sum))/sum(g5)
report<- rbind(cbind(nmonths,round(pmonths,3)),cbind(month,round(pmonth,3)),cbind(times,NA),cbind("D",NA),cbind(choose,NA))
#rownames(report)<- c("number of months","start month","number of iterations","metric","analyzed sample")
#colnames(report)<- c("months/month","p")
g6<- rbind(g5[7:12,],g5[1:6,])
labelX<- c("JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN")
labelX2<- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
mybluecolors<- c("white","#00FFCC","#00EFCC","#00EECC","#00DECC","#00DDCC","#00CDCC","#00CCCC","#00BCCC","#00BBCC","#00AACC","#0099CC",
                 "#0088CC","#0087CC","#0077CC","#0076CC","#0066CC","#0065CC","#0055CC","#0054CC","#0044CC","#0043CC","#0033CC","#0032CC",
                 "#0022CC","#0021CC","#0011CC","#0011BB","#0011AB","#0010AA","#001099","#001088","#001077","#001066","#001055","#001044",
			"#001033","#001022","#001011","black")
colbins<- round(seq(0,max(g6),max(g6)/39))
#jpeg(file=paste("myplotnew_", i, ".jpg", sep=""))
if (i-12*floor(i/12)==0) {vval=12}
if (i-12*floor(i/12)!=0) {vval=i-12*floor(i/12)}
if (vval<7) {vval=vval+6} else {vval=vval-6}
if (vval==12) {vval=11.99}
if (ceiling(i/12)==12) {hval=11.99} else {hval=ceiling(i/12)}
myim<- filled.contour(z=g6,x=1:12,y=1:12,col=mybluecolors, levels=colbins,
	  plot.axes={axis(1,c(1:12),labels=labelX,cex.axis=0.7,las=2); abline(h=hval, v=vval, col="orange", lwd=2, lty=2); axis(2,c(1:12),labels=c(1:12),cex.axis=0.7)},key.axes={})
mtext("Time of Harvesting",side=1, at=5, line=2.7)
mtext("Length of Harvesting [Months]",side=2,line=2.5,las=3)
mtext(paste("Harvest start month =", if (i-12*floor(i/12)==0) {labelX2[12]},if (i-12*floor(i/12)!=0) {labelX2[i-12*floor(i/12)]}),side=3,line=-1.5, at=3,cex=0.8,col="black")
mtext(paste("Harvest length =",ceiling(i/12), "months"),side=3,line=-1.5, at=7,cex=0.8,col="black")
mtext(paste("Iterations =",times),side=3,line=-2.5, at=3,cex=0.8,col="black")
mtext(paste("Fossil sample =",choose),side=3,line=-2.5, at=7,cex=0.8,col="black")
}
})

#--------------------------------------- END -----------------------------------------------------
