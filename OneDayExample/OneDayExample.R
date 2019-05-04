rm(list = ls())
# sourcing functions used in the analysis
library(wavelets)
library(dplyr)
source('./OneDayExample/OneDayExampleFunctions.R')

################################################################################
#                             Quadratic variation                              # 
################################################################################
BNday_5min <- readRDS("BNdayExample_5min.rds")
BXday_5min <- readRDS("BXdayExample_5min.rds")

# QV estimation
QV_BNBN <- QVCalculation(BNday_5min$dlClose, BNday_5min$dlClose)
QV_BXBX <- QVCalculation(BXday_5min$dlClose, BXday_5min$dlClose)
QV_BNBX <- QVCalculation(BNday_5min$dlClose, BXday_5min$dlClose)

################################################################################
#           Integrated covariance estimation - with jumps (IC TSCV)            # 
################################################################################
BNday_1min <- readRDS("BNdayExample_1min.rds")
BXday_1min <- readRDS("BXdayExample_1min.rds")

# Extract returns from the data 
retBN <- BNday_1min$dlClose
retBX <- BXday_1min$dlClose

# IC TSCV estimation
ICTSCVmat.BNBX <- ICestimation(retBN, 
                               retBX, 
                               nrow(BNday_5min),
                               "BN",
                               "BX")

################################################################################
#                       Jump estimation - 1 minute data                        # 
################################################################################

# Jump identification and value estimation 
BNday_1min <- cbind(BNday_1min, 
                    JumpIdentification(BNday_1min$dlClose))

BXday_1min <- cbind(BXday_1min, 
                    JumpIdentification(BXday_1min$dlClose) )

################################################################################
#                   Co-Jump estimation - 1 minute data                         # 
################################################################################
CJ.BNBX <- BNday_1min$JumpSize * BXday_1min$JumpSize 
CJintraday <- sum(CJ.BNBX)

################################################################################
#          Integrated covariance estimation - without jumps (IC JWC)           # 
################################################################################
# Extract returns adjusted from jumps (created in "Jump estimation part")
retBNNoJ <- BNday_1min$dlCloseNoJ
retBXNoJ <- BXday_1min$dlCloseNoJ

# IC JWC estimation
ICJWCmat.BNBX <- ICestimation(retBNNoJ,
                              retBXNoJ, 
                              nrow(BNday_5min),
                              "BN",
                              "BX")

################################################################################
#                                 Bootstrap                                    # 
################################################################################
# We are generating 1 second data in the bootstrap - Nobservations.boot 
Nobservations <- nrow(BNday_1min)
Nobservations.boot <- Nobservations * 60

# Fixed G is used in this case
fixedG <- 75 #? total number of observations in our frequency / number of 5 min observations, should be 60 then

# number of bootsrap loops
b <- 100

# set seed for replication purposes 
seed <- 12
set.seed(seed)

# Bootstrap realizations - Z* values
Zvalues <- data.frame("Z.12" = NA, "Z.11" = NA, "Z.22" = NA)
for(B in 1:b){
   Zvalues <- rbind(Zvalues,
                    bootstrapIntraday(ICJWCmat.BNBX,Nobservations.boot,fixedG)[1:3])
   print(B)
}
Zvalues <- Zvalues[-1,]
colnames(Zvalues) <- c("Z.BNBX", "Z.BNBN", "Z.BXBX")  
Zvalues

################################################################################
#                                  Results                                     # 
################################################################################
# values for one-sided and two-sided t-test
ci <- 2.5767
ci2 <- 1.968

# Combining data of one pair together
QVmat.BNBX <- data.frame(QV_BNBX,
                         QV_BNBN,
                         QV_BXBX)

CJmat.BNBX <- c(CJintraday,0,0)

# Calculation of Z value from calculated data -
Zvalue.BNBX <- (QVmat.BNBX - ICJWCmat.BNBX[1:3])/QVmat.BNBX

# Calculation of Z bootstrap statistic
Zstatistic.BNBX <- (Zvalue.BNBX - colMeans(Zvalues))/sqrt(diag(var(Zvalues)))
Zstatus <- c(Zstatistic.BNBX[1] > ci2, Zstatistic.BNBX[2:3] > ci) 

# Identification of significant co-jump variation
results <- hypothesisAnalysis(Zstatus,QVmat.BNBX,ICJWCmat.BNBX[1:3],CJmat.BNBX)
rownames(results) <- c("BNBX","BNBN", "BXBX")
results

# correlation difference
corrTrue.BNBX <- ICTSCVmat.BNBX$rho.BNBX
corrContinuous.BNBX <- results["BNBX",]$ICJWCfinal/(sqrt(results["BNBN",]$ICJWCfinal) * sqrt(results["BXBX",]$ICJWCfinal))
corrDifference.BNBX <- corrTrue.BNBX - corrContinuous.BNBX

correlation <- data.frame("True"       = corrTrue.BNBX,
                          "Continuous" = corrContinuous.BNBX,
                          "Difference" = corrDifference.BNBX)

correlation