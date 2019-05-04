################################################################################
#                             Quadratic variation                              # 
################################################################################
# Calculation of quadratic variation and covariation  
QVCalculation <- function(returns1, returns2){
  QV <- sum(returns1 * returns2)
  return(QV)
}

################################################################################
#                      Integrated covariance estimation                        # 
################################################################################
ICestimation <- function(return1, 
                         return2, 
                         Nobservations5Min,
                         nameReturn1 = "Return1",
                         nameReturn2 = "Return2",
                         fixedG = NA_real_){
  # Zhang(2011) G parameter - tuned based on number of intraday observations
  # Calculated as floor of "Number of intraday observations in examined frequency" (1min in our example) 
  # devieded by ("number of intraday observations in 5 minute frequency"/2.3)
  
  # For bootstap we use fixed G value - see Zhang(2011) for more details
  Nobservations <- length(return1)
  
  if(is.na(fixedG)) {
    Nobservations5Min <- Nobservations5Min
    G <- floor(Nobservations/(Nobservations5Min / 2.3)) 
  } else {
    G <- fixedG
  }
  
  # data reshape - for ICG and IC calculation 
  GNobservations <- floor(Nobservations/G)
  split <- Nobservations - G*GNobservations + 1
  
  dataReturn1 <- matrix(0, nrow = GNobservations, ncol = G)
  dataReturn2 <- matrix(0, nrow = GNobservations, ncol = G)
  
  for (start in 1:split) {
    dataReturn1[1:GNobservations,start] <- rowSums( matrix(return1[start:((start-1)+G*floor((Nobservations - start + 1)/G))],ncol=G, byrow = T) )
    dataReturn2[1:GNobservations,start] <- rowSums( matrix(return2[start:((start-1)+G*floor((Nobservations - start + 1)/G))],ncol=G, byrow = T) )
  }
  
  for (start in (split+1):G) {
    dataReturn1[1:(GNobservations-1),start] <- rowSums( matrix(return1[start:((start-1)+G*floor((Nobservations - start + 1)/G))],ncol=G, byrow = T) )
    dataReturn2[1:(GNobservations-1),start] <- rowSums( matrix(return2[start:((start-1)+G*floor((Nobservations - start + 1)/G))],ncol=G, byrow = T) )
  }
  
  # QV for every time point
  dataReturn1Return1 <- dataReturn1 *dataReturn1 
  dataReturn2Return2 <- dataReturn2 *dataReturn2 
  dataReturn1Return2 <- dataReturn1 *dataReturn2 
  
  # IC(G) 
  ICG.Return1Return1    <- colSums(dataReturn1Return1)
  ICG.Return1Return1avg <- mean(ICG.Return1Return1)
  
  ICG.Return2Return2    <- colSums(dataReturn2Return2)
  ICG.Return2Return2avg <- mean(ICG.Return2Return2)
  
  ICG.Return1Return2    <- colSums(dataReturn1Return2)
  ICG.Return1Return2avg <- mean(ICG.Return1Return2)
  
  # IC (WRC)
  ICWRC.Return1Return1 <- QVCalculation(return1,return1) 
  ICWRC.Return2Return2 <- QVCalculation(return2,return2) 
  ICWRC.Return1Return2 <- QVCalculation(return1,return2)  
  
  #constants
  nbar_N <- (( GNobservations*split + ( (GNobservations-1)*(G-split) ) )/G)/Nobservations
  const_ICG <- 1/(1-nbar_N)
  const_ICWRC <- const_ICG*nbar_N
  
  # IC calculation
  IC.Return1Return1 <- const_ICG * ICG.Return1Return1avg - const_ICWRC * ICWRC.Return1Return1
  IC.Return2Return2 <- const_ICG * ICG.Return2Return2avg - const_ICWRC * ICWRC.Return2Return2
  IC.Return1Return2 <- const_ICG * ICG.Return1Return2avg - const_ICWRC * ICWRC.Return1Return2
  
  # correlation culculation
  rho.Return1Return2 <- IC.Return1Return2/(sqrt(IC.Return1Return1)*sqrt(IC.Return2Return2))
  
  # results compilation
  ICmat.Return1Return2 <- data.frame(IC.Return1Return2,
                                     IC.Return1Return1,
                                     IC.Return2Return2, 
                                     rho.Return1Return2)
  
  names(ICmat.Return1Return2) <- c(paste0("IC.",nameReturn1,nameReturn2), 
                                   paste0("IC.",nameReturn1,nameReturn1),
                                   paste0("IC.",nameReturn2,nameReturn2), 
                                   paste0("rho.",nameReturn1,nameReturn2))
  
  return(ICmat.Return1Return2)
}

################################################################################
#                       Jump estimation - 1 minute data                        # 
################################################################################
# Function for jump identification - use "modwt" from "wavelets" package for calculation of wavelet coefficients
# carefull about the boundary option - "reflection" returns twice as many coefficients
# "periodic" is better for matching the time stamp of the jump
JumpIdentification <- function(return, filter = "d4", boundary = "periodic"){
  
  # calculation of first wavelet coefficients
  modwt <- align(modwt(as.numeric(return),filter = filter, n.levels = 1, boundary = boundary))@W[[1]] 
  nonzero_modwt <- modwt[!modwt == 0]                                                                     
  
  # Donoho & Johnstone (1994) threshold
  epsilon <- sqrt(2)*median(abs(nonzero_modwt))*sqrt(2*log(length(nonzero_modwt)))/0.6745
  
  # jump identification
  Jump <- abs(modwt) > epsilon
  
  # Calculating returns value without jumps, if jump occured the return is replaced with intraday variation 
  returnNoJ <- return * (Jump == FALSE) + Jump * var(return) * (return > 0) - Jump * var(return) *(return < 0)
  
  # Jump is calculated as returns - returns withou jumps
  JumpSize <- return - returnNoJ 
  
  return(data.frame("Jump" = Jump,
                    "dlCloseNoJ" = returnNoJ,
                    "JumpSize" = JumpSize))
  
}

################################################################################
#                                 Bootstrap                                    # 
################################################################################
# function for simulation 1 second data
sim_ret_intraday <- function(IC_mtx,N,day){
  error_TS1 <- rnorm(N,mean = 0, sd = 1)
  error_TS2 <- rnorm(N,mean = 0, sd = 1) 
  rho_TS1_TS2 <- IC_mtx[,4]
  ret_sim_TS1 <- sqrt(1/N*IC_mtx[,2][day])*error_TS1 
  ret_sim_TS2 <- sqrt(1/N*IC_mtx[,3][day])*(IC_mtx[,4][day]*error_TS1 + sqrt(1-IC_mtx[,4][day]^2)*error_TS2)
  df <- cbind.data.frame(ret_sim_TS1,ret_sim_TS2)
  df
}

# bootstrap function 
bootstrapIntraday <- function(ICJWCmatrix,Nobservations.boot,fixedG){
  # simulate 1 second data
  ret_1_2_mtx <- sim_ret_intraday(ICJWCmatrix,Nobservations.boot,1)             
  
  # reshape data into 5 minute intervals - 1 row contains 5 minute data i.e. 300s 
  ret_sim_R1 <- matrix(ret_1_2_mtx[,1],ncol = 5*60, byrow = T)
  ret_sim_R2 <- matrix(ret_1_2_mtx[,2],ncol = 5*60, byrow = T)
  
  # calculate intraday realized variance / covariance
  RV_boot1 <- sum( rowSums(ret_sim_R1)^2 )
  RV_boot2 <- sum( rowSums(ret_sim_R2)^2 )
  RV_boot12 <- sum( rowSums(ret_sim_R1)*rowSums(ret_sim_R2) )
  
  # estimate IC 
  ICJWCSim <- ICestimation(ret_1_2_mtx[,1],ret_1_2_mtx[,2], Nobservations5Min = NA, fixedG = fixedG)
  
  # calculate z value - (RV - IC)/RV
  Z_12 <- (RV_boot12 - ICJWCSim$IC.Return1Return2)/(RV_boot12) 
  Z_11 <- (RV_boot1  - ICJWCSim$IC.Return1Return1)/(RV_boot1)
  Z_22 <- (RV_boot2  - ICJWCSim$IC.Return2Return2)/(RV_boot2)
  
  return(data.frame(Z.12 = Z_12,
                    Z.11 = Z_11,
                    Z.22 = Z_22))
}

################################################################################
#                                  Results                                     # 
################################################################################
# Function to calculate final QV, IC, CJ for intraday data
hypothesisAnalysis <- function(Zstatus, QVmat, ICmat, CJmat){
  finalresults <- data.frame("QV"          = NA,
                             "CJvariation" = NA,
                             "CJ"          = NA,
                             "Jidio"       = NA, 
                             "ICJWCfinal"  = NA)
  
  for(i in 1:length(Zstatus)){
    QV          <- QVmat[i]
    
    if(Zstatus[i]){
      CJvariation <- QV - ICmat[i]
      CJ          <- CJmat[i]
      Jidio       <- CJvariation - CJmat[i]
      ICJWCfinal  <- ICmat[i]
    } else {
      CJvariation <- 0
      CJ          <- 0
      Jidio       <- 0
      ICJWCfinal  <- QVmat[i]
    }
    
    finalresults <- rbind(finalresults, list("QV"          = as.numeric(QV) ,
                                             "CJvariation" = as.numeric(CJvariation),
                                             "CJ"          = CJ,
                                             "Jidio"       = as.numeric(Jidio),
                                             "ICJWCfinal"  = as.numeric(ICJWCfinal))
    )
  }
  
  finalresults <- finalresults[-1,] 
  return(finalresults)
}

