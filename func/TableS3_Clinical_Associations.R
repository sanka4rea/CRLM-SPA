TableS3_Clinical_Associations <- function(data=SYS, featureIndex = NULL,shortName = NULL){
  
  index <- which(colnames(data))
  
  # Gender
  sys_gen_Female <- summary(SYS[,index][which(ZZ_os_allv2$Gender=="1")])[c(2,3,5)]
  sys_gen_Male <- summary(SYS[,index][which(ZZ_os_allv2$Gender=="2")])[c(2,3,5)]
  sys_gen_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$Gender=="1")],
                           SYS[,index][which(ZZ_os_allv2$Gender=="2")])$p.value
  sys_gen_p <- c(sys_gen_p,NA,NA)
  sys_gen <- rbind(sys_gen_Female, sys_gen_Male, sys_gen_p)
  
  # Primary tumor locations
  sys_crcLoc_Left <- summary(SYS[,index][which(ZZ_os_allv2$CRC_location=="0")])[c(2,3,5)]
  sys_crcLoc_Right <- summary(SYS[,index][which(ZZ_os_allv2$CRC_location=="1")])[c(2,3,5)]
  sys_crcLoc_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$CRC_location=="0")],
                              SYS[,index][which(ZZ_os_allv2$CRC_location=="1")])$p.value
  sys_crcLoc_p <- c(sys_crcLoc_p,NA,NA)
  sys_crcLoc <- rbind(sys_crcLoc_Left, sys_crcLoc_Right, sys_crcLoc_p)
  
  # Pathological T-stage
  sys_T_L <- summary(SYS[,index][which(ZZ_os_allv2$Tstage=="0")])[c(2,3,5)]
  sys_T_H <- summary(SYS[,index][which(ZZ_os_allv2$Tstage=="1")])[c(2,3,5)]
  sys_T_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$Tstage=="0")],
                         SYS[,index][which(ZZ_os_allv2$Tstage=="1")])$p.value
  sys_T_p <- c(sys_T_p,NA,NA)
  sys_T <- rbind(sys_T_L, sys_T_H, sys_T_p)
  
  # Pathological N-stage 
  sys_N_L <- summary(SYS[,index][which(ZZ_os_allv2$Nstage=="0")])[c(2,3,5)]
  sys_N_H <-summary(SYS[,index][which(ZZ_os_allv2$Nstage=="1")])[c(2,3,5)]
  sys_N_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$Nstage=="0")],
                         SYS[,index][which(ZZ_os_allv2$Nstage=="1")])$p.value
  sys_N_p <- c(sys_N_p,NA,NA)
  sys_N <- rbind(sys_N_L, sys_N_H, sys_N_p)
  
  # DFI
  sys_DFI_S <- summary(SYS[,index][which(ZZ_os_allv2$DFI==0)])[c(2,3,5)]
  sys_DFI_M <- summary(SYS[,index][which(ZZ_os_allv2$DFI>0)])[c(2,3,5)]
  sys_DFI_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$DFI==0)],
                           SYS[,index][which(ZZ_os_allv2$DFI>0)])$p.value
  sys_DFI_p <- c(sys_DFI_p,NA,NA)
  sys_DFI <- rbind(sys_DFI_S, sys_DFI_M, sys_DFI_p)
  
  # Resection margin
  sys_RR_R0 <- summary(SYS[,index][which(ZZ_os_allv2$RR=="0")])[c(2,3,5)]
  sys_RR_R1 <- summary(SYS[,index][which(ZZ_os_allv2$RR=="1")])[c(2,3,5)]
  sys_RR_p <- wilcox.test(SYS[,index][which(ZZ_os_allv2$RR=="0")],
                          SYS[,index][which(ZZ_os_allv2$RR=="1")])$p.value
  sys_RR_p <- c(sys_RR_p,NA,NA)
  sys_RR <- rbind(sys_RR_R0, sys_RR_R1, sys_RR_p)
  
  Final <- rbind(sys_gen, sys_crcLoc, sys_T, sys_N, sys_DFI, sys_RR)
  colnames(Final) <- paste0(shortName,"_",colnames(Final))
  
  return(Final)
}

