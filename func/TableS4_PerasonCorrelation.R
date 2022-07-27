TableS4_PerasonCorrelation <- function(data=SYS, featureIndex = NULL,shortName = NULL){
  
  age <- c(cor(SYS[,featureIndex],SYS$Age), cor.test(SYS[,featureIndex],SYS$Age)$p.value)
  NumofLM <- c(cor(SYS[,featureIndex],SYS$liver_M_number), cor.test(SYS[,featureIndex],SYS$liver_M_number)$p.value)
  SizeofLM <- c(cor(SYS[,featureIndex],SYS$liver_M_size), cor.test(SYS[,featureIndex],SYS$liver_M_size)$p.value)
  CEA <- c(cor(SYS[,featureIndex],SYS$CEA), cor.test(SYS[,featureIndex],SYS$CEA)$p.value)
  
  Final <- rbind(age, NumofLM, SizeofLM, CEA)
  colnames(Final) <- paste0(shortName,"_",c("Coef","p-value"))
  
  return(Final)
}

