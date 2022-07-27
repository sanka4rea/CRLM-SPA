# Optimal cutoff selection
opti_cut <- function(SYSUCC_data =SYSUCC_data ,lowThresh=0.1,highThresh=0.9,steps=10000){
  ## cutoff
  # Specify range
  lq <- lowThresh
  uq <- highThresh
  n.q <- quantile(SYSUCC_data$train_score_aic, c(lq, uq))
  # count <- floor(as.numeric(signif(n.q[2], 3)/(1/steps)))
  count <- floor(as.numeric((signif(n.q[2], 6)-signif(n.q[1], 6))/(1/steps) ) )
  nop.value <- mclapply(1:count, function(xx){
    no.value <- as.numeric(n.q[1])+xx/steps
    a <- summary(coxph(Surv(as.numeric(os.time),as.numeric(os.event)) ~ (train_score_aic >= no.value),
                  data = SYSUCC_data))
    tmp <- a$coefficients[5]
    return(c(no.value,tmp))
  },mc.cores = 30)
  nop.value <- do.call(rbind,nop.value)
  colnames(nop.value) <- c("cutoff","p_value")
  result.no <- as.data.frame(nop.value)
  aic_used <- result.no[which(result.no$p_value == min(result.no$p_value)),]

  return(aic_used[1,1]) # just select the first without artificial selection
}