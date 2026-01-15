
################## Basic function ##################

judge.star <- function(pvalue){
  pvalue = 1 - pvalue
  if (pvalue > 0.99){
    output = "$^{***}$"
  } else if (pvalue > 0.95){
    output = "$^{**}$"
  } else if (pvalue > 0.90){
    output = "$^{*}$"
  } else{
    output = ""
  }
  return(output)
}

judge.star_HDA <- function(pvalue1){
  pvalue = 1 - pvalue1
  if (pvalue >= 0.99){
    output = "***"
  } else if (pvalue >= 0.95){
    output = "**"
  } else if (pvalue >= 0.90){
    output = "*"
  } else{
    output = "."
  }
  return(output)
}

calc_Max_SR2 = function(factor_RHS){
  
  mu = colMeans(factor_RHS)
  W = cov(factor_RHS)
  Max_SR2 = t(mu)%*%solve(W)%*%mu
  
  return(Max_SR2)
}


GRS_test = function(factor_RHS, factor_LHS){

  K = ncol(factor_RHS)
  N = ncol(factor_LHS)
  Ts = nrow(factor_LHS)
  
  if(Ts > N+K){

    W1 = solve(cov(factor_RHS))
    mu1 = matrix(colMeans(factor_RHS))
    SR1 = 1+t(mu1)%*%W1%*%mu1
      
    factor_all = cbind(factor_LHS, factor_RHS) 
    W = solve(cov(factor_all))
    mu = matrix(colMeans(factor_all))
    SR = 1+t(mu)%*%W%*%mu
    
    stat_GRS = ((Ts - N - K )/(N)*(SR/SR1-1))
    
    pvalue = 1- pf(stat_GRS, N , Ts - N - K)
    GRS_star = paste0(sprintf("%0.2f", stat_GRS) , judge.star(pvalue))
  }else{
    stat_GRS = 0
    pvalue = 0
    GRS_star = 0
  }

  
  return(list(pvalue,stat_GRS,GRS_star))
}



HDA_test = function(factor_RHS, factor_LHS){
  
  K = ncol(factor_RHS)
  N = ncol(factor_LHS)
  Ts = nrow(factor_LHS)
  

  X1 = t(factor_RHS)
  X2 = t(factor_LHS)
  ones = matrix(1,Ts,1)
  M_X1 = diag(Ts) - t(X1)%*%solve((X1)%*%t(X1))%*%(X1)
  hat_alpha2 = X2%*%M_X1%*%ones%*%solve(t(ones)%*%M_X1%*%ones)

  full_X = cbind(ones,t(X1))
  M_F = diag(Ts) - full_X%*%solve(t(full_X)%*%full_X)%*%t(full_X)
  
  error = X2%*%M_F
  error_Sigma = error%*%t(error)/(Ts - K -1)
  
  m = solve(t(ones)%*%M_X1%*%ones)
  var_alpha2 =  as.vector(m)*error_Sigma 
  T_stat2 = hat_alpha2^2/diag(var_alpha2)
  
  
  deg = Ts - K - 1
  rho2 = 0
  Rut = cor(t(error))
  pn = 0.1/(N-1)
  thetan = (qnorm(1-pn/2))^2
  rho2 = (sum((Rut[Rut^2*deg >thetan])^2)-N)/2
  rho2 = rho2*2/N/(N-1)
  HDAk =  N^(-1/2)*(sum(T_stat2 - deg/(deg-2)))/((deg/(deg-2))*sqrt(2*(deg-1)/(deg-4) *(1+(N-1)*rho2)  )  )
  

  pvalue = 1-pnorm(HDAk)   
  HDA_star = paste0(sprintf("%0.2f", HDAk), judge.star(pvalue))
  return(list(pvalue,HDAk, HDA_star))
}


################## FSE function ##################

FSE_stage_bench = function(factor_all, lambda, bench_factor){
  
  bench_num = length(bench_factor)
  
  if (is.null(bench_factor)){
    print('Starting from an empty model')
  }

  
  S =  bench_factor
  Ts = ncol(factor_all)
  Ns = nrow(factor_all)
  names_all = rownames(factor_all)
  
  
  k_forward = Ns - 1 - bench_num
  
  
  for (i in 1:k_forward) {

    Ns_i = Ns - length(S)
    names_all_i = setdiff(names_all, S)
    
    Max_SR2i = NULL
    for (j in 1:Ns_i){
      
      Sj = union(x = names_all_i[j], y = S)
      factor_RHS = factor_all[c(Sj), , drop = FALSE]
      
      W_Sj = cov(t(factor_RHS))
      W_Sj_inv = solve(W_Sj)
      mu_Sj = matrix(rowMeans(factor_RHS), ncol = 1)
      Max_SR2_Sj = t(mu_Sj) %*% W_Sj_inv %*% mu_Sj
      Max_SR2i = c(Max_SR2i, Max_SR2_Sj)
    }

 
    a_i = names_all_i[which(Max_SR2i == max(Max_SR2i))]
    S = c(S, a_i)
    factor_RHS = factor_all[c(S), , drop = FALSE]  
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]

    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    
    result_p = HDA_test(F1, F2)
    p_HDAi = result_p[[1]]

    if (p_HDAi > lambda) {
      break;
    }
  }

  S_FSE = S
  return(S_FSE)
  
}


FSE_stage_bench_figure = function(factor_all, lambda, bench_factor){
  
  bench_num = length(bench_factor)
  
  if (is.null(bench_factor)){
    print('Starting from an empty model')
  }

  
  S =  bench_factor
  Ts = ncol(factor_all)
  Ns = nrow(factor_all)
  names_all = rownames(factor_all)
  
  k_forward = Ns - 1 - bench_num

  GRS_all = SR_all = NULL
  factor_RHS = factor_all[c(S), , drop = FALSE]
  factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]
  F1 = t(factor_RHS)
  F2 = t(factor_LHS)
  GRS_all = GRS_test(F1, F2)[[2]]
  SR_all = sqrt(calc_Max_SR2(t(factor_RHS)))*sqrt(12)
  
  for (i in 1:k_forward) {
    
    Ns_i = Ns - length(S)
    names_all_i = setdiff(names_all, S)
    
    Max_SR2i = NULL
    for (j in 1:Ns_i) {
      
      Sj = union(x = names_all_i[j], y = S)
      factor_RHS = factor_all[c(Sj), , drop = FALSE]
      
      W_Sj = cov(t(factor_RHS))
      W_Sj_inv = solve(W_Sj)
      mu_Sj = matrix(rowMeans(factor_RHS), ncol = 1)
      Max_SR2_Sj = t(mu_Sj) %*% W_Sj_inv %*% mu_Sj
      Max_SR2i = c(Max_SR2i, Max_SR2_Sj)
      
    }
    
    a_i = names_all_i[which(Max_SR2i == max(Max_SR2i))]
    S = c(S, a_i)
    
    factor_RHS = factor_all[c(S), , drop = FALSE]  
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]

    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    GRS_alli = GRS_test(F1, F2)[[2]]
    GRS_all = c(GRS_all,GRS_alli)
    SR_alli = sqrt(calc_Max_SR2(F1))*sqrt(12)
    SR_all = c(SR_all,SR_alli)
    
    # if (p_HDAi > lambda) {
    #   break;
    # }
  }
  
  S_FSE = S
  
  
  return(list(S_FSE,GRS_all,SR_all))
  
}

################## BSE function ##################

BSE_stage_bench = function(factor_all, lambda,  bench_factor_BSE){
  
  bench_num = length(bench_factor_BSE)

  S =  bench_factor_BSE
  Ts = ncol(factor_all)
  Ns = nrow(factor_all)
  names_all = rownames(factor_all)
  
  k_backward = bench_num - 1

  for (i in 1:k_backward) {
    
    Ns_i = length(S)
    
    S_BSE = S
    Max_SR2i = NULL
    for (j in 1:Ns_i) {
      
      Sj = setdiff(S, S[j])
      factor_RHS = factor_all[c(Sj), , drop = FALSE]

      W_Sj = cov(t(factor_RHS))
      W_Sj_inv = solve(W_Sj)
      mu_Sj = matrix(rowMeans(factor_RHS), ncol = 1)
      Max_SR2_Sj = t(mu_Sj) %*% W_Sj_inv %*% mu_Sj
      Max_SR2i = c(Max_SR2i, Max_SR2_Sj)
      
    }
    

    b_i = S[which(Max_SR2i == max(Max_SR2i))]
    
    S = setdiff(S, b_i)
    
    factor_RHS = factor_all[c(S), , drop = FALSE]  
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]

    
    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    
    result_p = HDA_test(F1, F2)
    p_HDAi = result_p[[1]]
    

    if (p_HDAi < lambda) {
      break;
    }
  }
  
  S_BSE = S_BSE
  
  return(S_BSE)
}


BSE_stage_bench_figure = function(factor_all, lambda, bench_factor_BSE){
  
  bench_num = length(bench_factor_BSE)

  S =  bench_factor_BSE
  Ts = ncol(factor_all)
  Ns = nrow(factor_all)
  names_all = rownames(factor_all)
  
  k_backward = bench_num - 1
  
  BSE_Path = NULL
  GRS_all = SR_all = NULL
  factor_RHS = factor_all[c(S), , drop = FALSE]
  factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]
  F1 = t(factor_RHS)
  F2 = t(factor_LHS)
  GRS_all = GRS_test(F1, F2)[[2]]
  SR_all = sqrt(calc_Max_SR2(t(factor_RHS)))*sqrt(12)

  for (i in 1:k_backward) {
    
    Ns_i = length(S)
  
    S_BSE = S
    Max_SR2i = NULL
    for (j in 1:Ns_i) {
      
      Sj = setdiff(S, S[j])
      factor_RHS = factor_all[c(Sj), , drop = FALSE]
      
      # Max SR2
      W_Sj = cov(t(factor_RHS))
      W_Sj_inv = solve(W_Sj)
      mu_Sj = matrix(rowMeans(factor_RHS), ncol = 1)
      Max_SR2_Sj = t(mu_Sj) %*% W_Sj_inv %*% mu_Sj
      Max_SR2i = c(Max_SR2i, Max_SR2_Sj)
      
    }
    

    b_i = S[which(Max_SR2i == max(Max_SR2i))]
    S = setdiff(S, b_i)
    BSE_Path = c(BSE_Path, b_i)
    
    factor_RHS = factor_all[c(S), , drop = FALSE]  
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]

    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    GRS_alli = GRS_test(F1, F2)[[2]]
    GRS_all = c(GRS_all,GRS_alli)
    SR_alli = sqrt(calc_Max_SR2(F1))*sqrt(12)
    SR_all = c(SR_all,SR_alli)

  }
  
  
  S_BSE = S_BSE
  
  return(list(BSE_Path,GRS_all,SR_all))
}


BSE_stage_bench_path = function(factor_all, lambda, bench_factor_BSE){
  
  bench_num = length(bench_factor_BSE)
  
  S =  bench_factor_BSE
  Ts = ncol(factor_all)
  Ns = nrow(factor_all)
  names_all = rownames(factor_all)
  
  k_backward = bench_num - 1
  
  BSE_Path = NULL
  for (i in 1:k_backward) {
    
    Ns_i = length(S)

    S_BSE = S
    Max_SR2i = NULL
    for (j in 1:Ns_i) {
      
      Sj = setdiff(S, S[j])
      factor_RHS = factor_all[c(Sj), , drop = FALSE]
      
      W_Sj = cov(t(factor_RHS))
      W_Sj_inv = solve(W_Sj)
      mu_Sj = matrix(rowMeans(factor_RHS), ncol = 1)
      Max_SR2_Sj = t(mu_Sj) %*% W_Sj_inv %*% mu_Sj
      Max_SR2i = c(Max_SR2i, Max_SR2_Sj)
      
    }
    
    b_i = S[which(Max_SR2i == max(Max_SR2i))]
    
    BSE_Path = c(BSE_Path, b_i)
    
    S = setdiff(S, b_i)
    
    factor_RHS = factor_all[c(S), , drop = FALSE]  
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S),, drop = FALSE]

    F1 = t(factor_RHS)
    F2 = t(factor_LHS)
    
    result_p = HDA_test(F1, F2)
    p_HDAi = result_p[[1]]
    

    if (p_HDAi < lambda) {
      break;
    }
    

  }
  
  
  return(BSE_Path)
}


######## Investment performance function ####### 

investing_asset = function(factor_all,model_fin){
  invest_metric_result = matrix(, ncol(model_fin), 4)
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    invest_metric_result[z,] = invest_perform_metric(factor_all,S_factor)
  }
  
  
  return(invest_metric_result)
}

investing_asset_out = function(factor_all_in,factor_all_out,model_fin){
  invest_metric_result = matrix(, ncol(model_fin), 1)
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    invest_metric_result[z,] = invest_perform_metric_out(factor_all_in,factor_all_out,S_factor)
  }

  return(invest_metric_result)
}

investing_asset_new = function(Factor97,Factor_RPPCA5,Factor_RRA5,model_fin){
  invest_metric_result = matrix(, ncol(model_fin), 4)
  for (z in 1:ncol(model_fin)) {
    
    if (model_names[z] == 'RPPCA'){
      factor_all = rbind(Factor97,Factor_RPPCA5)
    } else if (model_names[z] == 'RRA'){
      factor_all = rbind(Factor97,Factor_RRA5)
    } else {
      factor_all = Factor97
    }
    
    S_factor = na.omit(model_fin[,z])
    invest_metric_result[z,] = invest_perform_metric(factor_all,S_factor)
  }
  
  return(invest_metric_result)
}

invest_perform_metric = function(factor_all,S_factor){
  
  factor_RHS = factor_all[c(S_factor),, drop = FALSE]
  
  if (length(S_factor) == 1){ 
    factor_MVE = factor_RHS
  } else {
  W_S_factor = cov(t(factor_RHS))
  W_S_factor_inv = solve(W_S_factor)
  mu_S_factor = matrix(rowMeans(factor_RHS), ncol = 1)
  weight_MVE = W_S_factor_inv %*% mu_S_factor
  weight_MVE_adj =  2 * weight_MVE/sum(abs(weight_MVE))
  factor_MVE = t(weight_MVE_adj)%*%factor_RHS
  }
  
  AVG = mean(factor_MVE)*100
  AVG = round(AVG,2)
  SR = mean(factor_MVE)/sd(factor_MVE)
  Ann_SR = sqrt(12)*SR
  Ann_SR = round(Ann_SR,2)
  
  S_CAPM = c("MKT" )
  factor_CAPM = factor_all[c(S_CAPM),, drop = FALSE]
  
  lm_CAPM = lm(t(factor_MVE) ~ t(factor_CAPM))
  alpha_CAPM = summary(lm_CAPM)$coefficients[,'Estimate'][[1]]*100
  p_value_CAPM = summary(lm_CAPM)$coefficients[,'Pr(>|t|)'][[1]]  
  alpha_CAPM_star = paste0(sprintf("%0.2f", alpha_CAPM) , judge.star(p_value_CAPM))
  
  
  S_FF5 = c("MKT","SMB","HML","RMW","CMA")
  factor_FF5 = factor_all[c(S_FF5),, drop = FALSE]
  
  lm_FF5 = lm(t(factor_MVE) ~ t(factor_FF5))
  alpha_FF5 = summary(lm_FF5)$coefficients[,'Estimate'][[1]]*100
  p_value_FF5 = summary(lm_FF5)$coefficients[,'Pr(>|t|)'][[1]]
  alpha_FF5_star = paste0(sprintf("%0.2f", alpha_FF5) , judge.star(p_value_FF5))
  
  result_metric = c(AVG, Ann_SR,alpha_CAPM_star,alpha_FF5_star)
  
  
  return(result_metric)
}


invest_perform_metric_out = function(factor_all_in,factor_all_out,S_factor){
  
  factor_RHS_in = factor_all_in[c(S_factor),, drop = FALSE]
  factor_RHS_out = factor_all_out[c(S_factor),, drop = FALSE]
  
  if (length(S_factor) == 1){ 
    factor_MVE = factor_RHS_out
  } else {
  W_S_factor = cov(t(factor_RHS_in))
  W_S_factor_inv = solve(W_S_factor)
  mu_S_factor = matrix(rowMeans(factor_RHS_in), ncol = 1)
  weight_MVE = W_S_factor_inv %*% mu_S_factor
  weight_MVE_adj =  2 * weight_MVE/sum(abs(weight_MVE)) 
  factor_MVE = t(weight_MVE_adj)%*%factor_RHS_out
  }
  
  SR = mean(factor_MVE)/sd(factor_MVE) 
  Ann_SR = sqrt(12)*SR
  Ann_SR = round(Ann_SR,2)
  
  
  return(Ann_SR)
}

######### Asset Pricing performance function #########

R2_all_factor = function(factor_all, model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),5+1)
  
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S_factor),, drop = FALSE]
    factor_LHS = t(factor_LHS)
    
    AP_perform_metric_result[z,] = c(length(S_factor), AP_perform_metric(factor_all,factor_LHS,S_factor))
    
  }
  
  return(AP_perform_metric_result)
}




R2_all_factor_out = function(factor_all_in, factor_all_out,model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),2)
  
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    
    AP_perform_metric_result[z,] = c(AP_perform_metric_out(factor_all_in,factor_all_out,test_asset_in = NULL, test_asset_out=NULL,S_factor))
    
  }
  
  
  return(AP_perform_metric_result)
}


R2_all_factor_new = function(Factor97,Factor_RPPCA5,Factor_RRA5,model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),1+5)
  
  for (z in 1:ncol(model_fin)) {
    
    if (model_names[z] == 'RPPCA'){
      factor_all = rbind(Factor97,Factor_RPPCA5)
    } else if (model_names[z] == 'RRA'){
      factor_all = rbind(Factor97,Factor_RRA5)
    } else {
      factor_all = Factor97
    }
    
    S_factor = na.omit(model_fin[,z])
    
    factor_LHS = factor_all[!rownames(factor_all) %in% c(S_factor),, drop = FALSE]
    factor_LHS = t(factor_LHS)
    
    AP_perform_metric_result[z,] = c(length(S_factor), AP_perform_metric(factor_all,factor_LHS,S_factor) )
    
  }
  
  
  return(AP_perform_metric_result)
}

R2_all_test_asset = function(factor_all, test_asset,model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),5)
  
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    factor_LHS = test_asset
    
    AP_perform_metric_result[z,] = AP_perform_metric(factor_all,factor_LHS,S_factor)
  
  }
  
  return(AP_perform_metric_result)
}


R2_all_test_asset_out = function(factor_all_in, factor_all_out, test_asset_in,test_asset_out,model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),2)
  
  for (z in 1:ncol(model_fin)) {
    
    S_factor = na.omit(model_fin[,z])
    
    AP_perform_metric_result[z,] = AP_perform_metric_out(factor_all_in, factor_all_out,test_asset_in, test_asset_out,S_factor)
    
  }
  
  
  return(AP_perform_metric_result)
}


R2_all_test_asset_new = function(Factor97,Factor_RPPCA5,Factor_RRA5,test_asset,model_fin){
  
  AP_perform_metric_result = matrix(,ncol(model_fin),5)
  
  for (z in 1:ncol(model_fin)) {
    
    if (model_names[z] == 'RPPCA'){
      factor_all = rbind(Factor97,Factor_RPPCA5)
    } else if (model_names[z] == 'RRA'){
      factor_all = rbind(Factor97,Factor_RRA5)
    } else {
      factor_all = Factor97
    }
    
    S_factor = na.omit(model_fin[,z])
    factor_LHS = test_asset
    
    AP_perform_metric_result[z,] = AP_perform_metric(factor_all,factor_LHS,S_factor)
    
  }
  
  
  return(AP_perform_metric_result)
}

AP_perform_metric = function(factor_all, factor_LHS,S_factor){
  
  factor_RHS = factor_all[c(S_factor), , drop = FALSE]
  Ts = ncol(factor_RHS)

  
  alpha_vector = talpha_vector = NULL
  for (i in 1:ncol(factor_LHS)) {
    
    factor_LHSi = factor_LHS[,i, drop = FALSE]
    
    lm1 = lm(factor_LHSi ~ t(factor_RHS))
    
    alphai = summary(lm1)$coefficients[,'Estimate'][1]*100
    talphai = summary(lm1)$coefficients[,'t value'][1]
    alpha_vector = c(alpha_vector, abs(alphai))
    talpha_vector = c(talpha_vector,abs(talphai))
  }
  
  Aalpha  = round(mean(alpha_vector),2)
  Atalpha = round(mean(talpha_vector),2)
  sign2 = length(which(talpha_vector > 1.96))
  
  
  #### R2
  calc_total_R2 = function(factor_LHS, factor_RHS){
    
    Ts = nrow(factor_LHS)
    ones = matrix(1,Ts,1)
    full_factor_RHS = cbind(ones,t(factor_RHS))
    alpha_beta = solve(t(full_factor_RHS)%*%full_factor_RHS)%*%t(full_factor_RHS)%*%factor_LHS
    beta = alpha_beta[-1,, drop = FALSE]
    
    Xhat = t(factor_RHS)%*%beta

    return(list(Xhat,beta))
  }
  
  S_CAPM = c("MKT" )
  factor_CAPM = factor_all[c(S_CAPM),, drop = FALSE]

  total_R2_MKT = calc_total_R2(factor_LHS, factor_CAPM)
  Xhat_MKT = total_R2_MKT[[1]]
  beta_MKT = total_R2_MKT[[2]]
  total_R2_S = calc_total_R2(factor_LHS, factor_RHS)
  Xhat_S = total_R2_S[[1]]
  beta_S = total_R2_S[[2]]
  
  Total_R2 = 1 - sum((factor_LHS - Xhat_S)^2) / sum((factor_LHS - Xhat_MKT)^2)
  Total_R2 = Total_R2*100
  
  calc_CS_R2 = function(factor_LHS, factor_RHS, beta){
    
    bar_X2 = colMeans(factor_LHS)
    b = cbind(rep(1,ncol(beta)),t(beta))
    gamma = solve(t(b)%*%b)%*%t(b)%*%bar_X2
    
    bar_hat = t(beta) %*%gamma[-1]
    return(bar_hat)
  }
  

  
  bar_hat_MKT = calc_CS_R2(factor_LHS, factor_CAPM, beta_MKT)
  bar_hat_S = calc_CS_R2(factor_LHS, factor_RHS, beta_S)
  
  bar_factor_LHS = as.matrix(colMeans(factor_LHS))
  CS_R2 = 1 - sum((bar_factor_LHS - bar_hat_S)^2) / sum((bar_factor_LHS - bar_hat_MKT)^2)
  CS_R2 = CS_R2*100
  
  
  return(
    c(
      round(Aalpha,2),round(Atalpha,2),sign2,
      round(Total_R2,1),round(CS_R2,1))
         )

}



AP_perform_metric_out = function(factor_all_in, factor_all_out,test_asset_in, test_asset_out,S_factor){

  
  factor_RHS_in = factor_all_in[c(S_factor),, drop = FALSE]
  factor_RHS_out = factor_all_out[c(S_factor),, drop = FALSE]

  if (is.null(test_asset_in)){
    factor_LHS_in = factor_all_in[!rownames(factor_all_in) %in% c(S_factor),, drop = FALSE]
    factor_LHS_in = t(factor_LHS_in)
  } else {
    factor_LHS_in = test_asset_in
  }
  
  if (is.null(test_asset_out)){
    factor_LHS_out = factor_all_out[!rownames(factor_all_out) %in% c(S_factor),, drop = FALSE]
    factor_LHS_out = t(factor_LHS_out)
  } else {
    factor_LHS_out = test_asset_out
  }

  
  #### R2
  calc_total_R2 = function(factor_LHS_in, factor_RHS_in, factor_RHS_out){
    
    Ts = nrow(factor_LHS_in)
    ones = matrix(1,Ts,1)
    full_factor_RHS = cbind(ones,t(factor_RHS_in))
    alpha_beta = solve(t(full_factor_RHS)%*%full_factor_RHS)%*%t(full_factor_RHS)%*%factor_LHS_in
    beta = alpha_beta[-1,, drop = FALSE]
    
    Xhat = t(factor_RHS_out)%*%beta
    
    return(list(Xhat,beta))
  }
  
  S_CAPM = c("MKT" )
  factor_CAPM_in = factor_all_in[c(S_CAPM),, drop = FALSE]
  factor_CAPM_out = factor_all_out[c(S_CAPM),, drop = FALSE]
  
  total_R2_MKT = calc_total_R2(factor_LHS_in, factor_CAPM_in,factor_CAPM_out)
  Xhat_MKT = total_R2_MKT[[1]]
  beta_MKT = total_R2_MKT[[2]]
  
  total_R2_S = calc_total_R2(factor_LHS_in, factor_RHS_in,factor_RHS_out)
  Xhat_S = total_R2_S[[1]]
  beta_S = total_R2_S[[2]]
  
  Total_R2 = 1 - sum((factor_LHS_out - Xhat_S)^2) / sum((factor_LHS_out - Xhat_MKT )^2)
  Total_R2 = Total_R2*100
  
  calc_CS_R2 = function(factor_LHS_out, beta){
    
    bar_X2 = colMeans(factor_LHS_out)
    b = cbind(rep(1,ncol(beta)),t(beta))
    gamma = solve(t(b)%*%b)%*%t(b)%*%bar_X2
    
    bar_hat = t(beta) %*%gamma[-1]
    return(bar_hat)
  }
  
  
  bar_hat_MKT = calc_CS_R2(factor_LHS_out, beta_MKT)
  bar_hat_S = calc_CS_R2(factor_LHS_out, beta_S)
  
  bar_factor_LHS = as.matrix(colMeans(factor_LHS_out))
  CS_R2 = 1 - sum((bar_factor_LHS - bar_hat_S)^2) / sum((bar_factor_LHS - bar_hat_MKT)^2)
  CS_R2 = CS_R2*100
  
  
  return(
    c(round(Total_R2,1),round(CS_R2,1))
  )
  
}



