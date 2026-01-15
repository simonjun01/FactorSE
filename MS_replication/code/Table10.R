############################################################################
# Table 10: Bootstrapped Maximal Squared Sharpe ratios
############################################################################
rm(list=ls())

# Create output folder for tables if it does not exist
folder_name <- "../output"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
output_path <- "../output/Table10"
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Redirect all console output to a log file (while keeping it on screen)
sink("../output/Table10/Table10_run.log", split = TRUE)
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Load supporting functions
source('fun_Section.R')

# Load 97 factor returns
Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)

# Load 285 test assets
Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

# Load RP-PCA factors
Factor_RPPCA5 = read.csv('../data/RP-PCA/RPPCA_Factor.csv')
Factor_RPPCA5 = as.matrix(t(Factor_RPPCA5))

# Load RRA factors
Factor_RRA5 = read.csv('../data/RRA/RRA_Factor.csv')
Factor_RRA5 = as.matrix(t(Factor_RRA5))
Factor_all = rbind(Factor97,Factor_RPPCA5,Factor_RRA5)
names_all = rownames(Factor_all)

###### all model
S_CAPM = c("MKT")
S_FF3 = c("MKT","SMB","HML")
S_FF5 = c("MKT","SMB","HML","RMW","CMA" )
S_FF6 = c(S_FF5,'UMD' )
S_Q5 = c("MKT","SMB","IA", "ROE" , "REG" ) 
S_DHS3 = c('MKT', "PEAD", "FIN")
S_BS6 =  c("MKT","SMB","IA", "ROE" , 'UMD',"HMLM" )

S_M8 = c('MKT','SMB','REG','PEAD','HMLM','STR','ILR','EPRD')
S_RPPCA5 = c('RPCA1','RPCA2','RPCA3','RPCA4','RPCA5')
S_RRA5 =  c('RRA1','RRA2','RRA3','RRA4','RRA5')  
S_FGX2020 = c('MKT','UMD','BAB','CIM','R5A')
S_KNS2020 = c('MKT','SMB','BAB','ROE','ABR', 'CIM','dROE')

model_fin =list(S_CAPM,S_FF3, S_FF5,S_RRA5,S_FF6,S_FGX2020, S_KNS2020,S_RPPCA5,S_BS6, S_DHS3,S_Q5,
                S_M8)

model_all_names = c('CAPM','FF3', 'FF5','RRA','FF6','FGX2020','KNS2020','RP-PCA','BS6','DHS3', 'Q5','M8')


M <- 1000

### sample data
T1 = ncol(Factor_all)
N1 = nrow(Factor_all)

nMonths = T1
halfN <- nMonths / 2

model_SR_in = model_SR_out = matrix(,M,length(model_all_names))
colnames(model_SR_in) =  colnames(model_SR_out) = model_all_names

set.seed(2025)
pairStartMonths <- seq(from = 1, to = nMonths, by = 2)
randomBinaryInd <- matrix(sample(0:1, halfN * M, replace = TRUE),
                            nrow = halfN, ncol = M)
randomPairsIndicator <- matrix(sample(halfN, halfN * M, replace = TRUE),
                                 nrow = halfN, ncol = M)

inSampleInd  <- matrix(nrow = halfN, ncol = M)
outSampleInd <- matrix(nrow = halfN, ncol = M)

for (m in 1:M) {
  # print(m)
  isMonths  <- pairStartMonths + randomBinaryInd[, m]
  oosMonths <- pairStartMonths + 1 - randomBinaryInd[, m]

  inSampleInd[, m]  <- isMonths[randomPairsIndicator[, m]]
  outSampleInd[, m] <- oosMonths[randomPairsIndicator[, m]]

  
  Factor_all_in = as.matrix(Factor_all[,c(isMonths)])
  Factor_all_out = as.matrix(Factor_all[,c(oosMonths)])

for (z in 1:length(model_fin)) {

model1 = unlist(model_fin[z])

Factor1 = t(Factor_all_in[c(model1),,drop=FALSE])
Factor1_out = t(Factor_all_out[c(model1),,drop=FALSE])


muA = colMeans(Factor1)
WA = cov(Factor1)
# INS SR2
SR2_in = t(muA)%*%solve(WA)%*%muA 
muA_out = colMeans(Factor1_out)
WA_out = cov(Factor1_out)
# OOS SR2
SR2_out = (t(muA_out)%*%solve(WA)%*%muA)^2 / (t(muA)%*%solve(WA)%*%WA_out%*%solve(WA)%*%muA)

model_SR_in[m,z] = SR2_in
model_SR_out[m,z] = SR2_out
}

}

model_SR_probability_in = model_SR_probability_out =  matrix(,length(model_fin),length(model_fin)+2)
rownames(model_SR_probability_in) = rownames(model_SR_probability_out) = model_all_names
colnames(model_SR_probability_in) = colnames(model_SR_probability_out) = c('M-SR2',model_all_names,'Best')

model_SR_probability_in[,1] = round(colMeans(model_SR_in),2)
model_SR_probability_out[,1] = round(colMeans(model_SR_out),2)
for (i in 1:length(model_fin)) {
  for (j in 1:length(model_fin)) {
    
    model_SR_probability_in[i,j+1] = mean(model_SR_in[,i] >model_SR_in[,j])*100
    model_SR_probability_out[i,j+1] = mean(model_SR_out[,i] >model_SR_out[,j])*100
    
  }
}


max_prob_matrix_in <- matrix(0, M, length(model_fin))

for (i in 1:M) {
  max_prob_matrix_in[i, ] <- as.numeric(model_SR_in[i, ] == max(model_SR_in[i, ]))
}

model_SR_probability_in[,length(model_fin)+2] <- colMeans(max_prob_matrix_in)*100

### out
max_prob_matrix_out <- matrix(0, M, length(model_fin))

for (i in 1:M) {
  max_prob_matrix_out[i, ] <- as.numeric(model_SR_out[i, ] == max(model_SR_out[i, ]))
}

model_SR_probability_out[,length(model_fin)+2] <- colMeans(max_prob_matrix_out)*100

model_SR_probability_in_out = rbind(model_SR_probability_in, model_SR_probability_out)
write.csv(model_SR_probability_in_out,'../output/Table10/Table10.csv')

end_time <- Sys.time()
cat("End time:", format(end_time), "\n")
cat("Total elapsed time:",
    round(difftime(end_time, start_time, units = "secs"), 2),
    "seconds\n")

sink()