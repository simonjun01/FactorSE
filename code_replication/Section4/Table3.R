
# Table 3: Comparing Asset Pricing Performance

folder_name <- "Table3"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}


source('../fun_Section.R')

Fama285 = read.csv('../data/Fama285.csv')
Fama285 = as.matrix(Fama285[,-c(1,2)])

Factor97 = read.csv('../data/Factor97.csv', check.names = FALSE)
Factor97 = as.matrix(t(Factor97[,-c(1,2)]))
names97 = rownames(Factor97)


model_fin_all = read.csv('./Table1/Model_fin_all.csv')
model_fin_all = model_fin_all[,c(2:16)]

### Asset Pricing (unselected candidate factors)
AP_result_remaining = R2_all_factor(Factor97,model_fin_all)

### Asset Pricing (285 basis portfolios)
AP_result_test_asset = R2_all_test_asset(Factor97,Fama285,model_fin_all)

##### Investment performance
invest_result_all = investing_asset(Factor97,model_fin_all)

result_all = cbind(AP_result_remaining,AP_result_test_asset,invest_result_all)

colnames(result_all) = c('#M', 
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         'Aalpha','Atalpha','sign2',
                         'Total_R2','CS_R2',
                         "AVG","Ann.SR",'alpha_CAPM',"alpha_FF5") 
rownames(result_all) = c(
  'CAPM','FF3', 'FF5','FF6', 'Q5','DHS3','BS6',
  'CAPM$^F$','FF3$^F$', 'FF5$^F$','FF6$^F$', 'Q5$^F$','DHS3$^F$','BS6$^F$','M8')


write.csv(result_all,'Table3/Table3.csv')




