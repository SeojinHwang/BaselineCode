rm(list = ls())
library(data.table)
library(glmnet)

# whether ALL or AML is shown in "sample" file 
y_tr = c(rep(0,27), rep(1,11)) # 0: ALL, 1: AML
y_tst = c(rep(0,11), rep(1,5),0,0,1,1,0,rep(1,7),rep(0,6))
y = c(y_tr, y_tst)
# table(y_tr); table(y_tst)

dat_tr = fread("/Volumes/GoogleDrive/공유 드라이브/2021-1 함수추정 term project/code/dat/data_set_ALL_AML_train.tsv", header = T)
dat_tst = fread("/Volumes/GoogleDrive/공유 드라이브/2021-1 함수추정 term project/code/dat/data_set_ALL_AML_independent.tsv", header = T)


### preprocessing ###
col_nm_tr = paste0("X",as.character(1:38))
col_nm_tst = paste0("X",as.character(39:72))

dat_tr = data.frame(dat_tr)
dat_tr = dat_tr[,col_nm_tr]
dat_tst = data.frame(dat_tst)
dat_tst = dat_tst[,col_nm_tst]
dat = cbind(dat_tr,dat_tst)

### modeling ###
n = ncol(dat_tr)
p = nrow(dat_tr)
d = floor(2*n/log(n)) # 20

cv_test_acc_r = cv_test_acc_wr = matrix(0, 1, 5)
cv_fit_wr_lasso = cv_fit_r_lasso = list(0,0,0,0,0)
best_lambda_loc_wr = best_lambda_loc_r = matrix(0, 1, 5)
tst_test_acc_wr = tst_test_acc_r = matrix(0, 1, 5)
test_id_list = list(c(1:5,28:29), c(6:10,30:31), c(11:15,32:33), c(16:21,34:35), c(22:27,36:38))

## wrong case ##
fold = 5
cor_wr = apply(dat_tr, 1, function(x){sum(scale(x) * y_tr)}) # matrix calculation -> vector calculation for speed
scr_tr_wr = dat_tr[order(abs(cor_wr), decreasing = TRUE),][1:d,]
scr_tr_wr = t(scr_tr_wr)
# dim(scr_tr_wr)
scr_tst_wr = t(dat_tst[order(abs(cor_wr), decreasing = TRUE),][1:d,])

f=1
for (f in 1:fold) {
  test_id = test_id_list[[f]]
  dat_wr = data.frame(cbind(y_tr, scr_tr_wr))
  
  cv_fit_wr_lasso[[f]] = glmnet(y=as.matrix(dat_wr[-test_id,1]), x=as.matrix(dat_wr[-test_id,-1]), family = "binomial", 
                           lambda = seq(0.001, 0.3, 0.001)) 
  cv_pred_wr_lasso = predict(cv_fit_wr_lasso[[f]], newx=as.matrix(dat_wr[test_id,-1]), type="response")
  cv_pred_bin_wr = ifelse(cv_pred_wr_lasso>=0.5, 1, 0)
  cv_test_acc_lasso_wr = apply(cv_pred_bin_wr, 2, 
                               function(x){sum(abs(y_tr[test_id]==x))/length(test_id)})
  best_lambda_loc_wr[, f] = which.max(cv_test_acc_lasso_wr[[f]])
  cv_test_acc_wr[, f] = max(cv_test_acc_lasso_wr[[f]])
  print(paste0(f,"th iteration for wrong case is done"))
}

tst_pred_wr = predict(cv_fit_wr_lasso[[which.max(cv_test_acc_wr)]], newx=as.matrix(scr_tst_wr), type="response")[,best_lambda_loc_wr[, which.max(cv_test_acc_wr)]]
tst_pred_bin_wr = ifelse(tst_pred_wr>=0.5, 1, 0)
tst_test_acc_wr = sum(y_tst==tst_pred_bin_wr)/length(y_tst)


## right case ## 
cor_r = matrix(0, fold, p)
for (f in 1:fold) {
  test_id = test_id_list[[f]]
  cor_r[f, ] = apply(dat_tr[,-test_id], 1, function(x){sum(scale(x) * y_tr[-test_id])})
  scr_tr_r = t(dat_tr[order(abs(cor_r[f,]), decreasing = TRUE),][1:d,])
  dat_r = data.frame(cbind(y_tr, scr_tr_r))
  
  cv_fit_r_lasso[[f]] = glmnet(y=as.matrix(dat_r[-test_id,1]), x=as.matrix(dat_r[-test_id,-1]), family = "binomial", 
                          lambda = seq(0.001, 0.3, 0.001)) 
  cv_pred_r_lasso = predict(cv_fit_r_lasso[[f]], newx=as.matrix(dat_r[test_id,-1]), type="response")
  cv_pred_bin_r = ifelse(cv_pred_r_lasso>=0.5, 1, 0)
  cv_test_acc_lasso_r = apply(cv_pred_bin_r, 2, 
                              function(x){sum(abs(y[test_id]==x))/length(test_id)})
  best_lambda_loc_r[, f] = which.max(cv_test_acc_lasso_r[[f]])
  cv_test_acc_r[, f] = max(cv_test_acc_lasso_r[[f]])
  print(paste0(f,"th iteration for right case is done"))
}

scr_tst_r = t(dat_tst[order(abs(cor_r[which.max(cv_test_acc_r),]), decreasing = TRUE),][1:d,])
tst_pred_r = predict(cv_fit_r_lasso[[which.max(cv_test_acc_r)]], newx=as.matrix(scr_tst_r), type="response")[,best_lambda_loc_r[, which.max(cv_test_acc_r)]]
tst_pred_bin_r = ifelse(tst_pred_r>=0.5, 1, 0)
tst_test_acc_r = sum(y_tst==tst_pred_bin_r)/length(y_tst)

# save(list=ls(), file="/Volumes/GoogleDrive/공유 드라이브/2021-1 함수추정 term project/code/result/real_bin_lasso.RData")
# load("/Volumes/GoogleDrive/공유 드라이브/2021-1 함수추정 term project/code/result/real_bin_lasso.RData")

print(mean(cv_test_acc_wr, na.rm=TRUE))
print(mean(cv_test_acc_r, na.rm=TRUE))
print(tst_test_acc_wr)
print(tst_test_acc_r)
