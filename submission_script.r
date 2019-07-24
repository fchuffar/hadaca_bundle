# submission_script.r 
# Authors: Magali Richard (CNRS), Florent Chuffart (INSERM)
#
#---------------------------------------------

### DO NOT CHANGE THIS PART
dataset = readRDS("data.rds")
D = dataset
#metadata = dataset$exp_grp

### PUT YOUR SCRIPT HERE
library("NMF") #here we use the NMF package

cohorts = names(D)
Dest = list()
Aest = list()

for (name in cohorts){
  cur_D = D[[name]]
  #cur_metadata = metadata[[name]]
  # Add below your own method
  results <-
    nmf(cur_D, rank = 4, 'lee') # here we do a basic nmf factorization with k =4
  cur_T <- basis(results)
  cur_A <- coef(results)
  for (i in 1:dim(cur_A)[2]){
    cur_A[,i]=cur_A[,i]/sum(cur_A[,i])
    cur_T[i,]=cur_T[i,]*sum(cur_A[,i])
  }
  Aest[[name]] = cur_A
}

res = Aest 

### DO NOT CHANGE THIS PART, it needs *res* variable
saveRDS(res, "results.rds")
zip_filename = paste(sep="",  "results_", format(Sys.time(), format="%m_%d_%Y_%s"), ".zip")
zip(zip_filename, "results.rds")
print(zip_filename)