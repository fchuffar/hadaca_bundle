# Authors: Magali Richard, UGA
# magali.richard@univ-grenoble-alpes.fr
#
#---------------------------------------------

##############################################
### SCORING

install.packages("combinat")

MAE <- function(M1, M2) {
  mean(abs(M1 - M2))
}

compare_A <- function(A_r, A_est) {
  
  N <- ncol(A_r)
  K <- nrow(A_r)
  stopifnot(K > 1)
  
  stopifnot(ncol(A_est) == N)
  stopifnot(nrow(A_est) < ncol(A_est))
  stopifnot(nrow(A_est) <= 10)
  stopifnot(!anyNA(A_est))
  
  # if not supplying enough types (make sure that {nrow(A_est) >= K})
  if (nrow(A_est) < K) A_est <- rbind(A_est, matrix(0, K - nrow(A_est), N))
  
  comb <- unlist(
    combinat::combn(nrow(A_est), K, fun = combinat::permn, simplify = FALSE),
    recursive = FALSE
  )
  comb_MAE <- sapply(comb, function(perm) {
    MAE(A_r, A_est[perm, , drop = FALSE])
  })
  
  perm <- comb[[which.min(comb_MAE)]]
  A_estim_perm <- A_est[perm, , drop = FALSE]
  
  return(MAE  = MAE(A_r, A_estim_perm))
}

scoring_function = function(Aref,Aest,output_file){
  resA <- compare_A(Aref, Aest)
}

##############################################
### SESSION
### DO NOT CHANGE THIS PART
# define input/output/ref/res from command line args (should in principle never be changed)
args = commandArgs(TRUE)
CHALLENGER_SESSION = length(args) == 0
print(paste("CHALLENGER_SESSION:", CHALLENGER_SESSION))
SERVER_SESSION = !CHALLENGER_SESSION
print(paste("SERVER_SESSION:", SERVER_SESSION)) 

# Environment variables
if (CHALLENGER_SESSION) {
  input = output = ref = res = "./"
} else {
  if (!exists("input"))  input = trimws(args[1]) #get args from command line and remove white spaces
  if (!exists("output")) output = trimws(args[2])
  if (!exists("ref"))    ref = "/ref/"
  if (!exists("res"))    res = "/res/"  
}

# read ref data (if on the server or admin)
data_full_filename = paste0(input, ref, "/data_full.rds")
ADMIN_SESSION = file.exists(data_full_filename)
print(paste("ADMIN_SESSION:", ADMIN_SESSION)) 


##############################################
###  EVALUATION
if (ADMIN_SESSION) {
  
  # Load submited results from participant
  Aest = readRDS(paste0(input, res, "results.rds"))
  
  # Load reference (ground truth)
  ref_res = readRDS(data_full_filename)
  Aref = ref_res$Aref
  
  # define the scores files
  #output_file <-paste0(output,"/scores.txt")
  
  #score participants
  scores = sapply(names(Aest), function(n){
    Aref = Aref[[n]]
    Aest = Aest[[n]]
    scoring_function(Aref,Aest)
  })
  
  initial_names = names(scores)

  scores[["MAE_total"]] = sum(scores)
  
  ordered_scores = c("MAE_total", initial_names)  #reorder scores for the leaderboard
  
  write_scores = function(scores, ordered_scores, output_file) {
    for (grp in ordered_scores) {
      print(grp)
        key = paste(grp, sep="_")
        #cat(sprintf(paste(key, ": %f\n", sep=""), scores[[grp]]), file=output_file, append=!(grp==names(scores)[1])) 
        cat(sprintf(paste(key, ": %f\n", sep=""), scores[[grp]]), file=output_file, append=TRUE)              
        
      
    }
    cat(readLines(output_file), sep = "\n")     
  }
  write_scores(scores, ordered_scores, output_file=paste0(output,"/scores.txt"))
  
}
  
##############################################
### EXPORT BUNDLE
# export codallab bundle (if local run with reference data, it means if admin run)
if (ADMIN_SESSION & CHALLENGER_SESSION) {
  print("Export codallab bundle, because of (CHALLENGER_SESSION & ADMIN_SESSION).")
  
  # rmarkdown::render("overview.Rmd")
  # rmarkdown::render("evaluation.Rmd")
  # rmarkdown::render("submission_script.Rmd", output_file="data.html")
  
  
  write_board = function(scores) {
    output_file = "competition.yaml"
    cat("\n", file=output_file, append=FALSE)
    foo = sapply(readLines("competition_head.yaml"), cat, "\n", file=output_file, append=TRUE)
    end_line = length(readLines(output_file))
    writeLines(readLines(output_file)[-c(1,end_line)], output_file) # remove an unwanted tab character and the first empty line
    cat(paste("leaderboard:                                        \n" , sep=""), file=output_file, append=TRUE)              
    cat(paste("  columns:                                          \n" , sep=""), file=output_file, append=TRUE)              
    i = 1
    for (grp in ordered_scores) {
     #for (ind in rownames(scores)) {
       # key = paste(grp, ind, sep="_")
      key = grp
        #FIRST_ELEMENT = grp==colnames(scores)[1]&ind==rownames(scores)[1]
       # FIRST_ELEMENT = grp==names(scores)[1]
      FIRST_ELEMENT = grp=="MAE_total"
        cat(paste("    ", key, ":                                      \n" , sep=""), file=output_file, append=TRUE)              
        cat(paste("      label: ", key, "                              \n" , sep=""), file=output_file, append=TRUE)              
        cat(paste("      leaderboard: ", ifelse(FIRST_ELEMENT,"&","*"), "id001\n" , sep=""), file=output_file, append=TRUE)              
        if (FIRST_ELEMENT) {
          
          cat(paste("        label: Results                              \n" , sep=""), file=output_file, append=TRUE)              
          cat(paste("        rank: 1                                     \n" , sep=""), file=output_file, append=TRUE)              
        }        
        cat(paste("      rank: ",i , "                                 \n" , sep=""), file=output_file, append=TRUE)
        # cat(paste("      rank: ",as.numeric(ind=="MSE"), "             \n" , sep=""), file=output_file, append=TRUE)
        # cat(paste("      rank: 1                                       \n" , sep=""), file=output_file, append=TRUE)
        cat(paste("      sort: asc                                     \n" , sep=""), file=output_file, append=TRUE)              
        i = i+1
      #}
    }
    cat(paste("  leaderboards:           \n" , sep=""), file=output_file, append=TRUE)              
    cat(paste("    Results: *id001    \n" , sep=""), file=output_file, append=TRUE)              
    cat(readLines(output_file), sep = "\n")     
  }
  write_board(scores)
  
  # zip the bundle
  zip_filename = "reference_data.zip"
  zip(zip_filename, "data_full.rds")
  
  zip_filename = "scoring_program.zip"
  zip(zip_filename, "scoring_program/metadata")
  zip(zip_filename, "scoring_program/scoring.r")    
  
  zip_filename = "starting_kit.zip"
  zip(zip_filename, "data.rds")
  zip(zip_filename, "starting_kit.Rmd")
  zip(zip_filename, "overview.Rmd")
  zip(zip_filename, "evaluation.Rmd")
  zip(zip_filename, "get_starting_kit.Rmd")
  zip(zip_filename, "data.Rmd")
  zip(zip_filename, "submission_script.Rmd")
  zip(zip_filename, "scoring_program/metadata")
  zip(zip_filename, "scoring_program/scoring.r")
  zip(zip_filename, ".Rhistory")
  
  zip_filename = "./codalab_bundle.zip"
  zip(zip_filename, "competition.yaml")
  zip(zip_filename, "data.html")
  zip(zip_filename, "terms.html")
  zip(zip_filename, "evaluation.html")
  zip(zip_filename, "overview.html")
  zip(zip_filename, "get_starting_kit.html")
  zip(zip_filename, "vignette.png")
  zip(zip_filename, "reference_data.zip")
  zip(zip_filename, "scoring_program.zip")
  zip(zip_filename, "starting_kit.zip")
  
  file.remove("reference_data.zip")
  file.remove("scoring_program.zip")
  file.remove("starting_kit.zip")    
}

# stop("EFN")















