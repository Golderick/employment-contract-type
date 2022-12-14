
## This script uses the results from the folders 'dataset_2016', dataset_2017, and dataset_2018.
## For running this script, the aforementioned folders should be present in the same folder as this script.

#-------------------------------------------------------------------------------

# load package
library(haven)

# function for extracting LG output
extract.output <- function(outPath, Nob) {
  
  output <- readLines(con = outPath) # read text lines
  output <- output[-1] # remove headers that are created when using `write` in LG
  output <- strsplit(x = output, split = ",") # LG creates `.csv` file
  maxLength <- max( sapply(X = output, FUN = length) ) # max number of columns in `.csv` file
  output <- lapply(X = output, FUN = `length<-`, maxLength) # create elements of equal length needed for dataframe
  output <- as.data.frame( do.call(rbind, output) ) # create dataframe
  row.names(output) <- NULL
  
  nams <- unique( output[ , 1] )
  note <- output[ which( output[ , 4] == 0 ), 5 ]
  note[is.na(note)] <- "OK"
  
  LLs <- as.numeric( output[ which( output[ , 5] == "Statistics" ), 6 ] ) # log-likelihoods
  # LPs <- as.numeric( output[ which( output[ , 5] == "Statistics" ), 8 ] ) # log-posteriors
  # L2s <- as.numeric( output[ which( output[ , 5] == "Statistics" ), 9 ] ) # likelihood-ratio chi-square statistic
  # BVRs <- as.numeric( output[ which( output[ , 5] == "Statistics" ), 13 ] ) # total bivariate residuals
  Npars <- as.numeric( rowSums( !is.na(output[ which( output[, 5] == "Parameters" ), 6:ncol(output) ] ) ) )
  AIC.LL <- -2*LLs + Npars*2
  BIC.LL <- -2*LLs + Npars*log(Nob)
  # BIC.L2 <- L2s - df*log(Nob)
  
  return( data.frame(nam = nams, LL = LLs, Npars = Npars, AIC.LL, BIC.LL = BIC.LL, note = note) )
}

#-------------------------------------------------------------------------------

#######################
## As reference 2018 ##
####################### 

path2018 <- "2018_dataset"

# final models 2016 data
final2018 <- read.csv( paste0(path2018, "/final-BIC-2018.csv") ) # -R
final2018R <- read.csv( paste0(path2018, "/final-BIC-R-2018.csv") ) # +R

# sample size
dat <- read_sav( file = paste0(path2018, "/snapshot2018you.sav") )
N <- nrow(dat)

rm(list = "dat")

#-------------------------------------------------------------------------------

########################
## Compared with 2016 ##
########################

path2016 <- "2016_dataset"

#-------------------------------------------------------------------------------

## without restrictions ##

final2016 <- read.csv( paste0( path2016, "/final-BIC-2016.csv") ) # -R

# disagreement for: "company_size" and "software_cluster"
covar <- final2018$covariate[!final2018$description == final2016$description]

# "company_size"
outputName <- paste0("output-", covar[1], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t. 2018
LGoutput[LGoutput$nam == final2016[final2016$covariate == covar[1], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018[final2018$covariate == covar[1], "description"], "BIC.LL"]
# difference = 9.519727; "strong" evidence (Raftery, 1995, p.139)

# "software_cluster"
outputName <- paste0("output-", covar[2], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t. 2018
LGoutput[LGoutput$nam == final2016[final2016$covariate == covar[2], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018[final2018$covariate == covar[2], "description"], "BIC.LL"]
# difference = 22.40447; "very strong" evidence (Raftery, 1995, p.139)

rm(final2016)

#-------------------------------------------------------------------------------

## with restrictions ##

final2016R <- read.csv( paste0(path2016, "/final-BIC-R-2016.csv.") ) # +R

# disagreement for: "company_size", "economic_activity", "software_cluster" and "work_hours" 
covar <- final2018R$covariate[!final2018R$description == final2016R$description]

# "company_size"
outputName <- paste0("output-", covar[1], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t. 2018
LGoutput[LGoutput$nam == final2016R[final2016R$covariate == covar[1], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[1], "description"], "BIC.LL"]
# difference = 26.20254; "very strong" evidence (Raftery, 1995, p.139)

# "economic_activity"
outputName <- paste0("output-", covar[2], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t 2018
LGoutput[LGoutput$nam == final2016R[final2016R$covariate == covar[2], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[2], "description"], "BIC.LL"]
# difference = 86.21897; "very strong" evidence (Raftery, 1995, p.139)

# "software_cluster"
outputName <- paste0("output-", covar[3], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t 2018
LGoutput[LGoutput$nam == final2016R[final2016R$covariate == covar[3], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[3], "description"], "BIC.LL"]
# difference = 7.33267; "strong" evidence (Raftery, 1995, p.139)

# "work_hours"
outputName <- paste0("output-", covar[4], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2016 w.r.t 2018
LGoutput[LGoutput$nam == final2016R[final2016R$covariate == covar[3], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[3], "description"], "BIC.LL"]
# difference = 19.36768; "strong" evidence (Raftery, 1995, p.139)

rm(final2016R)

#-------------------------------------------------------------------------------

########################
## Compared with 2017 ##
########################

path2017 <- "2017_dataset"

#-------------------------------------------------------------------------------

## without restrictions ##

final2017 <- read.csv( paste0(path2017, "/final-BIC-2017.csv") ) # -R

# disagreement for: "company_size" 
covar <- final2018$covariate[!final2018$description == final2017$description]

# "company_size"
outputName <- paste0("output-", covar, "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2017 w.r.t. 2018
LGoutput[LGoutput$nam == final2017[final2017$covariate == covar[1], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018[final2018$covariate == covar[1], "description"], "BIC.LL"]
# difference = 9.519727; "strong" evidence (Raftery, 1995, p.139)

rm(final2017)

#-------------------------------------------------------------------------------

## with restrictions ##

final2017R <- read.csv( paste0(path2017, "/final-BIC-R-2017.csv") ) # +R

# disagreement for: "company_size" and "software_cluster"
covar <- final2018R$covariate[!final2018R$description == final2017R$description]

# "company_size"
outputName <- paste0("output-", covar[1], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2017 w.r.t. 2018
LGoutput[LGoutput$nam == final2017R[final2017R$covariate == covar[1], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[1], "description"], "BIC.LL"]
# difference = 26.20254; "very strong" evidence (Raftery, 1995, p.139)

# "software_cluster"
outputName <- paste0("output-", covar[2], "-2018.csv")
LGoutput <- extract.output( outPath = paste0(path2018, "/", outputName), Nob = N)
# 2017 w.r.t. 2018
LGoutput[LGoutput$nam == final2017R[final2017R$covariate == covar[2], "description"], "BIC.LL"] -
  LGoutput[LGoutput$nam == final2018R[final2018R$covariate == covar[2], "description"], "BIC.LL"]
# difference = 1.867509; "weak" evidence (Raftery, 1995, p.139)

rm(final2018R)

