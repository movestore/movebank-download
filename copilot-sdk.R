library(jsonlite)
source("logger.R")
source("RFunction.R")

inputFileName = NULL #important to set to NULL for movebank-download
outputFileName = "output.rds"

args <- list()
#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exaclty the same as in the r function signature
# Example:
# rFunction = function(username, password)
# The paramter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r function here
args[["username"]] = "TeamWikelski"
args[["password"]] = 
args[["study"]] = 	212096177 # needs to be study ID!!!
args[["animals"]] = c("Ida + / DER AW838 (eobs 3040)","Erich + / DER AU642 (eobs 2762)","Lorenz + / DER AU597 (eobs 3018)")
args[["duplicates_handling"]] = "first" #"first" or "combi"
args[["timestamp_start"]] = NULL
args[["timestamp_end"]] = NULL
args[["thin"]]=TRUE
args[["thin_numb"]] = 1
args[["thin_unit"]] = "day"
args[["minarg"]] = TRUE

#args = fromJSON(txt="{\"study\":1300703741,\"animals\":[],\"username\":\"TeamWikelski\",\"password\":\"        \",\"duplicates_handling\":\"first\"}")

#args[["username"]] = "andreakoelzsch"
#args[["password"]] = 
#args[["study"]] = 1126572166 # needs to be study ID!!!
#args[["animals"]] = NULL
#args[["duplicates_handling"]] = "first" #"first" or "combi"
#args[["timestamp_start"]] = NULL
#args[["timestamp_end"]] = NULL


#################################################################
#################################################################
inputData <- NULL
if(!is.null(inputFileName) && inputFileName != "" && file.exists(inputFileName)) {
  cat("Loading file from", inputFileName, "\n")
  inputData <- readRDS(file = inputFileName)
} else {
  cat("Skip loading: no input File", "\n")
}

# Add the data paramter if input data is available
if (!is.null(inputData)) {
  args[["data"]] <- inputData
}

result <- tryCatch({
    do.call(rFunction, args)
  },
  error = function(e) { #if in RFunction.R some error are silenced, they come back here and break the app... (?)
    print(paste("ERROR: ", e))
    stop(e) # re-throw the exception
  }
)

if(!is.null(outputFileName) && outputFileName != "" && !is.null(result)) {
  cat("Storing file to", outputFileName, "\n")
  saveRDS(result, file = outputFileName)
} else {
  cat("Skip store result: no output File or result is missing", "\n")
}