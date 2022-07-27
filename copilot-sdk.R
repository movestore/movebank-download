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
args[["username"]] = "andreakoelzsch" #"TeamWikelski"
args[["password"]] = "Elska%Pferdchen42" #"e8kF*sdB"
args[["study"]] = 	171287018 # needs to be study ID!!!
args[["animals"]] =  c("71526a","71527a")
args[["duplicates_handling"]] = "first" #"first" or "combi"
args[["timestamp_start"]] = NULL
args[["timestamp_end"]] = NULL #"20080101120000000"
args[["thin"]]= FALSE
args[["thin_numb"]] = 1
args[["thin_unit"]] = "hour"
args[["minarg"]] = FALSE
args[["select_sensors"]] <- NULL #653 #NULL #does not work for numeric sensor id... ??
args[["incl_outliers"]] = FALSE #seems to work


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



