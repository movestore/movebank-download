library('move')
library('foreach')

rFunction  <- function(username, password, study, animals, timestamp_start=NULL, timestamp_end=NULL, data=NULL) {
  credentials <- movebankLogin(username, password)
  arguments <- list()
  arguments[["study"]] = study
  arguments[["login"]] = credentials
  arguments[["removeDuplicatedTimestamps"]] = TRUE
  arguments[["includeExtraSensors"]] = FALSE
  arguments[["deploymentAsIndividuals"]] = FALSE
  
  if (exists("timestamp_start")&& !is.null(timestamp_start)) {
    print(paste0("timestamp_start is set and will be used: ", timestamp_start))
    arguments["timestamp_start"] = timestamp_start
  }else {
    print("timestamp_start not set.")
  }
  
  if (exists("timestamp_end") && !is.null(timestamp_end)) {
    print(paste0("timestamp_end is set and will be used: ", timestamp_end))
    arguments["timestamp_end"] = timestamp_end
  }else {
    print("timestamp_end not set.")
  }
  
  if(length(animals) != 0) {
    print(paste0(length(animals), " animals: ", animals))
    all <- foreach(animal = animals) %do% {
      arguments["animalName"] = animal
      print(animal)
      do.call(getMovebankData, arguments)
    }
    result <- moveStack(all)
  } else {
    print("no animals set, using full study")
    result <- do.call(getMovebankData, arguments)
  }
  
  # Fallback to make sure it is always a moveStack object and not a move object.
  if (is.(result,'Move')) {
    result <- moveStack(result)
  }

  if (exists("data") && !is.null(data)) {
    print("Merging input and result together")
    result <- moveStack(result, data)
  }
  result
}

