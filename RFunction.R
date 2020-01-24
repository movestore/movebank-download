library('move')

rFunction = function(username, password, study, timestamp_start=NULL, timestamp_end=NULL, animals) {
  credentials <- movebankLogin(username, password)
  arguments <- list()
  arguments[["study"]] = study
  arguments[["login"]] = credentials
  arguments[["removeDuplicatedTimestamps"]] = FALSE
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
    arguments["animalName"] = animals
  } else {
    print("no animals set, using full study")
  }

  do.call(getMovebankData, arguments)
}
