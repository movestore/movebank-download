library('move')
library('foreach')

rFunction = function(username, password, study, animals, timestamp_start=NULL, timestamp_end=NULL, data=NULL) {
  credentials <- movebankLogin(username, password)
  arguments <- list()
  arguments[["study"]] = study
  arguments[["login"]] = credentials
  arguments[["removeDuplicatedTimestamps"]] = FALSE
  arguments[["includeExtraSensors"]] = FALSE
  arguments[["deploymentAsIndividuals"]] = FALSE

  if (exists("timestamp_start")&& !is.null(timestamp_start)) {
    logger.info(paste0("timestamp_start is set and will be used: ", timestamp_start))
    arguments["timestamp_start"] = timestamp_start
  }else {
    logger.info("timestamp_start not set.")
  }

  if (exists("timestamp_end") && !is.null(timestamp_end)) {
    logger.info(paste0("timestamp_end is set and will be used: ", timestamp_end))
    arguments["timestamp_end"] = timestamp_end
  }else {
    logger.info("timestamp_end not set.")
  }

  if(length(animals) != 0) {
    logger.info(paste0(length(animals), " animals: ", animals))
    all <- foreach(animal = animals) %do% {
      arguments["animalName"] = animal
        logger.info(animal)
      do.call(getMovebankData, arguments)
    }
    result = moveStack(all)
  } else {
    logger.info("no animals set, using full study")

    result <- do.call(getMovebankData, arguments)
  }

  if (exists("data") && !is.null(data)) {
    logger.info("Merging input and result together")
    result <- moveStack(result, data)
  }
  result
}
