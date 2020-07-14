library('move')
library('foreach')
library('dplyr')
library('data.table')

rFunction = function(username, password, study, animals, duplmethod="first", timestamp_start=NULL, timestamp_end=NULL, data=NULL) {
  credentials <- movebankLogin(username, password)
  arguments <- list()
  
  if (duplmethod=="first")
  {
    arguments <- list()
    arguments[["study"]] = study
    arguments[["login"]] = credentials
    arguments[["removeDuplicatedTimestamps"]] = TRUE
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
      result <- moveStack(all)
    } else {
      logger.info("no animals set, using full study")
      result <- do.call(getMovebankData, arguments)
    }
  }
  
  if (duplmethod=="combi")
  {
    logger.info("you have selected to combine your duplicated values to retain the max amount of information in your data. Note that this might take long, under usual conditions about 3-5 times longer.")
    arguments[["study"]] = study
    arguments[["login"]] = credentials
    arguments[["includeOutliers"]] = FALSE
    arguments[["underscoreToDots"]] = TRUE
    
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
    
    if (length(animals)==0)
    {
      logger.info("no animals set, using full study")
      animals <- unique(do.call(getMovebankAnimals, list(study,credentials))$local_identifier)
    }
    
    SensorInfo <- getMovebankSensors(login=credentials)
    names(SensorInfo)
    SensorStudy <- getMovebankSensors(study,login=credentials)
    sensors <- intersect(SensorInfo$id[as.logical(SensorInfo$is_location_sensor)==TRUE],unique(SensorStudy$sensor_type_id))
    arguments[["sensorID"]] = sensors
    
    logger.info(paste(length(animals), "animals:", paste(animals,collapse=", ")))
    
    all <- list()
    for (animal in animals)
    {
      arguments["animalName"] = animal
      logger.info(animal)
      locs <- do.call(getMovebankLocationData, arguments)
      dupls <- getDuplicatedTimestamps(locs)
      
      if (length(dupls)==0)
      {
        logger.info("no dupliated values. direct download")
        alli <- do.call(getMovebankData, arguments)
      } else
      {
        logger.info(paste("Data of this ID contain",length(dupls),"duplicated timestamps. They will be combined for maximum information."))
        replix <- as.numeric(unlist(lapply(dupls,function(x) x[1]) ))
        remoix <-as.numeric(unlist(lapply(dupls,function(x) x[-1]) ))
        
        repls <- foreach(dupl = dupls) %do% {
          nNA <- apply(locs[dupl,],1,function(x) length(which(is.na(x))))
          o <- order(nNA)
          datai <- data.table(t(locs[dupl[o],]))
          c(coalesce(!!!datai))
        }
        repls_df <- do.call("rbind", lapply(repls,function(x) as.data.frame(t(x))))
        names(repls_df) <- names(locs)
        locs_classes <- lapply(locs, class)
        timeix <- which(names(repls_df)=="timestamp")
        repls_df$timestamp <- as.POSIXct(repls_df$timestamp,tz="GMT")
        for (i in seq(along=locs_classes)[-timeix]) class(repls_df[,i]) <- locs_classes[[i]]
        
        alli <- locs
        alli[replix,] <- repls_df
        alli <- alli[-remoix,]
        
        alli_move <- move(alli)
      }
      
      all <- c(all,list(alli_move))
    }
    names(all) <- animals
    result <- moveStack(all)
  }
  
  # Fallback to make sure it is always a moveStack object and not a move object.
  if (is(result,'Move')) {
    result <- moveStack(result)
  }

  if (exists("data") && !is.null(data)) {
    logger.info("Merging input and result together")
    result <- moveStack(result, data)
  }
  result
}
