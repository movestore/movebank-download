library('move')
library('foreach')
library('dplyr')
library('data.table')

rFunction = function(username, password, study, animals, duplicates_handling="first", timestamp_start=NULL, timestamp_end=NULL, data=NULL, ...) {
  credentials <- movebankLogin(username, password)
  arguments <- list()

  if (duplicates_handling=="first")
  {
    logger.info("you have (possibly by default) selected to combine your duplicated values to retain the first value of each. Note that this is the fastest option, but might loose some information.")
    
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

    if (length(animals) == 0) 
    {
      logger.info("no animals set, using full study")
      animals <- unique(do.call(getMovebankAnimals, list(study,credentials))$local_identifier)
    }
    
    logger.info(paste(length(animals), "animals:", paste(animals,collapse=", ")))
    
    all <- list()
    all <- foreach(animal = animals) %do% {
      arguments["animalName"] = animal
      logger.info(animal)
      
      d <- tryCatch(do.call(getMovebankData, arguments), error = function(e){
        logger.info(e)
        return(NA)}) 
    }
    
    names(all) <- animals
    all <- all[unlist(lapply(all, is.na)==FALSE)] #take out NA animals
    result <- moveStack(all)
  }

  if (duplicates_handling=="combi")
  {
    logger.info("you have selected to combine your duplicated values to retain the max amount of information in your data. Note that this might take long, under usual conditions about 3-5 times longer.")
    
    arguments[["study"]] = study
    arguments[["login"]] = credentials
    arguments[["includeOutliers"]] = FALSE
    arguments[["underscoreToDots"]] = TRUE

    if (exists("timestamp_start") && !is.null(timestamp_start)) {
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

      locs <- tryCatch(do.call(getMovebankLocationData, arguments), error = function(e){
        logger.info(e)
        return(NA)}) 
      
      if (is.na(locs))
      {
        alli_move <- NA
      } else
      {
        dupls <- getDuplicatedTimestamps(locs)
        
        if (length(dupls)==0)
        {
          logger.info("no dupliated values. direct download")
          alli <- do.call(getMovebankData, arguments)
          alli_move <- alli
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
      }
      all <- c(all,list(alli_move))
    }
    names(all) <- animals
    all <- all[unlist(lapply(all, is.na)==FALSE)] #take out NA animals
    result <- moveStack(all)
  } #end of different duplicate removal methods
  
  # Fallback to make sure it is always a moveStack object and not a move object.
  if (is(result,'Move')) {
    result <- moveStack(result)
  }

  # give warning if there are timestamps in the future
  time_now <- Sys.time()
  animal_f <- namesIndiv(result)
  last_time <- foreach(animal = animal_f) %do% {
    all_animal <- result[namesIndiv(result)==animal,]
    max(timestamps(all_animal))
  }
  if (any(last_time>time_now)) 
  {
    ix <- which(last_time>time_now)
    logger.info(paste("Warning! Data of the animal(s)",paste(animal_f[ix],collapse=", "),"contain timestamps in the future. They are retained here, but can be filtered out with other Apps, e.g. `Remove Outliers`"))
  }
  
  if (exists("data") && !is.null(data)) {
    logger.info("Merging input and result together")
    result <- moveStack(result, data)
  }

  result
}
