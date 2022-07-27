library('move')
library('foreach')
library('dplyr')
library('data.table')
library('lubridate')

rFunction = function(username, password, study, animals=NULL, duplicates_handling="first", timestamp_start=NULL, timestamp_end=NULL, thin=FALSE, thin_numb = 1, thin_unit ='hour', minarg=FALSE, incl_outliers=FALSE, select_sensors=NULL, data=NULL, ...) {
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
    arguments[["includeOutliers"]] = incl_outliers
   
    if (exists("timestamp_start")&& !is.null(timestamp_start)) {
      logger.info(paste0("timestamp_start is set and will be used: ", timestamp_start))
      arguments["timestamp_start"] = timestamp_start
    } else {
      logger.info("timestamp_start not set.")
    }

    if (exists("timestamp_end") && !is.null(timestamp_end)) {
      logger.info(paste0("timestamp_end is set and will be used: ", timestamp_end))
      arguments["timestamp_end"] = timestamp_end
    } else {
      logger.info("timestamp_end not set.")
    }

    if (thin==FALSE) logger.info("You have selected to download your dataset in full resolution, without thinning.")
    if (thin==TRUE) logger.info(paste("You have selected to thin your dataset to one position per",thin_numb,thin_unit,"."))
    
    if (minarg==FALSE) logger.info("You have seleted to download all available arguments of the selected dataset.")
    if (minarg==TRUE) logger.info("You have selected to download only the minimum number of arguments: Animal ID, Longitute, Latitude, Timestamp, Species, Sensor, Outlier_visibility")
    
    if (length(animals) == 0) 
    {
      logger.info("no animals set, using full study")
      animals <- as.character(unique(do.call(getMovebankAnimals, list(study,credentials))$local_identifier))
    }
    
    logger.info(paste(length(animals), "animals:", paste(animals,collapse=", ")))
    
    SensorInfo <- getMovebankSensors(login=credentials)
    SensorStudy <- getMovebankSensors(study,login=credentials)
    allsensors <- intersect(SensorInfo$id[as.logical(SensorInfo$is_location_sensor)==TRUE],unique(SensorStudy$sensor_type_id))
    allsensors_names <- SensorInfo$name[which(SensorInfo$id %in% allsensors)]
    
    #it seems that the attribute "sensor_type_id" is not possible in getMovebankData... shall I switch to getMovebank("event",...)?
    # then need to change argument names and add: arguments[["entity_type"]] <- "event"
    if (is.null(select_sensors))
    {
      logger.info(paste("You have selected to download locations of all available sensor types:",paste(allsensors_names,collapse=", ")))
      arguments[["sensor_type_id"]] <- allsensors
    } else
    {
      select_sensors_names <- SensorInfo$name[which(SensorInfo$id %in% select_sensors)]
      logger.info(paste("You have selected to download only locations of these selected sensor types:",paste(select_sensors_names,collapse=", ")))
      arguments[["sensor_type_id"]] <- select_sensors
    } 
    
    all <- list()
    all <- foreach(animal = animals) %do% {
      arguments["animalName"] = animal
      logger.info(animal)
      
      d <- tryCatch(do.call(getMovebankData, arguments), error = function(e){
        logger.info(e)
        return(NULL)}) 
      if (thin==TRUE & is.null(d)==FALSE) d <- d[!duplicated(round_date(timestamps(d), paste0(thin_numb," ",thin_unit))),]
      if (minarg==TRUE & is.null(d)==FALSE) 
      {
        minargdata0 <- as.data.frame(d)
        names(minargdata0) <- make.names(names(minargdata0),allow_=FALSE)
        if (any(names(minargdata0)=="individual.taxon.canonical.name")) taxon.name <- "individual.taxon.canonical.name" else if (any(names(minargdata0)=="taxon.canonical.name")) taxon.name <- "taxon.canonical.name" else {
          taxon.name <- NULL
          logger.info("No taxon information available. This attribute will not be added to the data.")
        }
        minargdata <- minargdata0[,c("timestamp","location.long","location.lat","sensor.type",taxon.name,"visible")]
        minargdata <- data.frame("individual.local.identifier"=namesIndiv(d),minargdata)
        minargdata$timestamp <- as.POSIXct(timestamps(d))
        d@data <- minargdata
      }
      d
    }
    
    names(all) <- animals
    all <- all[unlist(lapply(all, is.null)==FALSE)] #take out NULL data animals
    if (length(all)>0) result <- moveStack(all,forceTz="UTC") else result <- NULL
  }

  if (duplicates_handling=="combi")
  {
    logger.info("you have selected to combine your duplicated values to retain the max amount of information in your data. Note that this might take long, under usual conditions about 3-5 times longer.")
    
    arguments[["study"]] = study
    arguments[["login"]] = credentials
    arguments[["includeOutliers"]] = incl_outliers
    arguments[["underscoreToDots"]] = TRUE

    if (exists("timestamp_start") && !is.null(timestamp_start)) {
      logger.info(paste0("timestamp_start is set and will be used: ", timestamp_start))
      arguments["timestamp_start"] = timestamp_start
    } else {
      logger.info("timestamp_start not set.")
    }

    if (exists("timestamp_end") && !is.null(timestamp_end)) {
      logger.info(paste0("timestamp_end is set and will be used: ", timestamp_end))
      arguments["timestamp_end"] = timestamp_end
    } else {
      logger.info("timestamp_end not set.")
    }

    if (thin==FALSE) logger.info("You have selected to download your dataset in full resolution, without thinning.")
    if (thin==TRUE) logger.info(paste("You have selected to thin your dataset to one position per",thin_numb,thin_unit,"."))
    
    if (length(animals)==0)
    {
      logger.info("no animals set, using full study")
      animals <- as.character(unique(do.call(getMovebankAnimals, list(study,credentials))$local_identifier))
    }

    
    logger.info(paste(length(animals), "animals:", paste(animals,collapse=", ")))
    
    SensorInfo <- getMovebankSensors(login=credentials)
    SensorStudy <- getMovebankSensors(study,login=credentials)
    allsensors <- intersect(SensorInfo$id[as.logical(SensorInfo$is_location_sensor)==TRUE],unique(SensorStudy$sensor_type_id))
    allsensors_names <- SensorInfo$name[which(SensorInfo$id %in% allsensors)]
    
    if (is.null(select_sensors))
    {
      logger.info(paste("You have selected to download locations of all available sensor types:",paste(allsensors_names,collapse=", ")))
      arguments[["sensorID"]] <- allsensors
    } else
    {
      select_sensors_names <- SensorInfo$name[which(SensorInfo$id %in% select_sensors)]
      logger.info(paste("You have selected to download only locations of these selected sensor types:",paste(select_sensors_names,collapse=", ")))
      arguments[["sensorID"]] <- as.character(select_sensors)
    } 

    all <- list()
    for (animal in animals)
    {
      arguments["animalName"] = animal
      logger.info(animal)

      locs <- tryCatch(do.call(getMovebankLocationData, arguments), error = function(e){
        logger.info(e)
        return(NULL)}) 
      
      if (is.null(locs))
      {
        alli_move <- NULL
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
          repls_df$timestamp <- as.POSIXct(repls_df$timestamp,tz="UTC")
          for (i in seq(along=locs_classes)[-timeix]) class(repls_df[,i]) <- locs_classes[[i]]
          
          alli <- locs
          alli[replix,] <- repls_df
          alli <- alli[-remoix,]
          alli_move <- move(alli)
        }
        if (thin==TRUE & is.null(alli_move)==FALSE) alli_move <- alli_move[!duplicated(round_date(timestamps(alli_move), paste0(thin_numb," ",thin_unit))),]
        if (minarg==TRUE & is.null(alli_move)==FALSE)
        {
          minargdatac0 <- as.data.frame(alli_move)
          names(minargdatac0) <- make.names(names(minargdatac0),allow_=FALSE)
          minargdatac <- minargdatac0[,c("timestamp","location.long","location.lat","sensor.type","visible")]
         minargdatac <- data.frame("individual.local.identifier"=namesIndiv(alli_move),minargdatac)
         minargdatac$timestamp <- as.POSIXct(timestamps(alli_move))
         if (any(names(idData(alli_move))=="individual.taxon.canonical.name")) minargdatac <- data.frame(minargdatac[,1:5],"taxon.canonical.name"=idData(alli_move)$individual.taxon.canonical.name,"visible"=minargdatac[,6])
         if (any(names(idData(alli_move))=="taxon.canonical.name")) minargdatac <- data.frame(minargdatac[,1:5],"taxon.canonical.name"=idData(alli_move)$taxon.canonical.name,"visible"=minargdatac[,6])
         alli_move@data <- minargdatac
        }
      }
      all <- c(all,list(alli_move))
    }
    names(all) <- animals
    all <- all[unlist(lapply(all, is.null)==FALSE)] #take out NULL animals
    if (length(all)>0) result <- moveStack(all,forceTz="UTC") else result <- NULL
  } #end of different duplicate removal methods
  
  # Fallback to make sure it is always a moveStack object and not a move object.
  if (is(result,'Move')) {
    result <- moveStack(result,forceTz="UTC")
  }

  # give warning if there are timestamps in the future
  if (length(result)>0)
  {
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
  }
  
  if (exists("data") && !is.null(data)) {
    logger.info("Merging input and result together")
    result <- moveStack(result, data, forceTz="UTC")
  }

  result
}
