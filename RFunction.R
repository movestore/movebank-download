library('move')
library('foreach')
library('dplyr')
library('data.table')
library('lubridate')

rFunction = function(username, password, config_version=NULL, study, animals=NULL, duplicates_handling="first", timestamp_start=NULL, timestamp_end=NULL, thin=FALSE, thin_numb = 1, thin_unit ='hour', minarg=FALSE, incl_outliers=FALSE, select_sensors=NULL, data=NULL, ...) {
  credentials <- movebankLogin(username, password)
  arguments <- list()

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
    
  if (duplicates_handling=="first")
  {
    logger.info("you have (possibly by default) selected to combine your duplicated values to retain the first value of each. Note that this is the fastest option, but might loose some information.")
  }
  if (duplicates_handling=="combi")
  {
    logger.info("you have selected to combine your duplicated values to retain the max amount of information in your data. Note that this might take long, under usual conditions about 3-5 times longer.")
  }
  
  if (incl_outliers==TRUE) logger.info ("You have selected to download also locations marked as outliers previously (e.g. Movebank). Note that this may lead to unexpected results.") else logger.info ("Only data that were not marked as outliers previously are downloaded (default).")
  
  SensorInfo <- getMovebankSensors(login=credentials)
  SensorStudy <- getMovebankSensors(study,login=credentials)
  u_study_sensors <- unique(SensorStudy$sensor_type_id)
  SensorAnimals <- getMovebankAnimals(study,login=credentials)
  names(SensorAnimals) <- make.names(names(SensorAnimals),allow_=FALSE)
  
  sensorIDs <- as.list(unique(SensorAnimals$local.identifier))
  names(sensorIDs) <- unique(SensorAnimals$local.identifier)
  sensors_byID <- lapply(sensorIDs, function(x) SensorAnimals$sensor.type.id[SensorAnimals$local.identifier==x])
  names(sensors_byID) <- names(sensorIDs)
  
  ## for old Apps without sensor selection, here set "select_sensors" parameter to all possible location data sensors of the selected study; no else needed because select_sensors given
  if (is.null(config_version) | config_version==0) select_sensors <- SensorInfo$id[which(SensorInfo$is_location_sensor=="true" & SensorInfo$id %in% u_study_sensors)]
  
  if (is.null(select_sensors))
  {
    logger.info("The selected study does not contain any location sensor data. No data will be downloaded (NULL output) by this App.")
    result <- NULL
  } else if (length(select_sensors)==0)
  {
    logger.info("Either the selected study does not contain any location sensor data or you have deselected all available location sensors. No data will be downloaded (NULL output) by this App.")
    result <- NULL
  } else
  {
    select_sensors_names <- SensorInfo$name[which(SensorInfo$id %in% select_sensors)]
    if (is.null(config_version) | config_version==0) logger.info(paste0("You are using the Movebank App with old version Settings. They do not allow selection of sensor types. Therefore, data of all location sensor types will be downloaded: ", paste(select_sensors_names,collapse=", ") ,". If you want to use the sensor selection option, please completely reconfigure the App.")) else logger.info(paste("You have selected to download locations of these selected sensor types:",paste(select_sensors_names,collapse=", ")))
    
    sensors_byID <- lapply(sensors_byID, function(x) x[which(x %in% select_sensors)])
    
    all <- foreach(animal = animals) %do% {
      arguments["animalName"] = animal
      logger.info(animal)
      
      sensors_animal <- sensors_byID[[which(names(sensors_byID)==animal)]]
      
      if (length(sensors_animal)==0)
      {
        logger.info("There are no data of the required sensor type for this animal.")
        locs <- NULL
      } else 
      {
        arguments[["sensorID"]] <- sensors_animal
        locs <- tryCatch(do.call(getMovebankLocationData, arguments), error = function(e){
          logger.info(e)
          return(NULL)}) #can return NULL if there are no data by this animal
      }
      
      if (is.null(locs) | length(locs[,1])==0) #either not possible to load or empty table
      {
        logger.info("There are no data available for this animal.")
        alli_move <- NULL
      } else
      {
        ###
        # here comes a fix for missing idData of the getMovebankLocationData function (needs to be fixed by the move package, afterwards this part can be taken out again)
        
        iddt <- SensorAnimals[SensorAnimals$local.identifier==animal,]
        ix <- which(names(iddt) %in% names(locs)==FALSE)
        locs <- merge(locs,iddt[,ix],by.x="individual.local.identifier",by.y="local.identifier")
        ###
        
        dupls <- getDuplicatedTimestamps(locs,onlyVisible=FALSE) #this is a list, onlyVisible includes outliers here
        
        if (length(dupls)==0)
        {
          logger.info("No dupliated values in your data set.")
          alli_move <- move(locs)
        } else
        {
          replix <- as.numeric(unlist(lapply(dupls,function(x) x[1]) ))
          remoix <- as.numeric(unlist(lapply(dupls,function(x) x[-1]) ))
          
          if (duplicates_handling=="first")
          {
            logger.info(paste("Data of this track contain",length(dupls),"duplicated timestamps. The first location each will be retained."))
            alli_move <- move(locs[-remoix,])
          }
          
          if (duplicates_handling=="combi")
          {
            logger.info(paste("Data of this track contain",length(dupls),"duplicated timestamps. They will be combined for maximum information."))
            
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
        }
        
        if (thin==TRUE & is.null(alli_move)==FALSE) 
        {
          logger.info(paste("Your data will be thinned as requested to one location per",thin_numb,thin_unit))
          alli_move <- alli_move[!duplicated(round_date(timestamps(alli_move), paste0(thin_numb," ",thin_unit))),]
        }
        
        if (minarg==TRUE & is.null(alli_move)==FALSE)
        {
          logger.info("Your data will be returned with minimum attributes only.")
          minargdatac0 <- as.data.frame(alli_move)
          names(minargdatac0) <- make.names(names(minargdatac0),allow_=FALSE)
          minargdatac <- minargdatac0[,c("timestamp","location.long","location.lat","sensor.type","visible")]
          minargdatac <- data.frame("individual.local.identifier"=namesIndiv(alli_move),minargdatac)
          minargdatac$timestamp <- as.POSIXct(timestamps(alli_move))
          if (any(names(idData(alli_move))=="individual.taxon.canonical.name")) minargdatac <- data.frame(minargdatac[,1:5],"taxon.canonical.name"=idData(alli_move)$individual.taxon.canonical.name,"visible"=minargdatac[,6])
          if (any(names(idData(alli_move))=="taxon.canonical.name")) minargdatac <- data.frame(minargdatac[,1:5],"taxon.canonical.name"=idData(alli_move)$taxon.canonical.name,"visible"=minargdatac[,6])
          alli_move@data <- minargdatac
        }
        #print(alli_move)
        alli_move
      }
    }
    names(all) <- animals
    
    all <- all[unlist(lapply(all, is.null)==FALSE)] #take out NULL animals
    if (length(all)>0) result <- moveStack(all,forceTz="UTC") else result <- NULL
    
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
  } 
  

  if (exists("data") && !is.null(data))
    {
      if (is.null(result))
      {
        result <- data
        logger.info("No data downloaded, but input data returned.")
      } else
      {
        logger.info("Merging input and result together")
        result <- moveStack(result, data, forceTz="UTC")
      }
    }

  return(result)
}
