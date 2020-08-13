#CPRD Additional Clinical Data extraction script
#By: Anthony Nash PhD
#University of Oxford, Department of Clinical Neuroscience
#National Institute for Health Research Oxford Health Biomedical Research Centre
#(grant BRC-1215-20005). The views expressed are those of the authors and not
#neceessarily those of the UK National Health Service, the NIHR, or the
#UK Department of Health. 

alcoholMedcodeDescriptionList <- list(
  `Alcohol consumption`=27,
  `non-drinker alcohol`=4447,
  `Social drinker`=956,
  `Drinks occasionally`=749,
  `Drinks rarely`=385,
  `Tee totaller`=12949,
  `Light drinker - 1-2u/day`=12972,
  `Trivial drinkers - <1u/day`=12975,
  `Non drinker alcohol`=12970,
  `Moderate drinker - 3-6u/day`=322,
  `Alcohol consumption unknown`=12978,
  `Current non-drinker`=12979,
  `Suspect alcohol abuse - denied`=12976,
  `Heavy drinker - 7-9u/day`=1618,
  `Alcohol intake above recommended sensible limits`=12989,
  `Spirit drinker`=12971,
  `Alcohol units per week`=93415,
  `Alcohol screen - AUDIT completed`=95663,
  `Alcohol use disorders identification test`=94838,
  `Light drinker`=12980,
  `Beer drinker`=2689,
  `Drinks wine`=12969,
  `Binge drinker`=19401,
  `Alcohol intake within recommended sensible limits`=26472,
  `Alcohol problem drinker`=1399,
  `Moderate drinker`=12985,
  `Alcohol scren - fast alcohol screening test completed`=94963,
  `Ex-light drinker - (1-2u/day)`=26471,
  `Hazardous alcohol use`=19494,
  `Health ed. - alcohol`=11491,
  `Ex-heavy drinker (>9u/day)`=12983,
  `Alcohol consumption NOS`=12981,
  `Alcohol consumption screen`=9264,
  `Alcohol units consumed on heaviest drinking day`=97126,
  `Heavy drinker`=8999,
  `Self-help advice leaflet given`=9570,
  `Nondependent alcohol abuse`=7746,
  `Alcohol screen - AUDIT completed` = 90714
)

bmiMedcodeDescriptionList <- list(
  `O/E - weight`=2,
  `O/E - underweight`=126,
  `O/E - overweight`=2839,
  `O/E - obese`=7984,
  `Body Mass Index`=8105,
  `O/E - weight 10-20% over ideal`=16404,
  `O/E - weight NOS`=21520,
  `O/E - weight within 10% ideal`=23376,
  `O/E - weight 10-20% below ideal`=29029,
  `O/E - weight > 20% over ideal`=32973
)

smokingMedcodeDescriptionList <- list(
  `Health ed. - smoking`=2111,
  `Smoker`=1823,
  `Ex smoker`=90,
  `Cigarette smoker`=93,
  `Never smoked tobacco`=33,
  `Non-smoker`=11788,
  `Tobacco consumption`=54,
  `Ex-moderate smoker (10-19/day)`=12955,
  `Current smoker`=10558,
  `Stopped smoking`=776,
  `Heavy smoker - 20-39 cigs/day`=3568,
  `Moderate smoker - 10-19 cigs/d`=1878,
  `Ex-heavy smoker (20-39/day)`=12956,
  `Smoking cessation advice`=7622,
  `Trivial smoker - < 1 cig/day`=12958,
  `Light smoker - 1-9 cigs/day`=12944,
  `Ex-smoker - amount unknown`=12946,
  `Very heavy smoker - 40+cigs/d`=1822,
  `Occasional smoker`=12941,
  `Trying to give up smoking`=12240,
  `Ex-light smoker (1-9/day)`=12957,
  `Rolls own cigarettes`=12945,
  `Recently stopped smoking`=99838,
  `Date ceased smoking`=12878,
  `Smoking restarted`=12951,
  `Smoker - amount smoked`=12942,
  `Cigar smoker`=12943,
  `Not interested in stopping smoking`=30762,
  `Referral for smoking cessation service offered`=102361,
  `Brief intervention for smoking cessation`=98137,
  `Lifestyle advice regarding smoking`=18926,
  `Smoking reduced`=12966,
  `Ex-very heavy smoker (40+/day)`=12959,
  `Keeps trying to stop smoking`=12964,
  `Ready to stop smoking`=31114,
  `Thinking about stopping smoking`=30423,
  `Cigarette consumption`=12965,
  `Tobacco consumption unknown`=12962,
  `Negotiated date for cessation of smoking`=34126,
  `Pipe smoker`=12947,
  `Ex-cigarette smoker`=97210,
  `Ex cigar smoker`=19488,
  `Ex-trivial smoker (<1/day)`=12961,
  `Stop smoking monitor.chck done`=19485,
  `Passive smoking risk`=13350, 
  `Tobacco consumption NOS`=12960,
  `Passive smoker`=13351
)


BMI <- 13
smoking <- 4
alcoholConsumption <- 5
hdlRatio <- 338
ethnicity <- 496
familyHistoryOf <- 87
exercise <- 30

#A list of search terms. Caps sensitive at the moment. 
outputCurrentOutput <- function() {
  print("BMI") #completed
  print("smoking") #completed
  print("alcoholConsumption")
  print("hdlRatio")
  print("ethnicity")
  print("familyHistoryOf")
  print("exercise")
}

#===============================================================================
#' Loads in the additional CPRD date (e.g., smoking) and returns a data frame
#' from the additional clinical flat files for those patients requested. This function
#' returns what data is available for those patients specified using the idList
#' argument. 
#'
#' @param filePathVector A string vector to the folder only containing the additional 
#' flat files from CPRD.
#' @param idList A list or vector of patient IDs. To return data for all patients enter NULL.
#'
#' @return A data frame with the columns: patid, enttype, adid, data1, data2, data3,
#' data4, data5, data6, data7. Will return NULL if no patients were identified.
#' @export
#'
#' @examples
getAdditionalCPRDData <- function(filePathVector, idList=NULL) {
  print("Opening additional CPRD data files.")
  fileDFList <- list()
  listOfFiles <- list.files(filePathVector)
  counter <- 1
  for(i in 1:length(listOfFiles)) {
    fileName <- paste0(filePathVector,"\\",listOfFiles[[i]])
    print(paste0("Reading in file", fileName))
    if(i==1) {
      tempDF <- read.csv(fileName, header=TRUE, sep="\t")
      headers <- names(tempDF)
      if(!is.null(idList)) {
        tempDF <- subset(tempDF, tempDF$patid %in% idList)
      }
    } else {
      tempDF <- read.csv(fileName, header=FALSE, sep="\t", col.names = headers)
      if(!is.null(idList)) {
        tempDF <- subset(tempDF, tempDF$patid %in% idList)
      }
    }
    if(nrow(tempDF) > 0) {
      fileDFList[[counter]] <- tempDF
      counter <- counter + 1
    }
  }
  if(length(fileDFList)==0) {
    print("No patients identified. Returning NULL.")
    return(NULL)
  } else if(counter == 1) {
    return(fileDFList[[1]])
  }
  df <- do.call(rbind, fileDFList)
  
  return(df)
}

#===============================================================================
#' Return a data frame of all CPRD clinical data trimmed for the purpose of additional
#' data (e.g., smoking) and filtered by patients of interest. 
#'
#' @param filePathVector A string vector to the folder only containing the clinical 
#' flat files from CPRD.
#' @param idList A list or vector of patient IDs. To return data for all patients enter NULL.
#'
#' @return A data frame for all clinical data from the idList. The columns returned are: 
#' patid, eventdate, medcode, enttype and adid.
#' @export
#'
#' @examples
getClinicalData <- function(filePathVector, idList=NULL) {
  print("Opening clinical CPRD data files.")
  fileDFList <- list()
  listOfFiles <- list.files(filePathVector)
  counter <- 1
  for(i in 1:length(listOfFiles)) {
    fileName <- paste0(filePathVector,"\\",listOfFiles[[i]])
    print(paste0("Reading in file", fileName))
    if(i==1) {
      tempDF <- read.csv(fileName, header=TRUE, sep="\t")
      headers <- names(tempDF)
      if(!is.null(idList)) {
        tempDF <- subset(tempDF, tempDF$patid %in% idList)
      }
    } else {
      tempDF <- read.csv(fileName, header=FALSE, sep="\t", col.names = headers)
      if(!is.null(idList)) {
        tempDF <- subset(tempDF, tempDF$patid %in% idList)
      }
    } 
    if(nrow(tempDF) > 0) {
      fileDFList[[counter]] <- tempDF
      counter <- counter + 1
    }
  }
  #every single patient
  if(length(fileDFList) > 1) {
    df <- do.call(rbind, fileDFList)
  } else if(length(fileDFList)==1) {
    df <- fileDFList[[1]]
  } else {
    print("No patients identified. Returning NULL.")
    return(NULL)
  }
  
  #make the eventdate character into a Date and then change the format
  df$eventdate <- as.character(as.Date(df$eventdate, format = "%d/%m/%Y"))
  
  return(df)
}

#===============================================================================
#' This returns the additional information data frame given a patient list, the 
#' additional information entity type and the additional information data frame. 
#' 
#' This is not to be called by the user. 
#'
#' @param idList A vector or list of patient ids.
#' @param entity An integer which represents the entity e.g., smoking <- 4.
#' @param additionalDataDF A data frame of the CPRD data frame.
#'
#' @return A patient data frame of additional information associated with the 
#' specified entity request.
#' @export
#'
#' @examples
getEntityData <- function(idList, entity, additionalDataDF) {
  #gets out the smoking data from the additional data frames
  entityDF <- subset(additionalDataDF, additionalDataDF$enttype==entity)
  #then filters by those patients available
  patientDF <- subset(entityDF, entityDF$patid %in% idList)
  return(patientDF)
}

#===============================================================================
#' Returns the patient smoking status for patients. This looks for the value in data1
#' column of the returned Additional data. 
#' 
#' This is not to be called by the user. 
#' 
#' The smoking status is of a value defined in the YND.txt file. The values are:
#' 0 - data not entered, 1 - YES, 2 - NO, 3 - Ex smoker.  As CPRD is longitudinal, 
#' a patient can have multiple values and is matched by the adid value between data. In most
#' cases (for smoking) the entity == 4 smoking data has an equal number of clinical and additional
#' clinical records. If there are fewer additional clinical records to clincal record, then the adid 
#' values are matched and the returning data will have the number of rows matching the additional
#' clinical data and not the clinical data. If there is no clinical or additional 
#' clinical data then that patient is skipped.
#'
#' @param idList A vector or a list of patient ids. 
#' @param additionalDataDF The data frame of all patient clinical additional data. 
#' @param clinicalDataDF The data frame of all patient clinical data.
#'
#' @return A data frame of the concatenated patient clinical data with the corresponding 
#' smoking additional clinical data.
#' @export
#'
#' @examples
getSmokingData <- function(idList, additionalDataDF, clinicalDataDF) {
  #gets all the smoking entity lines from the additional clinical data
  patientSmokingDT <- data.table::as.data.table(getEntityData(idList, smoking, additionalDataDF))
  
  patientSmokingPatid <- getUniquePatidList(patientSmokingDT)
  
  smokingStatusVector <- patientSmokingDT$data1
  colnames(patientSmokingDT) <- c("patid","enttype","adid","Smoking","Cigs_per_day","Cigars_per_day","ounces_tobacco_per_day","startdate","stopdate","data7")
  patientSmokingDT$Smoking <- sapply(smokingStatusVector, function(x) {
    if(x==0) {
      "Data not entered"
    } else if(x==1) {
      "Yes"
    } else if(x==2) {
      "No"
    } else if(x==3) {
      "Ex smoker"
    }
  })
  
  #data1 column for the smoking status
  patientSmokingMatrix <- trimws(as.matrix(patientSmokingDT))
  cliniclDataMatrix <- trimws(as.matrix(clinicalDataDF))
  counter <- 1
  smokingDFList <- list()
  for(i in 1:length(patientSmokingPatid)) {
    indSmokingMatrix <- patientSmokingMatrix[patientSmokingMatrix[,c("patid")]==patientSmokingPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- cliniclDataMatrix[cliniclDataMatrix[,c("patid")]==patientSmokingPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- indClinicalMatrix[indClinicalMatrix[,c("enttype")] == "4", ,drop=FALSE]
    
    if(nrow(indSmokingMatrix)==0) {
      print(paste("There is no additional clinical data for patient", patientSmokingPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix)==0) {
      print(paste("There is no clinical data for patient", patientSmokingPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix) != nrow(indSmokingMatrix)) {
      adidSubset <- indClinicalMatrix$adid
      indSmokingMatrix <- indSmokingMatrix[indSmokingMatrix[,c("adid")] %in% adidSubset, ]
    }
    
    tempIndClinicalDF <- as.data.frame(indClinicalMatrix)
    tempSmokingDF <- as.data.frame(indSmokingMatrix[,!(colnames(indSmokingMatrix) %in% c("patid","enttype","adid")),drop=FALSE])
    
    smokingDF <- cbind(tempIndClinicalDF, tempSmokingDF)
    
    smokingDFList[[counter]] <- smokingDF
    counter <- counter + 1
  }
  patientSmokingDF <- do.call(rbind, smokingDFList)
  return(patientSmokingDF)
}

#===============================================================================
#' Returns the patient BMI status and returns a weight in kilos, weight centile and a BMI.
#' 
#' The BMI/weight additional clinical data is combined with the clinical data of all those
#' patients with a valid BMI/weight recording. The BMI/weight record is inline with the
#' associated clinical data, therefore when the recording was taken is associated with
#' a clinical event date. The associated clinical entry for a BMI reading is a medcode = 2 for
#' "O/E - weight". 
#'
#' @param idList A vector or a list of patient ids.
#' @param additionalDataDF The data frame of all patient clinical additional data. 
#' @param clinicalDataDF The data frame of all patient clinical data.
#'
#' @return A data frame of the patient clinical data associated with weight along with 
#' weight and BMI measurements. 
#' @export
#'
#' @examples
getBMIData <- function(idList, additionalDataDF, clinicalDataDF) {
  #gets all the BMI related entity lines from the additional clinical data
  patientBMIDT <- data.table::as.data.table(getEntityData(idList, BMI, additionalDataDF))
  
  patientBMIPatid <- getUniquePatidList(patientBMIDT)
  
  colnames(patientBMIDT) <- c("patid","enttype","adid","Weight_in_kilos","Weight_centile","BMI","data4","data5","data6","data7")
  
  #data1 column for the smoking status
  patientBMIMatrix <- trimws(as.matrix(patientBMIDT))
  cliniclDataMatrix <- trimws(as.matrix(clinicalDataDF))
  counter <- 1
  bmiDFList <- list()
  for(i in 1:length(patientBMIPatid)) {
    indBMIMatrix <- patientBMIMatrix[patientBMIMatrix[,c("patid")]==patientBMIPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- cliniclDataMatrix[cliniclDataMatrix[,c("patid")]==patientBMIPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- indClinicalMatrix[indClinicalMatrix[,c("enttype")] == "13", ,drop=FALSE]
    
    if(nrow(indBMIMatrix)==0) {
      print(paste("There is no additional clinical data for patient", patientBMIPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix)==0) {
      print(paste("There is no clinical data for patient", patientBMIPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix) != nrow(indBMIMatrix)) {
      adidSubset <- indClinicalMatrix$adid
      indBMIMatrix <- indBMIMatrix[indBMIMatrix[,c("adid")] %in% adidSubset, ]
    }
    
    tempIndClinicalDF <- as.data.frame(indClinicalMatrix)
    tempBMIDF <- as.data.frame(indBMIMatrix[,!(colnames(indBMIMatrix) %in% c("patid","enttype","adid")),drop=FALSE])
    
    bmiDF <- cbind(tempIndClinicalDF, tempBMIDF)
    
    bmiDFList[[counter]] <- bmiDF
    counter <- counter + 1
  }
  patientBMIDF <- do.call(rbind, bmiDFList)
  return(patientBMIDF)
}

#===============================================================================
#' Returns the patient alcohol consumption status, units per week and start/stop dates.
#' 
#' The alcohol consumption additional clinical data is combined with the clinical data of all those
#' patients with a valid alcohol consumption clinical record. The alcohol consumption record is inline with the
#' associated clinical data, therefore when the recording was taken it is associated with
#' a clinical event date. The associated clinical entry for an alcohol consumption reading is a medcode = 27 for
#' "alcohol consumption". 
#' 
#' There is an occasional crossover between clinical data and addication clinical data. For example, 
#' a clinical record for non-smoker (medcode 60) may accompany an additional clinical data entity for
#' alcohol consumption "No". 
#'
#' @param idList A vector or a list of patient ids.
#' @param additionalDataDF The data frame of all patient clinical additional data. 
#' @param clinicalDataDF The data frame of all patient clinical data.
#'
#' @return A data frame of the patient clinical data associated with alcohol consumption along with 
#' units per day and start and stop date.
#' @export
#'
#' @examples
getAlcoholConsumptionData <- function(idList, additionalDataDF, clinicalDataDF) {
  #gets all the BMI related entity lines from the additional clinical data
  patientAcDT <- data.table::as.data.table(getEntityData(idList, alcoholConsumption, additionalDataDF))
  
  patientAcPatid <- getUniquePatidList(patientAcDT)
  
  colnames(patientAcDT) <- c("patid","enttype","adid","Alcohol_consumption","units_per_week","start_date","stop_date","data5","data6","data7")
  patientAcDT$Alcohol_consumption[patientAcDT$Alcohol_consumption == "0"] <- "Data not entered"
  patientAcDT$Alcohol_consumption[patientAcDT$Alcohol_consumption == "1"] <- "Yes"
  patientAcDT$Alcohol_consumption[patientAcDT$Alcohol_consumption == "2"] <- "No"
  patientAcDT$Alcohol_consumption[patientAcDT$Alcohol_consumption == "3"] <- "Ex drinker"
  
  #data1 column for the smoking status
  patientAcMatrix <- trimws(as.matrix(patientAcDT))
  cliniclDataMatrix <- trimws(as.matrix(clinicalDataDF))
  counter <- 1
  acDFList <- list()
  for(i in 1:length(patientAcPatid)) {
    indAcMatrix <- patientAcMatrix[patientAcMatrix[,c("patid")]==patientAcPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- cliniclDataMatrix[cliniclDataMatrix[,c("patid")]==patientAcPatid[[i]],,drop=FALSE]
    indClinicalMatrix <- indClinicalMatrix[indClinicalMatrix[,c("enttype")] == "5", ,drop=FALSE]
    
    if(nrow(indAcMatrix)==0) {
      print(paste("There is no additional clinical data for patient", patientAcPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix)==0) {
      print(paste("There is no clinical data for patient", patientAcPatid[[i]], "Skipping this patient."))
      next()
    }
    
    if(nrow(indClinicalMatrix) != nrow(indAcMatrix)) {
      adidSubset <- indClinicalMatrix$adid
      indAcMatrix <- indAcMatrix[indAcMatrix[,c("adid")] %in% adidSubset, ]
    }
    
    tempIndClinicalDF <- as.data.frame(indClinicalMatrix)
    tempAcDF <- as.data.frame(indAcMatrix[,!(colnames(indAcMatrix) %in% c("patid","enttype","adid")),drop=FALSE])
    
    acDF <- cbind(tempIndClinicalDF, tempAcDF)
    
    acDFList[[counter]] <- acDF
    counter <- counter + 1
  }
  patientAcDF <- do.call(rbind, acDFList)
  return(patientAcDF)
}

#===============================================================================
#' Returns the additional clinical data for a list of patients. 
#'
#' The function combines the entity, lookup data and adid from additional clinical data
#' with the medical codes from the clinical records. All is returned as a coded data frame
#' for each patient along all eventdate times.  
#' 
#' To view the latest lookup entity strings execute the function outputCurrentOutput()
#'
#' @param entityString To indicate the patient characteristic entity e.g., "smoking". A list can be 
#' found at the top of this R script. This version only looks for one entity per function call.
#' @param additionalFileList List of folder locations. There must only be two, the
#' additional clinical file folder location and the clinical file folder location. The list elements
#' must be labeled in order: "additional" and "clinical". The algorithm 
#' will load in all files in each folder, make sure they are only CPRD data. If this is 
#' not of length==2, this function will fair and return NULL. 
#' @param idList A list or vector of patient IDs. 
#'
#' @return A data frame of all corresponding clinical events with additional clinical information. 
#' @export
#'
#' @examples
#' additionalFileList <- list(additional=additionalFiles, clinical=clinicalFiles)
#' idList <- c(getUniquePatidList(clinicalTriptanDF))
#' resultDF <- getEntityValue("smoking", additionalFileList, idList)
getEntityValue <- function(entityString, additionalFileList, idList=NULL) {
  if(is.null(additionalFileList)) {
    print("Error: additionalFileList parameter of function getEntityValue is null. Returning NULL.")
    return(NULL)
  }
  
  if(length(additionalFileList) != 2) {
    print("Error: The additionalFileList must contain two entries. the folder for additional clinical files
          and the folder for clinical files.")
  }
  
  clinicalDataDF <- getClinicalData(additionalFileList$clinical, idList)
  additionalClinicalDataDF <- getAdditionalCPRDData(additionalFileList$additional, idList)
  
  if(is.null(clinicalDataDF) | is.null(additionalClinicalDataDF) == TRUE) {
    print("Could not find any clinical or additional clinical data for the patients. Returning NULL.")
    return(NULL)
  }
  
  if(tolower(entityString) == "smoking") {
    resultDF <- getSmokingData(idList, additionalClinicalDataDF, clinicalDataDF) 
    resultDF <- addMedcodeDescription(resultDF, smokingMedcodeDescriptionList)
  } else if(tolower(entityString) == "bmi") {
    resultDF <- getBMIData(idList, additionalClinicalDataDF, clinicalDataDF) 
    resultDF <- addMedcodeDescription(resultDF, bmiMedcodeDescriptionList)
  } else if(tolower(entityString) == "alcoholconsumption") {
    resultDF <- getAlcoholConsumptionData(idList, additionalClinicalDataDF, clinicalDataDF) 
    resultDF <- addMedcodeDescription(resultDF, alcoholMedcodeDescriptionList)
  }
  else {
    print("Unrecognised entity type. Try one of the following:")
    outputCurrentOutput()
    return(NULL)
  }
  
  return(resultDF)

}


#===============================================================================
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
getUniquePatidList <- function(df) {
  uniquePatIDList <- as.list(unique(df$patid))
  checkForNA <- length(which(is.na(uniquePatIDList)))
  if(checkForNA > 0) {
    print(which(is.na(uniquePatIDList)))
    print(paste("Number of NA entries:", checkForNA))
  } else {
    names(uniquePatIDList) <- unique(df$patid)
  }

  return(uniquePatIDList)
}

#===============================================================================

#' Title
#'
#' @param dataDF 
#' @param descriptionList 
#'
#' @return
#' @export
#'
#' @examples
addMedcodeDescription <- function(dataDF, descriptionList) {
  medcodeVector <- dataDF$medcode
  descriptionVector <- rep("",length(medcodeVector))
  
  for(i in 1:length(descriptionVector)) {
    medcode <- medcodeVector[i]
    descriptionSubset <- descriptionList[descriptionList %in% medcode]
    if( length(descriptionSubset)==1) {
      descriptionNames <- names(descriptionSubset)
      descriptionVector[i] <- descriptionNames
    }
  }
  
  dataDF <- cbind(dataDF, medcode_description=descriptionVector)
  return(dataDF)
}
