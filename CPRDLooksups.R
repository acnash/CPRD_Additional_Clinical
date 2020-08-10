#CPRD Additional Clinical Data extraction script
#By: Anthony Nash PhD
#University of Oxford, Department of Clinical Neuroscience
#National Institute for Health Research Oxford Health Biomedical Research Centre
#(grant BRC-1215-20005). The views expressed are those of the authors and not
#neceessarily those of the UK National Health Service, the NIHR, or the
#UK Department of Health. 


BMI <- 13
smoking <- 4
alcoholConsumption <- 5
hdlRatio <- 338
ethnicity <- 496
familyHistoryOf <- 87
exercise <- 30

#A list of search terms. Caps sensitive at the moment. 
outputCurrentOutput <- function() {
  print("BMI")
  print("smoking")
  print("alcoholConsumption")
  print("hdlRatio")
  print("ethnicity")
  print("familyHistoryOf")
  print("exercise")
}

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


#' Returns the additional clinical data for a list of patients. 
#'
#' The function combines the entity, lookup data and adid from additional clinical data
#' with the medical codes from the clinical records. All is returned as a coded data frame
#' for each patient along all eventdate times.  
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
  
  if(entityString == "smoking") {
    resultDF <- getSmokingData(idList, additionalClinicalDataDF, clinicalDataDF) 
  } else {
    print("Unrecognised entity type. Try one of the following:")
    outputCurrentOutput()
    return(NULL)
  }
  
  return(resultDF)

}

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
