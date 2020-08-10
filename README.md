# CPRD_Additional_Clinical
R scripts and information concerning the retrieval of patient additional clinical data. 

An CPRD (Clinical Practice Research Datalink) data request will usually return a primary care clinic file(s) and corresponding additional clinic file(s). The clinic files hold the patient to GP consultation concerns (a medical diagnosis and/or complaint) along with anything prescribed by the GP and on the NHS (drugs, supports, special foods etc). Particular patient characteristics that may be of a medical concern are recorded by the GP into the additional clinic file and linked back into the clinic file by way of patid, adid and the enttype (event type). Using this link between the two tables, a series of look up files and the medcodes provided by CPRD, a researcher could, for example, decode the complete smoking history, BMI records, family history of a patient, and any blood works of patient. 

However, bringing all of this information together is difficult and there are few, if any, free scripts available to researcher. This R script retrieve patient clinical linked data to a number of patient characteristics e.g., smoking. The R script hasn't been packaged, it's easy to use, and should be relatively easy to expand for a typical R user with a little knowledge on the CPRD framework.

Instructions will follow, but essentially the user calls the getEntityValue() to yield a data frame of clinical and additional clinical records. For example:

```
additionalFiles <- "C:\\Users\\yewro\\Documents\\CPRD_Raw_Data\\head_Extract_Additional"
clinicalFiles <- "C:\\Users\\yewro\\Documents\\CPRD_Raw_Data\\Clinical"
additionalFileList <- list(additional=additionalFiles, clinical=clinicalFiles)
resultDF <- getEntityValue("smoking", additionalFileList, idList)
```
The `idList` is either a list where each entry is a patient ID or it is a vector of patient IDs. 

The returned data frame will include clinical data linked to additional clinical data, as such (IDs removed & all dates fabricated). 

![Image of smoking output](https://github.com/acnash/CPRD_Additional_Clinical/blob/master/smoking.PNG)

Further information on how to use should be forth coming in an F1000 publication. 
