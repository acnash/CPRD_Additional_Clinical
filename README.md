# CPRD_Additional_Clinical
R scripts and information concerning the retrieval of patient additional clinical data. 

An CPRD (Clinical Practice Research Datalink) data request will usually return a primary care clinic file(s) and corresponding additional clinic file(s). The clinic files hold the patient to GP consultation concerns (a medical diagnosis and/or complaint) along with anything prescribed by the GP and on the NHS (drugs, supports, special foods etc). Particular patient characteristics that may be of a medical concern are recorded by the GP into the additional clinic file and linked back into the clinic file by way of patid, adid and the enttype (event type). Using this link between the two tables, a series of look up files and the medcodes provided by CPRD, a researcher could, for example, decode the complete smoking history, BMI records, family history of a patient, and any blood works of patient. 

However, bringing all of this information together is difficult and there are few, if any, free scripts available to researcher. This R script retrieve patient clinical linked data to a number of patient characteristics e.g., smoking. The R script hasn't been packaged, it's easy to use, and should be relatively easy to expand for a typical R user with a little knowledge on the CPRD framework.

The code is very simple to use. Once the R file is sourced, the user calls the getEntityValue() per look up entity to yield a data frame of clinical and additional clinical records. The supported lookp entites are currently:
1) "smoking"
2) "BMI"
3) "alcoholConsumption"

For example, when passing "smoking":

```
source("CPRDLookups.R")
additionalFiles <- "C:\\Users\\yewro\\Documents\\CPRD_Raw_Data\\head_Extract_Additional"
clinicalFiles <- "C:\\Users\\yewro\\Documents\\CPRD_Raw_Data\\Clinical"
additionalFileList <- list(additional=additionalFiles, clinical=clinicalFiles)
resultDF <- getEntityValue("smoking", additionalFileList, idList)
```
The `idList` is a list or vector of CPRD patient IDs (patid). The code loads the CPRD clinical and CPRD additional clinical txt files.  

The returned data frame will include clinical data linked to additional clinical data. For example, the output for a "smoking" lookup entity might look similar to (IDs removed & all dates fabricated):

![Image of smoking output](https://github.com/acnash/CPRD_Additional_Clinical/blob/master/smoking.PNG)

The retrieved medcodes are assigned a description related to the lookup for interest. For example, if "alcoholConsumption" is entered, the description "Moderate drinker" is assigned to any patient records with the medcode of 12985. As I find more medcodes related to each lookup they will be added. Interestingly, I am finding cross overs between conditions. For example, there are records where a GP has recoded "Non-smoker" in the additional clinical information at the same time as recorded "Non-drinker alcohol" on the patient clinical record. These entries, and anything else that isn't matching the additional clinical look up, are left blank in the "medcode_description" column. 

The lists of medcode descriptions are loaded into the environment when the R code is sourced. They can be viewed by executing the following on the R console:
```
alcoholMedcodeDescriptionList
bmiMedcodeDescriptionList
smokingMedcodeDescriptionList
```



Further information on how to use should be forth coming in a F1000 publication. 
