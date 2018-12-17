# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  MANM354 MACHINE LEARNING & VISULISATION
#
# R version 3.3.3
#
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 14th February 2018
#
# v1.01
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.01      14/2/2018   NPREPROCESSING_discreetNumeric - changed for total of ten bins
#                       Added bin number as x-axis label
# 1.02      26/2/2018   Added NcalcMeasures(), updated NcalcConfusion() to use this.
#                       Updated NPREPROCESSING_splitdataset() with descripition
#                       Updated Npreprocessdataset() with scaleFlag
# 1.03      2/3/2018    Renamed Npreprocessdataset() to NPREPROCESSING_dataset()
#                       Added library(neuralnet) inside NLEARN_BasicNeural()
# 1.04      5/3/2018    Added global constants - although these values should be PASSED
#                       NPREPROCESSING_dataset() added vector manualTypes
#                       Updated all functions to include manualTypes where required
#                       Added NPREPROCESSING_setInitialFieldType()
#                       Added MAX_LITERALS as a constant
#                       Added NPREPROCESSING_removeAllSameValueField()
#                       NPREPROCESSING_categorical() does not convert very sparse fields
#                       Removed N_LEARN_BasicNeural() and put in lab functions source
# 1.05      7/3/2018    NPREPROCESSING_discreetNumeric() updated to show plot for all numeric fields
#                       & title on plot shows if field type manually set or determined by preprocessing
#                       Added NPREPROCESSINGpretty_dataset_table()
# 1.06      12/3/2018   Added Nr2() to calculate r2, catch NA condition
#                       SPLIT_TRAIN as constant for % training dataset
# 1.07      15/4/2018   NPREPROCESSING_discreetNumeric() added y-axis lable
#                       NPREPROCESSING_removeAllSameValueField() fixed bug & better message
#                       NPREPROCESSING_dataset() print "no field type..."
#                       NPREPROCESSING_splitdataset() added parameter splitTrain
#                       Removed global variables
#                       NPREPROCESSING_categorical() added parameter maxLiterals
#                       Added progress bar to PREPROCESSING_discreetNumeric(), NPREPROCESSING_outlier
#                       Fixed NPREPROCESSING_categorical() to maintain correct field names
#                       NPREPROCESSING_redundantFields() updated to not plot over 50 fields
# ************************************************

# Common functions to use in lab 6

# ************************************************
# preprocessdataset() : Run the steps discussed to pre-process a dataset
# INPUT: Frame - dataset to preprocess
#        Bool - true to scale dataset
#        Vector String - optional manual setting for each field
#                        DISCREET, ORDINAL, SYMBOLIC
# OUTPUT : Frame - dataset
# ************************************************
NPREPROCESSING_dataset<-function(dataset, scaleFlag, manualTypes,discreetBins, maxLiterals){

    # ---- Local scope function in NPREPROCESSING_dataset()
    # printAnalysis()
    # Added 150418
    # For each stemmed noun, look up in dictionary and provide original noun(s)
    # INPUT:  dataframe - string original and its stem for every word (so not unique entries)
    #         dataframe -  StemWord text word, Frequency integer count of same stem word
    # OUTPUT: None
    # ----
    printAnalysis<-function(fields,message){
      if (length(fields)==0){
        print(paste("No", message, "fields"))
      } else
      {
        print(paste(message,"Fields:"))
        print(paste(fields,collapse = ","))
      }
    }
    # ----

  #Remove fields if their values are all the same
  #This could remove the entire output field!
  #dataset<-NPREPROCESSING_removeAllSameValueField(dataset)

  # Determine initial field types: NUMERIC or SYMBOLIC
  field_types<-NPREPROCESSING_initialFieldType(dataset,manualTypes)

  numeric_fields<-names(dataset)[(field_types==TYPE_NUMERIC) | (field_types==TYPE_ORDINAL) ]
  printAnalysis(numeric_fields,"Numeric")       #Added 150418

  symbolic_fields<-names(dataset)[field_types==TYPE_SYMBOLIC]
  printAnalysis(symbolic_fields,"Symbolic")      #Added 150418

  #Determine if the numeric fields might be discreet numeric
  #If there are over 3 bins with less than 1% of the values, then the field is
  #marked as a discreet numeric
  field_types<-NPREPROCESSING_discreetNumeric(dataset,field_types,discreetBins,manualTypes)
mvdfmvbvdfkvk
fdkdfgkdfhgdfhk
dfgkdffkghdf
hshsffgdfgd
gfddfgdfgdf
gfdfkhgkdfhgk
sfkssgfsfhksdf
sdfgsdfggksdf
gdfgdfgdf
dfgdfgdfgdf
fasgdsfksgkfgasf
fgdsjfksdfsdfsd
sdkfgdskfgksd
sdfjgsdfgksf
bsdfkgkgfds
sdkgkdsfgksdf
sdfgksdfkkgsdf
dsfgksdfgkksdf
fgsdksfgkdsgkfd
dsfgksdfgksdfkgsdf
dsfkgdsfgksdfsdfsk
dsfgsdfgksdfjksdfgk
  # IF SYMBOLIC TYPES:
  # This function undertakes 1-hot-encoding

  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset,field_types,maxLiterals)

  #Combine the two sets of data that are ready for ML
  #These are now all numeric values
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)

  # Are any of the fields redundant?
  combinedML<-NPREPROCESSING_redundantFields(combinedML,0.95)

  # 150418 removed as this as causes issues
  # Are any of the fields still stuck at a single value?
  # combinedML<-NPREPROCESSING_removeAllSameValueField(combinedML)

  #The dataset for ML information
  print(paste("Total Fields=",ncol(combinedML)))

  # Returns the pre-processed dataset
  return(combinedML)
}

# ************************************************
# NreadDataset() : Read a CSV file from working directory
# INPUT: text - filename
# OUTPUT : Frame - dataset
# ************************************************
NreadDataset<-function(csvFilename){
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)

  #The field names "confuse" some of the library algorithms
  #As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))

  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_removeAllSameValueField
# INPUT: Frame - dataset
# OUTPUT : Frame - dataset with columns removed if all same value
# ************************************************
NPREPROCESSING_removeAllSameValueField<-function(dataset){

  original_columns<-length(names(dataset))
  #The variance of zero returns false - which is handy here
  newdataset<-Filter(var, dataset)
  new_columns<-length(names(newdataset))  #150418 corrected bug here
  if (original_columns!=new_columns){
    #150418 get index of columns removed from dataset so can print these
    indexRemovedColumns<-which(!(names(dataset) %in% names(newdataset)))
    namedRemoved<-paste(names(dataset)[indexRemovedColumns],collapse = ", ")
    print(paste(original_columns-new_columns,"Removed column(s) as values all the same:",namedRemoved))
    return(newdataset)
  }
  return(dataset)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
# INPUT: String - name of field with possible punctuation/spaces
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# ************************************************
# NPREPROCESSING_initialFieldType() : Test each field for NUMERIC or SYNBOLIC
# INPUT: Frame - dataset
#        Vector String - optional manual setting for each field
#                        DISCREET, ORDINAL, SYMBOLIC
#        String - name of the field to manually set
#        String - manual type
# OUTPUT : Vector - List of types per field {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_setInitialFieldType<-function(dataset,manualTypes,name,type){
  manualTypes[which(names(dataset)==name)]=type
  return(manualTypes)
}

# ************************************************
# NPREPROCESSING_initialFieldType() : Test each field for NUMERIC or SYNBOLIC
# INPUT: Frame - dataset
#        Vector String - optional manual setting for each field
#                        DISCREET, ORDINAL, SYMBOLIC
# OUTPUT : Vector - List of types per field {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset,manualTypes){

  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    if (manualTypes[field]==""){
      if (is.numeric(dataset[,field])) {
        field_types[field]<-TYPE_NUMERIC
      }
      else {
        field_types[field]<-TYPE_SYMBOLIC
        }
    } else {
      field_types[field]<-manualTypes[field]
      }
  }
  return(field_types)
}

# ************************************************
# NPREPROCESSING_discreetNumeric() :
# Test NUMERIC field if DISCREET or ORDINAL
# INPUT: Frame - dataset
#        Vector - List of types per field {NUMERIC, SYMBOLIC}
#        int - Number of empty bins needed to determine discreet (1-10)
#        Vector String - optional manual setting for each field
#                        DISCREET, ORDINAL, SYMBOLIC

# OUTPUT : Vector - Updated Llist of types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram equalisation
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff,manualTypes){

  print("NPREPROCESSING_discreetNumeric")
  pb <- txtProgressBar(min = 0, max = ncol(dataset), style = 3)

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

      #Only for fields that are all numeric
      #Does not use field_types as these may be manually overwritten
      if (is.numeric(dataset[,field])) {

        #Scale the whole field (column) to between 0 and 1
        library(plotrix) #Has rescale function in it
        scaled_column<-rescale(dataset[,field],range(0,1))

        #Generate the "cutoff" points for each of 10 bins
        #so we will get 0-0.1, 0.1-0.2...0.9-1.0
        cutpoints<-seq(0,1,length=11)

        #This creates an empty vector that will hold the counts of ther numbers in the bin range
        bins<-vector()

        #Now we count how many numbers fall within the range
        #length(...) is used to count the numbers that fall within the conditional
        for (i in 2:11){
          bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
        }

        # the 10 bins will have a % value of the count (i.e. density)
        bins<-(bins/length(scaled_column))*100.0

        #Only change the field type if it has not been manually set
        if (manualTypes[field]==""){
          graphTitle<-"AUTO:"
          #If the number of bins with less than 1% of the values is greater than the cutoff
          #then the field is deterimed to be a discreet value
          if (length(which(bins<1.0))>cutoff)
            field_types[field]<-TYPE_DISCREET
          else
            field_types[field]<-TYPE_ORDINAL
        } else {
          graphTitle<-"MANUAL:"
          field_types[field]<-manualTypes[field]
        }

        #Bar chart helps visulisation. Type of field is the chart name
        barplot(bins, main=paste(graphTitle,field_types[field]),
                xlab=names(dataset[field]),ylab="Frequency",
                names.arg = 1:10)

      } #endif numeric types
    setTxtProgressBar(pb, field)  #Update the progress bar
    } #endfor
  close(pb)
  return(field_types)
}

# ************************************************
# NPREPROCESSING_categorical() :
# Transform SYMBOLIC or DISCREET fields using 1-hot-encoding
# INPUT: Frame - dataset
#        Vector - List of types per field {ORDINAL, SYMBOLIC,DISCREET}
#        Vector String - optional manual setting for each field
#                        DISCREET, ORDINAL, SYMBOLIC
#
# OUTPUT : Frame - Converted fields with their rows of data
# ************************************************
# Small number of literals only otherwise too many dimensions
# Uses 1-hot-encoding if more than 2 unique literals in the field
# Otherwise converts the 2 literals into one field of {0,1}
# ************************************************
NPREPROCESSING_categorical<-function(dataset,field_types, maxLiterals){

  print("NPREPROCESSING_categorical")
  pb <- txtProgressBar(min = 0, max = ncol(dataset), style = 3)

  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields marked SYMBOLIC or DISCREET
    if ((field_types[field]==TYPE_SYMBOLIC)||(field_types[field]==TYPE_DISCREET)) {

      #Create a list of unique values in the field (each is a literal)
      literals<-as.vector(unique(dataset[,field]))
      numberLiterals<-length(literals)

      #if there are just two literals in the field we can convert to 0 and 1
      if (numberLiterals==2){
        transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
        catagorical<-cbind(catagorical,transformed)
        colnames(catagorical)[ncol(catagorical)]<-colnames(dataset)[field]

      } else
      {
        #We have now to one-hot encoding FOR SMALL NUMBER of literals
        if (numberLiterals<=maxLiterals){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)

            # 5/3/2018 - do not convert the field if their are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            if (literalsActive>log(length(hotEncoding))) {
              catagorical<-cbind(catagorical,hotEncoding)
              colnames(catagorical)[ncol(catagorical)]<-paste(colnames(dataset)[field],NPREPROCESSING_removePunctuation(nameOfLiteral),sep="")
            }
            else {
              print(paste("Ignoring in field:",names(dataset)[field],"Literal:",nameOfLiteral))
            }
          }
        } else {
          stop(paste("Error - too many literals in:",names(dataset)[field], numberLiterals))
        }

      } #endof else 1-hot-encoding
    } #endof if fieldtypes
    setTxtProgressBar(pb, field)  #Update the progress bar
  } #endof for()

  #Remove that first column that was full of NA due to R
  catagorical$first<-NULL  #150418 updated to ensure name of column is not lost

  close(pb)

  return(catagorical)
}

# ************************************************
# NplotOutliers() :
# Scatter plot of field values and colours outliers in red
# INPUT: Vector - sorted -  points to plot as literal values
#        Vector - outliers - list of above points that are considered outliers
#        String - fieldName - name of field to plot
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){

  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName))
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}

# ************************************************
# NPLOT_correlagram() : Plots PLOT_correlagram
# INPUT: Frame - cr - n x n frame of correlation coefficients for all fields
# OUTPUT : None
# ************************************************
NPLOT_correlagram<-function(cr){

  library(corrplot)
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))

  #To fit on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)

  corrplot(cr,method="square",order="FPC",cl.ratio=0.2, cl.align="r",tl.cex = 0.6,cl.cex = 0.6,cl.lim = c(0, 1),mar=c(1,1,1,1))
}

# ************************************************
# NPREPROCESSING_redundantFields() :
# Determine if an entire field is redundant
# INPUT: Frame - dataset
#        float - cutoff - Value above which is determined redundant (0.0-1.0)
# OUTPUT : Frame - dataset with any fields removed
# ************************************************
# Uses LINEAR correlation, so use with care as information will be lost

NPREPROCESSING_redundantFields<-function(dataset,cutoff){

  print(paste("Before redundancy check Fields=",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1

  if (length(xx)>0L)
    dataset<-dataset[,-xx]

  cr<-cor(dataset, use="everything")
  cr[(which(cr<0))]<-0 #Positive correlation coefficients only

  #150418 Updated no plot if matrix too large
  if (nrow(cr)<=50)
    NPLOT_correlagram(cr)

  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]

  if (length(list_fields_correlated)>0){

    print("Following fields are correlated")
    print(list_fields_correlated)

    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])

    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}

# ************************************************
# NPREPROCESSING_outlier() :
# Determine if a value of a record is an outlier for each field
# INPUT: Frame - ordinals - numeric fields only
#        int - confidence - Confidencevalue above which
#                            is determined an outlier (0.0-1.0)
# OUTPUT : Frame - ordinals with any outlier values replaced
#                  with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){

  library(outliers)

  print("NPREPROCESSING_outlier")
  pb <- txtProgressBar(min = 0, max = ncol(ordinals), style = 3)

  for(field in 1:(ncol(ordinals))){
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=confidence)) #Updated 13/5/17 to include library name
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])

    if (length(outliers>0)){
      outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
      sorted<-unique(sort(outliersGone,decreasing=TRUE))
      NplotOutliers(sorted,vector(),colnames(ordinals)[field])
      ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
      }
    setTxtProgressBar(pb, field)  #Update the progress bar
  }
  close(pb)
  return(ordinals)
}

# ************************************************
# Various measures :
# Calculate various confusion matrix measures
# INPUT: TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
# OUTPUT : float - calculated results
# ************************************************

NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}

# ************************************************
# Nrmse() : Calculate the RMSE statistic
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
# OUTPUT : Frame - dataset
# ************************************************
Nrmse<-function(actual_y,y_predicted){

  return(sqrt(mean((actual_y-y_predicted)^2)))
}

# ************************************************
# Calculate the R2 measure
#
# INPUT:      vector - float values for expected values
#             vector - float values of predicted values
# OUTPUT :    float - calculated RMSE
# ************************************************
Nr2<-function(expected,predicted){

  r<-cor(as.vector(expected), as.vector(predicted)) ^ 2
  if (is.na(r)) r<-0

  return (r)
}
# ************************************************
# NcalcMeasures() :
# Calculate measures from values in confusion matrix
# INPUT: int TP, FN, TN, FN
#
# OUTPUT: A list with the following entries:
#        accuracy - float - accuracy measure
#        pgood - float - precision for "good" (values are 1) measure
#        pbad - float - precision for "bad" (values are 1) measure
#        FPR - float - FPR measure
#        TPR - float - FPR measure
#        MCC - float - Matthew's Correlation Coeficient
# ************************************************
NcalcMeasures<-function(TP,FN,TN,FP) {
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "pgood"=NcalcPgood(TP,FP,TN,FN),
                  "pbad"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "MCC"=NcalcMCC(TP,FP,TN,FN)
      )
  return(retList)
  }

# ************************************************
# NcalcConfusion() :
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expected - {0,1}, Expected outcome from each row (labels)
#        vector - predicted - {0,1}, Predicted outcome from each row (labels)
#        vector - actualp - [0,1], predicted real values (scores)
#        float - cost - Set to null
#
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy measure
#        pgood - float - precision for "good" (values are 1) measure
#        pbad - float - precision for "bad" (values are 1) measure
#        FPR - float - FPR measure
#        TPR - float - FPR measure
#        MCC - float - Matthew's Correlation Coeficient
# ************************************************
NcalcConfusion<-function(expected,predicted,actualp,cost){

  #Marked in the dataset, set as "1" for the class we are looking for

  TP<-0 #A fraud transaction was expected and was correctly classified by the decision system.
  FN<-0 #A fraud transaction was expected but was wrongly classified as genuine.
  TN<-0 #A genuine transaction was expected and was correctly classified by the decision system.
  FP<-0 #A genuine transaction was expected and was wrongly classified as fraud.

  for (x in 1:length(predicted)){
    fire<-predicted[x]
    marked<-expected[x]

    toadd<-1L

    if(!is.null(cost))
      toadd<-cost[x]

    #In the case of a POSITIVE
    if (fire==TRUE){
      #A fraud transaction was expected and was correctly classified by the rules
      #TRUE POSITIVE
      if (marked==1.0){
        TP<-TP+toadd
      }
      else
      {
        #A genuine transaction was expected and was wrongly classified as fraud by the rules
        #FALSE POSITIVE
        FP<-FP+toadd
      }
    }
    else {
      #A genuine transaction was expected and was correctly classified by the rules
      #TRUE NEGATIVE
      if (marked==0.0){
        TN<-TN+toadd
      }
fdjgfjdsgfsf
dsfksdgfksdgfk
dfkgdskfgsdkfgks
gsdkfgksdfkgsdk
dfgdskfgksdfk
fdsfksdfdskfhsdkhf
sdfksdfksdfgdsf
dsfksdgfkgsdkf
dsfgksdgfkgsdkfg
sdfgksdfgksfksdgksdf
sdfkgsdfgksdfgkdf
sdfksdfgkdsfgkdsf
dskgfgkdskgdsfkg
dfkdsfgkdsfkgdsf
sdksdfkgsdfkdsf
sdjdsgsdfkgjdsf

jgsdfgjsdfjgfds
dsfjgdsfgjdfjfjgsd
sdfgjsdfgjsdfjg
dshjsdfksadjgsdfj
dfjgsdfjgsdjfgjsd

dsgjdsfhjdsfhjdsf      else
      {
        #A fraud transaction was expected but was wrongly classified as genuine by the rules
        #FALSE NEGATIVE
        FN<-FN+toadd
      }
    }
  }

  RMSE<-round(Nrmse(expected,predicted),digits=2)

  measure<-NcalcMeasures(TP,FN, TN, FP )

  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=measure$accuracy,
                  "pgood"=measure$pgood,
                  "pbad"=measure$pbad,
                  "FPR"=measure$FPR,
                  "TPR"=measure$TPR,
                  "MCC"=measure$MCC,
                  "RMSE"=RMSE,
                  "expected"=expected,
                  "predicted"=predicted,
                  "actualp"=actualp
  )
  return(retList)
}

# ************************************************
# NPREPROCESSING_splitdataset() :
# Randomise and split entire data set
# INPUT: Frame - dataset
#        Int - % size of train dataset (added 150418)
#
# OUTPUT : Frame - test dataset
#          Frame - train dataset
# ************************************************
NPREPROCESSING_splitdataset<-function(combinedML, splitTrain){

# RANDOMISE the dataset

  combinedML<-combinedML[order(runif(nrow(combinedML))),]

  # **** Create a TRAINING dataset using splitTrain of the records
  training_records<-round(nrow(combinedML)*(splitTrain/100))

  train <- 1:training_records
  test <- -train

  training_data <- combinedML[train,]
  testing_data = combinedML[test,]

  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}

# ************************************************
# NPREPROCESSINGpretty_dataset_table() :
# Output simple dataset field analysis results as a table in "Viewer"
# REQUIRES: formattable
#
# INPUT: Frame - dataset
#
# OUTPUT : None
# ************************************************
NPREPROCESSINGpretty_dataset_table<-function(dataset){

  library(formattable)

  tidyTable<-data.frame(Field=names(dataset),Catagorical=FALSE, Symbols=0, Min=0.0, Mean=0.0, Max=0.0,Skew=0.0,stringsAsFactors = FALSE)
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }

  t<-formattable::formattable(tidyTable,list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                                     x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                             Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                             Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                             Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                             Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                             Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
  ))
  print(t)
}


