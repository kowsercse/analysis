csvData = read.table("data.csv", sep = ",", header = TRUE);

bad_rows <- which( (csvData$pain == 0) 
       & (csvData$tiredness == 0) 
       & (csvData$nausea == 0) 
       & csvData$depression == 0
       & csvData$anxiety == 0
       & csvData$drowsiness == 0
       & csvData$lackappetite == 0
       & csvData$illness == 0
       & csvData$shortbreath == 0
       & csvData$constipation == 0 
       & csvData$sleepquality == 0 )

data_cleaned <- csvData[-bad_rows,]; 

# write data
write.csv(data_cleaned, file = "cleaned-data.csv",row.names=FALSE)

data_reduced <- data_cleaned[,-c(1:5)] # remove all columns before "pain"
data_reduced <- data_reduced[,-c(13:21)] # remove all columns starting from "maxpain"

# covert data frame to matrix
dataMatrix <- data.matrix(data_reduced)

#
correlationMatrixP <- cor(dataMatrix, method="pearson")
correlationMatrixK <- cor(dataMatrix, method="kendall")
correlationMatrixS <- cor(dataMatrix, method="spearman")

# group by location
cleanDataBangladesh <- csvData[which(data_cleaned$location=="Bangladesh"),]
cleanDataUsa <- csvData[which(data_cleaned$location=="South Dakota, USA"),]
cleanDataNepal <- csvData[which(data_cleaned$location=="Nepal"),]

#ranking 
ndx <- order(correlationMatrixP$pain, decreasing = T)[1:5]


aboveThreshold <- function(sourceMatrix, threshold) {
  totalRow <- nrow(sourceMatrix);
  totalCol <- ncol(sourceMatrix);
  resultMatrix <- matrix(0, nrow=totalRow, ncol = totalCol)
  for (row in 2:totalRow) {
    for (col in 2:totalCol) {
      if(!is.na(sourceMatrix[row, col]) && sourceMatrix[row, col] > threshold) {
        resultMatrix[row, col] = 1
      }
    }
  }
  
  return (resultMatrix)
}

thresholdMatrixK <- aboveThreshold(correlationMatrixK, 0.5)
thresholdMatrixP <- aboveThreshold(correlationMatrixP, 0.5)
thresholdMatrixS <- aboveThreshold(correlationMatrixS, 0.5)

write.csv(thresholdMatrixK, "thresholdMatrixK.csv")
write.csv(thresholdMatrixP, "thresholdMatrixP.csv")
write.csv(thresholdMatrixS, "thresholdMatrixS.csv")




excelData = read.table("/Users/kowser/Google Drive/study/course/DataMining/assignment/hw2/Data.csv", sep = ",", header = TRUE);

totalRow <- nrow(excelData);
totalColumn <- ncol(excelData);
matrix <- excelData[1:totalRow,2:totalColumn]

# Assignment 1
covarienceMatrix = cov(matrix)
write.table(covarienceMatrix, file="/Users/kowser/Google Drive/study/course/DataMining/assignment/hw2/cov.csv", sep = ",")

# Assignment 2
medianMatrix <- matrix(1:16, nrow=16, ncol = 20)
for (column in 1:20) {
  columnVector <- (matrix[,column]);
  for (i in 0:3) {
    vectorMedian <- median(columnVector[(4*i+1):(4*i+4)])
    for(row in (4*i+1):(4*i+4)) {
      medianMatrix[row, column] <- vectorMedian
    }
  }
}
covarienceMedianMatrix = cov(medianMatrix)
write.table(covarienceMedianMatrix, file="/Users/kowser/Google Drive/study/course/DataMining/assignment/hw2/med.csv", sep = ",")


# Assignment 3
zNormalizedMatrix <- scale(matrix)
zConvariencedMatrix <- cov(zNormalizedMatrix) 
write.table(zConvariencedMatrix, file="/Users/kowser/Google Drive/study/course/DataMining/assignment/hw2/zcov.csv", sep = ",")

diffZCovarience <- zConvariencedMatrix - covarienceMatrix

# Assignment 4
minMaxNormalize <- function(x) {
  ranx <- range(x, na.rm = TRUE)
  (x - ranx[1]) / diff(ranx)
}

minMaxMatrix <- apply(matrix[], 2, minMaxNormalize)
minMaxCovarienceMatrix <-cov(minMaxMatrix)
write.table(minMaxCovarienceMatrix, file="/Users/kowser/Google Drive/study/course/DataMining/assignment/hw2/mmcov.csv", sep = ",")

diffMinMax1 <- minMaxCovarienceMatrix - covarienceMatrix
diffMinMax2 <- minMaxCovarienceMatrix - zConvariencedMatrix


# Assignment 6
discretize <- function(x, threshold) {
  m <- mean(x)  
  for (i in 1:length(x)){
    if (x[i] > threshold * m){
      x[i] <- 1
    }else if (x[i] < m/threshold){
      x[i] <- -1
    }else{
      x[i] <- 0
    }
  }  
  x
}

median(matrix[,3])
for (i in 1:16){
  print(discretize(matrix[,i],1.01))
}

