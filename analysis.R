csvData = read.table("data.csv", sep = ",", header = TRUE);
dataMatrix <- data.frame(
  csvData[,2],
  csvData[,3],
  csvData[,6],
  csvData[,7],
  csvData[,8],
  csvData[,9],
  csvData[,10],
  csvData[,11],
  csvData[,12],
  csvData[,13],
  csvData[,14],
  csvData[,15],
  csvData[,16],
  csvData[,17]
);

plot(dataMatrix[,3], dataMatrix[,6])
correlationMatrixP <- cor(dataMatrix, method="pearson")
correlationMatrixK <- cor(dataMatrix, method="kendall")
correlationMatrixS <- cor(dataMatrix, method="spearman")

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

# Assignment 7
soda <- rbind(c(84,32), c(48,122))
colnames(soda) <- c("No","Yes")
rownames(soda) <- c("No","Yes")
soda <- as.table(soda)

chisq.test(soda, p = pt)$p.value
chisq.test(soda)$expected
