knn <- function(data, k, corMethod = "pearson", ...) {
        
        mCor <- list()
        mCor$x <- cor(data$x, method = corMethod)
        mCor$y <- cor(data$y, method = corMethod)
        
        dataPredict <- list()    
        dataPredict$x <- data$x
        dataPredict$y <- data$y
        
        x <- as.matrix(data$x)
        y <- as.matrix(data$y)
        
        pointsNumber <- dim(as.matrix(data$x))[1]
        
        for (i in 1:dim(data$x)[2]) {
                
                neighbor <- which(mCor$x[i,] %in% sort(mCor$x[i,], decreasing = TRUE)[c(2:k+1)])
                
                numerator <- 0
                denumerator <- 0
                meani <- mean(x[-pointsNumber,i])
                
                for (j in neighbor) {
                        numerator <- numerator + mCor$x[i, j] * (x[pointsNumber, j] - mean(x[, j]))
                        denumerator <- denumerator + abs(mCor$x[i, j])
                }
                
                dataPredict$x[pointsNumber, i] <- meani + numerator / denumerator
        }
        
        for (i in 1:dim(data$y)[2]) {
                
                neighbor <- which(mCor$y[i,] %in% sort(mCor$y[i,], decreasing = TRUE)[c(2:j+1)])
                
                numerator <- 0
                denumerator <- 0
                meani <- mean(y[-pointsNumber,i])
                
                for (j in neighbor) {
                        numerator <- numerator + mCor$y[i, j] * (y[pointsNumber, j] - mean(y[, j]))
                        denumerator <- denumerator + abs(mCor$y[i, j])
                }
                
                dataPredict$y[pointsNumber, i] <- meani + numerator / denumerator
        }
        
        return(list(x = dataPredict$x, y = dataPredict$y))
}
