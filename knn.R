knn <- function(x, y, xCor, yCor, k, ...) {

    dataPredict <- list()    
    dataPredict$x <- x
    dataPredict$y <- y
    
    x <- as.matrix(x)
    y <- as.matrix(y)
    
    nRow <- dim(x)[1]
    nCol <- dim(x)[2]
    
    pointsNumber <- dim(as.matrix(x))[1]
    
    for (i in 1:nCol) {
        
        neighbor <- which(xCor[i,] %in% sort(xCor[i,], decreasing = TRUE)[c(2:k+1)])
        
        numerator <- 0
        denumerator <- 0
        meani <- mean(x[-pointsNumber,i])
        
        for (j in neighbor) {
            numerator <- numerator + xCor[i, j] * (x[pointsNumber, j] - 
                                                       mean(x[, j]))
            denumerator <- denumerator + abs(xCor[i, j])
        }
        
        dataPredict$x[pointsNumber, i] <- meani + numerator / denumerator
    }
    
    for (i in 1:nCol) {
        
        neighbor <- which(yCor[i,] %in% sort(yCor[i,], decreasing = TRUE)[c(2:j+1)])
        
        numerator <- 0
        denumerator <- 0
        meani <- mean(y[-pointsNumber,i])
        
        for (j in neighbor) {
            numerator <- numerator + yCor[i, j] * (y[pointsNumber, j] - 
                                                       mean(y[, j]))
            denumerator <- denumerator + abs(yCor[i, j])
        }
        
        dataPredict$y[pointsNumber, i] <- meani + numerator / denumerator
    }
    
    return(list(x = dataPredict$x, y = dataPredict$y))
}
