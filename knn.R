knn <- function(data, cor, k, ...) {
        
        predict <- data
        
        # data <- as.matrix(data)
        
        nRow <- dim(data)[1]
        nCol <- dim(data)[2]
        
        pointsNumber <- dim(as.matrix(data))[1]
        
        for (i in 1:nCol) {
                
                neighbor <- which(cor[i,] %in% 
                                          sort(cor[i,], 
                                               decreasing = TRUE)[c(2:k+1)])
                
                numerator <- 0
                denumerator <- 0
                meani <- mean(data[-pointsNumber,i])
                
                for (j in neighbor) {
                        numerator <- numerator + cor[i, j] * 
                                (data[pointsNumber, j] - mean(data[, j]))
                        
                        denumerator <- denumerator + abs(cor[i, j])
                }
                
                predict[pointsNumber, i] <- meani + numerator / denumerator
        }
        
        return(predict)
}
