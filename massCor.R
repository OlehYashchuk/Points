massCor <- function(x, y, corMethod = "pearson", ...) {
    
    nRow <- dim(x)[1]
    nCol <- dim(x)[2]
    
    mCor <- list()
    
    if (corMethod == "concord") {
        
        concord <- matrix(ncol = nCol, nrow = nCol)
        
        for (i in 1:nCol) {
            for (j in 1:nCol) {
                concord[i, j] <- kendall(t(cbind(data.frame(x[,c(i,j)]),
                                                 data.frame(y[,c(i,j)]))))$value
            }
        }
        
        mCor$x <- concord
        mCor$y <- concord
        
    } else {
        
        mCor$x <- cor(x, method = corMethod)
        mCor$y <- cor(y, method = corMethod)
        
    }
    
    return(list(xCor = mCor$x, yCor = mCor$y))
}
