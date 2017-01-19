concordance <- function(x, y, ...) {
        
        nRow <- dim(x)[1]
        nCol <- dim(x)[2]
        
        concord <- matrix(ncol = nCol, nrow = nCol)
        
        for (i in 1:nCol) {
                for (j in 1:nCol) {
                        concord[i, j] <- kendall(t(cbind(data.frame(x[,c(i,j)]),
                                                 data.frame(y[,c(i,j)]))))$value
                }
        }
        
        return(concord)
}
