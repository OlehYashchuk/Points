rmse <- function(true, estimate, ...) {

    # sqrt(mean(unlist(c$x[pointsNumber,] - pointsCor$x[pointsNumber,])^2))
    # sqrt(mean(unlist(c$y[pointsNumber,] - pointsCor$y[pointsNumber,])^2))
    
    pointsNumber <- dim(as.matrix(true$x))[1]
    
    sqrt(mean(sqrt((estimate$x[pointsNumber,] - true$x[pointsNumber,])^2 +
                   (estimate$y[pointsNumber,] - true$y[pointsNumber,])^2)^2))
}