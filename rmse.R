rmse <- function(true, estimate, ...) {

    pointsNumber <- dim(as.matrix(true$x))[1]
    
    sqrt(mean(sqrt((estimate$x[pointsNumber,] - true$x[pointsNumber,])^2 +
                   (estimate$y[pointsNumber,] - true$y[pointsNumber,])^2)^2))
}