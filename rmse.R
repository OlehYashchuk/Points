rmse <- function(true, estimate, ...) {

    # sqrt(mean(unlist(c$x[15,] - pointsCor$x[15,])^2))
    # sqrt(mean(unlist(c$y[15,] - pointsCor$y[15,])^2))

    sqrt(mean(sqrt((estimate$x[15,] - true$x[15,])^2 +
                   (estimate$y[15,] - true$y[15,])^2)^2))
}