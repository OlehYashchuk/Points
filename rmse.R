rmse <- function(true, estimate, from = 1, to = 0, ...) {
        
        # number of the last row
        pointsNumber <- dim(as.matrix(true$x))[1]
        
        # number of respondents
        studentsNumber <- dim(as.matrix(true$x))[2]
        
        # select respondents who's points will be included 
        # in the accuracy calculation
        if (to == 0) {
                # if the parameters "from" and "to" does't defined then
                # calculate accuracy for all respondents
                students <- as.factor(c(from:studentsNumber))
                trueStudents <- which(colnames(true$x) %in% students)
                estimateStudents <- which(colnames(true$x) %in% students)
        } else {
                # else calculate accuracy for determined respondents [from:to]
                students <- as.factor(c(from:to))
                trueStudents <- which(colnames(true$x) %in% students)
                estimateStudents <- which(colnames(true$x) %in% students)
        }

        # Calculating accuracy as
        # Euclidean distance between two points on the two-dimensional plane
        sqrt(
                mean(
                        sqrt(
                                # [c_x^(i,j)-c_x(i,j)]^2
                                (estimate$x[pointsNumber,estimateStudents] -
                                true$x[pointsNumber,estimateStudents])^2 +

                                # [c_y^(i,j)-c_y(i,j)]^2
                                (estimate$y[pointsNumber,trueStudents] -
                                true$y[pointsNumber,trueStudents])^2
                                )^2
                        )
                )
}
