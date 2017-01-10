rmse <- function(true, estimate, from = 1, to = 0, ...) {
        
        # Номер последней точки
        pointsNumber <- dim(as.matrix(true$x))[1]
        
        # Колличество респондентов
        studentsNumber <- dim(as.matrix(true$x))[2]
        
        # Определяем респондентов по точкам которых считаем RMSE
        if (to == 0) {
                # Если на входе в функцию from и to не заданы, 
                # тогда RMSE считаем для полных данных
                students <- as.factor(c(from:studentsNumber))
                trueStudents <- colnames(true$x) %in% students
                estimateStudents <- colnames(true$x) %in% students
        } else {
                # В противном случае 
                # сичтаем RMSE для заданного диапазона респондентов [from:to]
                students <- as.factor(c(from:to))
                trueStudents <- colnames(true$x) %in% students
                estimateStudents <- colnames(true$x) %in% students
        }

        # Считаем отклонение рассчитаной точки от действительной
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
