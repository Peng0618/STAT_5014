f_x <- function(x){
    y <- 3^x - sin(x) + cos(5*x)
    return(y)
}

f_x_deri <- function(x){
    y <- log(3)*3^x - cos(x) - 5*cos(5*x)
    return(y)
}

#x <- 0:1000/1000
#y <- f_x(x)
#plot(x,y)

solve_func <- function(Min,Max,Tolerance){
    set.seed(1)
    InitValue <- runif(1,min=Min,max=Max)

    tolerance <- Tolerance
    error <- 1
    X1 <- InitValue
    while (error > tolerance){
        X2 <- X1 - f_x(X1)/f_x_deri(X1)/100
        error <- abs(X2-X1)
        X1 <- X2
    }
    return(X2)
}

solve_func(0,1,0.001)

