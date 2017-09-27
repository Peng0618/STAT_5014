CountSuccess <- function(vec){
    count <- sum(vec)
    return(count)
}


set.seed(12346)
P4b_data <- matrix(rbinom(10, 1, prob = (30:40)/100), nrow = 10,
                   ncol = 10)

