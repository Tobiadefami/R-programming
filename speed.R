# SPEED
  
  # Vectorized Code:
  # the function below is not a vectorised one cos it uses a for loop to manipulate each element one at a time
  abs_loop <- function(vec){
    for (i in 1:length(vec)){
      if (vec[i] < 0){
        vec[i] <- -vec[i]
      }
    }
      vec
  }
  
  # second functiom: this is faster because it depends on operations that R does quickly
  abs_sets <- function(vec){
    negs <- vec < 0
    vec[negs] <- vec[negs] * -1
    vec
  }
  
  # To compare  the above functions, first make a long vector of positive and negative integers
  long <- rep(c(-1, 1), 5000000)
  # then use system.time to measure how much time it takes each function to evaluate long
  system.time(abs_loop(long))
  system.time(abs_sets(long))
  
  #EXERCISE
  system.time(abs(long))
  
  # identifying all of the elements of a vector that fall into a case with a logical test
  vec <- c(1, -2, 3, -4, 5, -6, 7, -8, 9, -10 )
  vec < 0
  # selecting the values
  vec[vec < 0]
  # multiplying each of the negative values by a negative one 
  vec[vec < 0] * -1
  
  vec[vec < 0] <- vec[vec < 0] * -1
  
  # EXERCISE: convert the following code into a vectorised code
  change_symbols <- function(vec) {
    for (i in 1:length(vec)) {
      if(vec[i] == "DD"){
        vec[i] <- "joker"
      } else if (vec[i] == "C"){
        vec[i] <- "ace"
      } else if (vec[i] == "7"){
        vec[i] <- "king"
      } else if (vec[i] == "B"){
        vec[i] <- "queen"
      } else if (vec[i] == "BB"){
        vec[i] <- "jack"
      } else if (vec[i] == "BBB"){
        vec[i] <- "ten"
      } else {
        vec[i] <- "nine"
      }
      
    }
    vec
  }
  
  vec <- c("DD", "C", "7", "B", "BB", "BBB", "0")
  change_symbols(vec)
  
  many <- rep(vec, 1000000)
  change_symbols(many)
  system.time(change_symbols(many))
  
  # to vectorize the above code, create a logical test that can identify each case
  vec[vec == "DD"]
  
  vec[vec == "C"]
  
  vec[vec == "7"]
  
  vec[vec == "B"]
  
  vec[vec == "BB"]
  
  vec[vec == "BBB"]
  
  vec[vec == "0"]
  
  #then write code that can change the symbols for each case
  vec[vec == "DD"] <- "joker"
  vec[vec == "C"]  <- "ace"
  vec[vec == "7"]  <- "king"
  vec[vec == "B"]  <- "queen"
  vec[vec == "BB"] <- "jack"
  vec[vec == "BBB"] <- "ten"
  vec[vec == "0"] <- "nine"
  
  #combine it into a function, this makes it vectorised
  change_vec <- function(vec) {
    vec[vec == "DD"] <- "joker"
    vec[vec == "C"]  <- "ace"
    vec[vec == "7"]  <- "king"
    vec[vec == "B"]  <- "queen"
    vec[vec == "BB"] <- "jack"
    vec[vec == "BBB"] <- "ten"
    vec[vec == "0"] <- "nine"
    
    
    vec
  }
  
  change_vec(many)
  system.time(change_vec(many))
  
  # Or using a look up table
  change_vec2 <- function(vec){
    tb <- c("DD" = "joker", "C" = "ace", "7" = "king",
            "B" = "queen", "BB" = "jack", "BBB" = "ten",
            "0" = "nine" )
    unname(tb[vec])
  }
  
  change_vec2(vec)
  change_vec2(many)
  system.time(change_vec2(many))
  
  # Vectorised Code in Practise
  winnings <- vector(length = 1000000)
  for (i in 1:1000000) {
    winnings[i] <- play()
  }
  
  mean(winnings)
  
  system.time(for (i in 1:1000000) {
    winnings[i] <- play()
  }
          
