# Slot machine

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
} 

# if statements
num <- -2
if (num < 0) {
  num <- num * -1
}

num <- 5
if (num < 0) {
  num <- num * -1
}

num <- -1
if(num < 0) {
  print("num is negative.")
  print("Dont worry, i'll fix it.")
  num <- num * -1
  print("Now num is positive.")
}

x <- 1
if(3 == 3) {
  x <- 2
}

x <- 1
if(TRUE){
  x <- 2
}

x <- 1
if(x == 1) {
  x <- 2
  if(x == 1) {
    x <- 3
  }
}

# ELse statements

if (this) {
  plan A
} else {
  plan B
}

a <- 3.14
dec <-  a - trunc(a)
if (dec >= 0.5) {
  a <- trunc(a) + 1
}else{
  a <- trunc(a)
}

# else if statements
a <- 1
b <- 1

if (a > b){
  print("A wins!")
} else if (a < b) {
  print("B wins!")
} else{
    print("Tie.")
}


# using if and else to link the subtasks in the slot-machine function

symbols <- c("7", "7", "7")

# test if all the symbols contained in the vector are the same
symbols[1] == symbols[2] & symbols[2] == symbols[3]
symbols[1] == symbols[2] & symbols[1] == symbols[3]
all(symbols == symbols[1])
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]

#another example
symbols <- c("B", "BBB", "BB")

#test if the symbols vector contains only symbols that are a type of bar
all(symbols %in% c("B", "BB", "BBB"))
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

# NEW TASK: assign a prize for symbols  
#lookup Tables

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
             "B" = 10, "C" = 10, "0" = 0)
#Extracting the correct prize for any symbol using subsetting
payouts["DD"]
#subsetting without the object's name
unname(payouts["DD"])
unname(payouts["7"])
#subsetting payouts with symbols[1]
symbols <- c("7", "7", "7")
symbols[1]
payouts[symbols[2]]

symbols <- c("C", "C", "C")
payouts[symbols[1]]

c(0, 2, 5)[1]
cherries + 1
c(0, 2, 5)[cherries + 1]

score <- function(symbols) {
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  # get prize
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
  }
  
  # identify for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds 
}

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

# S3

num <- 1000000000
print(num)

# But assigning a class POSIXct followed by POSIXt
class(num) <- c("POSIXct", "POSIXt")

#attr
 
one_play <- play()
attributes(one_play)
# Adding a general attribute to an  object using attr
attr(one_play, "symbols") <- c("B", "0", "B")
attributes(one_play)

#exercise
play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}
two_play <- play()

#or using the structure function
play <- function(){
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}

three_play <- play()


# Writing a function that looks up and uses the attribute 
#and displays one_play in a pretty manner

slot_display <- function(prize){
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into a single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a regular expression
  #\n is a regular exoression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  
  #display regular expression in console without quotes
  cat(string)
}

slot_display(one_play)

# Methods
#methods dispatch
class(one_play) <-"slots"
# writing an s3 method for the slots class

args(print)

print.slots <- function(x, ...){
  cat("i'm using the print.slots method")
}

print(one_play)
one_play
rm(print.slots)

print.slots <- function(x, ...){
  slot_display(x)
}

# modify the play function so it assigns slots to the class attribute of its output

play <- function(){
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

# LOOPS
# expected value
# calculate the exact payout rate of my machine with the score program
die <- c(1, 2, 3, 4, 5, 6)
rolls <- expand.grid(die, die)
rolls
# determine the value of each roll by adding a new column 
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls)
# probability
prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" =1/8,
          "5" = 1/8, "6" = 3/8)
# subsetting the above table by rolls$var1
rolls$Var1
prob[rolls$Var1]

# adding a new column to rolls
rolls$prob1 <- prob[rolls$Var1]
rolls$prob1
head(rolls, 3)

# for rolls$Var2
rolls$prob2 <- prob[rolls$Var2]

#calculate the prob of rolling each combination by multiplying prob1 by prob2
rolls$prob <- rolls$prob1 * rolls$prob2
rolls

# calculating the expected value
sum(rolls$value * rolls$prob)

# CALCULATE THE EXPECTED VALUE OF THE SLOT MACHINE

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

# Calculate the probability of getting each combination
get_symbols <- function(){
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE)
  prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
}

# isolate the previous probabilities in a lookup table
prob = c("DD" = 0.03, "7" = 0.03,"BBB" = 0.06, "BB" = 0.1,"B" = 0.25, "C" = 0.01, "0" = 0.52)
# time to look up our probabilities
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)

# calculate the overall probabilities for each combination
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos, 3)
sum(combos$prob)
# Determine the prize of for each combination in combos
symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])
# using the score function
score(symbols)

# For loops

for (sentence in c("my", "first", "for", "loop")) {
  print("one run")
}
for (string in c("my", "second", "for", "loop")) {
  print(value)
}

# Save output from a for loop
chars <- vector(length = 4)
# The next loop will fill its strings
words <- c("My", "third", "for", "loop")

for (i in 1:4) {
  chars[i] <- words[i]
}

# Usw a for loop to calculate the prize for each row in combos
combos$prize <- NA
head(combos, 3)

# construct a for loop that will run score on all 343 rowa of combos
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

# Calculate the expected value
sum(combos$prize * combos$prob)

  #version  of score that handles wild diamonds
score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  # identify case
  # since are wild, only nondiamonds
  # matter for three of a kind
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0 )
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
      prize <- 5
  } else if (cherries > 0) {
      # Diamonds count as cherries
      # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]  
  } else{
      prize <- 0
    }
  # double for each diamond
  prize * 2^diamonds
}

# calculate the expected value of the slot machine when it uses the slot function
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i,3])
  combos$prize[i] <- score(symbols)
}

# then calculate the expexted value
sum(combos$prize * combos$prob)

# WHILE LOOPS; Used to do things that take a varying number of iterations
while (condition) {
  code
}

# Calculating how long it takes to go broke playing slots
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

plays_till_broke(100)

# Repeat loops
plays_till_broke <- function(start_with){
  cash <- start_with
  n <- 0
  repeat{
    cash <- cash - 1 + play()
    n <- n +1
    if (cash <= 0){
      break
    }
    
    }
  
  n
}
plays_till_broke(100)


