## Lab 9 extra - Programming in R - Control Structures and Functions

## Control Structures 

## if-else statements 

# Only if - 
set.seed(50) # Set the seed for the next randomly chosen number
x <- runif(1,0,10) # Choose a number at random from a uniform distribution
x
if(x > 5) {
  writeLines('x is greater than 3')
}

# if-else
set.seed(30) # Set a different seed
y <- rnorm(1, 10, 1) # Choose from a normal distribution this time for fun
y
if(y > 10) {
  writeLines('y is greater than 10, the mean of the normal distribution.')
} else {
  writeLines('y is less than or equal to 10, the mean of the normal distribution.')}

# if-else if-else

set.seed(100)
z <- rexp(1, 1) # This time choose a random number from an exponential distribution
z
if(z < 0.5) {
  writeLines('z is less than 0.5')
} else if(z >= 0.5 & z <= 1) {
  writeLines('z is between 0.5 and 1, inclusive')
} else {
  writeLines('z is greater than 1')
}
# Note that the ampersand must be used for 'and', unlike Python. Also note that it 
# is possible to put multiple conditions in a single if statement

# Tutorial example -
# Generate a uniform random number
x <- runif(2, 0, 15)
if(x[1] > 5 & x[2] > 5) {
  y <- 15
} else {
  y <- 0
}
writeLines(sprintf('The value of x is [%.2f, %.2f] and the value of y is %.2f', 
                   x[1], x[2], y))


## for loops

# Basic - 
for(i in seq(1, 10, by=0.5)) {
  print(i)
}

# These three loops have the same behavior

x <- c('aardwolf', 'bandicoot', 'capybara', 'dik-dik deer', 'egret')
for(i in 1:5) {
  # Print out each element of x
  print(x[i])
}

for(i in seq_along(x)) {
  print(x[i])
}

for (animal in x) {
  print(animal)
}

# For one line loops, curly brackets aren't necessary
for(i in 1:5) print(x[i])

# Nested for loops

x <- matrix(1:12, 3, 4) # Note that this fills the matrix along the columns
# Print along the rows (horizontally)
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j]) 
  }
}


# While loops 


# Basic
count <- 2
while (count < 5000) {
  print(count)
  count <- count*2
}

# Multiple conditions while

z <- 5
set.seed(10)
while(z >= 2 && z <= 8) {
  coin <- rbinom(1, 1, 0.5)
  print(z)
  if(coin == 1) {  ## random walk
    z <- z + 0.5
  } else {
    z <- z - 0.5
  } 
}
sprintf('The final value of z is %.1f.', z)


## Functions

# Minimal function that does nothing
f <- function() {
  # Empty function that doesn't do anything right now
}
# Check that the class of f is a function
class(f)
# Now exectute the function to see what happens
f()

# Basic function that does something - print exponents of 2
f <- function() {
  print(2^0)
}
f()

# Basic function with arguments
f <- function(num) {
  for (i in 0:num) {
    print(2^i)
  }
}
f(4)

# Basic function that returns a value
f <- function(num) {
  sum_exp <- 0
  for (i in 0:num) {
    print(2^i)
    sum_exp <- sum_exp + 2^i
  }
  sum_exp
}
summed_exp <- f(4)
print(summed_exp)

# Default values can be specified
f <- function(num = 3) {
  sum_exp <- 0
  for (i in 0:num) {
    print(2^i)
    sum_exp <- sum_exp + 2^i
  }
  sum_exp
}
f()
# Can still specify values
f(6)

# Can specify arguments by name 
f(num = 5)