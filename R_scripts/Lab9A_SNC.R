# Lab 9a: Regular Expressions in R

## Regular Expressions

# Regular expressions (regex) are concise sequence of characters that are used to define a 
# search pattern for strings, like in a find operation for a document. There are different 
# syntaxes used to define regular expressions, such as POSIX standards or Perl syntax. 

## Base R Functions

# R's base regular expression functions work similarly as in other languages. You can read
# about them in the main help page via ?regexp. As an overview, you can check the cheat 
# sheet for Base R regular expressions. Additionally, here are the basic functions - 

# grep() and grepl() - Search for matches of a regular expression in a character vector. 
# grep() returns the indices that contain a match or strings that have a match. grepl()
# returns a logic vector to indicate which strings have a match vs don't.

# regexpr() and gregexpr() - Search a character vector for matches and return the indices 
# of the string where the match begins and the length of the match. gregexpr() returns the 
# starting positions of every (disjointed, aka non-overlapping) match.

# sub() and gsub() - Search a character vector for matches and replace the match with another
# string. gsub() replaces all matches while sub() only replaces the first.

# regexec() - Search a character fector for a regular expression like regexpr(), but also 
# return the locations for any parenthesized subexpressions.

# The default syntax for R is POSIX 1003.2 extended regular expressions, but Perl syntax 
# can be set using perl = TRUE argument.


## The stringr Package

# stringr is a package part of tidyverse that wraps the underlying stringi package, which 
# provides a series of convenience functions that hides some of the complexities of the 
# base R regular expression functions. In general, stringr functions provide a more 
# rational interface to regular expressions with more consistent arguments and argument
# ordering. In general, the data are the first argument and regular expression the second.
# The mapping between base R and stringr functions is straigtforward - 
# str_subset() is like grep(value = TRUE), returning a character vector of strings that 
# have the input match.
# str_detect() is nearly equivalent ot grepl().
# str_extract() is like regexpr() and regmatches(), extracted matches for the output.
# str_match() is similar to regexec() by providing a matrix of parenthesized suexpressions.

# You can find cheat sheets for stringr and read more about it on its tidyverse page.

# Now, we will go over stringr via a shortened version of the Strings chapter in R for Data
# Science by Hadley Wickham.


## 14.3 Matching patterns with regular expressions

# To learn the basics of regular expressions, we will start with str_viwe() and 
# str_view_all(), which take a character vector and regular expression and highlight the 
# matches in the input character vector.
library(stringr)
library(tidyverse)

# 14.3.1 Basic matches

# The most basic is exact string matches - 
x <- c('apple', 'banana', 'pear')
str_view(x, 'an') # search for 'an', matching the first instance only

# Now, increase complexity with the dot, ".", which matches any character except newline
str_view(x, '.a.') # search for the first instance of a surrounded by two characters

# Escape characters 
# To create the regular expression, define the dot escape character for ease using \\.
dot <- "\\."

# Now, the expression itself only contains one backslash - 
writeLines(dot)

# This tells R to search for the only the dot, .
x <- c('abc', 'a.c', 'bef')
str_view(x, 'a\\.c')
# Now, if we only search for dot, we will only match the dot - 
str_view(x, dot)

# To search for a backslash only, you need to escape the backslashes that surround the 
# expression with a total of 4 backslashes.
x <- 'a\\b'
writeLines(x)
str_view(x, '\\\\')
# To match the a\\b in the regular expression, you need to use four backslashes (two are 
# used to escape the backslashes, leaving two left to match the string)
str_view(x, 'a\\\\b')

# Exercises -
# 1 - See Rmd file.
# 2 - 
match <- "\"'\\\\"
writeLines(match)
str_view('"\'\\', match)
# 3 -
match <- '\\..\\..\\..'
writeLines(match)
str_view('.a.a.a', match)

# 14.3.2 Anchors

# ^ anchors to the beginning of a string while $ anchors to the end 
x <- c('apple', 'banana', 'pear')
str_view(x, '^a') # Only highlight the a of apple since it is at the start of the string
str_view(x, 'a$') # Only highlight the end a of banana since it is at the end

# Surround pattern with these characters to match single words 
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, '^apple$')

# This can also be defined using \b to surround the word to be searched (e.g., \bsum\b).

# 14.3.2.1 Exercises
# 1 -
match <- '"\\$\\^\\$"'
writeLines(match)
str_view('"$^$"', match)
# Without quotes
match_noquote <- '\\$\\^\\$'
writeLines(match_noquote)
str_view(c('a$^$b', '$^$'), match_noquote)
# Exact, without quotes
match_exact_noquote <- '^\\$\\^\\$$'
writeLines(match_exact_noquote)
str_view(c('a$^$b', '$^$'), match_exact_noquote)

# 2 -
x <- c('yak', 'xylophone', 'regex', 'raccoon', 'cat', 'cox')
# Starts with y
str_view(x, '^y.+')
# Ends in x
str_view(x, '.+x$')
# Exactly 3 letters long
str_view(x, '^...$')
# Seven or more letters
str_view(x, '.{7,}')

# 14.3.3 Character classes and alternatives

# Character classes
# Look for a literal character that normally has a special meaning in regex
x <- c('abc', 'a.c', 'a*c', 'a c')
str_view(x, 'a[.]c') # Gives the same results as str_view(x, 'a\\c')
# Gives different results than just the dot - 
str_view(x, 'a.c') # Matches all input strings
str_view(x, '.[*]c') # Matches only the string with the asterisk
str_view(x, 'a[ ]') # Matches only the string with the space

# Alternation using |
x <- c('gray', 'grey')
str_view(x, 'gr(e|a)y') # Matches both
str_view(x, 'gre|ay') # Only matches the parts for each word, not the entire word

# 14.3.3.1 Exercises

# 1 - 
x <- c('orange', 'pepper', 'lynx', 'sphynx', 'tweed', 'rowed', 'biking', 'exercise', 'mug',
       'plants', 'oxygen')
# Start with a vowel
str_view(x, '^[aeiou].+')
# Contains only consonants
str_view(x, '^[^aeiou]+$')
# Ends with ed but not eed
str_view(x, '.+[^e]ed$')
# Ends with ing or ise
str_view(x, '.+(ing|ise)$')

# 2 - 
# Using the above word set as an example, the number of entries that follow 
# a regex pattern can be counted as follows - 
count  <-  sum(!is.na(str_extract(x, '^[aeiou].+'))) 
count
# Note that a word that doesn't have the pattern returns NA using str_extract(), 
# so counting the number that don't return NA gives the number that do follow the pattern
# To use the word set from stringr,
follow <- sum(!is.na(str_extract(stringr::words, '([^c]ie)|(cei)')))
follow
do_not_follow <- sum(!is.na(str_extract(stringr::words, 'cie')))
do_not_follow

# 3 - Does u always follow a q?
qu <- sum(!is.na(str_extract(stringr::words, 'q[^u]')))
qu
q <- sum(!is.na(str_extract(stringr::words, 'q')))
q

# 4 - British vs American English via 'ou' vs 'o' and 'ise' vs 'ize'
word_set <- c('color', 'colour', 'labor', 'labour', 'recognize', 'recognise',
           'organize', 'organise')
Brit_ou <- str_view(word_set, '.+our$')
Brit_ou
Brit_ise <- str_view(word_set, '.+ise$')
Brit_ise
Both <- str_view(word_set, '.+((our)|(ise))$')
Both 

# 5 - 
phone_numbers <- c('+1 987-654-3210', '+1 963-852-7410', '+1 012-345-6789',
                   '+2 987-654-3210', '+1 987-012-3456', '1 987-654-3210')
str_view(phone_numbers, '[+]1 [2-9]\\d{2}-[2-9]\\d{2}-\\d{4}')

## 14.3.4 Repetition

# ? is for 0 or 1; + is for 1+; * is for any number
x <- '1888 is the longest year in Roman numerals: MDCCCLXXXVIII'
str_view(x, 'CC?') # Matches only the first 2 Cs
str_view(x, 'CC+') # Matches the first 3 Cs
str_view(x, 'C[LX]+') # Matches from the last C to the last X
# Extra messing around with the operators - 
str_view(x, 'C+LX+') # Matches from first C to last X
str_view(x, '[MDCLXVI]?') # No matches
str_view(x, 'M[DCLXVI]?') # Matches the first two Roman numerals
str_view(x, '[MDCLXVI]+') # Matches the entire Roman numeral set
str_view(x, '[MDCLXVI]*') # No matches
str_view(x, 'M[DCLXVI]*') # Matches the entire Roman numeral set
str_view(x, '[MDCLXVI]{6}') # Matches the first six Roman numerals
# Seems like the metacharacters that have 0 as a possible number of matches require 
# a sort of seed to start the match before they match anything, but when they have 
# that seed, they then are greedy in that they match as many characters as possible.

# Curly brackets specify the exact number of matches 
str_view(x, 'C{2}') # First 2 Cs
str_view(x, 'C{2,}') # All 3 Cs
str_view(x, 'C{1,3}') # All 3 Cs. Also, I tried to not specify a min and it didn't work
str_view(x, '[CLX]{6}') # Matches six characters starting with the first C
str_view(x, 'C{2,3}?') # 'Lazy', so only matches the first 2 Cs
str_view(x, 'C[LX]+?') # Also 'Lazy', so only matches the first CL (no Xs)
str_view(x, 'C[LX]??') # Matches only the C. Would not recommend doing this format

## 14.3.4.1 Exercises
# 1 - See Rmd file

# 2 - Put regular expressions into words
# first
x <- c('.', '.a', 'a', '.ab', '  a  s')
str_view(x, '^.*$')
# second
x <- c('a', '{}', '{a}', '{AB}')
str_view(x, "\\{.+\\}")
# third
x <- c('1111-11-11', '5847-56-23', '058-1721-2', '00000000')
str_view(x, '\\d{4}-\\d{2}-\\d{2}')
# fourth
x <- c('\\', '\\\\', '\\\\\\', '\\\\\\\\', '\\\\\\\\\\')
writeLines(x)
str_view(x, '\\\\{4}')

# 3 - Make regular expressions for the following
# Start with three consonants
x <- c('sphinx', 'lynx', 'tire', 'string', 'lawbook')
str_view(x, '^[^aeiou]{3}.*')
# For only three consonants,
str_view(x, '^[^aeiou]{3}[aeiou].*')
# Have three or more vowels in a row
x <- c('congruous', 'conscientious', 'continuous', 'aeiou', 'extreme', 'value')
str_view(x, '.*[aeiou]{3,}.*')
# Have two or more vowel-consonant pairs in a row
x <- c('banana', 'care', 'potato', 'arrange', 'inept', 'rear', 'average', 'bottle',
       'apple', 'pepper', 'Gibbs')
str_view(x, '.*([aeiou][^aeiou]){2,}.*') # vowel-consonant patterns only, not consonant-vowel

## 14.3.5 Grouping and backreferences

str_view(fruit, '(..)\\1', match = TRUE) # Don't forget the double escape for the regex
# Also, note that the match = TRUE argument shows only the matching characters

# 14.3.5.1 Exercises

# 1 - 
# First
x <- c('a', 'aa', 'aaa', 'aaaa', 'bbb', 'bab')
match <- '(.)\\1\\1'
writeLines(match)
str_view(x, match)
# Second
x <- c('abab', 'abba', 'baab', 'baba', 'bababa', 'baabba')
match <- "(.)(.)\\2\\1"
writeLines(match)
str_view(x, match)
# Third
match <- '(..)\\1'
writeLines(match)
str_view(x, match) # Same set of strings as second part
# Fourth
x <- c('abaca', 'banana', 'abcde', 'abcda', 'abab')
match <- "(.).\\1.\\1"
writeLines(match)
str_view(x, match)
# Fifth
x <- c('abcdddcba', 'abccba', 'abcdefcba', 'abcdabcd')
match <- "(.)(.)(.).*\\3\\2\\1"
writeLines(match)
str_view(x, match)

# 2 - 
# Start and end with the same character
x <- c('abra', 'kadabra', 'abrakadabra', 'banana', 'stoats', 'curtains')
match <- '^(.).*\\1$'
str_view(x, match)
# Contain a repeated pair of letters 
x <- c('church', 'banana', 'kiwi', 'abcdefgab', '126541')
match <- '([A-Za-z][A-Za-z]).*\\1'
str_view(x, match)
# Contains one letter repeated in at least three different places
x <- c('water bottle', 'totality', 'concoction', 'eleven',
       'crocodile', 'alligator', '12131')
match <- '([A-Za-z]).*\\1.*\\1'
str_view(x, match)


## 14.4 Tools


## 14.4.1 Detect matches 

# Detect matches with str_detect(), which returns a logic vector
x <- c('apple', 'banana', 'pear')
str_detect(x, 'e')
# Count the number of entries that have a match in a large data set using sum()
# How many of the common words in the words set start with t?
sum(str_detect(words, '^t'))
# What proprotion of these common words end with a vowel?
mean(str_detect(words, '[aeiou]$'))

# Complex logic by finding words with no vowels. Two methods.
# Method 1 - find all words that contain at least one vowel and negate them 
no_vowels_1 <- !str_detect(words, '[aeiou]')
# Method 2 - find all words consisting of only consonants (non-vowels)
no_vowels_2 <- str_detect(words, '^[^aeiou]+$')
identical(no_vowels_1, no_vowels_2) # Check that the results are the same

# Subset with logical subsetting or using str_subset()
words[str_detect(words, 'x$')] # Words that end with x
str_subset(words, 'x$') # More convenient way to subset the words that end with x
# For a column in a data frame, you will want to use filter() instead
df <- tibble(word = words, i = seq_along(word)) # Make a tibble of the words set
# seq_along() gives the position of the word in the words word set
df %>% 
  filter(str_detect(word, 'x$'))

# str_count() returns the number of matches in each string of a character vector
x <- c('apple', 'banana', 'pear')
str_count(x, 'a')
# On average, how many vowels are there per word in the words word set?
mean(str_count(words, '[aeiou]'))
# For data frames, it is common to use str_count() with mutate()
df %>% 
  mutate(
    vowels = str_count(word, '[aeiou]'),
    consonants = str_count(word, '[^aeiou]'))

# Note that matches don't overlap
str_count('abababa', 'aba')
str_view_all('abababa', 'aba')


## 14.4.1.1 Exercises

# Find all words that start or end with x
# Method 1 - single regex
start_end_x_1 <- words[str_detect(words, '^x|x$')]
start_end_x_1
# Method 2 - Two regex
start_x <- words[str_detect(words, '^x')] # No words
end_x <- words[str_detect(words, 'x$')] # Four words
start_end_x_2 <- c(start_x, end_x)
start_end_x_2
# Check that they are identical
identical(start_end_x_1, start_end_x_2)

# Find all words that start with a vowel and end with a consonant.
# Method 1 - single regex
vowel_consonant_1 <- words[str_detect(words, '^[aeiou].*[^aeiou]$')]
vowel_consonant_1
# Method 2 - two regex
start_vowel <- str_detect(words, '^[aeiou]')
end_consonant <- str_detect(words, '[^aeiou]$')
vowel_consonant_2 <- words[start_vowel & end_consonant]
vowel_consonant_2
# Check that all entries are identical
identical(vowel_consonant_1, vowel_consonant_2)

# Find any word in the words word set that contains at least one of each vowel
# Method 1 - single regex
# To do in a single regex, all possible orders of the vowels must be constructed
# with '.*' between them for any possible characters between the vowels. To do this,
# all permutations of the vowels must be created (5! = 120 variants). Based on some 
# Googling, I found a function to list all permutations in a matrix (found at 
# https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r).
# This function returns a matrix of all permutations, with a permutations per row and 
# the order of each element in each column
permutations <- function(n){
  if(n==1){
    return(matrix(1)) # Return a matrix of 1 if 1 is entered
  } else {
    sp <- permutations(n-1) # Recursive until 1 is reached
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n) # Define the size of the matrix
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i)) # Make the elements of the matrix
    }
    return(A)
  }
}
# For example, for a permutation of 4,
head(permutations(4))
# To make a permutation for all five vowels, make a vector of all vowels, 
# turn the matrix into a data frame, unite an additional column to combine 
# the permutation elements into one with the ".*" between each element, and
# finally combine all entries into one string, with each entry separated by 
# a pipe, |
vowels <- c('a','e','i','o','u')
vowels_df <- as_tibble(matrix(vowels[permutations(5)], ncol = 5))
head(vowels_df) # Looks correct
# Now unite to get the combined column
vowels_unite <- unite(vowels_df, string, V1, V2, V3, V4, V5, sep = '.*')
head(vowels_unite)
pattern <- character(0) # Make the initial string object for the regex pattern
for (string in vowels_unite$string) {
  pattern <- paste(pattern, string, sep = ')|(')
} 
pattern <- paste(substr(pattern, 3, nchar(pattern)), ')', sep = '') # properly format the pattern
pattern
# Now, test with a positive and negative case to make sure the pattern works
str_view('authorize', pattern) # Works as expected
str_view('promote', pattern)
# Finally, use the pattern to find any words with all vowels in words
all_vowels_1 <- words[str_detect(words, pattern)]
all_vowels_1 # There are none. Great

# Method 2 - multiple regex
a <- str_detect(words, 'a')
e <- str_detect(words, 'e')
i <- str_detect(words, 'i')
o <- str_detect(words, 'o')
u <- str_detect(words, 'u')
all_vowels_2 <- words[a & e & i & o & u]
all_vowels_2 # There are none
# Altogether, this one was much, much easier with the multiple regex instead of a single,
# monster regex

# 2 -
# What is the word with the most vowels and
# the words with the highest proportion of vowels?
# Most vowels - 
words_df <- tibble(word = words, i = seq_along(words), 
                   num_vowels = str_count(words, '[aeiou]'),
                   prop_vowels = str_count(words, '[aeiou]')/nchar(words))
head(words_df) # Looks correct
most_vowels <- words_df %>% 
  arrange(desc(num_vowels)) %>% 
  # Select all words with the highest number of vowels
  slice(1:sum(words_df$num_vowels == max(words_df$num_vowels)))
most_vowels # There are 8 words in the words data set that have the most vowels

# Highest proportion of vowels - same process
highest_prop_vowels <- words_df %>% 
  arrange(desc(prop_vowels)) %>% 
  slice(1:sum(words_df$prop_vowels == max(words_df$prop_vowels)))
highest_prop_vowels # Only a is created. One further thought, I should be included, too


## 14.4.2 Exact matches

length(sentences)
head(sentences)

# Find all sentences that contain a color
colours <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple')
colour_match <- str_c(colours, collapse = '|') 
# str_c() joins all elements of a vector with a given separator specified by collapse
colour_match
# Now select the sentences that have a colour and extract the colours
has_colour <- str_subset(sentences, colour_match) 
# str_subset() returns the elements that have the given regex; = x[str_detect(x, regex)]
matches <- str_extract(has_colour, colour_match) # Extract the colours
head(matches)

# However, matches has an error, as seen when investigating has_colour
has_colour

# The error is that str_extract() only extracts the first match of each element. This 
# is most easily seen when looking at sentences with more than one match
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)
# str_view() doesn't highlight multiple colors
str_view(more, colour_match)
# Now test extracting using str_extract() and str_extract_all()
str_extract(more, colour_match) # This returns a simple vector
str_extract_all(more, colour_match) # This returns a nested vector

# Specify simplify = TRUE in extract_all() to create a simpler matrix
str_extract_all(more, colour_match, simplify = TRUE)
x <- c('a', 'a b', 'a b c') 
str_extract_all(x, '[a-z]', simplify = TRUE)


## 14.4.2.1 Exercises
# 1 - 
colours_fixed <- paste(' ', str_c(colours, collapse = ' | '), ' ', sep = '')
colours_fixed
# To show that it only extracts colors now, 
has_colour_fixed <- str_subset(sentences, colours_fixed)
has_colour_fixed

# 2 - 
# First word of each Harvard sentence
str_extract(sentences, '^[A-Za-z]*')
# All words ending with ing
ing_detect <- str_subset(sentences, '[A-Za-z]*ing')
head(str_extract_all(ing_detect, '[A-Za-z]*ing', simplify = TRUE))
# All plurals. Assume all plurals end with 'es' or '[consonant]s', so words like 'as' 
# aren't matched. Also, don't match apostrophes, duplicate s, and spaces.
plural_detect <- str_subset(sentences, "[A-Za-z]*[^aious ']s ")
head(str_extract_all(plural_detect, "[A-Za-z]*[^aious ']s ", simplify = TRUE))
# Unfortunately, this also catches present-tense action verbs, but that can't be easily 
# overcome without a much hardier code


# 14.4.3 Grouped matches

# Parentheses can be used to extract certain parts of a complex match,
# similar to backreferences.
# For example, extract nouns from the sentences data set, assuming nouns come after 'a' 
# or 'the' and are sequences of at least one character that isn't a space.
noun <- '(a|the) ([^ ]+)' # Define the pattern for finding the nouns

has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)

# str_extract give the complete match while str_match() gives each individual component
# of the match in a matrix, with one column for the complete match and subsequent columns
# for each individual group.
has_noun %>% 
  str_match(noun)

# tidyr::extract() works similar to str_match() but requires that the groups of the match
# also be named (for the corresponding columns of the created tibble)
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c('article', 'noun'), noun,
    remove = FALSE
)
# Note that it outputs a result for all sentences, including the ones where no match
# was detected. Additionally, setting remove = FALSE will keep the data that was matched 
# (in this case, the sentences) in the output tibble.
# Like for most stringr functions, str_match_all() will output all matches for each input
# string.

## 14.4.3.1 Exercises

# 1 - Find all words that come after a number, and pull out both the number and the word
# I will just choose the numbers zero through ten so I won't be writing out a numbered 
# dictionary.
numbers <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten')
pattern <- paste('\\b(', str_c(numbers, collapse = '|'), ') ([^ ]+)',  sep = '')
pattern # Recall that \b detects only the independent word (so it won't detect 'stone')
str_view('ten owls', pattern) # Works as expected
numbered_sentences <- str_subset(sentences, pattern)
str_match_all(numbered_sentences, pattern)

# 2 - Find all contractions, then separate out the pieces before and after the apostrophe
contraction_sentencs <- str_subset(sentences, "([A-Za-z]+)'([^ ]+)")
str_match_all(contraction_sentencs, "([A-Za-z]+)'([^ ]+)")


# 14.4.4 Replacing matches 

# Use str_replace() and str_replace_all() to replace matches with new text 
x <- c('apple', 'pear', 'banana')
str_replace(x, '[aeiou]', '-') # Again, note that only the first match is replaced
str_replace_all(x, '[aeiou]', '-') # Now all matches are replaced
# You can do multiple replacements with str_replace_all() by supplying a named vector
# that specifies each match and its corresponding replacement.
x <- c('1 house', '2 cars', '3 people')
str_replace_all(x, c('1' = 'one', '2' = 'two', '3' = 'three'))
# Backreferences can also be used to insert/flip around components of a match. The
# following example flips the order of the second and third words.
sentences %>% 
  str_replace('([^ ]+) ([^ ]+) ([^ ]+)', '\\1 \\3 \\2') %>% 
  head(5)


## 14.4.4.1 Exercises

# 1 - Replace all forward slashes in a string with backslashes.
string <- 'This/is/a/weird/sentence.'
# For some unknown reason, the following doesn't work - 
str_replace_all(string, '/', '\\')
str_replace_all(string, '/', '\\\\') # Can only replace with multiple backslashes

# 2 - Implement a simple version of str_to_lower() using replace_all(). 
# str_to_lower() takes a string and replaces all uppercase characters with lowercase - 
letter_cases <- c('A' = 'a', 'B' = 'b', 'C' = 'c', 'D' = 'd', 'E' = 'e', 'F' = 'f',
                  'G' = 'g', 'H' = 'h', 'I' = 'i', 'J' = 'j', 'K' = 'k', 'L' = 'l', 
                  'M' = 'm', 'N' = 'n', 'O' = 'o', 'P' = 'p', 'Q' = 'q', 'R' = 'r', 
                  'S' = 's', 'T' = 't', 'U' = 'u', 'V' = 'v', 'W' = 'w', 'X' = 'x',
                  'Y' = 'y', 'Z' = 'z')
str_replace_all('ABCDEFGHIJKLMNOPQRSTUVWXYZ', letter_cases)
string <- 'CrAzYcAsEs ArE rIdIcUlOuS aNd WeIrD tO rEaD.'
str_replace_all(string, letter_cases)

# 3 - Switch the first and last letters in words. Which of these strings are still words?
swap_first_last <- str_replace(words, '(.)(.+)(.)', '\\3\\2\\1')
head(swap_first_last, 10)
# To roughly find which ones are still words, choose the words that are the same in 
# the words and swap_first_last word sets using intersect()
still_words <- intersect(swap_first_last, words)
still_words
# However, note that some words with their first and last letters replaces are still 
# words, such as 'bad' to 'dab':
swap_first_last[68]


# 14.4.5 Splitting

# str_split() splits a string into pieces based on the input delimiter, returning a list.
# . For example, to split a sentence into words, split by the space character - 
sentences %>% 
  head(5) %>% 
  str_split(' ')
# If you are working with a length-1 vector, the easiest thing is to just extract the 
# first element of the list to get the results - 
'a|b|c|d' %>% 
  str_split('\\|') %>% 
  .[[1]] # This extracts just the first element of the resulting list 
# Otherwise, you can specify simplify = TRUE like the other stringr functions that return
# a list to return an ordered matrix - 
sentences %>% 
  head(5) %>% 
  str_split(' ', simplify = TRUE)

# You can also request a maximum number of pieces using the 'n' argument so that only
# the first n strings resulting from the split are returned
fields <- c('Name: Hadley', 'Country: NZ', 'Age: 35')
fields %>% str_split(': ', n = 2, simplify = TRUE) 
fields %>% str_split(': ', n = 3, simplify = TRUE) # Only returns first piece
str_split('a|b|c|d', '\\|', n = 2, simplify = TRUE)
str_split('a|b|c|d', '\\|', n = 3, simplify = TRUE)
# You can also split strings up by character, line, sentence, and word using the 
# boundary() function from stringr. This function defines regex boundaries based on 
# the input type. 'Character' breaks by each character, 'line_break' breaks by the 
# locale's line break character, 'sentence' breaks by sentence, and 'word' breaks by
# word. For example,
x <- 'This is a sentence. This is another sentence.'
str_view_all(x, boundary('word')) # Note how the period is not highlighted
str_view_all(x, boundary('character'))
str_view_all(x, boundary('sentence'))
str_view_all(x, boundary('line_break'))
str_split(x, ' ')[[1]]
str_split(x, boundary('word'))[[1]] # Gives similar results, just no periods

## 14.4.5.1 Exercises 

# 1 - Split up a string like 'apples, pears, and bananas' into individual components

tundra_animals <- 'bears, wolves, and caribou'
ind_tundra_animals <- str_split(str_replace_all(tundra_animals, c('and' =  '', ' ' = '')), ',',
                                       simplify = TRUE)
ind_tundra_animals

# 2 - Why is it better to split up by boundary('words') rather than ' '?
# Splitting words via boundary('words') will also remove any punctuation (like periods)
# from the split words.

# 3 - What does splitting an empty string do?
str_split('dog', '')
str_split('The quick brown dog jumped over the lazy fox.', '') # Sentence
# This splits the input string by characters and "is equivalent to boundary('character')".