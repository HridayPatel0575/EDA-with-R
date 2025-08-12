# Example 1
input <- readline(prompt = "Enter your input: ")

letter <- grepl("[a-zA-Z]", input)
digit <- grepl("[0-9]", input)
special <- grepl("[_@*/]", input)
length <- nchar(input) >= 8

if (letter && digit && special && length) {
  print("Valid input")
} else {
  print("Invalid input")
}



# Example of paste() and paste0()
paste("Hello", "World", sep = ", ")
paste0("Hello", "World")

# Example of print()
print("Hello World")

# Example of cat()
cat("Hello World\n")
cat("Line 1\nLine 2")


main_string <- "Hello World"
substring <- "World"

if (grepl(substring, main_string)) {
  print(paste("The string contains the substring:", substring))
} else {
  print(paste("The string does not contain the substring:", substring))
}

# Example 2
input <- readline(prompt = "Enter your input: ")
length = FALSE
digit = FALSE
special = FALSE
letter = FALSE

if(nchar(input) >= 8){
  length = TRUE
}