#' A random word (or words, if desired) containing the word provided.
#'
#' @param word single input string
#' @param number number of random words to return
#' @return character vector of length specified of words containing the word  
#' @export
#' @examples
#' random_word_containing_your_word("dog")
#' random_word_containing_your_word("that",3)
random_word_containing_your_word <- function(word,number = 1) {
englishwords <- readLines("http://www-personal.umich.edu/~jlawler/wordlist")
words_with_word <- englishwords[grep(word,englishwords)]
if (length(words_with_word) == 0) {
  stop("Sorry, couldn't find any words containing the word provided.")
}
else if (length(words_with_word) < number) {
  warning(paste0("User wanted ",number,"words, but only ",length(words_with_word),"words containing your word exist!"))
  number <- length(words_with_word)
}
sample(words_with_word,number,replace = FALSE)
}