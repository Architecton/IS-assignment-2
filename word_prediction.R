# Parse dictionary that maps words to table of occurances of anteceding words.
tab_dicts <- readRDS("data/tab_dicts.rds")

# Parse stop words
f <- file('english.stop.txt', 'r')
stop_words <- readLines(f)
close(f)

# Prompt user to enter sentence and replace missing with an underscore.
preceding_word <- NULL
while (TRUE) {
  words_proc <- strsplit(gsub("[.!?\\-]", "",tolower(readline(prompt="Enter a sentence. Replace the word you want to omit with an underscore. "))), " ")[[1]]
  words <- words_proc
  sel <- !(words_proc %in% stop_words)
  words_proc <- words_proc[sel]
  pos_missing <- which(as.logical(match(words_proc, '_')))  # Get location of underscore.
  if (length(pos_missing) == 0) {                     # Handle user input errors.
    cat(sprintf("Missing underscore. Please try again."))
  } else if (pos_missing == 1) {
    cat(sprintf("invalid position of underscore. The underscore must have at least one word preceding it. Please try again."))
  } else {
    preceding_word <- words_proc[pos_missing-1]
    break
  }
}

# n_most_prob: get n most probable words. If number of options < n, return as many as you can.
n_most_prob <- function(tab, n) {
  sorted_tab <- sort(tab, decreasing=TRUE)  # Sort by frequency.
  sorted_tab_selected <- sorted_tab[1:n]    # Select n most probable and remove any NAs
  sel <- unname(!is.na(sorted_tab_selected[1:n]))
  return (names(sorted_tab_selected[sel]))
}

# look_back_k: Sum frequency for provided list of frequency tables.
look_back_k <- function(word, tab_dicts) {
  name_union <- NULL              # Construct union of names found in documents.
  for (tab_dict in tab_dicts) {
    if (length(unlist(tab_dict[word])) > 0) {
      name_union <- union(name_union, names(get(word, tab_dict[word])))
    }
  }
  # Make a combined table of frequencies.
  combined_tab <- integer(length(name_union))
  names(combined_tab) <- name_union
	for (l in 1:length(tab_dicts)) {
	  tab_dict <- tab_dicts[[l]]
	  tab_dict <- tab_dict[word]
	  if (length(unlist(tab_dict)) > 0) {  # Increment for next set of values.
  	  sel_names <- names(get(word, tab_dict))
  	  tab_selection <- names(combined_tab) %in% sel_names
  	  combined_tab[tab_selection] <- combined_tab[tab_selection] + unname(get(word, tab_dict[word]))
	  }
	}
  
  # Remove redundant words.
  combined_tab <- combined_tab[-which(names(combined_tab) %in% c("cap", "caps", "capss", ""))]
  return (combined_tab)
}

# Get combined frequency table.
combined_tab <- look_back_k(preceding_word, tab_dicts)
n <- 5
list_most_prob <- n_most_prob(combined_tab, n)

# List n most probable words.
if (length(list_most_prob) == 0) {
  print("No suggestions available for this sentence.")
} else {
  cat(sprintf("displaying %d most probable missing words. Press enter to cycle to the next one.\n", length(list_most_prob)))
  for (k in 1:length(list_most_prob)) {
    words_tmp <- words
    words_tmp[which(words_tmp == "_")] <- list_most_prob[k]
    propose_nxt <- paste(words_tmp, collapse=" ")
    propose_nxt <- paste(propose_nxt, ".", sep="")
    propose_nxt <- gsub("(^[[:alpha:]])", "\\U\\1", propose_nxt, perl=TRUE)
    print(propose_nxt)
    invisible(readline())
  } 
}