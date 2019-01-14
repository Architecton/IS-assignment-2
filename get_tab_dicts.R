library('tm')
library('NLP')
library('openNLP')
library('stringr')
library('purrr')

# Loading the corpus and performing the preprocessing
wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)

# Parse list of stop words.
f <- file('english.stop.txt', 'r')
stop_words <- readLines(f)
close(f)

# Parse corpus and perform pre-processing.
raw_corpus <- Corpus(DirSource("word-predictor-train"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stop_words)


# Get dictionary that maps each word to words that follow it.
get_tab_dict <- function(back_num, corpus) {
  occurances <- list()
  for(k in seq_along(corpus)) {  
    text <- corpus[[k]]$content              # Get text of next piece in corpus.
    text <- strsplit(text, split=' ')[[1]]   # Get words in text.
    for(l in seq_along(text)) {              # Go over words
      word1 <- text[l]                       # word2 follows word1.
      word2 <- text[l+back_num]
      occurances[[word1]] <- c(occurances[[word1]], word2)   # Count occurances.
    }
  }
  
  # Make dictionary of tables for each word.
  tab_dict <- list()
  for (word in names(occurances)) {   # Go over all words found in corpus.
    table_next <- sort(table(occurances[[word]]), decreasing=T)  # Get sorted table.
    tab_dict[[word]] <- table_next                               # Save table in dictionary.
  }
  return (tab_dict)
}

lim = 5
tab_dicts <- vector(mode = "list", length = lim)
for (k in 1:lim) {
  tab_dicts[[k]] <- get_tab_dict(k, corpus)
}

saveRDS(tab_dicts, file = "data/tab_dicts.rds")