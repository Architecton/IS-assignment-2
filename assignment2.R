library('tm')
library('NLP')
library('stringr')
library('purrr')

# Loading the corpus and performing the preprocessing
wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)
raw_corpus <- Corpus(DirSource("essay"))
corpus <- tm_map(raw_corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)


# Assignment 1


#### (1) Computing the average number of sentences per document. #####

# Find average number of sentances by counting dots.
sum_dots <- 0
lengths <- integer(length(raw_corpus))
for (k in seq_along(raw_corpus)) {    # Go over each document in corpus and count dots
  num_dots_next <- str_count(raw_corpus[[k]]$content, '\\.')
  sum_dots <- sum_dots + num_dots_next
  lengths[k] <- num_dots_next
}
avg_num_sentences <- sum_dots / length(raw_corpus)
print(sprintf("Average number of sentances in documents = %.3f", avg_num_sentences))

# Visually present the results
par(mfrow=c(1,2))
barplot(lengths, col="lightblue")
abline(h=avg_num_sentences, col="red")
hist(lengths, col="lightblue", main="Distribution of Sentence Amounts", 
     xlab="number of sentences", ylab="relative frequency",  breaks=20)


#### (2) Computing the average length of sentences per document. #####

avg_sentence_lengths <- integer(length(raw_corpus))
sentence_lengths <-vector(mode="list", length=length(raw_corpus))
for (k in seq_along(raw_corpus)) {    # Go over each document
    sentences <- as.list(str_split(raw_corpus[[k]]$content, '\\.')[[1]])
    lengths <- unlist(map(sentences, nchar))
    sentence_lengths[[k]] <- lengths
    avg_length <- sum(lengths)/length(lengths)
    avg_sentence_lengths[k] <- avg_length
}

sum_sentence_lengths <- sum(unlist(map(sentence_lengths, sum)))
num_sentences_total <- sum(unlist(map(sentence_lengths, length)))
avg_sentence_length <- sum_sentence_lengths/num_sentences_total

print(sprintf("Average sentence length = %.3f", avg_sentence_length))

# Filter out outliers
sel <- unlist(map(unlist(avg_sentence_lengths), function (len) {
  return (len < avg_sentence_length + 3*sd(avg_sentence_lengths))
}))
avg_sentence_lengths <- avg_sentence_lengths[sel]


# Visually present the results

# bar plot
frame()
par(mfrow=c(1,2))
barplot(avg_sentence_lengths, col="lightblue", main="Average Sentence Lengths",
        xlab="document index", ylab="average sentence length")
abline(h=avg_sentence_length, col="red")

# distribution
hist(avg_sentence_lengths, col="lightblue", breaks=40, main="Distribution of Average Sentence Lengths",
     xlab="average sentence lengths")
abline(v=avg_sentence_length, col="red")




# (3) Computing the most popular words ###############################

# Get the term document matrix
corpus_words <- tm_map(raw_corpus, removePunctuation)
corpus_words <- tm_map(corpus_words, removeNumbers)
corpus_words <- tm_map(corpus_words, tolower)
corpus_words <- tm_map(corpus_words, removeWords, stopwords())
tdm <- TermDocumentMatrix(corpus_words)
word_frequencies <- apply(tdm, 1, function(row) {
  sum(row)
})

# Sort words by frequencies.
word_frequencies <- sort(word_frequencies, decreasing=TRUE)

# Print results to console.
print("Ten words with the highest frequencies:")
print(word_frequencies[1:10])

# Visually present the results.
frame()
par(mfrow=c(1,2))
barplot(word_frequencies[1:10], col='lightblue', main="Frequencies of the 10 most Frequent Words")

# Plot distribution of frequencies.
hist(word_frequencies, col="lightblue", breaks=30,
     main="Distribution of Word Frequencies", xlab="word frequency")


# (4) the association between words ##################################

# Find similar words based on documents they appear in.
corr_thresh <- 0.4
assocs <- map(names(word_frequencies[1:10]), function(word) {
  findAssocs(tdm, word, corr_thresh)
})

frame()
par(mfrow=c(2,2))
cat(sprintf("Associations for the 10 most frequent words (using correlation value of %f as threshold): ", corr_thresh))
for (k in seq_along(assocs)) {
  name = names(assocs[[k]])
  cat(sprintf("word = %s,\nassociations:\n", name))
  data <- get(name, assocs[[k]])
  if (length(data) != 0) {
    barplot(data, ylab="correlation value", main=sprintf("Associations for Word '%s'", name))
  }
  assoc <- names(get(name, assocs[[k]]))
  if (length(assoc) != 0) {
    cat(paste(assoc, collapse=", "))
    cat('\n')
  } else {
    cat('none\n')
  }
}
par(mfrow=c(1,1))