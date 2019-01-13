library('tm')
library('NLP')
library('openNLP')
library('stringr')
library('purrr')

# Loading the corpus and performing the preprocessing
wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)

# Parse corpus and perform pre-processing
raw_corpus <- Corpus(DirSource("essay"))
n <- names(raw_corpus)
n <- gsub("essay_", "", n)
n <- gsub(".txt", "", n)
o <- order(as.numeric(n))
raw_corpus <- raw_corpus[o]
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)

# Prepare the documentTermMatrix

# A document-term matrix or term-document matrix is a mathematical matrix that 
# describes the frequency of terms that occur in a collection of documents. 
# In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms. 
# There are various schemes for determining the value that each entry in the matrix should take. One such scheme is tf-idf. 
# They are useful in the field of natural language processing.

dataset <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))

# Initialize a sentence and word tokenizer.
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()


## SENTENCE LENGTHS ################################################################

# Use the lapply function to run avg_sent_len on every document in the corpus.
sent_lens <- lapply(corpus, function (text) {
  a1 <- annotate(text, sent_ann)  # Count the sentences.
  num_sents <- length(a1)
  a2 <- annotate(text, word_ann, a1)  # Count the words.
  num_words <- length(a2) - length(a1)
  return(num_words/num_sents)
})
sent_lens <- unlist(sent_lens, use.names=FALSE)  # Remove names.
sent_lens <- data.frame(sent_lens)

# Remove punctuation from corpus.
corpus <- tm_map(raw_corpus, removePunctuation)



## NUMBER OF DIFFERENT WORDS #######################################################

# Get number of different words in each document and
# use the data to create a new document feature.
num_diff_words <- lapply(corpus, function (text) {
  return (length(unique(unlist(strsplit(text, " ")))))  # Get number of different words in text
})
num_diff_words <- unlist(num_diff_words, use.names=FALSE)  # Remove names.
num_diff_words <- data.frame(num_diff_words)  # Create a data frame.





## NUMBER OF RARE WORDS ############################################################

# Get number of rare words used in each essay. Use the data
# to create a new document feature

get_rare_words <- function(dtm, num) {
  word_frequencies <- apply(dtm, 2, function(row) {
    sum(row)
  })
  freq <- sort(word_frequencies)
  return (names(freq[1:num]))
}

rare_words <- get_rare_words(dataset, 3000)  # Get rare words (last num by occurances)
num_rare_words <- lapply(corpus, function(text) {  # Compute number of rare words in each text.
  return (length(intersect(strsplit(text, " ")[[1]], as.list(rare_words))))
})
num_rare_words <- unlist(num_rare_words, use.names=FALSE)
num_rare_words <- data.frame(num_rare_words)





## TOTAL NUMBER OF WORDS ###########################################################

# Get total number of words in each text in corpus.
total_num_words <- lapply(corpus, function (text) {
  return (length(strsplit(text, " ")[[1]]))
})
total_num_words <- unlist(total_num_words, use.names=FALSE)
total_num_words <- data.frame(total_num_words)



# Merge new features into the dataset.
dataset <- cbind(as.matrix(dataset), sent_lens)
dataset <- cbind(dataset, num_rare_words)
dataset <- cbind(dataset, total_num_words)
dataset <- cbind(dataset, num_diff_words)


# Save dataset.
saveRDS(dataset, file = "dataset1.rds")

# Perform feature selection and save selector vector.
feature_sel <- feature_selection(dataset, 'vocabulary', 5, 'rbfdot', vocabulary)
saveRDS(feature_sel, file = "feature_sel_vocab.rds")