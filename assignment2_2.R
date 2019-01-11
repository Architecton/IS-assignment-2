library('tm')
library('NLP')
library('openNLP')
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


# Prepare the documentTermMatrix

# A document-term matrix or term-document matrix is a mathematical matrix that 
# describes the frequency of terms that occur in a collection of documents. 
# In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms. 
# There are various schemes for determining the value that each entry in the matrix should take. One such scheme is tf-idf. 
# They are useful in the field of natural language processing.

dataset <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))


# Initialize a sentence and word tokenizer
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()


# Use the lapply function to run avg_sent_len on every document in the corpus.
sent_lens <- lapply(corpus, function (text) {
  # Count the sentences.
  a1 <- annotate(text, sent_ann)
  num_sents <- length(a1)
  # Count the words.
  a2 <- annotate(text, word_ann, a1)
  # a2 also contains the sentence annotation, so we subtract them to obtain the correct number of words.
  num_words <- length(a2) - length(a1)
  return(num_words/num_sents)
})
sent_lens <- unlist(sent_lens, use.names=FALSE) # Remove names.
sent_lens <- data.frame(sent_lens)

# Get number of different words in each document and
# use the data to create a new document feature.
num_diff_words <- lapply(corpus, function (text) {
  length(unique(unlist(strsplit(text, " "))))
})
num_diff_words <- unlist(num_diff_words, use.names=FALSE) # Remove names.
num_diff_words <- data.frame(num_diff_words) # Create a data frame.

# Get number of rare words used in each essay. Use the data
# to create a new document feature

get_rare_words <- function(dtm) {
  word_frequencies <- apply(dtm, 2, function(row) {
    sum(row)
  })
  freq <- sort(word_frequencies)
  return (names(freq[1:200]))
}

get_rare_words(dataset)






# Merge new features onto the dataset.
dataset <- cbind(dataset, sent_lens)
dataset <- cbind(dataset, num_diff_words)
names(dataset)[ncol(dataset)]





# The dataset can be accessed as > dataset[, 'dataset']
# The sentence lengths can be accessed as > dataset[, 'sent_lens']
