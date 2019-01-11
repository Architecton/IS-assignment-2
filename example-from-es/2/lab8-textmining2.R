#
# Text classification in R
#

# Load the text-mining library
#install.packages("tm")
library(tm)

# Load the corpus

# Set working directory to the one containing the unzipped economics folder
setwd("<path to folder>")

# Load the corpus from the directory
corpus <- Corpus(DirSource("economics"))
# Inspect the results
length(corpus)
corpus[[1]]$content

# Do the basic preprocessing, as in the previous exercises
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

# Convert the dataset into a matrix
# Specify tf-idf weighting with the second parameter
dataset <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))
dataset

# Read the topics of the texts (the thing we will be trying to classify)
Topic <- read.table("economics-topics.txt")
Topic

# Merge the topics to the matrix
# The matrix must first be converted from a sparse representation 
# to a normal matrix with as.matrix
dataset <- cbind(as.matrix(dataset), Topic)

# Rename the column of topics to that it will be easier to referrence later
# "Topic" is explicitly capitalized so that the new name doesn't conflict with the names
# of the document-term matrix, which are all named after words that appear in the dataset
names(dataset)[ncol(dataset)] <- "Topic"
dataset$Topic

# Split the dataset into training and testing sets
# Sample selects 200 random numbers from 1 to the number of rows in the dataset 
# The third parameter specifies that the same row cannot be picked twice
sel <- sample(nrow(dataset), 200, F)

# Index with the obtained vector to select the correct rows
train <- dataset[-sel,]
test <- dataset[sel,]

# Support vector machines work well for classification based on document-term matrices
# Kernlab provides a good implementation
library(kernlab)

# Classification is done as with other models
model <- ksvm(Topic ~ ., train, kernel="rbfdot")
predicted <- predict(model, test, type="response")

# Check the results
t <- table(test$Topic, predicted)
t
sum(diag(t))/sum(t)

#
# Adding new features to the dataset
#

# We will add "average sentence length" as a new feature for each document
# Reload the corpus so that we can accurately count sentences and words
corpus <- Corpus(DirSource("economics"))

# Load the libraries needed for tokenization
#install.packages("NLP")
#install.packages("openNLP")
library(NLP)
library(openNLP)

# Initialize a sentence and word tokenizer
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()

# Create a function for calculating the average sentence length in a single document
avg_sent_len <- function(text, sent_ann, word_ann) {
  # Count the sentences
  a1 <- annotate(text, sent_ann)
  num_sents <- length(a1)
  # Count the words
  a2 <- annotate(text, word_ann, a1)
  # a2 also contains the sentence annotation, so we subtract them to obtain the 
  # correct number of words
  num_words <- length(a2) - length(a1)
  return(num_words/num_sents)
}

# Use the lapply function to run avg_sent_len on every document in the corpus
sent_lens <- lapply(corpus, avg_sent_len, sent_ann, word_ann) 

# The result of lapply is a list with names and we need to convert this to a
# vector
sent_lens
sent_lens <- unlist(sent_lens, use.names=F)

# We then convert the vector to a data frame 
sent_lens <- data.frame(sent_lens)
sent_lens

# We can then merge the new feature to the dataset
# For better performance, normalizing the new feature first would be a good thing
# to do
dataset <- cbind(dataset, sent_lens)
names(dataset)[ncol(dataset)]
dataset$sent_lens


#
# Creating a function for predicting a missing word
#

# We want to go over our dataset and count how often each word appears after
# another word
# We use a list and treat it as a dictionary
occurances <- list()

# A test on a single sentence 
text <- "Today's topic is text mining"

# We first split that into words 
text <- strsplit(text, split = ' ')

# strsplit returns a list that only contains a single element in our case
text <- text[[1]]

# Loop over every word
for(i in 1:length(text)) {
  word1 <- text[i]
  word2 <- text[i+1]
  # Add the information to the dictionary
  occurances[[word1]] <- c(occurances[[word1]], word2)
}
# A result is a dictionary where each word contains information of all the words
# that followed it in the training dataset
occurances

# Running this on a single sentence does not give us anything useful
# We run this on the entire corpus
occurances <- list()
for(j in 1:length(corpus)){
  text <- corpus[[j]]$content
  text <- strsplit(text, split=' ')
  text <- text[[1]]
  for(i in 1:length(text)) {
    word1 <- text[i]
    word2 <- text[i+1]
    occurances[[word1]] <- c(occurances[[word1]], word2)
  }
}

# We can't print the entire result, so we just print the names
names(occurances)

# We can print the result for one specific word
occurances[["public"]]

# To get the most frequent word, we first construct a table
table <- table(occurances[["public"]])
table

# Sort the table 
sorted_table <- sort(table(occurances[["public"]]), decreasing=T)
sorted_table

# The name of the first element will be the most frequent word
names(sorted_table[1])

