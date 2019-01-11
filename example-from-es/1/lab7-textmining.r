# Load the file
# Unlike the past lab exercises, read.table() won't work since the file does
# not contain a table. Instead, it contains 425 articles, with one line per
# article
# As always, use setwd() to set the working directory

# Reading this file works like it does in most programming languages
# Open the file
f <- file("time.mag.txt", 'r')
# Read the lines into a vector. Since each article is in its own line, this
# will give us a vector of articles
text <- readLines(f)
# Close the file
close(f)

# Inspect  our dataset
length(text)
text[1]

# Load the necessary libraries
#install.packages("tm")
#install.packages("SnowballC")
library(tm)

# Transform our loaded text into a corpus so that we can better work on it
# with the tm library
# With VectorSource, we specify that we are creating it from a vector.
# DirSource is an interesting alternative, and creates a corpus from a folder
# of text documents
corpus <- Corpus(VectorSource(text))
# Inspect the results.
corpus[[1]]$content
corpus[[1]]$meta

# Do some basic preprocessing on the text, which will make machine-learning
# methods work better
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
corpus[[1]]$content

# Put everything to lowercase
corpus <- tm_map(corpus, tolower)
corpus[[1]]$content

# Remove stopwords
# We load in a custom stopwords list. The tm library also provides a list that
# can be accessed with the stopwords() function, but that one is much less 
# comprehensive
f <- file('english.stop.txt', 'r')
mystopwords <- readLines(f)
mystopwords
corpus <- tm_map(corpus, removeWords, mystopwords)
corpus[[1]]$content

# Stemming
# We want to covert words with the same root to the same word.
# For example: walk, walking, walked, walks should all be converted to walk
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

# Create a term document matrix
# We want to transform text into a matrix of numbers
# A Term document matrix is the simplest way we can do that - it
# indicates how often each word appears in each document
tdm <- TermDocumentMatrix(corpus)
tdm

# Look at some of the results
rownames(tdm)
which(rownames(tdm) == 'berlin')
inspect(tdm[359,])

# We can immediatelly find similar words based on the articles they appear in
findAssocs(tdm, 'berlin', 0.5)

# Clustering
# We want to create an article recommendation system that will be capable
# of recommending similar articles to a person reading a specific article
# We start with a similar matrix, with two changes:
#   - We use a DocumentTermMatrix instead. This is just a transposed 
#   TermDocumentMatrix, with rows containing documents and columns containing
#   words
#   - We use tf-idf weighting to put more importance on rarer words
dtm <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))
# We also remove words that are too sparse (appear in too few articles)
dtm <- removeSparseTerms(dtm, sparse=0.8)
# If we print dtm, we can see that we drastically reduced the number of words
# (terms) in our matrix
dtm

# Clusterin with hierarchical clustering
# Instead of using the default euclidian distance, we want to use cosine
# distance instead. Cosine distance isn't effected by the length of the
# articles like euclidian distance is.
# This measure is provided by the proxy library
install.packages("proxy")
library(proxy)

# We specify the measure when constructing the distance matrix 
d <- dist(as.matrix(dtm), method="cosine")
result <- hclust(d)

# Plot the result. in R studio, they will probably be too small to see
# To fix that, export the results as a very large (100x100 inches) pdf file
plot(result)


# Part-of-speech tagging
# We want to determine whether each word is a noun/verb/adjective ...
# Load the required libraries
install.packages("NLP")
install.packages("openNLP")
library(NLP)
library(openNLP)

# We also need to install the POS-tagging model
install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")

# Use the first two sentences of the 1st article as an example
text <- "The allies after nassau in december 1960, the U.S. first proposed to help nato develop its own nuclear strike force. But europe made no attempt to devise a plan."

# We first need to split the text into sentences
sent_ann <- Maxent_Sent_Token_Annotator()
a <- annotate(text, sent_ann)
# Get the sentences by indexing with our result
as.String(text)[a]

# And then into words
word_ann <- Maxent_Word_Token_Annotator()
# Sentence annotations must be provided to the word annotator
a2 <- annotate(text, word_ann, a)
a2

# After that, we can obtain the POS tags
post_ann <- Maxent_POS_Tag_Annotator()
a3 <- annotate(text, post_ann, a2)
a3
