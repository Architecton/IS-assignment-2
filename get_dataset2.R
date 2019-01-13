library('purrr')
library('tm')
library('NLP')
library('openNLP')

# suppress warnings
options(warn=-1)

wd <- "/home/jernej/Desktop/Repositories/projects/IS-assignment-2"
setwd(wd)

dataset <- readRDS("data/dataset1.rds")

raw_corpus <- Corpus(DirSource("essay"))
n <- names(raw_corpus)
n <- gsub("essay_", "", n)
n <- gsub(".txt", "", n)
o <- order(as.numeric(n))
raw_corpus <- raw_corpus[o]
corpus <- tm_map(raw_corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)

get_avg_num_pos_type <- function(res, pos_type) {
  get_num_sent <- function(res) {
    get_constituents <- function(res) {
      sel <- integer(length(res))
      for (k in seq_along(res)) {
        sel[k] = names(res[k]$features[[1]]) == 'constituents'
      }
      return (sel)
    }
    sum(get_constituents(res))
  }
  count_pos_type <- function(res, pos_type) {
    counter <- 0
    for (k in seq_along(res)) {
      if (unname(unlist(res[k]$features)) == pos_type ) {
        counter <- counter + 1
      }
    }
    return (counter)
  }
  return (count_pos_type(res, pos_type)/get_num_sent(res));
}

pos_list <- strsplit("CC CD DT EX FW IN JJ JJR JJS LS MD NN NNS NNP NNPS PDT POS PRP PRP$ RB RBR RBS RP SYM TO UH VB VBD VBG VBN VBP VBZ WDT WP WP$ WRB", " ")[[1]]

sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()
post_ann <- Maxent_POS_Tag_Annotator()
res_feat_block <- matrix(nrow=0, ncol=length(pos_list))
for (text in corpus$content) {
  res_row <- integer(length(pos_list))
  k <- 0
  a <- annotate(text, sent_ann)
  a2 <- annotate(text, word_ann, a)
  a3 <- annotate(text, post_ann, a2)
  for (k in seq_along(pos_list)) {
    num_pos_nxt <- get_avg_num_pos_type(a3, pos_list[k])
    res_row[k] <- num_pos_nxt
  }
  res_feat_block <- rbind(res_feat_block, res_row)
}

res_feat_block <- data.frame(res_feat_block)
dataset_aug <- cbind(dataset, res_feat_block)
saveRDS(dataset_aug, file = "data/dataset2.rds")