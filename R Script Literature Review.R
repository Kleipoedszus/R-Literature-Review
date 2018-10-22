### This script reads in tables produced in AtlasTi for analysis
### Define Environment ####
#define some colors to use throughout
my_colors <- c("#AED581", "#FFD54F", "#29B6F6", "#78909C", "#8D6E63")

#Set Options
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'C')

# Function to remove blanks
blank.removal <- function(x) {
  x <- unlist(strsplit (x,' '))
  x <- subset(x,nchar(x)>0)
  x <- paste(x,collapse=' ')
}

theme_text <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

### Read Data and Clean it ####
library(tidyverse) 
library(tidytext)
library(tm)
library(pbapply) #adds progress bar
### to create HTML Tables ####
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function

corpus_table <- c("lit_review_keywords.csv") # Type in name of data table to be analysed
path <- file.path("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review", corpus_table)

corpus <- read.csv(path, header = TRUE, sep = ";")
documents <- read.csv("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/lit_rev_documents.csv",
                 header = TRUE, sep = ";")
documents <- select(documents, id = Number, Name, Groups)
removal_words <- read.csv("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/removal_words.csv",
                          header = TRUE, sep = ";")
removal_words <- removal_words$remove
corpus <- as.tibble(corpus) #Turn dataframe to tibble
corpus <- select(corpus, Number, Text.Content, Document, Codes)
corpus <- rename(corpus, text = Text.Content)
corpus <- merge(corpus, documents, by.x="Document", by.y="Name", sort = FALSE)
corpus <- add_column(corpus, dec_type = str_extract(corpus$Groups, "Dec_Typ: [A-Z].."))
corpus <- add_column(corpus, subject = str_extract(corpus$Groups, "Subj: [A-Z].."))
corpus <- add_column(corpus, approach = str_extract(corpus$Groups, "Appr: [A-Z].."))
corpus <- add_column(corpus, tools = str_extract(corpus$Groups, "Tool: [A-Z].."))


corpus$subject <- gsub("Subj: CF","Case Factors",corpus$subject)
corpus$subject <- gsub("Subj: OF","Organisational Factors",corpus$subject)
corpus$subject <- gsub("Subj: DF","Decision Maker Factors",corpus$subject)

corpus$dec_type <- gsub("Dec_Typ: CL","Clinical Decisions",corpus$dec_type)
corpus$dec_type <- gsub("Dec_Typ: SD","Strategic Decisions",corpus$dec_type)

corpus$approach <- gsub("Appr: QT","Quantitative",corpus$approach)
corpus$approach <- gsub("Appr: QL","Qualitative",corpus$approach)

corpus$tools <- gsub("Tool: QT","Quantitative Tools",corpus$tools)
corpus$tools <- gsub("Tool: CF","Case File Analysis",corpus$tools)
corpus$tools <- gsub("Tool: ET","Ethnographic Tools",corpus$tools)
corpus$tools <- gsub("Tool: PT","Psychometric Testing",corpus$tools)
corpus$tools <- gsub("Tool: SA","Statistical Analysis",corpus$tools)
corpus$tools <- gsub("Tool: VT","Vignette Studies",corpus$tools)

corpus$text <- iconv(corpus$text, "utf-8", "ASCII", sub = "")
corpus$text <- str_replace_all(corpus$text, "[\r\n]" , " ") #Remove CR
corpus$text <- str_replace_all(corpus$text, "- ", "")
corpus$text <- str_replace_all(corpus$text, "-", "")
corpus$text <- str_replace_all(corpus$text, "[,()&;:\\\"]", "")
corpus$text <- str_replace_all(corpus$text, " \\\\", "")
corpus$text <- tolower(corpus$text)
corpus$text <- removeNumbers(corpus$text)
corpus$text <- gsub(" *\\b[[:alpha:]]{1,4}\\b *", " ", corpus$text)
corpus$text <- removeWords(corpus$text, stopwords("english"))
corpus$text <- removeWords(corpus$text, removal_words)
corpus$text <- pblapply(corpus$text, blank.removal) # Remove Blanks
corpus$text <- unlist(corpus$text)


### Add column with Publication Year ####
corpus <- mutate(corpus, year = str_extract_all(corpus$Document, "\\d+"))
corpus$year <- unlist(corpus$year)
corpus$year <- as.integer(corpus$year)
#create the decade column
corpus <- corpus %>%
  mutate(decade = 
           ifelse(corpus$year %in% 1978:1979, "1970s", 
                  ifelse(corpus$year %in% 1980:1989, "1980s", 
                         ifelse(corpus$year %in% 1990:1999, "1990s", 
                                ifelse(corpus$year %in% 2000:2009, "2000s", 
                                       ifelse(corpus$year %in% 2010:2015, "2010s", 
                                              "NA"))))))

write.csv(corpus, file = "/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/Cleaned_Lit_Data.csv")
rm(removal_words, documents)

### Create a first overview ####
# Draw a barplot showing when articles where released
tiff(paste0("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/Plots/", "Publication_Yr", corpus_table, ".tiff"), 
     units="in", width=5, height=5, res=300)
corpus %>%
  group_by(year, subject) %>%
  summarise(number_of_articles = n()) %>%
  ggplot() + 
  geom_bar(aes(x = year, y = number_of_articles, 
               fill = subject), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Years of Publication") +
  labs(x = NULL, y = "Article Count")
dev.off()
# Circle Plot of number of documents
documents_year <- corpus %>%
  select(Document, year) %>%
  group_by(year) %>%
  summarise(doc_count = n())

id <- seq_len(nrow(documents_year))
documents_year <- cbind(documents_year, id)
label_data = documents_year
number_of_bar = nrow(label_data) #Calculate the ANGLE of the labels
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar #Center things
label_data$hjust <- ifelse(angle < -90, 1, 0) #Align label
label_data$angle <- ifelse(angle < -90, angle + 180, angle) #Flip angle
ggplot(documents_year, aes(x = as.factor(id), y = doc_count)) +
  geom_bar(stat = "identity", fill = alpha("darkslategray", 0.7)) +
  geom_text(data = label_data, aes(x = id, y = doc_count + 10, label = year, hjust = hjust), color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-10, 100) + #Size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))

### Relationship between Subject of Decision Making and Decision Type
library(circlize) #Visualizations - chord diagram
decade_chart <-  corpus %>%
  filter(dec_type != "NA") %>% #Remove documents without publication date
  count(dec_type, subject)  #Get document count per subject level per decade. Order determines top or bottom.

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("Case Factors" = my_colors[3], "Decision Maker Factors" = my_colors[4], "Organisational Factors" = "my_colors[2]", 
             "Clinical Decisions" = my_colors[5], "Strategic Decisions" = "darkviolet") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_chart[[1]])) - 1), 15,
                         rep(5, length(unique(decade_chart[[2]])) - 1), 15))

chordDiagram(decade_chart, grid.col = grid.col, transparency = .2)
title("Relationship Between Subject and Decision Type")

### Relationship between Subject of Decision Making and Tools used for research
decade_chart <-  corpus %>%
  filter(tools != "NA") %>% #Remove documents without publication date
  count(tools, subject)  #Get document count per subject level per decade. Order determines top or bottom.

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("Vignette Studies" = my_colors[1], "Ethnographic Tools" = my_colors[2], "Quantitative Tools" = my_colors[3], 
             "Case File Analysis" = my_colors[4], "Statistical Analysis" = my_colors[5],
             "Case Factors" = "gold2", "Decision Maker Factors" = "firebrick", "Organisational Factors" = "darkviolet") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_chart[[1]])) - 1), 15,
                         rep(5, length(unique(decade_chart[[2]])) - 1), 15))

chordDiagram(decade_chart, grid.col = grid.col, transparency = .2)
title("Relationship Between Subject and Tools used for research")


##################################################################
### Using Natural Language Processing to identify key terms ####
#### https://www.r-bloggers.com/an-overview-of-keyword-extraction-techniques/ 
library(udpipe) # Needed for NLP
library(textrank)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = corpus$text, doc_id = corpus$Document) # Annotate using udpipe
x <- as.data.frame(x)

stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

# Analysing Collocation and Co-occurence  
## Collocation (words following one another)
stats <- keywords_collocation(x = x, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(stats, 40)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 3) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")

# Create a WordCloud
library(textrank)
stats <- textrank_keywords(x$lemma, 
                           relevant = x$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 3)

library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)

# Using the RAKE method
stats <- keywords_rake(x = x, 
                       term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                       relevant = x$upos %in% c("NOUN", "ADJ"),
                       ngram_max = 4)
head(subset(stats, freq > 2))

#Use dependency parsing output to get the nominal subject and the adjective of it
stats <- merge(x, x, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))


### Create an overview of the dataset ####
# Add word frequency to tidy_corpus
corpus_words <- corpus %>% 
  unnest_tokens(word, text) %>%
  count(Document, id, word, dec_type, subject, year, sort = TRUE) %>%
  ungroup()
total_words <- corpus_words %>% 
  group_by(Document) %>% 
  summarize(total = sum(n))
corpus_words <- left_join(corpus_words, total_words)
# remove stopwords from corpus_words
data("stop_words")
corpus_words <- anti_join(corpus_words, stop_words, by = "word")
rm(total_words, stop_words)
# Create Document Term Matrix and Term Document Matrix on Basis of Ratings
dtm <- corpus_words %>%
  cast_dtm(Document, word, n)
tdm <- as.TermDocumentMatrix(dtm)
dtm.m <- as.matrix(dtm)
tdm.m <- as.matrix(tdm)

### Explore Lexical Diversity and density over time
# Lexical Diversity
library(gridExtra) #`grid.arrange()` for multi-graphs
lex_diversity_per_year <- corpus %>%
  filter(year != "NA") %>%
  unnest_tokens(word, text) %>%
  group_by(Document,year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(year, lex_diversity)) +
  geom_point(color = my_colors[3],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = year, y = lex_diversity), se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_text()

#Lexical Density
lex_density_per_year <- corpus %>%
  filter(year != "NA") %>%
  unnest_tokens(word, text) %>%
  group_by(Document,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(year, lex_density)) + 
  geom_point(color = my_colors[4],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", 
              se = FALSE, 
              method = "lm") +
  geom_smooth(aes(x = year, y = lex_density), 
              se = FALSE,
              color = "blue", 
              lwd = 2) +
  ggtitle("Lexical Density") + 
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_text()

grid.arrange(diversity_plot, density_plot, ncol = 2)

# Overal Word Frequencies
library(ggthemes)
term.freq <- rowSums(tdm.m)
freq.df <- data.frame(word=names(term.freq), frequency=term.freq)
freq.df <- freq.df[order(freq.df[,2], decreasing = T),]
freq.df$word <- factor(freq.df$word, levels=unique(as.character(freq.df$word)))
frequent_terms <- freq.df %>%
  top_n(30) 
ggplot(frequent_terms, aes(x=word, y=frequency))+
  geom_bar(stat="identity", fill='darkred') +
  coord_flip() + theme_gdocs()+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)+
  ggtitle("Most frequent words")

rm(term.freq, freq.df, frequent_terms)

#Plot high tf-idf Words in reports
corpus_words <- corpus_words %>%
  bind_tf_idf(word, Document, n)
plot_reports <- corpus_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(Document, word = factor(word, levels = rev(unique(word))))
plot_reports %>% 
  top_n(30) %>%
  ggplot(aes(word, tf_idf, fill = "darkred")) +
  theme(legend.position="none") +
  geom_col() +
  facet_wrap(~ subject, scales = "free") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  ggtitle("Overall top 30 tf-idf words")
rm(plot_reports)

### Topic Modelling ####
library (lda)
library(pbapply)
library(LDAvis)
library(treemap)
library(topicmodels)
library(car)
library(xlsx)
library(qdap)
library(tidytext)

options(stringsAsFactors = F)
k <- 4
num.iter <- 25
alpha <- 0.02
eta <- 0.02
set.seed(1234)

documents <- lexicalize(corpus$text)
wc <- word.counts(documents$documents, documents$vocab)
doc.length <- document.lengths(documents$documents)

fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, 
                                   K = k, vocab = documents$vocab, 
                                   num.iterations = num.iter, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0, 
                                   compute.log.likelihood = TRUE)

plot(fit$log.likelihoods[1,])

topic_words <- as.data.frame(top.topic.words(fit$topics, 7, by.score = TRUE)) # which words used in topic?
write.xlsx(x = topic_words, file = "/Users/stefan_kleipoedszus/Documents/@Promotion/Atlas Ti/Literature Review/Decision_Making_Topics.xlsx",
           sheetName = "TopTopicWords", row.names = FALSE)

top.topic.documents(fit$document_sums, 1) # which documents represent topic best?

theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
topics.json <- createJSON(phi = phi, theta = theta, doc.length = doc.length, 
                                    vocab = documents$vocab, term.frequency = as.vector(wc))
# type serVis(topics.json) into console to examine results

### Topic Modelling using the Tidy Approach ####
source_lda <- LDA(dtm, k = 4, control = list(seed = 1234))
topics <- tidy(source_lda, matrix = "beta")

tidy_lda <- tidy(source_lda)
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")


### Sentiment Analysis ####
library(ggrepel) #`geom_label_repel`

new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

corpus_words %>%
  mutate(words_in_text = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_text, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_text) %>%
  select(lexicon, lex_match_words,  words_in_text, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Words Found In Lexicons")

doc_nrc <- corpus_words %>%
  inner_join(get_sentiments("nrc"))
doc_nrc <- select(doc_nrc, id, word, year, sentiment)

### Create HTML Table of sentiment words
plot_words <- doc_nrc %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()

#Same comments as previous graph
plot_words %>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_text() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("NRC Sentiment") +
  coord_flip()


### Analyse Bigrams
doc_bigrams <- corpus %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- doc_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
AFINN <- get_sentiments("afinn")

negation_words <- c("findings", "feelings", "study", "found", "evidence")

negation_bigrams <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  slice(seq_len(20)) %>%
  arrange(word1,desc(contribution)) %>%
  ungroup()

bigram_graph <- negation_bigrams %>%
  graph_from_data_frame() #From `igraph`

set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_edge_density(aes(fill = score)) +
  geom_node_point(color = "purple1", size = 1) + 
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bigram Network")

