## bnary packages:
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)

## one source:
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

## On a Mac, save the folder to your desktop and use the following code chunk:
cname <- file.path("~", "Desktop", "projData")   
cname 

## Use this to check to see that your texts have loaded. 
dir(cname)

## Load the R package for text mining and then load your texts into R.
library(tm)   
docs <- Corpus(DirSource(cname))   
summary(docs) 

## If you so desire, you can read your documents in the R terminal 
## using inspect(docs). Or, if you prefer to look at only one of 
## the documents you loaded, then you can specify which one using something like:
inspect(docs[1])


##
## PART I
## Preprocessing
##
## Removing punctuation:
##  Your computer cannot actually read. Punctuation and other special characters only look like more words to your computer and R. Use the following to methods to remove them from your text
docs <- tm_map(docs, removePunctuation)

## If necessary, such as when working with emails, you can remove special characters.
## This list has been customized to remove punctuation that you commonly
## find in emails. You can customize what is removed by changing them as you see fit, to meet your own unique needs.
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
} 

## Converting to lowercase:
##   As before, we want a word to appear exactly the same every time it appears. We therefore change everything to lowercase.
docs <- tm_map(docs, tolower)

## Removing "stopwords" (common words) that usually have no analytic value.
## In every text, there are a lot of common, and uninteresting words (a, and, also, the, etc.). Such words are frequent by their nature, and will confound your analysis if they remain in the text.
# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english")) 

## Removing common word endings (e.g., "ing", "es", "s")
## This is referred to as "stemming" documents. 
## We stem the documents so that a word will be recognizable to the computer, 
## whether or not it may have a variety of possible endings in the original text.
library(SnowballC)   
docs <- tm_map(docs, stemDocument) 

## Stripping unnecesary whitespace from your documents:
##   The above preprocessing will leave the documents with a lot of "white space". White space is the result of all the left over spaces that were not removed along with the words that were deleted. The white space can, and should, be removed.
docs <- tm_map(docs, stripWhitespace)   

## To Finish
## Be sure to use the following script once you have completed preprocessing.
## This tells R to treat your preprocessed documents as text documents.
docs <- tm_map(docs, PlainTextDocument)   

##
## PART II:
## Staging the Data
##

## create a document term matrix.
## This is what you will be using from this point on.
dtm <- DocumentTermMatrix(docs[1:2])   
dtm 

## To inspect, you can use: inspect(dtm)
## This will, however, fill up your terminal quickly. So you may prefer to view a subset:
inspect(dtm[1:2, 1:20]) #view [first 2] docs & first 20 terms - modify as you like
dim(dtm) #This will display the number of documents & terms (in that order)

## You'll also need a transpose of this matrix. Create it using:
tdm <- TermDocumentMatrix(docs[1:2])   
tdm

##
## PART III
## Explore your data!
##

## Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)

##Export the matrix to Excel:   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv") 

## Have you ever seen an Excel file with tha tmany columns??
# Try this instead":
mt <- as.matrix(tdm)   
dim(mt)   
write.csv(mt, file="tdm.csv") 

#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

## Word Frequency

## There are a lot of terms, so for now, just check out some of the most and least frequently occurring words.
freq[head(ord)] 
freq[tail(ord)]

## Check out the frequency of frequencies.
head(table(freq), 20)

## The resulting output is two rows of numbers. 
## The top number is the frequency with which words appear 
## and the bottom number reflects how many words appear that frequently. 
## Here, considering only the 20 lowest word frequencies, we can see that 
## 7407 terms appear only once and 2562 terms appear twice. 
## There are also a lot of others that appear very infrequently:
tail(table(freq), 20) 

## Considering only the 20 greatest frequencies, we can see that there is a huge disparity in how frequently some terms appear.

## For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms, above. (Look just under the word "Focus".)
freq <- colSums(as.matrix(dtms))   
freq 

## Or:
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)   

## An alternate view of term frequency:
##  This will identify all terms that appear frequently (in this case, 50 or more times).
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.

## another way to do this:
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

## Plot Word Frequencies

## Plot words that appear at least 250 times.
library(ggplot2)   
p <- ggplot(subset(wf, freq<300 & freq>100), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 


##
## PART IV
## Statistics
##

## Relationships Between Terms

## Term Correlations

## If you have a term in mind that you have found to be particularly meaningful to your analysis, 
## then you may find it helpful to identify the words that most highly correlate with that term.
## If words always appear together, then correlation=1.0.
findAssocs(dtms, "vessel", corlimit=0.98) # specifying a correlation limit of 0.999   
## In this case, "vessel" was highly correlated with numerous other terms

## Word Clouds!

## Humans are generally strong at visual analytics. 
## That is part of the reason that these have become so popular. What follows are a variety of alternatives for constructing word clouds with your text.
## But first you will need to load the package that makes word clouds in R.
library(wordcloud)   

##Plot words that occur at least 250 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=250)  

## Note: The "set.seed() function just makes the configuration of the layout of the clouds consistent each time you plot them. You can omit that part if you are not concerned with preserving a particular layout.

## Plot the 100 most frequently used words.
set.seed(142)   
wordcloud(names(freq), freq, max.words=100) 

##Add some color and plot words occurring at least 250 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=250, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

##Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 


## Clustering by Term Similarity

## To do this well, you should always first remove a lot of the uninteresting or infrequent words. 
## If you have not done so already, you can remove these with the following code.
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)