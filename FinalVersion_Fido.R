

ISP_name<-"Fido"
# harvest some tweets
some_tweets <- userTimeline(ISP_name, n=1500)

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


filename<-paste("BarChartEmotion",ISP_name,".png")
# plot distribution of emotions
png(filename, width=12, height=8, units="in", res=600)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  stat_bin(aes(label=..count..), vjust=-0.5, 
           geom="text", position="identity")+
  
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(),  axis.text.y = element_blank(), legend.position="none",axis.text.x = element_text(size=20,face="bold"))+
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") 
dev.off()

filename<-paste("BarChartPolarity",ISP_name,".png")
png(filename, width=12, height=8, units="in", res=600)
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  stat_bin(aes(label=..count..), vjust=-0.5, 
           geom="text", position="identity")+
  
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(),  axis.text.y = element_blank(), legend.position="none",axis.text.x = element_text(size=20,face="bold"))+
  scale_fill_brewer(palette="Paired") +
  labs(x="polarity categories", y="number of tweets") 
dev.off()


# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
myStopwords <- c(stopwords('english'), ISP_name,'get','fido')

emo.docs = removeWords(emo.docs, myStopwords)
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos


# comparison word cloud
filename<-paste("WorldCloud",ISP_name,".png")
png(filename, width=12, height=8, units="in", res=600)
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
dev.off()
