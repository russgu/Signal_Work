library(qdap)

setwd("/Users/russ/signalds/Week 6/")
comments = read.csv("github-comments.csv", header=FALSE, sep="\t")
colnames(comments) = c("username", "comment")

comments$comment=as.character(comments$comment)
comments$username=as.character(comments$username)

result=check_text(comments$comment)
result=check_spelling_interactive(comments$comment)
comments$comment=result

comments$linus=as.numeric(comments$username=="torvalds")

senti.analysis=polarity(comments$comment,grouping.var = comments$linus)
View(senti.analysis$all[order(abs(senti.analysis$all$polarity),decreasing = TRUE),])

#plot polarity scores
library(ggplot2)
qplot(x=1:84, y=senti.analysis$all$polarity[senti.analysis$all$linus==0])+geom_smooth()

# Do a t-test to evaluate if the mean polarity between the two groups
# of comments are statistically significant

t.test(
  x=senti.analysis$all$polarity[senti.analysis$all$linus==0],
  y=senti.analysis$all$polarity[senti.analysis$all$linus==1]
  )
