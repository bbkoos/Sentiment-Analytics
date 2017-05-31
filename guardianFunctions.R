library(RCurl)

scrape_guardian = function() {
  #scrape urls 
  {
    
    frontPage =  getURL("https://www.theguardian.com/us", .opts = curlOptions(cookiejar="",  useragent = "Mozilla/5.0", followlocation = TRUE))
    
    ind1 = gregexpr( "title\"><a href=\"" , frontPage)
    ind2 = gregexpr( "title\"><a href=\".*?class=\"", frontPage)
    ind1 = ind1[[1]] + attr(ind1[[1]] , "match.length")
    ind2 = ind2[[1]] + attr(ind2[[1]] , "match.length") - 10
    
    url = substring(frontPage, ind1, ind2)
    url[1]
    rm(frontPage)
  }
  #init list/vectors
  {
    articles = NULL
    interactives = list()
    links = list()
    sentiments = NULL
    posWords = list()
    negWords = list()
    titles = NULL
    category = NULL
    subcategory = NULL
    date = NULL
    rating = NULL
  }
  #scrape data 
  {
    for(k in 1:length(url)) {
      
      print(length(url)-k)
      #lines =  getURL(url[k], .opts = curlOptions(cookiejar="",  useragent = "Mozilla/5.0", followlocation = TRUE))
      
      l = readLines(url[k])
      
      r = regexpr( "<title>.*?</title>", l)
      i = which( sapply (r , "[[" , 1) >0)
      t = substring( l[i] , 8 , nchar(l[i]) -8)
      
      r = gregexpr("article-review-body\">", l)
      i1 = which( sapply (r , "[[" , 1) >0)
      r = gregexpr("<footer class=", l)
      i2 = which( sapply (r , "[[" , 1) >0)
      
      length(i1)> 0 & length(i2)>0 
      
      if( length(i1)> 0 & length(i2)>0 ) {
        l =  l[(i1+1):(i2-1)]
        r = gregexpr("<p>.*?</p>", l)
        i3 = which( sapply (r , "[[" , 1) >0)
        l = l[i3]
        article = paste(l , collapse = " ")
        
        r =gregexpr("<a href=.*?>",article)[[1]]
        i =which( sapply(r , "[[", 1)>0 )
        L = length(i)
        link = NULL
        for(i in 1:L){
          reg =  regexpr("<a href=.*?>",article)
          ind1 = reg
          ind2 = reg + attr(reg, "match.length") 
          link[i] = substring( article, ind1,ind2-1)
          sub1 = substring(article,1,ind1)
          sub2 = substring(article,ind2)
          article = paste(sub1,sub2)
          
        }
      } else {
        link = NA
        article =NA
      }
      
      links[[k]] = link
      articles[[k]] = article 
      titles[[k]] = t[1]
      
      #for (i in (ind1+1):ind2) {
      #  line = lines[i]
      #  line
      #  if (regexpr("scr=\"", line)[[1]][1] > 0){
      #    interactives[[k]][j] = line
      #  } else if(regexpr("<p>", line)) {
      #    line = gsub( "<p>" , "" , strsplit(line, "</p>")[[1]][1])
      #    article  = paste(article, line )
      #  }
      #}
      
      #L = length(gregexpr( "</a>", article)[[1]])
      #link = NULL
      #for( i in 1:L) {
      #  ind1 = gregexpr( "<a href",article)[[1]][1]
      #  ind2 = gregexpr( "</a>", article)[[1]][1]
      #  link[i] = substring(article, ind1 , ind2)
      #  sub1 = substring(article,1,ind1)
      #  sub2 = substring(article,ind2)
      #  article = paste(sub1,sub2)    
      #}
      
    }
    
    i = which(nchar(articles) > 10) 
    articles = articles[i]
    links = links[i]
    titles = titles[i]
    url = url[i]
    
  }
  #get sentiments 
  {
    {library(rvest)
      library(readr)
      library(plyr)
      library(stringr)
      library(RTextTools)} # load libraries 
    {
      sentimentFunction <- function(sentences, positiveWords, negativeWords, .progress='none') {
        finalScore <- matrix('', 0, 3)
        scores <- laply(sentences, function(sentence, positiveWords, negativeWords) {
          compareSentence <- sentence
          # split into words
          word.list <- str_split(sentence, '\\s+')
          words <- unlist(word.list)
          # compare our words to the lexicon of +/- terms
          posMatches <- match(words, positiveWords)
          negMatches <- match(words, negativeWords)
          
          positives = words[!is.na(posMatches)]
          negatives = words[!is.na(negMatches)]
          L = length(positives)
          
          # find total number of words in each category
          posMatches <- sum(!is.na(posMatches))
          negMatches <- sum(!is.na(negMatches))
          score <- c(posMatches, negMatches)
          # add row to scores table
          new_row <- c(compareSentence, score)
          finalScore <- rbind(finalScore, new_row)
          finalScore <- c(finalScore, positives, negatives,L)
          return(finalScore)
        }, positiveWords, negativeWords)
        return(scores)
      }
    } # load sentiment function
    {
      pos <- as.character(read.csv("/Users/benkoos/Documents/Data_Analysis/Sentiment_Analytics/lexicon/positive.csv")[,1])
      neg <- as.character(read.csv("/Users/benkoos/Documents/Data_Analysis/Sentiment_Analytics/lexicon/negative.csv")[,1])
    } # load word libraries 
    {
      L =length(articles)
      for( k in 1:L) {
        sentiment = sentimentFunction( articles[[k]],pos,neg)
        l = length(sentiment)
        lp = as.numeric(sentiment[l])+3
        posWords[[k]] = sentiment[4:lp]
        negWords[[k]] = sentiment[(lp+1):(l-1)]
        numPos = as.numeric(sentiment[2])
        numNeg = as.numeric(sentiment[3])
        sentiment = data.frame( numPos, numNeg  ,stringsAsFactors = F)
        #colnames(sentiment) = paste("article",k,sep="")
        sentiments = rbind(sentiments, sentiment)
      }
      sentiments = data.frame(sentiments)
      colnames(sentiments) = c("pos", "neg")
    } # calc sentiments 
  }
  #finilize
  {
    {
      
      sentiments$article = articles 
      titles_beta =  strsplit( gsub("http://www.theguardian.com/" , "", url), "/" )
      for(i in 1:length(titles_beta)) {
        
        titles_beta
        L = length(titles_beta[[i]])
        #titles[i] = titles_beta[[i]][L]
        category[i] = titles_beta[[i]][4]
        date[i] = paste(  titles_beta[[i]][6] ,  titles_beta[[i]][7] ,  titles_beta[[i]][5] , sep="/" )
        #if(L>5) {
        #  subcategory[i]= titles_beta[[i]][5]
        #} else {
        #  subcategory[i] = NA
      }
      #titles= gsub("-", " ", titles)
      #titles =gsub( ".html", "", titles)
      sentiments$title = titles
      sentiments$category = category 
      score = sentiments$pos/sentiments$neg 
      ind = which( score  >= 1.5 )
      rating[ind] = "Positive"
      ind = which( score <= .66)
      rating[ind] = "Negative"
      ind = which(score < 1.5 & score > .66)
      rating[ind] = "Neutral"
      sentiments$score = score
      sentiments$rating = rating
      sentiments$date = date
      order = order(score)
      sentiments = sentiments[order,]
      posWords = posWords[order]
      negWords = negWords[order]
      links = links[order]
      order = c(8,4,5,6,7,1,2,3)
      sentiments = sentiments[,order]
      View(sentiments)
      rownames(sentiments) = c(1:nrow(sentiments))
    } #titles,categories,dates,ratings,scores
    {
      ind = !is.na(sentiments$rating)
      sentiments = sentiments[ind,]
      negWords = negWords[ind]
      posWords = posWords[ind]
      links  = links[ind]  
    }
    {
      rm( list = setdiff( ls() , c("sentiments","posWords","negWords","links")  ) )
    } #
  }
  list = list()
  list[[1]] = sentiments 
  list[[2]] = posWords
  list[[3]] = negWords
  list[[4]] = links
  
  return(list)
  
}


