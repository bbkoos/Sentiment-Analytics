

scrape_cnn = function() {   
  
  #load urls 
  {
    frontPage  = readLines ( "http://www.cnn.com")
    reg = gregexpr( "\"uri\"" , frontPage)
    ind1 = reg[[1]]
    reg = gregexpr( "\"uri\".*?headline" , frontPage)
    ind2 = reg[[1]]
    ind2 = ind2 + attr(ind2 , "match.length")
    urls = substring(frontPage , ind1+8, ind2-12)
    urls = paste( "http://www.cnn.com/", urls, sep="")
    urls = urls[-which( sapply( gregexpr( "/videos/" , urls) , "[[" , 1 ) >0 )]
    rm( list = setdiff( ls() , "urls"  ))
  }
  
  
  #init stuff
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
    for( i in 1:length(urls)) {
      
      print( length(urls)-i)
      page  = readLines(urls[i])   
      
      ind1 = gregexpr( "<title>", page[[1]])
      ind1 = ind1[[1]]
      ind2 = gregexpr( "<title>.*?</title>", page[[1]])    
      ind2 = ind2[[1]] + attr( ind2[[1]], "match.length")
      titles[[i]] = substring( page[[1]] , ind1+7 , ind2-19)
      
      
      
      urls[i]
      
      if( length(page) == 1 ) {
        
        reg = gregexpr( "</cite>", page)
        reg2 =gregexpr( "</cite>.*?footer\"" , page)
        reg2 = reg[[1]] + attr(reg2[[1]], "match.length")
        ind1 = reg[[1]][1]
        ind2 = reg2[[1]]
        article = substring ( page , ind1+11 ,ind2[1]-58)
        article
        article = article[1]
        
        split = strsplit( article, "</div><div class=\"")
        split = split[[1]]
        reg = gregexpr( "paragraph\">",split )
        ind = which( sapply ( reg, "[[", 1)>0 )
        split = split[ind]
        article = paste(split, collapse = " ")
        
        articles[[i]] = article
        
        
      } else if (length(page) == 3 ) {
        
        reg = gregexpr( "</cite>", page[1])
        reg2 =gregexpr( "</cite>.*?twitter\">" , page[1])
        reg2 = reg2[[1]] + attr(reg2[[1]] , "match.length")
        ind1 = reg[[1]]
        ind2 = reg2[[1]]
        sub1 = substring ( page , ind1+6 ,ind2[1]-57)
        
        reg = gregexpr( ">Read More<", page[3])
        reg2 =gregexpr( "footer\"" , page[3])
        ind1 = reg[[1]]
        ind2 = reg2[[1]]
        sub2 = substring ( page[3] , ind1+91 ,ind2[1]-51)
        
        article = paste(sub1,sub2)
        
        split = strsplit( article, "</div><div class=\"")
        split = split[[1]]
        reg = gregexpr( "paragraph\">",split )
        ind = which( sapply ( reg, "[[", 1)>0 )
        split = split[ind]
        article = paste(split, collapse = " ")
        
        articles[[i]] =     article[1]
        
      } else {
        articles[[i]] = NA
      }
      
      
    }
    ind = which( nchar(articles) >10)
    articles = articles[ ind ]
    titles = titles[ind]
    urls = urls[ind]
    
  }
  
  #get sentiments 
  {
    posWords = NULL
    negWords = NULL
    sentiments=NULL
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
      posWords = list()
      negWords = list()
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
      sentiments$urls = urls
      sentiments$article = articles 
      titles_beta =  strsplit( gsub("http://www.cnn.com/" , "", urls), "/" )
      for(i in 1:length(titles_beta)) {
        L = length(titles_beta[[i]])
        #titles[i] = titles_beta[[i]][L]  #see scrape_data section 
        titles_beta[[i]][1]
        category[i] = titles_beta[[i]][L-2]
        date[i] = paste(  titles_beta[[i]][2] ,  titles_beta[[i]][3] ,  titles_beta[[i]][1] , sep="/" )
        #if(L>8) {
        #  subcategory[i]= titles_beta[[i]][L-2]
        #} else {
        #  subcategory[i] = NA
        #}
      }
      #titles= gsub("-", " ", titles)
      #titles =gsub( ".html", "", titles)
      sentiments$title = titles
      sentiments$category = category 
      #sentiments$subcategory = subcategory 
      score = sentiments$pos/sentiments$neg 
      ind = which( score > 5/3)
      rating[ind] = "Very Pos"
      ind = which( score  >= 4/3 & score <= 5/3 )
      rating[ind] = "Positive"
      ind = which( score <= 3/4 & score >= 3/5)
      rating[ind] = "Negative"
      ind = which( score <= 3/5 )
      rating[ind] = "Very Neg"
      ind = which(score < 4/3 & score > 3/4)
      rating[ind] = "Neutral"
      sentiments$score = score
      sentiments$rating = rating
      sentiments$date = date
      
      order = order(score)
      sentiments = sentiments[order,]
      #links= links[order]
      posWords = posWords[order]
      negWords = negWords[order]
      
      order = c(5,6,7,8,9,1,2,3,4)
      sentiments = sentiments[,order]
      rownames(sentiments) = c(1:nrow(sentiments))
    } #titles,categories,dates,ratings,scores
    {
      ind = !is.na(sentiments$rating)
      sentiments = sentiments[ind,]
      negWords = negWords[ind]
      posWords = posWords[ind]
      links = links[ind]
      
    }
    {
      rm( list = setdiff( ls() , c("sentiments","posWords","negWords","links")  ) )
    } 
  }
  
  list = list()
  list[[1]] = sentiments 
  list[[2]] = posWords
  list[[3]] = negWords
  list[[4]] = links
  
  return(list)

}



