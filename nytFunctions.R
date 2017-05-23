
library(RCurl)

library(igraph)

scrape_nyt = function () {
  {
    #scrape urls 
    {
      frontPage = readLines("https://www.nytimes.com/")
      
      reg1 = gregexpr( "<h2 class=\"story-heading", frontPage)
      reg2 = gregexpr( ".html.*/h2>", frontPage)
      
      ind1  = which( sapply(reg1, "[[", 1)>0)
      ind2  = which( sapply(reg2, "[[", 1)>0)
      
      ind = (which(!ind2 %in% ind1))
      ind2 = ind2[ -ind]
      ind = (which(!ind1 %in% ind2))
      ind = ind1[-ind]
      
      page = frontPage[ind]
      ind1 = sapply (gregexpr("http", page), "[[",1)
      ind2 = sapply(reg2, "[[", 1)[ind]
      
      url = substring( page, ind1 ,ind2+4)
      
      rm("reg1")
      rm("reg2")
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

        lines =  getURL(url[k], .opts = curlOptions(cookiejar="",  useragent = "Mozilla/5.0", followlocation = TRUE))
        
        char1 = "<title>"
        char2= "<title>.*?</title>"
        ind1 = regexpr( char1 , lines) +7
        ind2 = regexpr( char2 , lines)
        ind2 = ind2 + attr( ind2, "match.length")-30
        title = substring(lines,ind1 ,ind2)
        
        ind1 = regexpr("data-total-count", lines)
        ind2 = regexpr( "<footer class=" , lines  )
        article = substring( lines, ind1+23, ind2)
        L = length(gregexpr("<a href=.*?>",article)[[1]])
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
        titles[k]  = title 
        links[[k]] = link
        articles[[k]] = article 
      }
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
        sentiments$urls = url
        sentiments$article = articles 
        titles_beta =  strsplit( gsub("http://www.nytimes.com/" , "", url), "/" )
        for(i in 1:length(titles_beta)) {
          L = length(titles_beta[[i]])
          #titles[i] = titles_beta[[i]][L]  #see scrape_data section 
          titles_beta[[i]][1]
          category[i] = titles_beta[[i]][L-1]
          date[i] = paste(  titles_beta[[i]][5] ,  titles_beta[[i]][6] ,  titles_beta[[i]][4] , sep="/" )
          if(L>8) {
            subcategory[i]= titles_beta[[i]][L-2]
          } else {
            subcategory[i] = NA
          }
        }
        #titles= gsub("-", " ", titles)
        #titles =gsub( ".html", "", titles)
        sentiments$title = titles
        sentiments$category = category 
        sentiments$subcategory = subcategory 
        score = (sentiments$pos+1)/ (sentiments$neg+1) 
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
        links= links[order]
        posWords = posWords[order]
        negWords = negWords[order]
        order = c(5,6,7,8,9,10,1,2,3,4)
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

}


related_links = function( link , layers, graph ) {
  graph = NULL
  
  if(layers == 0 ) {
    return()
  }
  
  center = strsplit( gsub("<a href=\"http://www.nytimes.com/" , "", link), "/" )
  center = center[[1]][length(center[[1]])]
  center = sub( ".html" , "" , gsub("-" , " " , center )) 
  center = sub ( "\">" , "" ,center)
  
  url = sub( "<a href=\"", "", link) 
  url = sub("\">" , "", url)
  
  lines =  getURL(url, .opts = curlOptions(cookiejar="",  useragent = "Mozilla/5.0", followlocation = TRUE))
  ind1 = regexpr("data-total-count", lines)
  ind2 = regexpr( "<footer class=" , lines  )
  article = substring( lines, ind1+23, ind2)
  L = length(gregexpr("<a href=.*?>",article)[[1]])
  links = NULL
  for(i in 1:L){
    reg =  regexpr("<a href=.*?>",article)
    ind1 = reg
    ind2 = reg + attr(reg, "match.length") 
    links[i] = substring( article, ind1,ind2-1)
    sub1 = substring(article,1,ind1)
    sub2 = substring(article,ind2)
    article = paste(sub1,sub2)
  }
  
  urls = gsub( "<a href=\"", "", links) 
  urls = gsub("\">" , "", urls)
  ind = which ( sapply ( gregexpr(".com/20[0-1][0-9]/", urls) ,"[[",1) >0 )
  urls = urls[ind]
  
  L = length(urls)
  alpha = urls 
  linked  = NULL
  
  if( L > 0) {
    alpha
    alpha =  strsplit( gsub("http://www.nytimes.com/" , "", alpha), "/" )
    for(i in 1:L) {
      l = length(alpha[[i]])
      linked[i] = alpha[[i]][l]
    }
    
    linked = gsub( ".html" , "" , gsub("-" , " " , linked))
    
    for( i in 1:length(linked)) { 
      graph = c(graph , center , linked[i])  
    }
  }
  
  layers = layers - 1  

  for(i in 1:L) {
      graph =  c(graph, related_links( urls[i], layers , graph=graph))
  }
  
  return(graph)            
}
