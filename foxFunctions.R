
#rm(list=ls())
#setwd("/Users/benkoos/Documents/Data_Analysis/Sentiment_Analytics/functions")
library(rvest)


scrape_fox  = function() {
  ################## initial stuff 
  {
    fox_html <- read_html("http://www.foxnews.com/",encoding="ISO-8859-1")
    fox_hp <- html_nodes(fox_html,"#latest a , b")
    fox_hp <- as.character(fox_hp)
    fox_hp1i <- regexpr("<a href",fox_hp)
    fox_hp1 <- fox_hp[which(fox_hp1i!=-1)]
    
    
    ## getting links bens way
    link_ind <- regexpr("<a href=\"",fox_hp1)
    link_beg_ind <- which(link_ind!=-1)
    link_beg<- link_ind[link_beg_ind] + attr(link_ind,"match.length")
    
    link_ind2 <- regexpr("<a href=\".*?\">",fox_hp1)
    link_end_ind <- which(link_ind2!=-1)
    link_end <- link_ind2[link_end_ind] + attr(link_ind2,"match.length") -3
    
    fox_links <- substring(fox_hp1,link_beg,link_end)
    
    
    ## getting titles bens way
    title_ind <- regexpr("\">",fox_hp1)
    title_beg_ind <- which(title_ind!=-1)
    title_beg <- title_ind[title_beg_ind] + attr(title_ind,"match.length")
    
    title_ind2 <- regexpr("\">.*?</a>",fox_hp1)
    title_end_ind <- which(title_ind2!=-1)
    title_end <- title_ind2[title_end_ind] + attr(title_ind2,"match.length")-5
    
    fox_titles_1 <- substring(fox_hp1,title_beg,title_end)
    fox_titles_2 <- strsplit(fox_titles_1, "<b>")
    fox_titles_3 <- c()
    
    for(i in 1:length(fox_titles_2)){
      fox_titles_3[i] <- fox_titles_2[[i]][length(fox_titles_2[[i]])]
    }
    
    fox_titles_4 <- unlist(strsplit(fox_titles_3,"</b>"))
    fox_titles_5 <- strsplit(fox_titles_4,"<img src")
    fox_titles_6 <- unlist(lapply(fox_titles_5,"[[",1))
    
    fox_titles <- fox_titles_6
    
    ##getting rid of videos
    videoi <- regexpr("video",fox_links)
    video_ind <- which(videoi==-1)
    fox_titles_nov <- fox_titles[video_ind]
    fox_links_nov <- fox_links[video_ind]
    
    #getting rid of aol links
    aoli <- regexpr("aol",fox_links_nov)
    aoli_ind <- which(aoli==-1)
    fox_titles_nov <- fox_titles_nov[aoli_ind]
    fox_links_nov <- fox_links_nov[aoli_ind]
    
    
    ##function with try catch
    readURL <- function(url){
      out<- tryCatch(
        {
          read_html(url,warn=FALSE)
        },
        error= function(cond){
          # message(paste("URL does not seem to exist:", url))
          # message("Here's the original error message:")
          # message(cond)
          return("NA")
        },
        warning=function(cond) {
          # message(paste("URL caused a warning:", url))
          # message("Here's the original warning message:")
          # message(cond)
          # Choose a return value in case of warning
          return("NA")
        },
        finally={
          # message(paste("Processed URL:", url))
          # message("Some other message at the end")
        }
        
      )
      return(out)
      
    }
  }
  ################## Getting bodies
  {
    fox_body <- list()
    for(i in 1:length(fox_links_nov)){
      
      
      print(i)
      
      fox_text <- readURL(fox_links_nov[i])
      if(fox_text=="NA"){next}
      fox_text1 <- html_nodes(fox_text,"article div div p")
      
      sch_i <- regexpr("schema",fox_text1)
      ind <- which(sch_i==-1)
      fox_text2 <- as.character(fox_text1[ind])
      
      fox_text3 <- strsplit(fox_text2,"<p>")
      fox_text4 <- c()
      if( length(fox_text3) >0 ) {
        for(j in 1:length(fox_text3)){
          fox_text4[j] <- fox_text3[[j]][length(fox_text3[[j]])]
        }
        
        fox_text5 <- unlist(strsplit(fox_text4,"</p>"))
      } else {
        fox_text5 = NA
      }
      fox_body[[i]]<- fox_text5
      
    }
  }
  ################## translate reeds code to bens code 
  {
    articles = fox_body
    titles = fox_titles_nov
    urls = fox_links_nov
    rm(list = setdiff( ls(),c("urls","articles", "titles")))
    i = which(nchar(articles) > 4) 
    articles = articles[i]
    urls = urls[i]
    titles = titles[i]
    art = NULL
    for( l in 1:length(articles)) {
      art[l] = paste(articles[[l]] , collapse = " ")
    }
    articles =art 
    #init list/vectors
    {
      links = list()
      sentiments = NULL
      posWords = list()
      negWords = list()
      category = NULL
      subcategory = NULL
      date = NULL
      rating = NULL
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
  {
    {
      sentiments$article = articles 
      titles_beta =  strsplit( gsub("http://www.nytimes.com/" , "", urls), "/" )
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
      rownames(sentiments) = c(1:nrow(sentiments))
    } #titles,categories,dates,ratings,scores
    {
      ind = !is.na(sentiments$rating)
      sentiments = sentiments[ind,]
      negWords = negWords[ind]
      posWords = posWords[ind]
      
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

