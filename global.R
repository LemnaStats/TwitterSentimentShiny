#Tweet Stripper
require(tidyverse)
require(tidytext)
require(httr)

APIkey <- "N6t47mWc75m3omWuuOK2efWR0"
APIsecret <- "e6fiNAFrpuuGakbtInoE8v9tdzDilE5zrXgl1tw0ukZxj1gHRx"
bearer_token <- "AAAAAAAAAAAAAAAAAAAAAMS6XwEAAAAA1D8a4lJWJZP2yVJLEn7r%2Fzsk5m0%3D3n1w2DqpF2HWN9HHHlEGrq56gFx2NHfQsOFlQ2yRXjAyhbBgIp"
AccessToken <- "128872936-RJp6XvOZNcFQNWAH3TqFfeVEJ0UpUUz6fZOsnKX7"
AccessSecret <- "LXWfbLPC1OQzY2meoMQERwPioMDIzWJmOhaTiAOxJFLfJ"
ClientID <- "YWdoVWRlYlpWcTlNNFBfRHFOR086MTpjaQ"
ClientSecret <- "R481m4n2uw_LIwwoJoF04eTs_FElx-lo4H2eRfoD2rx975Fe-5"

searchTwitterTextAndTimestamp <- function(search_terms,n_results){
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )
  
  params = list(
    `query` = search_terms,
    `max_results` = n_results,
    `tweet.fields` = 'created_at'
  )
  
  
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', 
                        httr::add_headers(.headers=headers), query = params)
  
  
  recent_search_body <-
    content(
      response,
      as = 'parsed',
      type = 'application/json',
      simplifyDataFrame = TRUE
    )
  
  return(recent_search_body$data)
}

  single_tweet_sentiment <- function(string,bad){
    scorp_words <- string
    afinn <- get_sentiments("afinn")
    scorp <- str_replace_all(scorp_words, "[^[:alnum:]]", " ")
    scorp <- paste(scorp, collapse = " ")
    scorp <- str_split(scorp," ") %>% unlist() %>% as.data.frame()
    colnames(scorp) <- c("word")
    scorp$word <- tolower(scorp$word)
    scorp <- scorp %>% filter(., !(word %in% bad))
    scorp_count <- scorp %>% count(word)
    scorp_count <- merge.data.frame(scorp_count,afinn)
    scorp_count <- mutate(scorp_count, score = n * value)
    final_score <- scorp_count$score %>% sum()
    return(final_score) }

stsvect <- Vectorize(single_tweet_sentiment, vectorize.args = "string")

text_amalgamator <- function(data,bad){
scorp_words <- data$text
afinn <- get_sentiments("afinn")
scorp <- str_replace_all(scorp_words, "[^[:alnum:]]", " ")
scorp <- paste(scorp, collapse = " ")
scorp <- str_split(scorp," ") %>% unlist() %>% as.data.frame()
colnames(scorp) <- c("word")
scorp$word <- tolower(scorp$word)
scorp <- scorp %>% filter(., !(word %in% bad))
scorp_count <- scorp %>% count(word)
scorp_count <- merge.data.frame(scorp_count,afinn)
scorp_count <- mutate(scorp_count, score = n * value)
scorp_count <- tibble(scorp_count)
return(scorp_count) }

twitter_sentiment <- function(query,filtered){
  tweets <- searchTwitterTextAndTimestamp(query,100)
  sentis <- text_amalgamator(tweets,filtered)
  return(sentis)
}

zodiac_signs <- tibble(c("aries","taurus","gemini","cancer","leo","virgo","libra","scorpio","sagittarius","capricorn","aquarius","pisces"))
colnames(zodiac_signs) <- "signs"

zodiac_men_list <- mutate(zodiac_signs, sign_men = str_glue("{signs} men"))
zodiac_men_list <- zodiac_men_list$sign_men 

count_tester <- function(input, yes_no){
  testvar <- filter(input, word == yes_no)$n 
  if (length(testvar) == 0) {
    testvar <- 0
  }
  return(testvar)
}

conduct_sentiment <- function(query,filtered_words){
  gem_tibble <- tibble(query)
  colnames(gem_tibble) <- "query"
  gem_sent <- twitter_sentiment(query,filtered_words)
  gem_tibble$word_count <- sum(gem_sent$n)
  gem_tibble$yes_count <- count_tester(gem_sent, "yes")
  gem_tibble$no_count <- count_tester(gem_sent,"no")
  gem_tibble$score <- sum(gem_sent$score)
  gem_sent <- arrange(gem_sent,desc(n))
  gem_tibble$common_words <- filter(gem_sent, n > 1)$word %>% paste(.,collapse = ", ")
  return(gem_tibble)
}

analyse_sentiment <- function(query,sent_table,tweets,filters){
  gem_tibble <- tibble(query)
  colnames(gem_tibble) <- "query"
  gem_tibble$word_count <- sum(sent_table$n)
  gem_tibble$tweet_count <- nrow(tweets)
  gem_tibble$average_score <- sum(sent_table$n)/nrow(tweets)
  gem_tibble$median <- stsvect(tweets$text,filters) %>% median()
  gem_tibble$score <- sum(sent_table$score)
  sent_table <- arrange(sent_table,desc(n))
  gem_tibble$common_words <- filter(sent_table, n > 1)$word %>% paste(.,collapse = ", ")
  return(gem_tibble)
}

many_sentiments <- function(query_list,filtered_words){
  results_tibble <- tibble(query=character(),word_count=integer(),
                           yes_count=integer(),no_count=integer(),
                           score=integer(),common_words=character())
  tweet_table_list <- list(results_tibble)
  names(tweet_table_list) <- c(query_list[[1]])
  sentiment_table_list <- list(results_tibble)
  names(sentiment_table_list) <- c(query_list[[1]])
  for (item in query_list){
    tweets <- searchTwitterTextAndTimestamp(item,100)
    tweets <- mutate(tweets, score = stsvect(text,filtered_words))
    sentis <- text_amalgamator(tweets,filtered_words)
    new_row <- analyse_sentiment(item,sentis,tweets,filtered_words)
    results_tibble <- rbind(results_tibble,new_row)
    tweet_table_list[[item]] <- tweets
    sentiment_table_list[[item]] <- sentis
  }
  master_list <- list(results_tibble,tweet_table_list,sentiment_table_list)
  names(master_list) <- c("sentiments","tweets","word lists")
  return(master_list)
}

sentiment_cannon <- function(query,filtered_words){
  #setup
    results_tibble <- tibble(query=character(),word_count=integer(),
                             yes_count=integer(),no_count=integer(),
                             score=integer(),common_words=character())
    tweet_table_list <- list(results_tibble)
    names(tweet_table_list) <- c(query)
    sentiment_table_list <- list(results_tibble)
    names(sentiment_table_list) <- c(query)
  
  #core cannon
    tweets <- searchTwitterTextAndTimestamp(query,100)
    tweets <- mutate(tweets, score = stsvect(text,filtered_words))
    sentis <- text_amalgamator(tweets,filtered_words)
    new_row <- analyse_sentiment(query,sentis,tweets,filtered_words)
    results_tibble <- rbind(results_tibble,new_row)
    tweet_table_list[[query]] <- tweets
    sentiment_table_list[[query]] <- sentis
    
  #sentiment histogram
    hist <- ggplot(tweets, aes(x=score)) + 
      geom_histogram(binwidth = 1, color= "black",fill='white') +
      ggtitle(str_glue("Sentiment Scores for tweets containing: ",query)) +
      xlab("Sentiment Score") +
      ylab("Number of Tweets")
    
  #top words
    topten <- sentis %>% arrange(desc(n)) %>% head(n=10) %>% 
      ggplot(aes(reorder(word,n),n,fill=value)) + 
      geom_col() + coord_flip() +
      ylab('Frequecy of word in tweet sample') +
      xlab("Top 10 Words")
    
  #returning results  
  master_list <- list(results_tibble,tweet_table_list,sentiment_table_list,
                      hist, topten)
  names(master_list) <- c("sentiments","tweets","word lists","distribution","top 10")
  return(master_list)
}

