######### How to find out if you're big on twitter #######
######### Project name: impacTwit
######### A set of functions using the twitter API to determine how big any term is on twitter
######### Intended to be used with scientific manuscripts (or blog posts, or hash tags)

#### set up libraries

require(twitteR)
require(ggplot2)
require(stringr)

#require(devtools)
#install_github("ROAuth","geoffjentry")


##### We can extract all the data we need from id's so lets get the twitter ids
##### twobj is a status object from the searchTwitter function
##### and it returns a dataframe with search id,screenname,the text of the tweet, and the time stamp 

extractTwitdat<- function(twobj){
  d.size <- length(twobj)
##### Create matrices to hold text output
tweet.df <- data.frame(matrix(NA,ncol=4,nrow=d.size))
  
  for(i in 1:d.size){
    tweet.df[i,]<- c(twobj[[i]]$id,twobj[[i]]$screenName,twobj[[i]]$text,as.POSIXct(twobj[[i]]$created))
  
    }

 colnames(tweet.df) <- c("Tweet ID","ScreenName","Text","Timestamp")
  
#### Last thing is that sometimes someone mentions something twice.  So here 
#### I remove the duplicates
  
tweet.df <- tweet.df[!duplicated(tweet.df$Screen),]
  return(tweet.df)
  
}


######### Based on user name we can see how many followers each user has
######### The function requires the dataframe from extractTwitdat and uses the vector of user names
######### it returns follower = the number of followers each user has
######### It uses a batch request style to limit the number of requests sent to twitter


extractUserdat <- function(tweet.df){
  users <- tweet.df$ScreenName
  out.df <- data.frame(matrix(NA,ncol=2,nrow=0))
  colnames(out.df) <- c("user.name","followers.count")
  ####### the requests must be submitted in 100 user chunks so first figure out the number of requests
  
  loops <- 1+length(users)%/%99
  count <- 1
  for (i in 1:loops){
    
    usersString <- paste(users[!is.na(users[count:(count+99)])],collapse=",")
    myurl <- paste("https://api.twitter.com/1/users/lookup.json?screen_name=",usersString,"&include_entities=false",sep="")
    status.dat <- getURL(myurl)
    status.dat <- fromJSON(status.dat)
      user.name <- vector()
      follower.count <- vector()
    ####Now to parse out the status.dat file to just extract the follower numbers
          for(z in 1:length(status.dat)){
            user.name[z] <- status.dat[[z]]$screen_name
            follower.count[z] <- status.dat[[z]]$followers_count
          }
    
       temp.df <- as.data.frame(cbind(user.name))
       temp.df$follower.count <- follower.count
       out.df <- rbind(out.df,temp.df)
       count <- count + 99
  }
  ##### The final component is to reorder follower count in the same order it was input
  ##### because twitter returns it in a random order, not the order you input the request to the API
  user.ind <- 1:length(users)
  user.ind <- user.ind[order(users)]
  out.df <- out.df[order(out.df$user.name),]
  follower.out <- out.df$follow[order(user.ind)]
  
  
  
  return(follower.out) 
}

####### One of the problems with the twitter api is that you're limited to 150 requests per hour
####### so I can't just request the info about every single tweet in the stream to see if it was retweeted.
####### to combat this first I see which tweets are quotes and which are orginal, relying on the fact 
####### that a quote will have a " RT " or a " MT " in it.
####### it then uses regular expressions to extract who the retweeter was.  It is 
####### significantly faster than making repeated calls to the twitter API
####### This function takes dataframe from extractTwitdat
####### in then returns the same data frame with a tracer appended.
findOrig <- function(tweet.df){
  orig <- rep(NA,dim(tweet.df)[1])
  to.search <- tweet.df$Text
  RTregexp <- "(RT @)[a-zA-Z0-9_]{1,15}"
  MTregexp <- "(RT @)[a-zA-Z0-9_]{1,15}"
  
  for(i in 1:length(orig)){
    my.str <- NA
    if(length(grep("RT ",to.search[i])) > 0){
      ####Extract a string
      my.str <- str_extract(to.search[i],RTregexp)
      #### now chop off the "RT " part to be left with a username
      orig[i] <- str_sub(my.str,5,nchar(my.str))
      }
    ### Check for the sometimes used MT
    if(length(grep("MT ",to.search[i])) > 0){
      ####Extract a string
      my.str <- str_extract(to.search[i],MTregexp)
      #### now chop off the "RT " part to be left with a username
      orig[i] <- str_sub(my.str,5,nchar(my.str))
      }
    if(is.na(my.str)) { orig[i] <- tweet.df$ScreenNam[i]}
    
    }
  
   tweet.df$origin <- orig
  return(tweet.df)
}


##### often time math comes back in different units.  I set the base unit of time as minutes but it could be whatever I want
##### POSIXct math happens in the smallest whole unit different, so we have to disect the object and apply the proper
##### transformation so this just takes a POSIX math result and converts it to minutes

posixUnits <- function(mathResult){
  if(units(mathResult)=="mins"){
    return(as.double(mathResult))
  }
  
  if(units(mathResult)=="secs"){
    return(as.double(mathResult) / 60)
  }
  
  if(units(mathResult)=="hours"){
    return(as.double(mathResult) * 60)
  }
 
  if(units(mathResult)=="days"){
    return(as.double(mathResult) * 24*60)
  }
  
  else {return("not a valid math object")}
}

###### cumlativeViews sum sorts the data frame based on time stamp and adds a cumulative sum and a time increment in decimal 
###### The input is a tweet.df with followers added
###### it outputs the same frame but sorted and with the cumulative sum added
###### It adds a cumulative sum for both the 
######  
cumulativeViews <- function(tweet.df){
  ###sort the data frame
  sort.tweet.df <- tweet.df[order(as.numeric(tweet.df$Timestamp)),]
  ###Check for NA's in follower count and replace them with 0's
  sort.tweet.df$followerCount[is.na(sort.tweet.df$followerCount)]<-0
  sort.tweet.df$cSum <- cumsum(sort.tweet.df$followerCount)
  sort.tweet.df$timeSinceOrig <- rep(0,dim(tweet.df)[1])
  
  for(i in 2:dim(tweet.df)[1]){
    
     sort.tweet.df$timeSinceOrig[i] <- posixUnits(as.POSIXct(as.numeric(sort.tweet.df$Timestamp[i]),origin='1970-01-01') - as.POSIXct(as.numeric(sort.tweet.df$Timestamp[1]),origin='1970-01-01')) 
    
  }
  
  ####add an index for easy reassembly
  sort.tweet.df$index <- 1:dim(sort.tweet.df)[1]
  ####### Next we'll run a unique cumsum by origin
  orig.timesince <- vector()
  orig.cs <- vector()
  ##### Now it will get all shuffled to keep track by using an index 
  mixed.ind <- vector()
  
  orig.n <- length(unique(sort.tweet.df$origin))
  u.ids <- unique(sort.tweet.df$origin)
  for( i in 1:orig.n){
      tmp.dat <- subset(sort.tweet.df,sort.tweet.df$origin==u.ids[i])
      if(dim(tmp.dat)[1]==1){orig.cs <- c(orig.cs,tmp.dat$followerCount)
                             orig.timesince <- c(orig.timesince,0)
                             mixed.ind <- c(mixed.ind,tmp.dat$index)}
      ##### Ugly nested loop here because there's no way to easily add the POSIX numbers
      
      if(dim(tmp.dat)[1] > 1){
        tmp.vec <- vector()
          for(j in 1:dim(tmp.dat)[1]){
               tmp.vec[j] <- posixUnits(as.POSIXct(as.numeric(tmp.dat$Timestamp[j]),origin='1970-01-01') - as.POSIXct(as.numeric(tmp.dat$Timestamp[1]),origin='1970-01-01')) 
            
          }
             orig.timesince <- c(orig.timesince,tmp.vec)
             orig.cs <- c(orig.cs,cumsum(tmp.dat$followerCount))
             mixed.ind <- c(mixed.ind,tmp.dat$index)
        }
     
      }
  sort.tweet.df$csByorigin <- orig.cs[order(mixed.ind)]
  sort.tweet.df$sinceRetweet <- orig.timesince[order(mixed.ind)]
 return(sort.tweet.df) 
}

##### Next we may want the cumulative views by source.  This works the same as the
##### the previous function cumulativeViews, but it takes a data frame
##### that has been sorted already and processed by the cumulativeViews function
##### (Or you could just sort it, but its probably easier to just process them sequentially)

cViewsbySource <- function(sorted.tweet.df){
  sorted.tweet.df$index <- 1:dim(sorted.tweet.df)[1]
  ####### Next we'll run a unique cumsum by origin
   source.timesince <- vector()
   source.cs <- vector()
  ##### Now it will get all shuffled to keep track by using an index 
  mixed.ind <- vector()
  
  source.n <- length(unique(sorted.tweet.df$sourceIndex))
  u.ids <- unique(sorted.tweet.df$sourceIndex)
  for( i in 1:source.n){
    tmp.dat <- subset(sorted.tweet.df,sorted.tweet.df$sourceIndex==u.ids[i])
    if(dim(tmp.dat)[1]==1){source.cs <- c(source.cs,tmp.dat$followerCount)
                           source.timesince <- c(source.timesince,0)
                           mixed.ind <- c(mixed.ind,tmp.dat$index)}
    ##### Ugly nested loop here because there's no way to easily add the POSIX numbers
    
    if(dim(tmp.dat)[1] > 1){
      tmp.vec <- vector()
      for(j in 1:dim(tmp.dat)[1]){
        tmp.vec[j] <- posixUnits(as.POSIXct(as.numeric(tmp.dat$Timestamp[j]),origin='1970-01-01') - as.POSIXct(as.numeric(tmp.dat$Timestamp[1]),origin='1970-01-01')) 
        
      }
      source.timesince <- c(source.timesince,tmp.vec)
      source.cs <- c(source.cs,cumsum(tmp.dat$followerCount))
      mixed.ind <- c(mixed.ind,tmp.dat$index)
    }
    
  }

  sorted.tweet.df$csBySource <- source.cs[order(mixed.ind)]
  sorted.tweet.df$sinceTweetSource <- source.timesince[order(mixed.ind)]
  return(sorted.tweet.df)
  
  
}


###### This is the main function, and returns a plottable data frame
###### The input it takes is a vector of search strings
###### To measure the total impact you'll probably need to put
###### in several sources, like a paper title, a url, and maybe some
###### links from new stories,  This function processes all your search terms
###### and then removes duplicates (in case a user tweets something twice or has
###### more than one search term in a given tweet)

###### This returns a very large data frame with lots of values so here is a guide to the headers
# Tweet ID = the id of each tweet

# ScreenName = the screen name of the person who tweeted the tweet

# Text = the actual text of the tweet

# Timestamp = a string holding the time stamp, can be converted to a POSIX date
# as as.POSIXct(df$Timestamp,origin="1970-01-01")

# origin = the screen name of the originating tweeter, same as screen name if 
# tweet is original, or different if it's a retweet

# followerCount = the number of followers each ScreenName has

#sourceIndex = the index value for the term in your search string that all the data are based on
# e.g. searchString[1] = "my search", all the data is based on tweets that match that first term

#cSum = the raw cumulative sum for all the data ignoring originator or source

#timeSinceOrig = the time since the very first tweet in minutes

#index = just a simple index value

# csByOrigin = cumulative sum by originator

# sinceRetweet = the time since the retweet

# csBySource = the cumulative sum by the source in your search vector

# sinceTweetSource = the time since the first tweet in each source

impacTwit <- function(searchString){
    #### Create the master data frame to return
    #### also it needs the right column names so 
    #### so I can use rbind
    master.df <- data.frame(matrix(NA,ncol=6,nrow=0))
    colnames(master.df) <-  c("Tweet ID","ScreenName","Timestamp","origin","followerCount","sourceIndex")
    test <- searchTwitter(searchString[1], n=1)
    if(length(test)<1){cat("Nobody cares about this on twitter, sorry :(")
                      }
    n.searches <- length(searchString)
    if (!is.character(searchString)){
      cat("Hey, you need a valid search string, those aren't real words!")
    }
    for(i in 1:n.searches){
      
      my.search <- searchTwitter(searchString[i], n=1000)
      tmp.tweet.df <- extractTwitdat(my.search)
      tmp.tweet.df <- findOrig(tmp.tweet.df)
      tmp.tweet.df$followerCount <- extractUserdat(tmp.tweet.df)
      #### remove the text field ####
   #   tmp.tweet.df <- tmp.tweet.df[,c(1,2,4,5,6)]
      tmp.tweet.df$sourceIndex <- rep(i,dim(tmp.tweet.df)[1])
      master.df <- rbind(master.df,tmp.tweet.df)
    }
    
  #### Just strip it of duplicates
    master.df <- master.df[!duplicated(master.df$Twee),]
    master.df <- master.df[!duplicated(master.df$Scree),]
    master.df <- cumulativeViews(master.df)
    master.df <- cViewsbySource(master.df)
return(master.df)  
}


################## Tutorial #########
#### This uses a recent 

test.str <- c("Scientists think math is hard too","http://www.pnas.org/content/early/2012/06/22/1205259109.abstract","Heavy use of equations impedes communication among biologists")
tweet.dat <- impacTwit(test.str)

###Make a nice little plot.

#### First we can look at the total spread of a tweet from all sources
ggplot(tweet.dat,aes(x=timeSinceOrig,y=cSum))+geom_point()+geom_line()+xlab("Time since first tweet (minutes)")+ylab("Cumulative sum of potential viewers")

#### Now we can examine how retweeters impact our data
#### Probably the best way is to subset the frame to only major retweets#
#### This can probably be done faster with plyr or something but I'm not
#### that facile with it.
retweet.count <- table(tweet.dat$origin)
influencers <- retweet.count[which(retweet.count >=5)]
i.index <- vector()
for(x in 1:length(influencers)){
  i.index <- c(i.index,which(tweet.dat$origin==names(influencers)[x]))}

if.df <- tweet.dat[i.index,]
####This plot will center them all on 0
ggplot(if.df,aes(x=sinceRetweet,y=csByorigin,colour=origin,group=origin))+geom_point()+geom_line()+xlab("Time since original tweet that was retweeted (minutes)")+ylab("Cumulative sum of potential viewers")
###OR let them come in naturally
ggplot(if.df,aes(x=timeSinceOrig,y=csByorigin,colour=origin,group=origin))+geom_point()+geom_line()+xlab("Time since very first tweet (minutes)")+ylab("Cumulative sum of potential viewers")



####We can also name the sources
tweet.dat$source[which(tweet.dat$source==1)] = "AP Story"
tweet.dat$source[which(tweet.dat$source==2)] = "Direct Link"
tweet.dat$source[which(tweet.dat$source==3)] = "Article Title"

####Aain with two plots, one centered on zero and one not
ggplot(tweet.dat,aes(x=sinceTweetSource,y=csBySource,colour=source,group=source))+geom_point()+geom_line()+xlab("Time since original source tweet (minutes)")+ylab("Cumulative sum of potential viewers")

ggplot(tweet.dat,aes(x=timeSinceOrig,y=csBySource,colour=source,group=source))+geom_point()+geom_line()+xlab("Time since first tweet (minutes)")+ylab("Cumulative sum of potential viewers")

