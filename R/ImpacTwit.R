######### How to find out if you're big on twitter #######
######### Project name: impacTwit
######### A set of functions using the twitter API to determine how big any term is on twitter
######### Intended to be used with scientific manuscripts (or blog posts, or hash tags)

#### set up libraries

require(twitteR)
library(ggplot2)

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
####### This function takes dataframe from extractTwitdat
####### in then returns a vector of indices that correspond to the original tweets
findOrig <- function(tweet.df){
  orig <- vector()
  to.search <- tweet.df$Text
  index <- 1:length(to.search)
  RT <- grep("RT ",to.search)
  MT <- grep("MT ",to.search)
  quotes <- unique(c(RT,MT))
  return(index[!index%in%quotes])
}

######### Based on tweet id we can see how many times it was retweeted and the total number of followers
######### The function requires the dataframe from extractTwitdat and "original tweet index" supplied by
######### findOrig
######## the function returns a data frame of each retweeter and their follower numbers.  Unfortunately I can't 
######## get access to the time stamp of the actual retweet without OAuth and I'm having trouble getting it to work


###########These functions really don't need to be used at the moment since retweets come up in the search
extractRetweetdat <- function(tweet.df,orig.ind){
  ids.to.search <- tweet.df$Tweet[orig.ind]
  retweet.id <- vector()
  retweet.fcount <- vector()
  retweet.date <- vector()
  orig.id <- vector()
  count <- 1
  for (i in 1:length(ids.to.search)){
  
     myurl <- paste("https://api.twitter.com/1/statuses/",ids.to.search[i],"/retweeted_by.json?count=100&page=1",sep="")
    status.dat <- getURL(myurl)
    status.dat <- fromJSON(status.dat)
        if(length(status.dat) > 0){
            for(z in 1:length(status.dat)){
              retweet.id[count] <- status.dat[[z]]$screen_name
              retweet.fcount[count] <- status.dat[[z]]$followers_count
              ###### This is just the date of the original tweet and maybe can be changed with OAuth help
              
           #   retweet.date[count] <- status.dat[[z]]$status$created_at
              retweet.date[count] <- tweet.df$Timestamp[orig.ind[i]]
              orig.id[count] <-  tweet.df$ScreenName[orig.ind[i]]
              count <- count + 1
            }
        }
    
  }
  
  out.df <- data.frame(retweet.id)
  out.df$retweet.id <- as.character(out.df$retweet.id)
  out.df$followers<- retweet.fcount
  out.df$date <- retweet.date
  out.df$orig.id <- orig.id
  
  return(out.df) 
}

####### Next there's a chance that an overlap exists between the retweeters and the original search
####### to control for this we just do a simple match and then remove them from the retweet database
####### The input is the original twitter dataframe and the dataframe from the extractRetweetdat function
####### it returns a cleaned dataframe of reteewers
####Again not needed
dupCheck <- function(tweet.df,retweet.df){
  ####Extract vectors of ids to match
  orig.ids <- tweet.df$ScreenName
  rt.ids <- retweet.df$retweet.id
  return(retweet.df[!rt.ids %in%orig.ids])
  
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
###### The time units should be 0
cumulativeViews <- function(tweet.df){
  ###sort the data frame
  sort.tweet.df <- tweet.df[order(as.numeric(tweet.df$Timestamp)),]
  ###Check for NA's in follower count and replace them with 0's
  sort.tweet.df$followerCount[is.na(sort.tweet.df$followerCount)]<-0
  sort.tweet.df$cSum <- cumsum(sort.tweet.df$followerCount)
  sort.tweet.df$timeSinceOrig <- rep(0,dim(tweet.df)[1])
  
  for(i in 2:dim(tweet.df)[1]){
    
     #####   
     
     sort.tweet.df$timeSinceOrig[i] <- posixUnits(as.POSIXct(as.numeric(sort.tweet.df$Timestamp[i]),origin='1970-01-01') - as.POSIXct(as.numeric(sort.tweet.df$Timestamp[1]),origin='1970-01-01')) 
    
  }
 return(sort.tweet.df) 
}



################## Tutorial #########




####Enter a serch term of your choice
sea <- searchTwitter('The diet of Australopithecus sediba', n=100)

#### Extract the data
tweet.dat <- extractTwitdat(sea)
#### add a follower count
tweet.dat$followerCount <- extractUserdat(tweet.dat)
####calculate the cumulative potential views
tweet.dat <- cumulativeViews(tweet.dat)


###Make a nice little plot.
ggplot(tweet.dat,aes(x=timeSinceOrig,y=cSum))+geom_point()+geom_line()+xlab("Time since first tweet (minutes)")+ylab("Cumulative sum of potential viewers")




