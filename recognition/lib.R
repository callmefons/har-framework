suppressMessages(suppressWarnings(
  library(dplyr)
))

####### parse arguments ->
get_args = function(){
  library(optparse)

  option_list <- list(
    make_option(c("-a", "--adapter"), default="sqlite3",
                help="Database adapter (sqlite3 or mysql) [default: %default]"),
    make_option(c("-D", "--database"), default="eneact_development_web",
                help="(mysql only) database name"),
    make_option(c("-H", "--host"),
                help="(mysql only) host"),
    make_option(c("-U", "--dbuser"), default="root",
                help="(mysql only) username"),
    make_option(c("-P", "--password"),
                help="(mysql only) password"),
    make_option(c("-O", "--optional"),
                help="optional value")
  )

  parser <- OptionParser(usage="%prog [options] RAILS_ENV [user_id]", option_list=option_list, description = "RAILS_ENV is the rails environment [default: development].")

  args <- parse_args(parser, positional_arguments = c(0,2)) # num arg is 0-2.
  version <- args$args[1]

  if(is.na(version)) version="development"
  user = args$arg[2]

  return(c(version=version, user=user, args$opt))
}

(args = get_args())
version = args$version
users = args$user

optional = args$optional
####### <- parse arguments

####### database connection ->
if(args$adapter=="mysql"){
  library(RMySQL)
  con <- dbConnect(MySQL(),
                   host=args$host,
                   #port=port,
                   dbname=args$database,
                   user=args$dbuser,
                   password=args$password
                   )
} else {
  library(RSQLite)

  driver=dbDriver("SQLite")
  con=dbConnect(driver, sprintf("db/%s.sqlite3", version))
}
####### <- database connection

####### variables ->
path <- paste("db/sensor/",version, sep="")
featurepath=paste("db/rdata/", version, sep="")
probpath=paste("db/rdata/", version, sep="")

dir.create(featurepath, showWarnings=F, recursive=T)
dir.create(probpath, showWarnings=F, recursive=T)

format =   "%Y-%m-%d %H:%M:%S"
time_format = "%H:%M:%S"
date_format = "%Y-%m-%d"
minute_format = "%Y-%m-%d %H:%M"
####### <- variables


####### functions ->
make_sqlvec = function(vec, collapse=NULL){
  paste0( "('",
          paste0(vec,collapse = "','"),"')"
          , collapse=collapse)
}

make_sqlseq = function(tmp, collapse=","){
  paste0(
    apply(tmp,1, function(row)make_sqlvec(row))
    , collapse=collapse)
}

sec2time <- function(sec, date=Sys.Date()){
  sec = sec %% (3600*24)
  hour <- sec %/% 3600
  min <- (sec %% 3600) %/% 60
  sec <- sec %% 60

  strptime(paste(date, hour, min,sec), format= "%Y-%m-%d %H %M %S","Japan")
}

time2sec <- function(time){
  with(time, {hour*3600 + min*60+sec})
}

str2time = function(timestrs){
  res = strptime(timestrs, format, "Japan")
  if (is.na(res[1])) res = strptime(timestrs, time_format, "Japan")
  if (is.na(res[1])) res = strptime(timestrs, minute_format, "Japan")
  return(res)
}

segmentstr2sec = function(startstrs, finishstrs){
  start = str2time(startstrs); finish = str2time(finishstrs)
  start = time2sec(start) ; finish = time2sec(finish)
  finish[finish<start] = finish[finish<start]  + 3600*24
  return(data.frame(start,finish))
}

resample <- function(vec, weights=rep(1, length(vec)), n=100, margin=3600){
  ix=!is.na(vec); vec<-vec[ix]; weights<-weights[ix]

  weights[is.na(weights)]=0.00001

  ix = order(vec)

  weights = weights / sum(weights) * length(weights)

  cumsum = (c(0,diff(vec[ix]))*weights[ix])

  cumsum= cumsum(cumsum[ix])

  cumsum=cumsum+min(vec)

  cumsum = c(cumsum, max(cumsum)+margin)

  samples = cumsum[findInterval(runif(n, min(cumsum), max(cumsum)) , cumsum)]
  samples = samples + rnorm(length(samples))

  return(samples)
}

clust <- function(dat, G){
  clust <- Mclust(dat, G=G)$parameters
  ix <- order(clust$pro, decreasing = T)

  return(clust$mean[ix])
}

get_plx = function(probs, act, starts, dates){
  if(!act%in%colnames(probs)) {
    res = 0
  } else {
    res = probs[
      rownames(probs)%in%strftime(sec2time(starts,date=dates),format)
      ,as.character(act)]
  }
  if(sum(res)==0) res=rep(0.0001, length(starts))
  return(res)
}
####### <- functions