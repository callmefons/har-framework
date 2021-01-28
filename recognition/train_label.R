# Activity recognition from label information and time only

source("script/lib.R")

write_results <- function(con, starts, finishes, activity_type_id){
  for (d in 7:0){
    date = Sys.Date() - d
    maxtry=10 # for the case database is locked
    while(maxtry>0){
      tryCatch({
        (tmp = data.frame(date, starts, finishes, user, activity_type_id))

        (sql = sprintf("DELETE FROM estimated_activities WHERE date IN %s AND user_id IN %s;", make_sqlvec(tmp$date), make_sqlvec(tmp$user)))
        dbSendQuery(con, sql) #delete the date and user

        (sql = sprintf("INSERT INTO estimated_activities (date, start, finish, user_id, activity_type_id) VALUES %s;", make_sqlseq(tmp, collapse = ",")))
        dbSendQuery(con, sql)

        dbClearResult(res, con)
        maxtry=0
      }, error = function(e) {maxtry<<-maxtry-1})

    }
  }
}

if(is.na(users))  users<-dbGetQuery(con,"SELECT id FROM users;")$id

for (user in users){try({
  cat("Predict label: user:", user, "\n")
  sql = sprintf("SELECT * FROM activities WHERE user_id=%s AND start IS NOT NULL AND finish IS NOT NULL;", user)
  (acts <- dbGetQuery(con,sql))

  if(nrow(acts)==0) { 
    acts = dbGetQuery(con, "SELECT * FROM template_activities;")
    if(nrow(acts)==0) {cat("Please enter template activities first.\n");next()}
    acts$start = strptime(acts$start, time_format, "Japan")
    acts$finish = strptime(acts$finish, time_format, "Japan")

    for(i in 1:nrow(acts)){
      write_results(con, acts$start[i], acts$finish[i], acts$activity_type_id[i])
    }

    next()
  }

  acts[,c("start","finish")]=segmentstr2sec(acts$start, acts$finish)

  acts$date = strptime(acts$date, date_format,"Japan")

  plt = lapply(unique(acts$activity_type_id), function(activity_type_id){
    acs = acts[acts$activity_type_id==activity_type_id,]

      if (nrow(acs)==0) return()

      df = data.frame(start=acs$start, duration=as.numeric(acs$finish-acs$start))
      dfplus = rbind(df-3600*24,df,df+3600*24)

      k=min(nrow(dfplus)-1, 10)
      ds = knn.dist(dfplus, k)[,k]

      ds = ds[(length(ds)/3+1):(length(ds)/3*2)]

      plt <- 1/ds^3
      plt[is.infinite(plt)] <- k
      plt <- plt / sum(plt)
      df$prob = plt
      df$date = acs$date

      df$act = activity_type_id
      return(df)
  })

  prob_lt <- data.frame()
  for(plt in plt)   prob_lt <- rbind(prob_lt, plt)

  save(prob_lt, file=paste(featurepath,"/prob_lt_",user,".rdata", sep=""))
  })
}
