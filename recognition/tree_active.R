# Activity recognition with Active learning using XGBoost

source("script/lib.R")
library(xgboost)
library(entropy, warn.conflicts=F)
library(caret)

# a function for writing accuracy in to database
write_accuracy = function(con, user, accuracy, activity_type_id, accuracy_type, time){
  (sql = sprintf("INSERT INTO accuracies (user_id, value, activity_type_id, accuracy_type, updated_at) VALUES (%s, %f, %s, '%s', '%s');",
                 as.character(user), accuracy, activity_type_id, accuracy_type, time))
  res = dbSendQuery(con, sql)
  dbClearResult(res, con)
  cat("write_accuracy!","\n")
}

# read feature values
files = dir(featurepath, pattern="*.csv", full.names = T)
files = files[grep("/tmp_features.+.csv$", files)]

features = lapply(files, function(file)read.csv(file)) %>% bind_rows %>% tbl_df

# if you haven't been given a username from the command line
if(is.na(users))  users<-dbGetQuery(con,"SELECT id FROM users WHERE state = 'active';")$id

df.total = data.frame()

# train by each user
for (user in users){
  
  try({
    cat("Predict from sensor: user:", user, "\n")
    
    # extract the activities of aTimeLogger for each user
    (acts <- dbGetQuery(con,sprintf("SELECT * FROM activities WHERE user_id=%s;", user )) )  #sozo removed
    acts = acts[,c("start","finish","activity_type_id")]
    (colnames(acts)=c("start","finish","act"))
    
    (nclass = dbGetQuery(con,sprintf("SELECT COUNT(DISTINCT activity_type_id) from activities where user_id=%s;", user )) )
    
    # users who have not registered any actions
    if(nrow(acts)==0) next()
    
    acts = na.omit(acts)
    
    (acts[,c("start","finish")] = segmentstr2sec(acts$start, acts$finish))
    
    feats = features[features$user==user,]
    
    get_plx_user = function(acts, features){
      # nrow(acttypes) # If it is # 0-5, it is 6 dummy and 300
      num_class = 300
      
      # multi-class simplified version
      cl = rep("Other", nrow(features))
      
      for(i in 1:nrow(acts)){
        start = acts$start[i] ; finish=acts$finish[i]; act = acts$act[i]
        
        minutes = features$time %>% #strptime(paste(minute_format, "%z")) %>% 
          as.POSIXct %>% as.POSIXlt(tz="UTC") %>% #utc
          time2sec 
        
        cl[minutes >= start & minutes <= finish+60] = act
      }
      
      cols = setdiff(colnames(features), c("user", "time")) # ignore user and time
      
      features[is.na(features)]=0
      
      x = as.matrix(features[,cols])
      
      tmp=matrix(0, ncol=length(cols), nrow=num_class) # add dummies for all classes
      x_tmp=rbind(tmp,x)
      
      y_tmp=c(1:num_class, cl) # add dummies for all classes
      
      x = features[,cols]
      x = predict(dummyVars(~.,x),x)
      
      ncl = length(unique(cl))
      
      param <- list("objective" = "multi:softprob",
                    metrics = "auc",
                    num_class = num_class,
                    nthread=2,
                    early_stopping_rounds=5,
                    max.depth=3,
                    eta=1#,
      )
      
      file = paste0(featurepath, "/xgb_model_", user, ".rdata")
      
      nround=10
      
      now <- strftime(Sys.time(), format)
      
      if(nlevels(factor(cl)) >= 2){
        model = xgboost(data = x, label = factor(cl), nround=nround, params = param)
        probs = predict(model, x) %>% matrix(ncol=num_class)
        probs = matrix(probs[,seq(ncl)],ncol=ncl)
        pre = apply(probs, 1, which.max)
        pre = unique(cl)[pre]
        
        tab = table(cl, pre)
        print(tab)
        
        (accuracy = round(sum(pre==cl)/length(cl) * 100))
        cat("Accuracy:", accuracy, "\n")
        
        #  ------------------- BL ------------------- #
        
        accuracy <- floor(accuracy^(1/nclass)*10)
        cat(sprintf("nclass Accuracy : %s \n", accuracy))
        write_accuracy(con, user, accuracy, 900, "baseline", now)
        
        #  ------------------- AL ------------------- #
        
        probs = predict(model, features[,cols],type="prob")
        score <-  apply(probs, 1, function(x) entropy.empirical(x, unit="log2") )
        1 - score
        df.scores <- data.frame(features[,cols], score)
        df <- cbind(df.scores,as.character(pre))
        names(df)[length(names(df))] <- "acttype"
        
        df <- df %>% group_by(acttype = df$acttype) %>%
          summarize_at("score", mean) %>%
          mutate(percent = score / sum(score) * 100)
        
        # write_accuracy
        write = function(x, output) {
          acttype = x[1]
          score = x[3]
          score = as.numeric(score)
          print(score)
          write_accuracy(con, user, score , acttype, "proposed", now)
        }
        
        apply(df, 1, write)
        df.total
        
        rownames(probs) = as.character(features$time)
        probs
      }
      
    }
    
    system.time({
      probs = get_plx_user(acts, feats)
      df.total = get_plx_user(acts, feats)
    })
    
    save(probs, file=paste(featurepath, "/prob_lx_", user, ".rdata", sep=""))
    
    cat("OK!","\n")
  })
}

dbDisconnect()