# Activity recognition from sensor data and labels

source("script/lib.R")
library(xgboost)

write_accuracy = function(con, user, accuracy){

  (sql = sprintf("INSERT INTO accuracies (user_id, value, updated_at) VALUES (%s, %f, '%s');",
          as.character(user), accuracy, strftime(Sys.time(), format)))
  res = dbSendQuery(con, sql)
  dbClearResult(res, con)
}

files = dir(featurepath, pattern="*.csv", full.names = T)
files = files[grep("/tmp_features.+.csv$", files)]

features = lapply(files, function(file)read.csv(file)) %>% bind_rows %>% tbl_df

features$time=as.POSIXct(features$time)

if(is.na(users))  users<-dbGetQuery(con,"SELECT id FROM users;")$id

for (user in users){

  try({
    cat("Predict from sensor: user:", user, "\n")

    (acts <- dbGetQuery(con,sprintf("SELECT * FROM activities WHERE user_id=%s;", user )) )
    acts = acts[,c("start","finish","activity_type_id")]
    (colnames(acts)=c("start","finish","act"))

    if(nrow(acts)==0) next()

    acts = na.omit(acts)

    (acts[,c("start","finish")] = segmentstr2sec(acts$start, acts$finish))

    feats = features[features$user==user,]

    get_plx_user = function(acts, features){
      num_class = 300
      
      cl = rep("Other", nrow(features))
      
      for(i in 1:nrow(acts)){
        start = acts$start[i] ; finish=acts$finish[i]; act = acts$act[i]
        
        minutes = features$time %>%
          as.POSIXct %>% as.POSIXlt(tz="UTC") %>% #utc
        time2sec 

        cl[minutes >= start & minutes <= finish+60] = act
      }

      cols = setdiff(colnames(features), c("user", "time")) # user, time??????

      features[is.na(features)]=0
      
      # xgboost
      x = as.matrix(features[,cols])
      
      tmp=matrix(0, ncol=length(cols), nrow=num_class) # add dummies for all classes
      x_tmp=rbind(tmp,x)
      
      y_tmp=c(1:num_class, cl) # add dummies for all classes

      x = features[,cols]
      x = predict(dummyVars(~.,x),x)

      ncl = length(unique(cl))

      param <- list("objective" = "multi:softprob",
                    metrics = "auc",
                    num_class = num_class,# ncl+1,
                    nthread=2,
                    early_stopping_rounds=5,
                    max.depth=3,
                    eta=1#,
      )

      file = paste0(featurepath, "/xgb_model_", user, ".rdata")

      print(file)

      nround=10

      model = xgboost(data = x, label = factor(cl), nround=nround, params = param)#, xgb_model=model)
      
      probs = predict(model, x) %>% matrix(ncol=num_class)

      probs = matrix(probs[,seq(ncl)],ncol=ncl)

      pre = apply(probs, 1, which.max)
      pre = unique(cl)[pre]

      tab = table(cl, pre)
      print(tab)
      
      (accuracy = round(sum(pre==cl)/length(cl) * 100))
      
      cat("Accuracy:", accuracy, "\n")

      write_accuracy(con, user, accuracy)

      # randomForest
      #  library(randomForest)
      #  model = randomForest(features[,cols],factor(cl))
      #  print(model)
      #  (accuracy = round((1 - mean(model$err.rate))*100, 1))
      #  write_accuracy(con, user, accuracy)
      #  pre = predict(model, features[,cols])
      #  table(cl, pre)
      #  probs = predict(model, features[,cols],type="prob")

      #   probs = sapply(unique(acts$activity_type_id), function(activity_type_id){
      #     cat("act_type:", activity_type_id,"\n")
      #     acs = acts[acts$activity_type_id==activity_type_id,]
      #
      #     act = rep(FALSE, nrow(features))
      #
      #     for(i in 1:nrow(acs)){
      #       start = acs$start[i] ; finish=acs$finish[i]
      #       act[features$time >= start & features$time <= finish] = TRUE
      #     }
      #
      #     k=min(nrow(features)-1, 10)
      #     cols = c("var","mean")
      #
      #     features[is.na(features)]=0
      #
      #     pre = knn(features[,cols], features[,cols], act, k=k, prob=T)
      #
      #     attr(pre, "prob") * (pre=="TRUE") + (1 - attr(pre, "prob")) * (pre=="FALSE")
      #   })
      #
      #   colnames(probs)=unique(acts$activity_type_id)

      rownames(probs) = as.character(features$time)

      probs

    }

    system.time({
      probs = get_plx_user(acts, feats)
    })

    save(probs, file=paste(featurepath, "/prob_lx_", user, ".rdata", sep=""))
    cat("OK!","\n")
  })
}
