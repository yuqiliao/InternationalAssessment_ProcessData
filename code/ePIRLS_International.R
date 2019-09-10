library(devtools)
library(purrr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

edsurvey_dir <- "/Users/Yuqi/Desktop/Files/AIR/GIT/edsurvey"
load_all(edsurvey_dir)

countryList <- c("aad", "adu", "are", "can", "dnk",	"geo", "irl", "isr", "ita", "nor", "prt", "sgp", "svn", "swe", "twn", "usa")
countryList_test <- c("aad", "adu")

ePIRLS <- read_ePIRLS("/Users/Yuqi/Desktop/Files/AIR/Conference/Conference 2020/ePIRLS/data/eP16_International/eP16_SPSSData/", countries = c("*"))
ePIRLS <- read_ePIRLS("/Users/Yuqi/Desktop/Files/AIR/Conference/Conference 2020/ePIRLS/data/eP16_International/eP16_SPSSData/", countries = countryList_test)

View(showCodebook(ePIRLS$datalist[[1]]))

# allvars <- tolower(union(ePIRLS$datalist[[1]]$dataList$Student$fileFormat$variableName, 
#                          ePIRLS$datalist[[1]]$dataList$School$fileFormat$variableName))
lastItemReached_vars <- c("en11mitem", #last item is 20
                          "en11ritem", #16
                          "en11bitem", #17
                          "en11zitem", #20
                          "en11titem") #18
ad_vars <- c("en11madz", "en11radz", "en11badz", "en11zadz", "en11tadz")
time_startlastitem_vars <- c("en11mtims", "en11rtims", "en11btims", "en11ztims", "en11ttims")
time_startlogout_vars <- c("en11mtiml", "en11rtiml", "en11btiml", "en11ztiml", "en11ttiml")

time_lastitem_logout_vars <- c("en11mtimr", "en11rtimr", "en11btimr", "en11ztimr", "en11ttimr") #not existed yet, I'll create them later

other_vars <- c("erea", "idcntry","idstud", "totwgt", "rotation", "itsex", "jkzone", "jkrep")
allvars <- c(ad_vars, time_startlastitem_vars, time_startlogout_vars, lastItemReached_vars, other_vars)

exportPath <- "/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_ProcessData/output"

# ePIRLS_lesdf_cnt1 <- getData(data = ePIRLS$datalist[[1]],
#                           varnames = allvars,
#                           omittedLevels = FALSE, addAttributes = TRUE)
# 
# ePIRLS_lesdf_cnt1 %>% 
#   select(idcntry , idstud, contains("en11")) %>% 
#   View()


### 1. click situation for each ad #####
out <- data.frame("IDCNTRY" = character(0),
                  "Var" = character(0),
                  "Min" = numeric(0),
                  "FirstQ" = numeric(0),
                  "Median" = numeric(0),
                  "Mean" = numeric(0),
                  "ThirdQ" = numeric(0),
                  "Max" = numeric(0),
                  "NACount" = numeric(0),
                  "n" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  for (x in ad_vars) {
    summaryTable_x <- summary(temp_lesdf[,x])
    
    out_temp <- data.frame("IDCNTRY" = cnt$country)
    out_temp$Var <- x
    out_temp$Min <- summaryTable_x[1]
    out_temp$FirstQ <- summaryTable_x[2]
    out_temp$Median <- summaryTable_x[3]
    out_temp$Mean <- summaryTable_x[4]
    out_temp$ThirdQ <- summaryTable_x[5]
    out_temp$Max <- summaryTable_x[6]
    out_temp$NACount <- summaryTable_x[7]
    out_temp$n <- nrow(temp_lesdf)
    
    out <- rbind.fill(out, out_temp)
    
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_adClickSummaryTable.csv")), row.names = FALSE)


### for looking at the pct and score for each levels of ad click in each module
# ePIRLS_lesdf_cnt1_new1 <- ePIRLS_lesdf_cnt1 %>% 
#   mutate(en11madz = as.factor(en11madz),
#          en11radz = as.factor(en11radz),
#          en11badz = as.factor(en11badz),
#          en11zadz = as.factor(en11zadz),
#          en11tadz = as.factor(en11tadz)) %>% 
#   rebindAttributes(ePIRLS$datalist[[1]])
#   
# cnt1_erea_en11madz <- edsurveyTable(formula = erea ~ en11madz,
#                                data = ePIRLS_lesdf_cnt1_new1,
#                                jrrIMax = Inf)
# # repeate 4 more times
# View(cnt1_erea_en11madz$data)


###  2. no-click vs click for each ad  #####

## PCT and Score
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    rebindAttributes(cnt)
  
  for (x in ad_vars) {
    print(x)
    
    temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ ", x, "_d_clicked" )),
                                                 data = temp_lesdf,
                                                 jrrIMax = Inf,
                                                 weightVar = "totwgt"),
                                   error = function(cond) {
                                     message(cond)
                                     return(0)
                                   })
    if (length(temp_edsurveytable) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
      out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
      out_temp$EqVarValue <- temp_edsurveytable$data[,1]
      out_temp$N <- temp_edsurveytable$data$N
      out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
      out_temp$PCT <- temp_edsurveytable$data$PCT
      out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
      out_temp$MEAN <- temp_edsurveytable$data$MEAN
      out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
      out_temp$n0 <- temp_edsurveytable$n0
      out_temp$nUsed <- temp_edsurveytable$nUsed
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country,
                             "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
    }
    
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_erea_adClick_pct.csv")), row.names = FALSE)


## Score

# out <- data.frame("IDCNTRY" = character(0),
#                   "YVar" = character(0),
#                   "EqVar" = character(0),
#                   "EqVarValue" = character(0),
#                   "N" = numeric(0),
#                   "WTD_N" = numeric(0),
#                   "PCT" = numeric(0),
#                   "SE_PCT" = numeric(0),
#                   "MEAN" = numeric(0),
#                   "SE_MEAN" = numeric(0),
#                   "n0" = numeric(0),
#                   "nUsed" = numeric(0))

out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    rebindAttributes(cnt)
  
  for (x in ad_vars) {
    print(x)
    
    # temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ ", x, "_d_clicked" )),
    #                                              data = temp_lesdf,
    #                                              jrrIMax = Inf,
    #                                              weightVar = "totwgt"),
    #                                error = function(cond) {
    #                                  message(cond)
    #                                  return(0)
    #                                })
    # if (length(temp_edsurveytable$data) != 1) {
    #   out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable)))
    #   out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
    #   out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
    #   out_temp$EqVarValue <- temp_edsurveytable$data[,1]
    #   out_temp$N <- temp_edsurveytable$data$N
    #   out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
    #   out_temp$PCT <- temp_edsurveytable$data$PCT
    #   out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
    #   out_temp$MEAN <- temp_edsurveytable$data$MEAN
    #   out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
    #   out_temp$n0 <- temp_edsurveytable$n0
    #   out_temp$nUsed <- temp_edsurveytable$nUsed
    # } else {
    #   out_temp <- data.frame("IDCNTRY" = cnt$country, 
    #                          "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
    # }
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0("erea ~ ", x, "_d_clicked" )),
                                                 data = temp_lesdf,
                                                 jrrIMax = Inf,
                                                 weightVar = "totwgt"),
                                   error = function(cond) {
                                     message(cond)
                                     return(0)
                                   })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_erea_adClick.csv")), row.names = FALSE)


# gap 
# gapResults <- tryCatch( gap(variable = "erea",
#     data = temp_lesdf,
#     groupA = en11madz_d_clicked == "0",
#     groupB = en11madz_d_clicked == "1",
#     jrrIMax = Inf),
# error = function(cond) {
#   message(cond)
#   return(0)
# })

### How to (almost) replicate the international report
# ePIRLS_lesdf_cnt1_new3 <- ePIRLS_lesdf_cnt1 %>% 
#   mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
#                                                ifelse(en11madz >= 1, 1, 0))),
#          en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
#                                                ifelse(en11radz >= 1, 1, 0))),
#          en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
#                                                ifelse(en11badz >= 1, 1, 0))),
#          en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
#                                                ifelse(en11zadz >= 1, 1, 0))),
#          en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
#                                                ifelse(en11tadz >= 1, 1, 0)))) %>% 
#   rebindAttributes(ePIRLS$datalist[[1]])
# 
# cnt1_erea_en11madz_d_clicked <- edsurveyTable(formula = erea ~ en11madz_d_clicked,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# # repeate 4 more times
# cnt1_erea_en11radz_d_clicked <- edsurveyTable(formula = erea ~ en11radz_d_clicked,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# cnt1_erea_en11badz_d_clicked <- edsurveyTable(formula = erea ~ en11badz_d_clicked,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# cnt1_erea_en11zadz_d_clicked <- edsurveyTable(formula = erea ~ en11zadz_d_clicked,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# cnt1_erea_en11tadz_d_clicked <- edsurveyTable(formula = erea ~ en11tadz_d_clicked,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# 
# # get the average pct and scores
# mean(c(cnt1_erea_en11madz_d_clicked$data$PCT[1],
#        cnt1_erea_en11radz_d_clicked$data$PCT[1],
#        cnt1_erea_en11badz_d_clicked$data$PCT[1],
#        cnt1_erea_en11zadz_d_clicked$data$PCT[1],
#        cnt1_erea_en11tadz_d_clicked$data$PCT[1]))
# 
# mean(c(cnt1_erea_en11madz_d_clicked$data$PCT[2],
#        cnt1_erea_en11radz_d_clicked$data$PCT[2],
#        cnt1_erea_en11badz_d_clicked$data$PCT[2],
#        cnt1_erea_en11zadz_d_clicked$data$PCT[2],
#        cnt1_erea_en11tadz_d_clicked$data$PCT[2]))
# 
# mean(c(cnt1_erea_en11madz_d_clicked$data$MEAN[1],
#        cnt1_erea_en11radz_d_clicked$data$MEAN[1],
#        cnt1_erea_en11badz_d_clicked$data$MEAN[1],
#        cnt1_erea_en11zadz_d_clicked$data$MEAN[1],
#        cnt1_erea_en11tadz_d_clicked$data$MEAN[1]))
# 
# mean(c(cnt1_erea_en11madz_d_clicked$data$MEAN[2],
#        cnt1_erea_en11radz_d_clicked$data$MEAN[2],
#        cnt1_erea_en11badz_d_clicked$data$MEAN[2],
#        cnt1_erea_en11zadz_d_clicked$data$MEAN[2],
#        cnt1_erea_en11tadz_d_clicked$data$MEAN[2]))



## Time spent

# out <- data.frame("IDCNTRY" = character(0),
#                   "YVar" = character(0),
#                   "EqVar" = character(0),
#                   "EqVarValue" = character(0),
#                   "N" = numeric(0),
#                   "WTD_N" = numeric(0),
#                   "PCT" = numeric(0),
#                   "SE_PCT" = numeric(0),
#                   "MEAN" = numeric(0),
#                   "SE_MEAN" = numeric(0),
#                   "n0" = numeric(0),
#                   "nUsed" = numeric(0))

out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    # seperate time_vars into _m and _s columns
    separate(col = en11mtims , into = c("en11mtims_m", "en11mtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtims , into = c("en11rtims_m", "en11rtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btims , into = c("en11btims_m", "en11btims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztims , into = c("en11ztims_m", "en11ztims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttims , into = c("en11ttims_m", "en11ttims_s"), sep = ":", convert = TRUE) %>% 
    
    separate(col = en11mtiml , into = c("en11mtiml_m", "en11mtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtiml , into = c("en11rtiml_m", "en11rtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btiml , into = c("en11btiml_m", "en11btiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztiml , into = c("en11ztiml_m", "en11ztiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttiml , into = c("en11ttiml_m", "en11ttiml_s"), sep = ":", convert = TRUE) %>% 
    # convert time_vars into date format and in the unit of seconds, and create the new vars
    mutate(en11mtims_seconds = dminutes(en11mtims_m) + dseconds(en11mtims_s),
           en11rtims_seconds = dminutes(en11rtims_m) + dseconds(en11rtims_s),
           en11btims_seconds = dminutes(en11btims_m) + dseconds(en11btims_s),
           en11ztims_seconds = dminutes(en11ztims_m) + dseconds(en11ztims_s),
           en11ttims_seconds = dminutes(en11ttims_m) + dseconds(en11ttims_s),
           
           en11mtiml_seconds = dminutes(en11mtiml_m) + dseconds(en11mtiml_s),
           en11rtiml_seconds = dminutes(en11rtiml_m) + dseconds(en11rtiml_s),
           en11btiml_seconds = dminutes(en11btiml_m) + dseconds(en11btiml_s),
           en11ztiml_seconds = dminutes(en11ztiml_m) + dseconds(en11ztiml_s),
           en11ttiml_seconds = dminutes(en11ttiml_m) + dseconds(en11ttiml_s)) %>% 
    # create time_vars that are between last item saved to logging out
    mutate(en11mtimr_seconds = en11mtiml_seconds - en11mtims_seconds,
           en11rtimr_seconds = en11rtiml_seconds - en11rtims_seconds,
           en11btimr_seconds = en11btiml_seconds - en11btims_seconds,
           en11ztimr_seconds = en11ztiml_seconds - en11ztims_seconds,
           en11ttimr_seconds = en11ttiml_seconds - en11ttims_seconds) %>%
    rebindAttributes(cnt)
  
  for (time_var in c(time_startlastitem_vars, time_startlogout_vars, time_lastitem_logout_vars)) {
    print(time_var)
    
    # temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0(time_var, "_seconds ~ ", str_sub(string = time_var, start = 1, end = 5), "adz_d_clicked" )),
    #                                              data = temp_lesdf,
    #                                              jrrIMax = Inf,
    #                                              weightVar = "totwgt"),
    #                                error = function(cond) {
    #                                  message(cond)
    #                                  return(0)
    #                                })
    # if (length(temp_edsurveytable) != 1) {
    #   out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable)))
    #   out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
    #   out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
    #   out_temp$EqVarValue <- temp_edsurveytable$data[,1]
    #   out_temp$N <- temp_edsurveytable$data$N
    #   out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
    #   out_temp$PCT <- temp_edsurveytable$data$PCT
    #   out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
    #   out_temp$MEAN <- temp_edsurveytable$data$MEAN
    #   out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
    #   out_temp$n0 <- temp_edsurveytable$n0
    #   out_temp$nUsed <- temp_edsurveytable$nUsed
    # } else {
    #   out_temp <- data.frame("IDCNTRY" = cnt$country, 
    #                          "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
    # }
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0(time_var, "_seconds ~ ", str_sub(string = time_var, start = 1, end = 5), "adz_d_clicked" )),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_timeVars_adClick.csv")), row.names = FALSE)





# The following code is for one cnt (and some code could be used for analyzing time_spent_vars alone and in-depth)
# ePIRLS_lesdf_cnt1_new3 <- ePIRLS_lesdf_cnt1 %>%
#   mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
#                                                ifelse(en11madz >= 1, 1, 0))),
#          en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
#                                                ifelse(en11radz >= 1, 1, 0))),
#          en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
#                                                ifelse(en11badz >= 1, 1, 0))),
#          en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
#                                                ifelse(en11zadz >= 1, 1, 0))),
#          en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
#                                                ifelse(en11tadz >= 1, 1, 0)))) %>% 
#   # seperate time_vars into _m and _s columns
#   separate(col = en11mtims , into = c("en11mtims_m", "en11mtims_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11rtims , into = c("en11rtims_m", "en11rtims_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11btims , into = c("en11btims_m", "en11btims_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11ztims , into = c("en11ztims_m", "en11ztims_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11ttims , into = c("en11ttims_m", "en11ttims_s"), sep = ":", convert = TRUE) %>% 
#   
#   separate(col = en11mtiml , into = c("en11mtiml_m", "en11mtiml_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11rtiml , into = c("en11rtiml_m", "en11rtiml_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11btiml , into = c("en11btiml_m", "en11btiml_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11ztiml , into = c("en11ztiml_m", "en11ztiml_s"), sep = ":", convert = TRUE) %>% 
#   separate(col = en11ttiml , into = c("en11ttiml_m", "en11ttiml_s"), sep = ":", convert = TRUE) %>% 
#   # convert time_vars into date format and in the unit of seconds, and create the new vars
#   mutate(en11mtims_seconds = dminutes(en11mtims_m) + dseconds(en11mtims_s),
#          en11rtims_seconds = dminutes(en11rtims_m) + dseconds(en11rtims_s),
#          en11btims_seconds = dminutes(en11btims_m) + dseconds(en11btims_s),
#          en11ztims_seconds = dminutes(en11ztims_m) + dseconds(en11ztims_s),
#          en11ttims_seconds = dminutes(en11ttims_m) + dseconds(en11ttims_s),
#          
#          en11mtiml_seconds = dminutes(en11mtiml_m) + dseconds(en11mtiml_s),
#          en11rtiml_seconds = dminutes(en11rtiml_m) + dseconds(en11rtiml_s),
#          en11btiml_seconds = dminutes(en11btiml_m) + dseconds(en11btiml_s),
#          en11ztiml_seconds = dminutes(en11ztiml_m) + dseconds(en11ztiml_s),
#          en11ttiml_seconds = dminutes(en11ttiml_m) + dseconds(en11ttiml_s)) %>% 
#   # create time_vars that are between last item saved to logging out
#   mutate(en11mtimr_seconds = en11mtiml_seconds - en11mtims_seconds,
#          en11rtimr_seconds = en11rtiml_seconds - en11rtims_seconds,
#          en11btimr_seconds = en11btiml_seconds - en11btims_seconds,
#          en11ztimr_seconds = en11ztiml_seconds - en11ztims_seconds,
#          en11ttimr_seconds = en11ttiml_seconds - en11ttims_seconds) %>%
#   rebindAttributes(ePIRLS$datalist[[1]])
# 
# 
# # unweighted
# summary(ePIRLS_lesdf_cnt1_new3$en11mtimr_seconds)
# 
# # with replicate weights
# cnt1_en11mtimr_seconds <- edsurveyTable(formula = en11mtimr_seconds ~ 1,
#                                               data = ePIRLS_lesdf_cnt1_new3,
#                                               jrrIMax = Inf)
# # time by ad click dummy var
# cnt1_en11mtimr_seconds_en11radz_d_clicked <- edsurveyTable(formula = en11mtimr_seconds ~ en11radz_d_clicked,
#                                         data = ePIRLS_lesdf_cnt1_new3,
#                                         jrrIMax = Inf)


## Last Item reached
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    rebindAttributes(cnt)
  
  for (lastItemReached_var in lastItemReached_vars) {
    print(lastItemReached_var)
    
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0(lastItemReached_var, " ~ ", str_sub(string = lastItemReached_var, start = 1, end = 5), "adz_d_clicked" )),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_lastItemReachedVars_adClick.csv")), row.names = FALSE)


## by ad click and by gender - edsurveyTable
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVar1Value" = character(0),
                  "EqVar2Value" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    rebindAttributes(cnt)
  
  for (x in ad_vars) {
    print(x)
    
    temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ ", x, "_d_clicked + itsex" )),
                                                 data = temp_lesdf,
                                                 jrrIMax = Inf,
                                                 weightVar = "totwgt"),
                                   error = function(cond) {
                                     message(cond)
                                     return(0)
                                   })
    if (length(temp_edsurveytable) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
      out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
      out_temp$EqVar1Value <- temp_edsurveytable$data[,1]
      out_temp$EqVar2Value <- temp_edsurveytable$data[,2]
      out_temp$N <- temp_edsurveytable$data$N
      out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
      out_temp$PCT <- temp_edsurveytable$data$PCT
      out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
      out_temp$MEAN <- temp_edsurveytable$data$MEAN
      out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
      out_temp$n0 <- temp_edsurveytable$n0
      out_temp$nUsed <- temp_edsurveytable$nUsed
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country,
                             "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
    }
    
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_erea_adClick_itsex_pct.csv")), row.names = FALSE)


## by ad click and by gender - gap
out <- data.frame("IDCNTRY" = character(0),
                  "EqVar" = character(0),
                  "groupA" = character(0),
                  "groupB" = character(0),
                  "pctA" = numeric(0),
                  "pctAse" = numeric(0),
                  "pctB" = numeric(0),
                  "pctBse" = numeric(0),
                  "diffAB" = numeric(0),
                  "covAB" = numeric(0),
                  "diffABse" = numeric(0),
                  "diffABpValue" = numeric(0),
                  "dofAB" = numeric(0),
                  "n0A" = numeric(0),
                  "n0B" = numeric(0),
                  "nUsedA" = numeric(0),
                  "nUsedB" = numeric(0))

for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(en11madz_d_clicked = as.factor(ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0))),
           en11radz_d_clicked = as.factor(ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0))),
           en11badz_d_clicked = as.factor(ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0))),
           en11zadz_d_clicked = as.factor(ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0))),
           en11tadz_d_clicked = as.factor(ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0)))) %>% 
    rebindAttributes(cnt)
  
  for (x in ad_vars) {
    print(x)
    
    #filter here
    varName <- paste0(x, "_d_clicked")
    varName_quote <- sym(varName)
    temp_lesdf_filtered <- temp_lesdf %>% 
      filter(!!varName_quote %in% c("1")) %>% 
      rebindAttributes(cnt)
    
    temp_gap <- tryCatch(gap(variable = "erea",
                                       data = temp_lesdf_filtered,
                                       groupA = itsex == "GIRL",
                                       groupB = itsex == "BOY",
                                       jrrIMax = Inf,
                                       weightVar = "totwgt"),
                                   error = function(cond) {
                                     message(cond)
                                     return(0)
                                   })
    if (length(temp_gap) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_gap$percentage)))
      out_temp$EqVar <- paste0(x, "_d_clicked")
      
      out_temp$groupA <- as.character(temp_gap$labels$A)[3]
      out_temp$groupB <- as.character(temp_gap$labels$B)[3]
      
      out_temp$pctA <- temp_gap$percentage$pctA
      out_temp$pctAse <- temp_gap$percentage$pctAse
      out_temp$pctB <- temp_gap$percentage$pctB
      out_temp$pctBse <- temp_gap$percentage$pctBse
      out_temp$diffAB <- temp_gap$percentage$diffAB
      out_temp$covAB <- temp_gap$percentage$covAB
      out_temp$diffABse <- temp_gap$percentage$diffABse
      out_temp$diffABpValue <- temp_gap$percentage$diffABpValue
      out_temp$dofAB <- temp_gap$percentage$dofAB
      
      out_temp$n0A <- temp_gap$labels$n0A
      out_temp$n0B <- temp_gap$labels$n0B
      out_temp$nUsedA <- temp_gap$labels$nUsedA
      out_temp$nUsedB <- temp_gap$labels$nUsedB
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country,
                             "EqVar" = paste0(x, "_d_clicked"))
    }
    
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_eachModule_adClick_itsex_pct_gap.csv")), row.names = FALSE)



### 3. click situation for all two ads #####
out <- data.frame("IDCNTRY" = character(0),
                  "Var" = character(0),
                  "Min" = numeric(0),
                  "FirstQ" = numeric(0),
                  "Median" = numeric(0),
                  "Mean" = numeric(0),
                  "ThirdQ" = numeric(0),
                  "Max" = numeric(0),
                  "NACount" = numeric(0),
                  "n" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
  
    summaryTable <- summary(temp_lesdf[,"adClickTotal"])
    
    out_temp <- data.frame("IDCNTRY" = cnt$country)
    out_temp$Var <- "adClickTotal"
    out_temp$Min <- summaryTable[1]
    out_temp$FirstQ <- summaryTable[2]
    out_temp$Median <- summaryTable[3]
    out_temp$Mean <- summaryTable[4]
    out_temp$ThirdQ <- summaryTable[5]
    out_temp$Max <- summaryTable[6]
    out_temp$NACount <- summaryTable[7]
    out_temp$n <- nrow(temp_lesdf)
    
    out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_adClickSummaryTable.csv")), row.names = FALSE)


###  4. no-click vs click for all two ads ##### 
## PCT and Score
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
    
    temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ adClickTotal_d_clicked")),
                                                 data = temp_lesdf,
                                                 jrrIMax = Inf,
                                                 weightVar = "totwgt"),
                                   error = function(cond) {
                                     message(cond)
                                     return(0)
                                   })
    if (length(temp_edsurveytable) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
      out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
      out_temp$EqVarValue <- temp_edsurveytable$data[,1]
      out_temp$N <- temp_edsurveytable$data$N
      out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
      out_temp$PCT <- temp_edsurveytable$data$PCT
      out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
      out_temp$MEAN <- temp_edsurveytable$data$MEAN
      out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
      out_temp$n0 <- temp_edsurveytable$n0
      out_temp$nUsed <- temp_edsurveytable$nUsed
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country,
                             "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
    }
    
    out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_adClickTotal_pct.csv")), row.names = FALSE)


## Score
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0("erea ~ adClickTotal_d_clicked")),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_adClickTotal.csv")), row.names = FALSE)


## Time spent
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>%
    # seperate time_vars into _m and _s columns
    separate(col = en11mtims , into = c("en11mtims_m", "en11mtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtims , into = c("en11rtims_m", "en11rtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btims , into = c("en11btims_m", "en11btims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztims , into = c("en11ztims_m", "en11ztims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttims , into = c("en11ttims_m", "en11ttims_s"), sep = ":", convert = TRUE) %>% 
    
    separate(col = en11mtiml , into = c("en11mtiml_m", "en11mtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtiml , into = c("en11rtiml_m", "en11rtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btiml , into = c("en11btiml_m", "en11btiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztiml , into = c("en11ztiml_m", "en11ztiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttiml , into = c("en11ttiml_m", "en11ttiml_s"), sep = ":", convert = TRUE) %>% 
    # convert time_vars into date format and in the unit of seconds, and create the new vars
    mutate(en11mtims_seconds = dminutes(en11mtims_m) + dseconds(en11mtims_s),
           en11rtims_seconds = dminutes(en11rtims_m) + dseconds(en11rtims_s),
           en11btims_seconds = dminutes(en11btims_m) + dseconds(en11btims_s),
           en11ztims_seconds = dminutes(en11ztims_m) + dseconds(en11ztims_s),
           en11ttims_seconds = dminutes(en11ttims_m) + dseconds(en11ttims_s),
           
           en11mtiml_seconds = dminutes(en11mtiml_m) + dseconds(en11mtiml_s),
           en11rtiml_seconds = dminutes(en11rtiml_m) + dseconds(en11rtiml_s),
           en11btiml_seconds = dminutes(en11btiml_m) + dseconds(en11btiml_s),
           en11ztiml_seconds = dminutes(en11ztiml_m) + dseconds(en11ztiml_s),
           en11ttiml_seconds = dminutes(en11ttiml_m) + dseconds(en11ttiml_s)) %>% 
    # create time_vars that are between last item saved to logging out
    mutate(en11mtimr_seconds = en11mtiml_seconds - en11mtims_seconds,
           en11rtimr_seconds = en11rtiml_seconds - en11rtims_seconds,
           en11btimr_seconds = en11btiml_seconds - en11btims_seconds,
           en11ztimr_seconds = en11ztiml_seconds - en11ztims_seconds,
           en11ttimr_seconds = en11ttiml_seconds - en11ttims_seconds) %>%
    
    # create the aggregated versions of the time_vars
    mutate(en11Totaltims_seconds = select(., en11mtims_seconds, en11rtims_seconds, en11btims_seconds, en11ztims_seconds, en11ttims_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltiml_seconds = select(., en11mtiml_seconds, en11rtiml_seconds, en11btiml_seconds, en11ztiml_seconds, en11ttiml_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltimr_seconds = select(., en11mtimr_seconds, en11rtimr_seconds, en11btimr_seconds, en11ztimr_seconds, en11ttimr_seconds) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(en11Totaltims_seconds = ifelse( (is.na(en11mtims_seconds) & is.na(en11rtims_seconds) & is.na(en11btims_seconds) & is.na(en11ztims_seconds) & is.na(en11ttims_seconds)), NA, en11Totaltims_seconds ),
           en11Totaltiml_seconds = ifelse( (is.na(en11mtiml_seconds) & is.na(en11rtiml_seconds) & is.na(en11btiml_seconds) & is.na(en11ztiml_seconds) & is.na(en11ttiml_seconds)), NA, en11Totaltiml_seconds ),
           en11Totaltimr_seconds = ifelse( (is.na(en11mtimr_seconds) & is.na(en11rtimr_seconds) & is.na(en11btimr_seconds) & is.na(en11ztimr_seconds) & is.na(en11ttimr_seconds)), NA, en11Totaltimr_seconds )) %>% 
    
    rebindAttributes(cnt)
  
  for (time_var in c("en11Totaltims_seconds", "en11Totaltiml_seconds", "en11Totaltimr_seconds")) {
    print(time_var)
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0(time_var, " ~ adClickTotal_d_clicked")),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                             error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_timeVars_adClickTotal.csv")), row.names = FALSE)

## Last item reached
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))

for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>%
    
    
    # create the aggregated versions of the lastItemReached_vars
    separate(col = rotation, into = c("passage", "module1", "and", "module2"), sep = " ", remove = FALSE) %>% 
    mutate(
      module1_max = ifelse(module1 == 1, 20, 
                           ifelse(module1 == 2, 16, 
                                  ifelse(module1 == 3, 17,
                                         ifelse(module1 == 4, 20, 18)))),
      module2_max = ifelse(module2 == 1, 20, 
                           ifelse(module2 == 2, 16, 
                                  ifelse(module2 == 3, 17,
                                         ifelse(module2 == 4, 20, 18)))),
      modules_max = module1_max + module2_max) %>% 
    mutate(ItemReached_count = select(., en11mitem, en11ritem, en11bitem, en11zitem, en11titem) %>% rowSums(na.rm = TRUE)) %>% ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(ItemReached_count = ifelse( (is.na(en11mitem) & is.na(en11ritem) & is.na(en11bitem) & is.na(en11bitem) & is.na(en11zitem)), NA, ItemReached_count)) %>% 
    mutate(
      lastItemReached_d = as.factor(ifelse(is.na(ItemReached_count), NA,
                                            ifelse(ItemReached_count == modules_max, 1, 0))),
      ItemReached_count = as.factor(ItemReached_count)
    ) %>% 
    rebindAttributes(cnt)
  
  for (var in c("ItemReached_count", "lastItemReached_d")) {
    print(var)
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0(var, " ~ adClickTotal_d_clicked")),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_lastItemReachedVars_adClickTotal.csv")), row.names = FALSE)

## by ad click and by gender
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVar1Value" = character(0),
                  "EqVar2Value" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
  
  temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ adClickTotal_d_clicked + itsex")),
                                               data = temp_lesdf,
                                               jrrIMax = Inf,
                                               weightVar = "totwgt"),
                                 error = function(cond) {
                                   message(cond)
                                   return(0)
                                 })
  if (length(temp_edsurveytable) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
    out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
    out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
    out_temp$EqVar1Value <- temp_edsurveytable$data[,1]
    out_temp$EqVar2Value <- temp_edsurveytable$data[,2]
    out_temp$N <- temp_edsurveytable$data$N
    out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
    out_temp$PCT <- temp_edsurveytable$data$PCT
    out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
    out_temp$MEAN <- temp_edsurveytable$data$MEAN
    out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
    out_temp$n0 <- temp_edsurveytable$n0
    out_temp$nUsed <- temp_edsurveytable$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = cnt$country,
                           "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
  }
  
  out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_adClickTotal_itsex_pct.csv")), row.names = FALSE)


## by ad click and by gender - gap
out <- data.frame("IDCNTRY" = character(0),
                  "EqVar" = character(0),
                  "groupA" = character(0),
                  "groupB" = character(0),
                  "pctA" = numeric(0),
                  "pctAse" = numeric(0),
                  "pctB" = numeric(0),
                  "pctBse" = numeric(0),
                  "diffAB" = numeric(0),
                  "covAB" = numeric(0),
                  "diffABse" = numeric(0),
                  "diffABpValue" = numeric(0),
                  "dofAB" = numeric(0),
                  "n0A" = numeric(0),
                  "n0B" = numeric(0),
                  "nUsedA" = numeric(0),
                  "nUsedB" = numeric(0))

for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
  

    #filter here
    temp_lesdf_filtered <- temp_lesdf %>% 
      filter(adClickTotal_d_clicked %in% c("1")) %>% 
      rebindAttributes(cnt)
    
    temp_gap <- tryCatch(gap(variable = "erea",
                             data = temp_lesdf_filtered,
                             groupA = itsex == "GIRL",
                             groupB = itsex == "BOY",
                             jrrIMax = Inf,
                             weightVar = "totwgt"),
                         error = function(cond) {
                           message(cond)
                           return(0)
                         })
    if (length(temp_gap) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_gap$percentage)))
      out_temp$EqVar <- paste0(x, "_d_clicked")
      
      out_temp$groupA <- as.character(temp_gap$labels$A)[3]
      out_temp$groupB <- as.character(temp_gap$labels$B)[3]
      
      out_temp$pctA <- temp_gap$percentage$pctA
      out_temp$pctAse <- temp_gap$percentage$pctAse
      out_temp$pctB <- temp_gap$percentage$pctB
      out_temp$pctBse <- temp_gap$percentage$pctBse
      out_temp$diffAB <- temp_gap$percentage$diffAB
      out_temp$covAB <- temp_gap$percentage$covAB
      out_temp$diffABse <- temp_gap$percentage$diffABse
      out_temp$diffABpValue <- temp_gap$percentage$diffABpValue
      out_temp$dofAB <- temp_gap$percentage$dofAB
      
      out_temp$n0A <- temp_gap$labels$n0A
      out_temp$n0B <- temp_gap$labels$n0B
      out_temp$nUsedA <- temp_gap$labels$nUsedA
      out_temp$nUsedB <- temp_gap$labels$nUsedB
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country,
                             "EqVar" = paste0(x, "_d_clicked"))
    }
    out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_adClickTotal_itsex_pct_gap.csv")), row.names = FALSE)


### 5. ad click situations#####
## students are given two modules, so their ad click situations should be one of c(NA_NA, NA_0, 0_NA, NA_1, 1_NA, 1_0, 0_1, 1_1, 0_0). Below code finds out about the score and pct of each situations
# could answer: what's the pct of students who click on ads in both modules?(1_1 situations) (so they know the ad doesn’t do anything but decide to click on them anyway)

out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  temp_lesdf <- temp_lesdf %>% 
    # create adClickTotal and adClickTotal_d_clicked
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    
    # create dummy version for each ad_vars
    mutate(en11madz_d_clicked = ifelse(is.na(en11madz), NA,
                                                 ifelse(en11madz >= 1, 1, 0)),
           en11radz_d_clicked = ifelse(is.na(en11radz), NA,
                                                 ifelse(en11radz >= 1, 1, 0)),
           en11badz_d_clicked = ifelse(is.na(en11badz), NA,
                                                 ifelse(en11badz >= 1, 1, 0)),
           en11zadz_d_clicked = ifelse(is.na(en11zadz), NA,
                                                 ifelse(en11zadz >= 1, 1, 0)),
           en11tadz_d_clicked = ifelse(is.na(en11tadz), NA,
                                                 ifelse(en11tadz >= 1, 1, 0))) %>% 
    
    # create number of modules taken var (levels should be either 0, 1, 2)
    mutate(n_na_modules = is.na(en11madz) + is.na(en11radz) + is.na(en11badz) + is.na(en11zadz) + is.na(en11tadz),
           n_modules_taken = 5 - n_na_modules) %>% 
    
    # create rotation var
    separate(col = rotation, into = c("passage", "module1", "and", "module2"), sep = " ", remove = FALSE) %>% 
    mutate(
      module1_adVar = ifelse(module1 == 1, "en11madz", 
                           ifelse(module1 == 2, "en11radz", 
                                  ifelse(module1 == 3, "en11badz",
                                         ifelse(module1 == 4, "en11zadz", "en11tadz")))),
      module2_adVar = ifelse(module2 == 1, "en11madz", 
                             ifelse(module2 == 2, "en11radz", 
                                    ifelse(module2 == 3, "en11badz",
                                           ifelse(module2 == 4, "en11zadz", "en11tadz"))))) %>% 
    rebindAttributes(cnt)
  
  
    # create ad click situations that covers all scenarios
    # i couldn't figure out a vectorized way to do it >_<, for now, will turn to a for loop
  temp_lesdf$adClickSituation <- NA
for (i in 1:nrow(temp_lesdf)){
  temp_lesdf[i, "adClickSituation"] <- paste0( as.character(temp_lesdf[i, paste0(temp_lesdf$module1_adVar[i], "_d_clicked")])  ,
                                     "_",
                                     as.character(temp_lesdf[i, paste0(temp_lesdf$module2_adVar[i], "_d_clicked")]))
}
  
  temp_lesdf$adClickSituation <- as.factor(temp_lesdf$adClickSituation)
  
  # create temp_edsurveytable
  temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ adClickSituation")),
                                               data = temp_lesdf,
                                               jrrIMax = Inf,
                                               weightVar = "totwgt"),
                                 error = function(cond) {
                                   message(cond)
                                   return(0)
                                 })
  if (length(temp_edsurveytable) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
    out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
    out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
    out_temp$EqVarValue <- temp_edsurveytable$data[,1]
    out_temp$N <- temp_edsurveytable$data$N
    out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
    out_temp$PCT <- temp_edsurveytable$data$PCT
    out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
    out_temp$MEAN <- temp_edsurveytable$data$MEAN
    out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
    out_temp$n0 <- temp_edsurveytable$n0
    out_temp$nUsed <- temp_edsurveytable$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = cnt$country,
                           "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
  }
  
  out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_adClickSituation_pct.csv")), row.names = FALSE)




## create adClickTotal_d_clicked_v2 that has 4 levels (NA, 0 click, 1 click, more than 1 click)
## to compare students who who click on ad only once with students who click on ads more than once
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = numeric(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0))

for (cnt in ePIRLS$datalist) {
  print(cnt$country)
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                    ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal_d_clicked_v2 = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal == 0, "0 click",
                                                            ifelse(adClickTotal == 1, "1 click", "more than 1 click")))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)
  
  temp_edsurveytable <- tryCatch(edsurveyTable(formula = as.formula(paste0("erea ~ adClickTotal_d_clicked_v2")),
                                               data = temp_lesdf,
                                               jrrIMax = Inf,
                                               weightVar = "totwgt"),
                                 error = function(cond) {
                                   message(cond)
                                   return(0)
                                 })
  if (length(temp_edsurveytable) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_edsurveytable$data)))
    out_temp$YVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1]
    out_temp$EqVar <- str_split(temp_edsurveytable$formula, pattern = "~")[[3]][1]
    out_temp$EqVarValue <- temp_edsurveytable$data[,1]
    out_temp$N <- temp_edsurveytable$data$N
    out_temp$WTD_N <- temp_edsurveytable$data$WTD_N
    out_temp$PCT <- temp_edsurveytable$data$PCT
    out_temp$SE_PCT <- temp_edsurveytable$data$`SE(PCT)`
    out_temp$MEAN <- temp_edsurveytable$data$MEAN
    out_temp$SE_MEAN <- temp_edsurveytable$data$`SE(MEAN)`
    out_temp$n0 <- temp_edsurveytable$n0
    out_temp$nUsed <- temp_edsurveytable$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = cnt$country,
                           "YVar" = str_split(temp_edsurveytable$formula, pattern = "~")[[2]][1])
  }
  
  out <- rbind.fill(out, out_temp)
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_adClickTotalv2_pct.csv")), row.names = FALSE)



###### Stop Here ######



## 6. score by time spent#####
# get the review time, categorize it into two 50% groups, see students' score by review/not review the

## Time spent - first half vs later half
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>%
    # seperate time_vars into _m and _s columns
    separate(col = en11mtims , into = c("en11mtims_m", "en11mtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtims , into = c("en11rtims_m", "en11rtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btims , into = c("en11btims_m", "en11btims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztims , into = c("en11ztims_m", "en11ztims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttims , into = c("en11ttims_m", "en11ttims_s"), sep = ":", convert = TRUE) %>% 
    
    separate(col = en11mtiml , into = c("en11mtiml_m", "en11mtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtiml , into = c("en11rtiml_m", "en11rtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btiml , into = c("en11btiml_m", "en11btiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztiml , into = c("en11ztiml_m", "en11ztiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttiml , into = c("en11ttiml_m", "en11ttiml_s"), sep = ":", convert = TRUE) %>% 
    # convert time_vars into date format and in the unit of seconds, and create the new vars
    mutate(en11mtims_seconds = dminutes(en11mtims_m) + dseconds(en11mtims_s),
           en11rtims_seconds = dminutes(en11rtims_m) + dseconds(en11rtims_s),
           en11btims_seconds = dminutes(en11btims_m) + dseconds(en11btims_s),
           en11ztims_seconds = dminutes(en11ztims_m) + dseconds(en11ztims_s),
           en11ttims_seconds = dminutes(en11ttims_m) + dseconds(en11ttims_s),
           
           en11mtiml_seconds = dminutes(en11mtiml_m) + dseconds(en11mtiml_s),
           en11rtiml_seconds = dminutes(en11rtiml_m) + dseconds(en11rtiml_s),
           en11btiml_seconds = dminutes(en11btiml_m) + dseconds(en11btiml_s),
           en11ztiml_seconds = dminutes(en11ztiml_m) + dseconds(en11ztiml_s),
           en11ttiml_seconds = dminutes(en11ttiml_m) + dseconds(en11ttiml_s)) %>% 
    # create time_vars that are between last item saved to logging out
    mutate(en11mtimr_seconds = en11mtiml_seconds - en11mtims_seconds,
           en11rtimr_seconds = en11rtiml_seconds - en11rtims_seconds,
           en11btimr_seconds = en11btiml_seconds - en11btims_seconds,
           en11ztimr_seconds = en11ztiml_seconds - en11ztims_seconds,
           en11ttimr_seconds = en11ttiml_seconds - en11ttims_seconds) %>%
    
    # create the aggregated versions of the time_vars
    mutate(en11Totaltims_seconds = select(., en11mtims_seconds, en11rtims_seconds, en11btims_seconds, en11ztims_seconds, en11ttims_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltiml_seconds = select(., en11mtiml_seconds, en11rtiml_seconds, en11btiml_seconds, en11ztiml_seconds, en11ttiml_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltimr_seconds = select(., en11mtimr_seconds, en11rtimr_seconds, en11btimr_seconds, en11ztimr_seconds, en11ttimr_seconds) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(en11Totaltims_seconds = ifelse( (is.na(en11mtims_seconds) & is.na(en11rtims_seconds) & is.na(en11btims_seconds) & is.na(en11ztims_seconds) & is.na(en11ttims_seconds)), NA, en11Totaltims_seconds ),
           en11Totaltiml_seconds = ifelse( (is.na(en11mtiml_seconds) & is.na(en11rtiml_seconds) & is.na(en11btiml_seconds) & is.na(en11ztiml_seconds) & is.na(en11ttiml_seconds)), NA, en11Totaltiml_seconds ),
           en11Totaltimr_seconds = ifelse( (is.na(en11mtimr_seconds) & is.na(en11rtimr_seconds) & is.na(en11btimr_seconds) & is.na(en11ztimr_seconds) & is.na(en11ttimr_seconds)), NA, en11Totaltimr_seconds )) %>% 
    
    ## create dummy versions of the aggregated time vars, into those who spent more than median time, and those who spent less than median time
    mutate(en11Totaltims_seconds_median = median(en11Totaltims_seconds, na.rm = TRUE),
           en11Totaltims_seconds_quickhalf = as.factor(ifelse(en11Totaltims_seconds < en11Totaltims_seconds_median, 1, 0)),
           en11Totaltiml_seconds_median = median(en11Totaltiml_seconds, na.rm = TRUE),
           en11Totaltiml_seconds_quickhalf = as.factor(ifelse(en11Totaltiml_seconds < en11Totaltiml_seconds_median, 1, 0)),
           en11Totaltimr_seconds_median = median(en11Totaltimr_seconds, na.rm = TRUE),
           en11Totaltimr_seconds_quickhalf = as.factor(ifelse(en11Totaltimr_seconds < en11Totaltimr_seconds_median, 1, 0))) %>% 
    
    rebindAttributes(cnt)
  
  for (time_var in c("en11Totaltims_seconds_quickhalf", "en11Totaltiml_seconds_quickhalf", "en11Totaltimr_seconds_quickhalf")) {
    print(time_var)
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0("erea ~ ", time_var)),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_timeQuickHalfVars.csv")), row.names = FALSE)


## Time spent - first quartile vs forth quartile
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "n0" = numeric(0),
                  "nUsed" = numeric(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "r2" = numeric(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                        varnames = allvars,
                        omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>%
    # seperate time_vars into _m and _s columns
    separate(col = en11mtims , into = c("en11mtims_m", "en11mtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtims , into = c("en11rtims_m", "en11rtims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btims , into = c("en11btims_m", "en11btims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztims , into = c("en11ztims_m", "en11ztims_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttims , into = c("en11ttims_m", "en11ttims_s"), sep = ":", convert = TRUE) %>% 
    
    separate(col = en11mtiml , into = c("en11mtiml_m", "en11mtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11rtiml , into = c("en11rtiml_m", "en11rtiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11btiml , into = c("en11btiml_m", "en11btiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ztiml , into = c("en11ztiml_m", "en11ztiml_s"), sep = ":", convert = TRUE) %>% 
    separate(col = en11ttiml , into = c("en11ttiml_m", "en11ttiml_s"), sep = ":", convert = TRUE) %>% 
    # convert time_vars into date format and in the unit of seconds, and create the new vars
    mutate(en11mtims_seconds = dminutes(en11mtims_m) + dseconds(en11mtims_s),
           en11rtims_seconds = dminutes(en11rtims_m) + dseconds(en11rtims_s),
           en11btims_seconds = dminutes(en11btims_m) + dseconds(en11btims_s),
           en11ztims_seconds = dminutes(en11ztims_m) + dseconds(en11ztims_s),
           en11ttims_seconds = dminutes(en11ttims_m) + dseconds(en11ttims_s),
           
           en11mtiml_seconds = dminutes(en11mtiml_m) + dseconds(en11mtiml_s),
           en11rtiml_seconds = dminutes(en11rtiml_m) + dseconds(en11rtiml_s),
           en11btiml_seconds = dminutes(en11btiml_m) + dseconds(en11btiml_s),
           en11ztiml_seconds = dminutes(en11ztiml_m) + dseconds(en11ztiml_s),
           en11ttiml_seconds = dminutes(en11ttiml_m) + dseconds(en11ttiml_s)) %>% 
    # create time_vars that are between last item saved to logging out
    mutate(en11mtimr_seconds = en11mtiml_seconds - en11mtims_seconds,
           en11rtimr_seconds = en11rtiml_seconds - en11rtims_seconds,
           en11btimr_seconds = en11btiml_seconds - en11btims_seconds,
           en11ztimr_seconds = en11ztiml_seconds - en11ztims_seconds,
           en11ttimr_seconds = en11ttiml_seconds - en11ttims_seconds) %>%
    
    # create the aggregated versions of the time_vars
    mutate(en11Totaltims_seconds = select(., en11mtims_seconds, en11rtims_seconds, en11btims_seconds, en11ztims_seconds, en11ttims_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltiml_seconds = select(., en11mtiml_seconds, en11rtiml_seconds, en11btiml_seconds, en11ztiml_seconds, en11ttiml_seconds) %>% rowSums(na.rm = TRUE),
           en11Totaltimr_seconds = select(., en11mtimr_seconds, en11rtimr_seconds, en11btimr_seconds, en11ztimr_seconds, en11ttimr_seconds) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
    mutate(en11Totaltims_seconds = ifelse( (is.na(en11mtims_seconds) & is.na(en11rtims_seconds) & is.na(en11btims_seconds) & is.na(en11ztims_seconds) & is.na(en11ttims_seconds)), NA, en11Totaltims_seconds ),
           en11Totaltiml_seconds = ifelse( (is.na(en11mtiml_seconds) & is.na(en11rtiml_seconds) & is.na(en11btiml_seconds) & is.na(en11ztiml_seconds) & is.na(en11ttiml_seconds)), NA, en11Totaltiml_seconds ),
           en11Totaltimr_seconds = ifelse( (is.na(en11mtimr_seconds) & is.na(en11rtimr_seconds) & is.na(en11btimr_seconds) & is.na(en11ztimr_seconds) & is.na(en11ttimr_seconds)), NA, en11Totaltimr_seconds )) %>% 
    
    ## create dummy versions of the aggregated time vars, into those who spent more than median time, and those who spent less than median time
    mutate(en11Totaltims_seconds_1q = quantile(en11Totaltims_seconds, na.rm = TRUE)[2],
           en11Totaltims_seconds_3q = quantile(en11Totaltims_seconds, na.rm = TRUE)[4],
           en11Totaltims_seconds_quickquartile = as.factor(ifelse(en11Totaltims_seconds < en11Totaltims_seconds_1q, 1, ifelse(en11Totaltims_seconds > en11Totaltims_seconds_3q, 0, NA))),
           en11Totaltiml_seconds_1q = quantile(en11Totaltiml_seconds, na.rm = TRUE)[2],
           en11Totaltiml_seconds_3q = quantile(en11Totaltiml_seconds, na.rm = TRUE)[4],
           en11Totaltiml_seconds_quickquartile = as.factor(ifelse(en11Totaltiml_seconds < en11Totaltiml_seconds_1q, 1, ifelse(en11Totaltiml_seconds > en11Totaltiml_seconds_3q, 0, NA))),
           en11Totaltimr_seconds_1q = quantile(en11Totaltimr_seconds, na.rm = TRUE)[2],
           en11Totaltimr_seconds_3q = quantile(en11Totaltimr_seconds, na.rm = TRUE)[4],
           en11Totaltimr_seconds_quickquartile = as.factor(ifelse(en11Totaltimr_seconds < en11Totaltimr_seconds_1q, 1, ifelse(en11Totaltimr_seconds > en11Totaltimr_seconds_3q, 0, NA)))) %>% 
  
    rebindAttributes(cnt)
  
  for (time_var in c("en11Totaltims_seconds_quickquartile", "en11Totaltiml_seconds_quickquartile", "en11Totaltimr_seconds_quickquartile")) {
    print(time_var)
    
    temp_lmsdf <- tryCatch(lm.sdf(formula = as.formula(paste0("erea ~ ", time_var)),
                                  data = temp_lesdf,
                                  jrrIMax = Inf,
                                  weightVar = "totwgt"),
                           error = function(cond) {
                             message(cond)
                             return(0)
                           })
    if (length(temp_lmsdf) != 1) {
      out_temp <- data.frame("IDCNTRY" = rep(cnt$country, nrow(temp_lmsdf$coefmat)))
      out_temp$YVar <- str_split(temp_lmsdf$formula, pattern = "~")[[2]][1]
      out_temp$EqVar <- row.names(temp_lmsdf$coefmat)
      out_temp$n0 <- temp_lmsdf$n0
      out_temp$nUsed <- temp_lmsdf$nUsed
      out_temp$coef <- temp_lmsdf$coefmat$coef
      out_temp$se <- temp_lmsdf$coefmat$se
      out_temp$t <- temp_lmsdf$coefmat$t
      out_temp$dof <- temp_lmsdf$coefmat$dof
      out_temp$pVal <- temp_lmsdf$coefmat$`Pr(>|t|)`
      out_temp$r2 <- temp_lmsdf$r.squared
    } else {
      out_temp <- data.frame("IDCNTRY" = cnt$country, 
                             "YVar" = str_split(temp_lmsdf$formula, pattern = "~")[[2]][1])
    }
    out <- rbind.fill(out, out_temp)
  }
}

# export
write.csv(out, file.path(exportPath, paste0(today(),"eP16_allModule_erea_timeQuickQuartileVars.csv")), row.names = FALSE)














# score

erea_overall <- edsurveyTable(formula = erea ~ 1,
              data = ePIRLS,
              jrrIMax = Inf,
              omittedLevels = FALSE)
erea_overall$data


erea_rotation <- edsurveyTable(formula = erea ~ rotation,
                       data = ePIRLS,
                       jrrIMax = Inf,
                       omittedLevels = FALSE)
View(erea_rotation$data)






  
  
### last item reached for each module#####

ePIRLS_lesdf_cnt1_lastitem <- ePIRLS_lesdf_cnt1 %>% 
  mutate(en11mitem = as.factor(en11mitem),
         en11ritem = as.factor(en11ritem),
         en11bitem = as.factor(en11bitem),
         en11zitem = as.factor(en11zitem),
         en11titem = as.factor(en11titem)) %>% 
  rebindAttributes(ePIRLS$datalist[[1]])

cnt1_erea_en11ritem <- edsurveyTable(formula = erea ~ en11ritem,
                                     data = ePIRLS_lesdf_cnt1_lastitem,
                                     jrrIMax = Inf)


### last item reached for both two modules#####

ePIRLS_lesdf_cnt1_lastitem_2 <- ePIRLS_lesdf_cnt1 %>% 
  separate(col = rotation, into = c("passage", "module1", "and", "module2"), sep = " ", remove = FALSE) %>% 
  mutate(
    module1_max = ifelse(module1 == 1, 20, 
                         ifelse(module1 == 2, 16, 
                                ifelse(module1 == 3, 17,
                                       ifelse(module1 == 4, 20, 18)))),
    module2_max = ifelse(module2 == 1, 20, 
                         ifelse(module2 == 2, 16, 
                                ifelse(module2 == 3, 17,
                                       ifelse(module2 == 4, 20, 18)))),
    modules_max = module1_max + module2_max,
    itemReachedTotal = select(., en11mitem, en11ritem, en11bitem, en11zitem, en11titem) %>% rowSums(na.rm = TRUE),
    itemReachedTotal_d = as.factor(ifelse(is.na(itemReachedTotal), NA,
                                          ifelse(itemReachedTotal == modules_max, 1, 0))),
    itemReachedTotal = as.factor(itemReachedTotal)
  ) %>% 
  rebindAttributes(ePIRLS$datalist[[1]])


table(ePIRLS_lesdf_cnt1_lastitem_2$itemReachedTotal)
table(ePIRLS_lesdf_cnt1_lastitem_2$itemReachedTotal_d)

class(ePIRLS_lesdf_cnt1_lastitem_2$itemReachedTotal_d)

cnt1_erea_reachedTotal_d <- edsurveyTable(formula = erea ~ itemReachedTotal_d,
                                          data = ePIRLS_lesdf_cnt1_lastitem_2,
                                          jrrIMax = Inf)
View(cnt1_erea_reachedTotal_d$data)






# backup codes (other approaches) #####




# # edsurvey.data.frame approach
# # make sure it works with one variable
# # erea_en11madz <- edsurveyTable(formula = erea ~ en11madz,
# #                                data = ePIRLS,
# #                                jrrIMax = Inf)
# 
# ad_vars <- c("en11madz", "en11radz", "en11badz", "en11zadz", "en11tadz")
# 
# purrr::map(ad_vars, function(x){
#   result <- edsurveyTable(formula = as.formula(paste0("erea ~ ", x)),
#                 data = ePIRLS,
#                 jrrIMax = Inf)
#   assign(paste0("erea_", x), result, envir = .GlobalEnv)
# })
# 
# View(erea_en11madz$data)
# View(erea_en11radz$data)
# View(erea_en11badz$data)
# View(erea_en11zadz$data)
# View(erea_en11tadz$data)


# make sure it works with one variable
# find out the max of "Max. of ad clicks"
# summaryTable <- summary2(data = ePIRLS, variable = "en11madz")
# 
# df <- data.frame(observation = double(), 
#                  stringsAsFactors=FALSE) 
# 
# for (i in 1:length(summaryTable)){
#   print(summaryTable[[i]]$summary$Max.)
#   df <- df %>% add_row(observation = summaryTable[[i]]$summary$Max.)
# }
# 
# max_ad_click <- max(df$observation)
# 
# erea_en11madz_dummy <- edsurveyTable(formula = erea ~ en11madz,
#                                data = ePIRLS$datalist[[1]],
#                                jrrIMax = Inf,
#                                recode = list(en11madz = list(
#                                  from = c(map(1:max_ad_click, as.character)),
#                                  to = c("one or more")
#                                )))
# 
# View(erea_en11madz_dummy$data)


ad_vars <- c("en11madz", "en11radz", "en11madz", "en11zadz", "en11tadz")

purrr::map(ad_vars, function(x){
  result <- edsurveyTable(formula = as.formula(paste0("erea ~ ", x)),
                          data = ePIRLS,
                          jrrIMax = Inf,
                          recode = list(en11madz = list(
                            from = c(map(1:300, as.character)),
                            to = c("one or more")),
                            en11radz = list(
                              from = c(map(1:300, as.character)),
                              to = c("one or more")),
                            en11madz = list(
                              from = c(map(1:300, as.character)),
                              to = c("one or more")),
                            en11zadz = list(
                              from = c(map(1:300, as.character)),
                              to = c("one or more")),
                            en11tadz = list(
                              from = c(map(1:300, as.character)),
                              to = c("one or more"))
                          ))
  assign(paste0("erea_", x, "_dummy"), result, envir = .GlobalEnv)
})
View(erea_en11madz_dummy$data)
View(erea_en11radz_dummy$data)
View(erea_en11madz_dummy$data)
View(erea_en11zadz_dummy$data)
View(erea_en11tadz_dummy$data)


### overall score (checking against the intl report) #####
output <- edsurveyTable(formula = erea ~ 1,
                        data = ePIRLS,
                        jrrIMax = Inf)
output

output2 <- edsurveyTable(formula = erea ~ 1,
                         data = ePIRLS_lesdf_cnt1,
                         jrrIMax = Inf)
output2

output3 <- edsurveyTable(formula = erea ~ itsex,
                         data = ePIRLS,
                         jrrIMax = Inf)
output3

output4 <- edsurveyTable(formula = erea ~ itsex,
                         data = ePIRLS_lesdf_cnt1,
                         jrrIMax = Inf)
output4

