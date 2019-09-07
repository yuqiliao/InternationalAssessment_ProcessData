library(devtools)
library(purrr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

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
time_startlogout_vars <- c("en11mtiml", "en11rtiml", "en11btiml", "en11ztiml", "en11ttiml")
time_startlastitem_vars <- c("en11mtims", "en11rtims", "en11btims", "en11ztims", "en11ttims")

other_vars <- c("erea", "idcntry","idstud", "totwgt", "rotation", "itsex")
allvars <- c(ad_vars, other_vars)


ePIRLS_lesdf_cnt1 <- getData(data = ePIRLS$datalist[[1]],
                          varnames = allvars,
                          omittedLevels = FALSE, addAttributes = TRUE)

ePIRLS_lesdf_cnt1 %>% 
  select(idcntry , idstud, contains("en11")) %>% 
  View()


### click situation for each ad #####

out <- data.frame("IDCNTRY" = character(0),
                  "Var" = character(0),
                  "Min" = character(0),
                  "FirstQ" = character(0),
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
exportPath <- "/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_ProcessData/output"
write.csv(out, file.path(exportPath,"eP16_summaryTable.csv"), row.names = FALSE)



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


###  no-click vs click for each ad  #####

# create an empty data frame for looping
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = character(0),
                  "n0" = integer(0),
                  "nUsed" = integer(0))


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
    if (length(temp_edsurveytable$data) != 1) {
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
exportPath <- "/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_ProcessData/output"
write.csv(out, file.path(exportPath,"eP16_erea_adClick_eachModule.csv"), row.names = FALSE)


### How to (almost) replicate the interantional report
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

### click situation for all two ads #####
ePIRLS_lesdf_cnt1_new2 <- ePIRLS_lesdf_cnt1 %>% 
  mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE),
         adClickTotal = as.factor(adClickTotal)) %>% 
  rebindAttributes(ePIRLS$datalist[[1]])

class(ePIRLS_lesdf_cnt1_new2$adClickTotal)

cnt1_erea_adClickTotal <- edsurveyTable(formula = erea ~ adClickTotal,
                                    data = ePIRLS_lesdf_cnt1_new2,
                                    jrrIMax = Inf)
View(cnt1_erea_adClickTotal$data)







###  no-click vs click for all two ads ##### 
#(compare with the international report for sanity check)
ePIRLS_lesdf_cnt1_new4 <- ePIRLS_lesdf_cnt1 %>% 
  mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
  mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
         adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                   ifelse(adClickTotal >= 1, 1, 0))),
         adClickTotal = as.factor(adClickTotal)) %>% 
  rebindAttributes(ePIRLS$datalist[[1]])

class(ePIRLS_lesdf_cnt1_new4$adClickTotal_d_clicked)

cnt1_erea_adClickTotal_d_clicked <- edsurveyTable(formula = erea ~ adClickTotal_d_clicked,
                                        data = ePIRLS_lesdf_cnt1_new4,
                                        jrrIMax = Inf,
                                        weightVar = "totwgt")
View(cnt1_erea_adClickTotal_d_clicked$data)



###  no-click vs click for all two ads - v2 ##### 
# to calculate the dummied version of the 5 vars first, then get the percent average and the mean average, and then get the average of those average values
#(compare with the international report for sanity check)
ePIRLS_lesdf_cnt1_new5 <- ePIRLS_lesdf_cnt1 %>% 
  mutate(en11madz_d = as.factor(ifelse(is.na(en11madz), NA,
                                     ifelse(en11madz >= 1, 1, 0))),
         en11radz_d = as.factor(ifelse(is.na(en11radz), NA,
                                     ifelse(en11radz >= 1, 1, 0))),
         en11badz_d = as.factor(ifelse(is.na(en11badz), NA,
                                     ifelse(en11badz >= 1, 1, 0))),
         en11zadz_d = as.factor(ifelse(is.na(en11zadz), NA,
                                     ifelse(en11zadz >= 1, 1, 0))),
         en11tadz_d = as.factor(ifelse(is.na(en11tadz), NA,
                                     ifelse(en11tadz >= 1, 1, 0)))) %>% 
  rebindAttributes(ePIRLS$datalist[[1]])

  
cnt1_erea_en11madz_d <- edsurveyTable(formula = erea ~ en11madz_d,
                                                  data = ePIRLS_lesdf_cnt1_new5,
                                                  jrrIMax = Inf,
                                                  weightVar = "totwgt")
View(cnt1_erea_en11madz_d$data)
  
#   select(contains("adz")) %>% 
#   View()
# 
# 
# 
#   
#   mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>%  ##NOTE THAT BY DOING THIS, "sum(NA, NA, NA, NA, na.rm = TRUE)" WOULD RETURN "0", TO FIX THAT,
#   mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
#          adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
#                                                    ifelse(adClickTotal >= 1, 1, 0))),
#          adClickTotal = as.factor(adClickTotal)) %>% 
#   rebindAttributes(ePIRLS$datalist[[1]])
# 
# class(ePIRLS_lesdf_cnt1_new4$adClickTotal_d_clicked)






# no-click vs click on all ad? - percent and score? 

# among students who click at least one ad, how many (or percent) of them click on all ad?

# click on just one ad overall vs click on more than one ad? - percent and score? 

# predicting score, using no-click vs click, and controlling for stuff?


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




### Loop through all countries #####

countryList <- c("aad", "adu", "are", "can", "dnk",	"geo", "irl", "isr", "ita", "nor", "prt", "sgp", "svn", "swe", "twn", "usa")

countryList <- c("aad", "adu")

ePIRLS <- read_ePIRLS("/Users/Yuqi/Desktop/Files/AIR/Conference/Conference 2020/ePIRLS/data/eP16_International/eP16_SPSSData/", countries = countryList)


lastItemReached_vars <- c("en11mitem", "en11ritem", "en11bitem", "en11zitem", "en11titem")
ad_vars <- c("en11madz", "en11radz", "en11badz", "en11zadz", "en11tadz")
other_vars <- c("erea", "idcntry","idstud", "totwgt", "en11mtiml",  "en11mtims", "rotation", "itsex")
allvars <- c(ad_vars, other_vars, lastItemReached_vars)

### erea ~ adclickTotal_d_clicked ###

# create an empty data frame for looping
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "EqVarValue" = character(0),
                  "N" = numeric(0),
                  "WTD_N" = numeric(0),
                  "PCT" = numeric(0),
                  "SE_PCT" = numeric(0),
                  "MEAN" = numeric(0),
                  "SE_MEAN" = character(0),
                  "n0" = integer(0),
                  "nUsed" = integer(0))


for (cnt in ePIRLS$datalist) {
  
  print(cnt$country)
  
  temp_lesdf <- getData(data = cnt,
                               varnames = allvars,
                               omittedLevels = FALSE, addAttributes = TRUE)
  
  print(nrow(temp_lesdf))
  
  
  temp_lesdf <- temp_lesdf %>% 
    mutate(adClickTotal = select(., en11madz, en11radz, en11badz, en11zadz, en11tadz) %>% rowSums(na.rm = TRUE)) %>% 
    mutate(adClickTotal = ifelse( (is.na(en11madz) & is.na(en11radz) & is.na(en11badz) & is.na(en11zadz) & is.na(en11tadz)), NA, adClickTotal ),
           adClickTotal_d_clicked = as.factor(ifelse(is.na(adClickTotal), NA,
                                                     ifelse(adClickTotal >= 1, 1, 0))),
           adClickTotal = as.factor(adClickTotal)) %>% 
    rebindAttributes(cnt)

  temp_edsurveytable <- tryCatch(edsurveyTable(formula = erea~adClickTotal_d_clicked,
                                                    data = temp_lesdf,
                                                    jrrIMax = Inf,
                                                    weightVar = "totwgt"),
                                          error = function(cond) {
                                            message(cond)
                                            return(0)
                                          })
  if (length(temp_edsurveytable$data) != 1) {
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
exportPath <- "/Users/Yuqi/Desktop/Files/AIR/Conference/Conference 2020/ePIRLS/output/edsurveyTable"
write.csv(out, file.path(exportPath,"eP15_erea_adclickTotal_d_clicked.csv"), row.names = FALSE)



### erea ~ adclickTotal_d_clicked ###

  
  
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

