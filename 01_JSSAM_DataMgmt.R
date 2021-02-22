
###############################################################################
### Data Mgmt ###
### F. Mittereder & B. T. West, 2021, JSSAM "A dynamic survival modeling approach to the prediction of web survey breakoff"
### last modified 02/21/2021
############################################################################### 

# Data is not publicly available. Please contact http://graham.umich.edu/campus/scip/materials#contact for data access and questions. 

library(dplyr)
library(stringr)
library(broom)
library(zoo)
library(data.table)
library(matrixStats)
library(gdata)

# This file creates the dataset the page by page analyses file

# Make one big file by page 

########################################
# For Respondent Only Anaylses by page #
########################################

# Get page level data (respondent data only)
#pageIDlevel<- read.csv("Orig/Scippagelevel_bysid.csv", header=T,  skipNul = TRUE)
pageIDlevel$SID <- as.character(pageIDlevel$SID)
# 972411 rows

# delete Year == 2013
page.ID <- filter(pageIDlevel, Year != 2013)
# 720211 rows
dim(unique(page.ID[c("Year", "SID")]))
# 13602 respondents (vs. 13602 originally)
dim(unique(page.ID[c("SID")]))
# 12168 unique respondents (vs. 12168 originally)

# Delete unimportant variables:
page.ID <- dplyr::select(page.ID, -c(ScreenWidth, ScreenHeight, BrowserHeight, BrowserWidth, OS_Specifc, OS, Browser, tapped, NumTaps, 
                                     OrientationChange, NumOrientationChanges, ScreenOrientation, NumScrolls, Scrolled, HorizontalScroll, 
                                     VerticalScroll,Zoomed, ZoomedIn, ZoomedOut, Keystroke, Keystroke_2, SurveyVersion))
page.ID$Year <- as.character(page.ID$Year)
# Merge to Frame data of FullSample get big data set
page.ID$Survey <- "InSurvey"
page.ID$SampType.Survey <- as.factor(ifelse(page.ID$SurveyName == "SCIP2014FS" | page.ID$SurveyName == "SCIP2015FS", "Faculty",
                                            "Student"))


# Only keep respondents
page.ID <- page.ID %>%
  filter(Resp == 1)

# Platform change#
################
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year, SID, id) %>%
  mutate(PlatformChange = ifelse(Platform == lag(Platform, default = page.ID$Platform[1]), 0, 1 ))%>%
  mutate(PlatformChange = ifelse(row_number() == 1, 0, PlatformChange)) %>%
  ungroup()

# Multiple Sessions #
#####################
page.ID$StartTime <- as.POSIXct(strptime(as.character(page.ID$PageStartTimeStamp), format='%d%b%Y:%H:%M:%S'))
page.ID$EndTime <- as.POSIXct(strptime(as.character(page.ID$PageEndTimeStamp), format='%d%b%Y:%H:%M:%S'))

page.ID$PageLengthMilliseconds <- as.numeric(page.ID$PageLengthMilliseconds)

# Session variable by: 
# 1. if action== expires
# 2. if action = restore -> TimeBetweenPage >1 -> we can get rid of restore pages
# 3. Pagetime indicates expiring 
# 4. platform change
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year, SID, id) %>%
  filter(Action != "Restore") %>%
  mutate(TimeBetweenPage = ifelse( row_number() == 1, 0, as.numeric(lead(StartTime) - (EndTime)))) %>%
  mutate(NewSession = ifelse(row_number() == 1, 0, 
                             ifelse(lag(Action, n=1) == "Exipred" , 1,
                                    ifelse(lag(PageLengthMilliseconds, n =1) >= 1200000, 1,
                                           ifelse(lag(TimeBetweenPage, n=1) > 1, 1, 
                                                  ifelse(PlatformChange == 1, 1, 0)))))) %>%
  ungroup()

table(page.ID$NewSession, useNA = "ifany")
table(page.ID$NewSession, page.ID$PlatformChange, useNA = "ifany")
page.ID$NewSession <- ifelse(is.na(page.ID$NewSession), 0, page.ID$NewSession)

# SessionMax = How many (additional) sessions overall (add 1 because the starting session is not counted)
# SessionCount = in which (additional) session are we currently? (add 1 because the starting session is not counted)
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(SessionMax = sum(NewSession)+1) %>%
  mutate(SessionCount = cumsum(NewSession)+1) %>%
  ungroup()

page.ID$MoreSessions <- ifelse(page.ID$SessionCount > 1, 1, 0)

# Fix LoginCount variable #
###########################
page.ID$SafeLoginCount <- page.ID$LoginCount

page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(NewLogin = ifelse(row_number() == 1, 1, 
                           ifelse(SafeLoginCount == lag(SafeLoginCount), 0, 1))) %>%
  ungroup()


# LoginMax = How many sessions overall 
# LoginCount = in which session are we currently?
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(LoginMax = sum(NewLogin)) %>%
  mutate(LoginCount = cumsum(NewLogin)) %>%
  dplyr::select(-SafeLoginCount) %>%
  ungroup()


# Breakoff Question #
#####################
# i.e. the question where the breakoff occurs
# BreakoffQuestion == People who breakoff at this question
# InterruptQuestion == People who interrupt at this quesiton but come back

page.ID<- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(BreakoffQuestion = ifelse(row_number() == n() & Breakoff == 1, 1, 0)) %>%
  mutate(InterruptQuestion = lead(NewSession)) %>%
  mutate(InterruptQuestion = ifelse(is.na(InterruptQuestion), 0, InterruptQuestion)) %>%
  ungroup()

# Delete expired people
page.ID.orig$drop <- ifelse(page.ID.orig$Action == "Expired", 1, 0)
page.ID <- filter(page.ID.orig, drop != 1)

page.ID<- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(BreakoffQuestion = ifelse(row_number() == n() & LastQuestion != "INCENTIVE", 1, 0)) %>%
  ungroup()

# Page Count #
##############
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year, SID, id) %>%
  mutate(PageCount = row_number()) %>%
  mutate(PageCountMax = n()) %>%
  ungroup()

# Cut at 80 pages (outliers) 
page.ID <- page.ID %>%
  filter(PageCountMax <= 80)


# Mobile vs Non Mobile: drop people with unclear devices
########################
checkIDs <- unique(dplyr::select(page.ID[page.ID$Platform == "Other",], c(SID, Year)))
checkIDs$drop <- 1

page.ID <- page.ID %>%
  dplyr::filter(is.na(page.ID$drop)) 
page.ID <-  dplyr::select(page.ID, -drop)

page.ID$Mobile <- (ifelse(page.ID$Platform == "PC" | page.ID$Platform == "Tablet", 0, 
                          ifelse(page.ID$Platform == "Smartphone", 1, NA)))

# Time standardization #
########################
# 1. Get QuestionSection (and PageName)
page.ID <- page.ID %>%
  mutate(PageQuestions = as.character(PageQuestions)) %>%
  mutate(QuestionSection = sapply(strsplit(PageQuestions, "^", fixed = TRUE), `[`, 1)) %>%
  ungroup() %>%
  tbl_df()

# 2. Get the PageNames from the crosswalk
PageNameX <- read.xls("SCIP_QuestionnaireCrossWalk_2018.xlsx",
                      sheet = "Xwalk", header = TRUE)
PageNameX <- rename(PageNameX, SampType.Survey = SampType)
PageNameX$QuestionSection <- as.character(PageNameX$QuestionSection)
PageNameX$PageName <- as.character(PageNameX$PageName)
PageNameX$TopicSection <- relevel(PageNameX$TopicSection, ref = "Intro")

page.ID <- left_join(page.ID, PageNameX, by = c("SampType.Survey", "QuestionSection"))

# 3a. For standardization: only use "plausible" values by PageName
ExTime <- list()
PageNames <- unique(page.ID$PageName)

for(i in 1: length(PageNames)) {
  ExTime[[i]] <- quantile(page.ID[page.ID$Action != "Expired" &
                                    page.ID$PageLengthMilliseconds > 0 &
                                    page.ID$PageName == PageNames[i], ]$PageLengthMilliseconds,
                          c(0.01,0.99))
}

# 3b. Trim the negative and Expired values to to 1% and 99% quantile by PageName
page.ID$PageTime <- 0

for(i in 1: length(PageNames)) {
  page.ID[page.ID$PageName == PageNames[i], ]$PageTime <-
    ifelse(page.ID[page.ID$PageName == PageNames[i], ]$PageLengthMilliseconds < ExTime[[i]][1] , ExTime[[i]][1],
           
           ifelse(page.ID[page.ID$PageName == PageNames[i], ]$PageLengthMilliseconds > ExTime[[i]][2], ExTime[[i]][2],
                  page.ID[page.ID$PageName == PageNames[i], ]$PageLengthMilliseconds))
}


# 4. Compute the overall QuestionTime.Mean and QuestionTime.Sd by PageName, Faculty, and Mobile
page.ID <- page.ID %>%
  mutate(QuestionTime = PageTime/NumQuestions) %>%
  group_by(SampType.Survey, Mobile, PageName) %>%
  mutate(QuestionTime.Mean = as.numeric(mean(QuestionTime, na.rm = TRUE)),
         QuestionTime.Median = as.numeric(median(QuestionTime, na.rm = TRUE)),
         QuestionTime.Q25 = as.numeric(quantile(QuestionTime, 0.25, na.rm = TRUE)),
         QuestionTime.Q75 = as.numeric(quantile(QuestionTime, 0.75, na.rm = TRUE)), 
         QuestionTime.Sd = as.numeric(sd(QuestionTime, na.rm = TRUE))) %>%
  ungroup() %>%
  tbl_df()

# 5. standardize Times
# 5a. Standaridze Question Time  
# 5c. QuestionTime.Slow.Box: Indicator whether QuestionTime is out of the boxplot
# -> if the RT is over the 75% R takes more time -> +1
# -> if the RT is under the 25% R takes less time -> -1
page.ID <- page.ID %>%
  mutate(zQuestionTime = as.numeric((QuestionTime-QuestionTime.Mean)/QuestionTime.Sd),
         #zQuestionTime = as.numeric(scale(QuestionTime)), Something went very very wrong here... When I used this I didn't group correctly.    
         QuestionTime.Slow.Box = ifelse(QuestionTime <= QuestionTime.Q75 & QuestionTime >= QuestionTime.Q25, 0, 
                                        ifelse(QuestionTime > QuestionTime.Q75, +1, 
                                               ifelse(QuestionTime < QuestionTime.Q25, -1, NA))))%>%
  ungroup()%>%
  tbl_df()

# 6. Response Time Changes:
# 6.1. RT.Change = the real change in response time by page (if RT.Change == NA then make 0)
# => time - lag > 0 if slowing down
# => time - lag < 0 if speeding up

# 6.2. RT.Slower: faster/slower indicator by page (0 if RT.Change == NA)
# => RT.Change > 0 if slowing down (more time)
# => RT.Change < 0 if speeding up (less time)

# 6.2a. RT.Change.Slow.Box = +1 -> slowing down a lot more than everybody else (more time)
# 6.2b. RT.Change.Slow.Box = -1 -> speeding up a lot more than everybody else (less time)

# 6.4. Crossing the boxplot RT.Change.CrossingBox
# 6.4a. if lag(OutBox) < OutBox -> speeding up = +1
# 6.4b. if lag(OutBox) > OutBox -> Slowing down = -1

# 6.5. Crossing the boxplot RT.Change.Slow.CrossingBox3 last 3 Questions
# 6.5a. if lag2(QuestionTime.Slow.Box) > lag(QuestionTime.Slow.Box) > QuestionTime.Slow.Box -> speeding up = -1
# 6.5b. if lag2(QuestionTime.Slow.Box) < lag(QuestionTime.Slow.Box) < QuestionTime.Slow.Box -> Slowing down = +1
RespTime <- page.ID %>% 
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(RT.Change = as.numeric(zQuestionTime) - lag(zQuestionTime, default = zQuestionTime[1])) %>%
  mutate(RT.Change = ifelse(is.na(RT.Change), 0, RT.Change),
         RT.Change.lag1 = lag(RT.Change, n = 1),
         RT.Change.lag2 = lag(RT.Change, n = 2)) %>%
  
  mutate(RT.Slower = as.numeric(ifelse(is.na(RT.Change), 0, sign(RT.Change))),
         RT.Slower.lag1 = lag(RT.Slower, n = 1), 
         RT.Slower.lag2 = lag(RT.Slower, n = 2)) %>%
  
  mutate(RT.Change.Slow.Box = ifelse(RT.Change <= quantile(RT.Change, 0.75, na.rm=T) & RT.Change >= quantile(RT.Change, 0.25, na.rm=T), 0,
                                     ifelse(RT.Change > quantile(RT.Change, 0.75, na.rm = T), 1,
                                            ifelse(RT.Change < quantile(RT.Change, 0.25, na.rm = T), -1, 0)))) %>%
  
  mutate(RT.Change.Slow.CrossingBox = ifelse(lag(QuestionTime.Slow.Box, n = 1) == QuestionTime.Slow.Box, 0, 
                                             ifelse(lag(QuestionTime.Slow.Box, n = 1) > QuestionTime.Slow.Box, -1, 
                                                    ifelse(lag(QuestionTime.Slow.Box, n = 1) < QuestionTime.Slow.Box, +1, 0))),
         
         RT.Change.Slow.CrossingBox3 = ifelse(lag(QuestionTime.Slow.Box, n = 2) == QuestionTime.Slow.Box & lag(QuestionTime.Slow.Box, n = 1) == QuestionTime.Slow.Box, 0,
                                              ifelse(lag(QuestionTime.Slow.Box, n = 2) > lag(QuestionTime.Slow.Box, n = 1) & lag(QuestionTime.Slow.Box, n = 1) > QuestionTime.Slow.Box, -1, 
                                                     ifelse(lag(QuestionTime.Slow.Box, n = 2) < lag(QuestionTime.Slow.Box, n = 1) & lag(QuestionTime.Slow.Box, n = 1) < QuestionTime.Slow.Box, -1, 0)))) %>%
  dplyr::select(Year, SID, id, starts_with("RT.")) %>%
  ungroup()%>%
  tbl_df()

# 7. Make the RT.Changes more stable (across 3 pages):
# 7a. RespTime$RT.Change.Mean3 = average RT Change of the last 3 pages
RespTime$RT.Change.Mean3 <- rowMeans(RespTime[,c(4:6)], na.rm = F)


RespTime$RT.Change.Mean2 <- rowMeans(RespTime[,c(4:5)], na.rm = F)
RespTime$RT.Abs.Change.Mean2 <- (abs(RespTime$RT.Change) + abs(RespTime$RT.Change.lag1))/2
RespTime$RT.Abs.Change.Mean3 <- (abs(RespTime$RT.Change) + abs(RespTime$RT.Change.lag1)+abs(RespTime$RT.Change.lag2))/3


# 7b. Check if the mean response time change is more then the other people
# => if RT.Change.Mean3 > 75 -> higher changes (more time) -> slowing down
# => if RT.Change.Mean3 > 25 -> higher changes (less time) -> speeding down
RespTime$RT.Change.Mean3.Slow.Box <- ifelse(RespTime$RT.Change.Mean3 <= quantile(RespTime$RT.Change.Mean3, 0.75, na.rm=T) & 
                                              RespTime$RT.Change.Mean3 >= quantile(RespTime$RT.Change.Mean3, 0.25, na.rm=T), 0,
                                            ifelse(RespTime$RT.Change.Mean3 > quantile(RespTime$RT.Change.Mean3, 0.75, na.rm = T), 1,
                                                   ifelse(RespTime$RT.Change.Mean3 < quantile(RespTime$RT.Change.Mean3, 0.25, na.rm = T), -1, NA)))

# 7b. becoming faster/slower in ALL 3 previous pages
RespTime$RT.Change.SlowAll3 <- ifelse(rowMeans(RespTime[,c(7:9)], na.rm = F) == 1 | 
                                        rowMeans(RespTime[,c(7:9)], na.rm = F) == -1, 
                                      rowMeans(RespTime[,c(7:9)], na.rm = F), 0)

RespTime[is.na(RespTime)] <- 0

RespTime <- dplyr::select(RespTime, -contains("lag"))

page.ID <- full_join(page.ID, RespTime, by = c("Year", "SID", "id"))


# Slower3 = was the mean RT.Change negative or positive
page.ID$RT.SlowerMean3 <- as.numeric(ifelse(is.na(page.ID$RT.Change.Mean3), 0, sign(page.ID$RT.Change.Mean3)))

# Previous Page QuestionNumber #
################################
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year,  SID, id) %>%
  mutate(NumQuestions.Prev = lag(NumQuestions, n = 1),
         NumQuestions.Prev = ifelse(is.na(NumQuestions.Prev), 0, NumQuestions.Prev)) %>%
  ungroup()

# Item Nonresponse #
####################
page.ID <- page.ID %>%
  mutate_at(vars(PageQuestions, PageAnswers), funs(as.character(.)))

page.ID$ItemNR.beg <- ifelse(substr(page.ID$PageAnswers, 1, 1)=="^", 1, 0)
page.ID$ItemNR.end <- ifelse(substr(page.ID$PageAnswers, nchar(page.ID$PageAnswers), nchar(page.ID$PageAnswers))=="^", 1, 0)
page.ID$ItemNR.9 <- (sapply(gregexpr("^^^^^^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*9

page.ID$ItemNR.8 <- (sapply(gregexpr("^^^^^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*8
page.ID$ItemNR.8 <- ifelse(page.ID$ItemNR.9 != 0, 0, page.ID$ItemNR.8)

page.ID$ItemNR.7 <- (sapply(gregexpr("^^^^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*7
page.ID$ItemNR.7 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0, 0, page.ID$ItemNR.7)

page.ID$ItemNR.6 <- (sapply(gregexpr("^^^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*6
page.ID$ItemNR.6 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0, 0, page.ID$ItemNR.6)

page.ID$ItemNR.5 <- (sapply(gregexpr("^^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*5
page.ID$ItemNR.5 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0 | page.ID$ItemNR.6 != 0, 0, page.ID$ItemNR.5)

page.ID$ItemNR.4 <- (sapply(gregexpr("^^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*4
page.ID$ItemNR.4 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0 | page.ID$ItemNR.6 != 0 | page.ID$ItemNR.5 != 0, 0, page.ID$ItemNR.4)

page.ID$ItemNR.3 <- (sapply(gregexpr("^^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*3
page.ID$ItemNR.3 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0 | page.ID$ItemNR.6 != 0 | page.ID$ItemNR.5 != 0 | page.ID$ItemNR.4 != 0, 0, page.ID$ItemNR.3)

page.ID$ItemNR.2 <- (sapply(gregexpr("^^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*2
page.ID$ItemNR.2 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0 | page.ID$ItemNR.6 != 0 | page.ID$ItemNR.5 != 0 | page.ID$ItemNR.4 != 0 | page.ID$ItemNR.3 != 0, 0, page.ID$ItemNR.2)

page.ID$ItemNR.1 <- (sapply(gregexpr("^^", page.ID$PageAnswers, fixed = TRUE), function(x) sum(x > -1)))*1
page.ID$ItemNR.1 <- ifelse(page.ID$ItemNR.9 != 0 | page.ID$ItemNR.8 != 0 | page.ID$ItemNR.7 != 0 | page.ID$ItemNR.6 != 0 | page.ID$ItemNR.5 != 0 | page.ID$ItemNR.4 != 0 | page.ID$ItemNR.3 != 0 | page.ID$ItemNR.2 != 0, 0, page.ID$ItemNR.1)

page.ID$ItemNR.1.1 <- ifelse(page.ID$PageQuestions != "HEADERSURVEYEXPLANATION" &
                               page.ID$PageQuestions != "INFCONS_1" &
                               page.ID$PageQuestions != "INFCONS1_2014" &
                               page.ID$PageQuestions != "INFCONS2" &
                               page.ID$PageQuestions != "INFCONS2_1" &
                               page.ID$PageAnswers == "", 1, 0)

page.ID$ItemNR <- 0

page.ID <- page.ID %>% 
  mutate(ItemNR = page.ID %>% dplyr::select(starts_with("ItemNR.")) %>% rowSums()) %>%
  dplyr::select(-(starts_with("ItemNR."))) %>%
  mutate(ItemNR.Rate = ItemNR / NumQuestions)

page.ID$ItemNR.Percent <- page.ID$ItemNR.Rate*100

# Straightlining #
##################
Answers <- page.ID %>%
  dplyr::select(Year, SampType.Survey, SID, id, PageQuestions, QuestionSection, PageAnswers, NumQuestions) %>%
  mutate(NumAnswers = str_count(PageAnswers, coll("^")) +1)

# Get all answers in different variables -> 19 variables
Answers <- cbind(Answers, as.data.frame(str_split_fixed(Answers$PageAnswers, coll("^"), n = max(Answers$NumAnswers))))

# Format the answers in 
# 1. Format everything into Characters
# 2. Change True/False to 1/0
# 3. Format everything into numeric -> sets all written words to NA (=Open-Ended Questions)
Answers <- Answers %>% 
  arrange(Year, SID, id) %>%
  mutate_at(vars(starts_with("V")), funs(as.character(.))) %>%
  mutate_at(vars(starts_with("V")), funs(ifelse(. == "True", 1,
                                                ifelse( . == "False", 0, .)))) %>%
  mutate_at(vars(starts_with("V")), funs(as.numeric(.)))

# Get the Variance of the numeric answers -> sets all pages with only 1 question to NA
# if Variance == 0 then there is straightlining
# Set variance to 1 if NA
Answers$AnswerVariance <- rowVars(as.matrix(Answers[, c(10:28)]), na.rm = TRUE)

page.ID <- page.ID %>%
  arrange(Year, SID, id) %>%
  mutate(Straightline = ifelse(Answers$AnswerVariance == 0, 1, 0),
         Straightline = ifelse(is.na(Straightline), 0, Straightline),
         AnswerVariance = ifelse(is.na(Answers$AnswerVariance), 1, Answers$AnswerVariance))

# Fix answervariance for 2015 FCST64_2014/FCST65_2014 and STUDQUES62/STUDQUES63:
page.ID$AnswerVariance <- ifelse(page.ID$QuestionSection == "STUDQUES62" | page.ID$QuestionSection == "FCST64_2014", 1, page.ID$AnswerVariance)


# Straightlining only possible with max 1 ItemNR:
page.ID$Straightline <- ifelse(page.ID$Straightline == 1 & page.ID$ItemNR > 1, 0, page.ID$Straightline)

# Previous Action #
###################
page.ID$Previous <- ifelse(page.ID$Action == "Previous", 1, 0)

page.ID$TopicSection <- droplevels(page.ID$TopicSection)

# Device change#
################
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year, SID, id) %>%
  mutate(MobileChange = ifelse(Mobile == lag(Mobile, default = Mobile[1]), 0, 1 ))%>%
  mutate(MobileChange = ifelse(row_number() == 1, 0, MobileChange)) %>%
  ungroup()

# New TopicSection
page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(Year, SID, id) %>%
  mutate(NewTopicSection = ifelse(TopicSection == lag(TopicSection, default = TopicSection[1]), 0, 1 ))%>%
  mutate(NewTopicSection = ifelse(row_number() == 1, 0, NewTopicSection)) %>%
  ungroup()

# RT change slow/fast indicator
page.ID$RT.Change.Slow3 <- sign(page.ID$RT.Change.Mean3)
page.ID$RT.Change.Mean3.Abs <- abs(page.ID$RT.Change.Mean3)

page.ID$RT.Change.Slow <- sign(page.ID$RT.Change)
page.ID$RT.Change.Abs <- abs(page.ID$RT.Change)


# Response history:
page.ID.2013 <- subset(pageIDlevel, Year == "2013")
page.ID.2014 <- subset(pageIDlevel, Year == "2014")

# who broke off in 2013?
page.ID.2014 <- page.ID.2013 %>%
  mutate(ParticipateYearBefore = 1) %>%
  mutate(BreakoffYearBefore = ifelse(PageQuestions == "THANKYOU" | PageQuestions == "INCENTIVE", 0, 1)) %>%
  mutate(Year = "2014") %>%
  select(SID, Year, ParticipateYearBefore, BreakoffYearBefore)

page.ID.2015 <- page.ID.2014 %>%
  mutate(ParticipateYearBefore = 1) %>%
  mutate(BreakoffYearBefore = ifelse(PageQuestions == "THANKYOU" | PageQuestions == "INCENTIVE", 0, 1)) %>%
  mutate(Year = "2015") %>%
  select(SID, Year, ParticipateYearBefore, BreakoffYearBefore)


# Use the normal RT.Change.Mean3
page.ID$RT.Change.Mean3.Slow <- ifelse(page.ID$RT.Change.Mean3 > 0, page.ID$RT.Change.Mean3, 0)
page.ID$RT.Change.Mean3.Speed <- ifelse(page.ID$RT.Change.Mean3 < 0, -page.ID$RT.Change.Mean3, 0)

# combine scrolling
page.ID$Scrolled.HV <- as.factor(ifelse(page.ID$VerticalScroll == 0 & page.ID$HorizontalScroll == 0, 0,
                                        ifelse(page.ID$VerticalScroll == 1 & page.ID$HorizontalScroll == 0, 1,
                                               ifelse(page.ID$VerticalScroll == 0 & page.ID$HorizontalScroll == 1, 2,
                                                      ifelse(page.ID$VerticalScroll == 1 & page.ID$HorizontalScroll == 1, 3, NA)))))


################
# Get the breakoff page:

page.ID <- page.ID %>%
  group_by(Year, SID) %>%
  arrange(id) %>%
  mutate(NumQuestions.Next = lead(NumQuestions),
         NewTopic.Next = lead(NewTopicSection),
         NewTopic.Prev = lag(NewTopicSection),
         PageName.Next = lead(PageName),
         PageName.Prev = lag(PageName)) %>%
  ungroup()


# Which question was actually seen next (depending on action)

# Faculty
NumQuest.Fac <- page.ID %>%
  dplyr::select(SampType.Survey, PageName, NumQuestions, TopicSection) %>%
  dplyr::filter(SampType.Survey == "Faculty") %>%
  group_by(PageName) %>%
  arrange(NumQuestions) %>%
  filter(row_number() == n())

# Old question order (without the 400er PageNames)
NumQuest.Fac$PageName <- factor(NumQuest.Fac$PageName, levels = c("1",	"2",	"3",	"4",	"4A",	"6",	"7",	"9",	"8",	
                                                                  "11",	"12",	"13",	"14",	"15", "101", "16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"25",	"26",	"27",	"28",	"29",	
                                                                  "30",	"30A",	"30B",	"30C",	"30D",	"30E",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	
                                                                  "40",	"41",	"42",	"43",	"43A",	"44",	"44A",	"45",	"47",	"47A",	"47B",	"47C",	"47D",	"47E",	"47F",	"47I",	"49",	
                                                                  "50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	"61",	"62",	"63",	"64"))

NumQuest.Fac <- arrange(NumQuest.Fac, PageName)
NumQuest.Fac$NumQuestions.Next.Break <- lead(NumQuest.Fac$NumQuestions)
NumQuest.Fac$TopicSection.Next <- lead(NumQuest.Fac$TopicSection)
NumQuest.Fac$TopicSection.Prev <- lag(NumQuest.Fac$TopicSection)
NumQuest.Fac$NewTopic.Next.Break <- ifelse(NumQuest.Fac$TopicSection == NumQuest.Fac$TopicSection.Next, 0, 1)
NumQuest.Fac$NewTopic.Prev.Break <- ifelse(NumQuest.Fac$TopicSection == NumQuest.Fac$TopicSection.Prev, 0, 1)
NumQuest.Fac$PageName.Next.Break <- lead(NumQuest.Fac$PageName)
NumQuest.Fac$PageName.Prev.Break <- lag(NumQuest.Fac$PageName)

# Students
NumQuest.Stu <- page.ID %>%
  dplyr::select(SampType.Survey, PageName, NumQuestions, TopicSection) %>%
  dplyr::filter(SampType.Survey == "Student") %>%
  group_by(PageName) %>%
  arrange(NumQuestions) %>%
  filter(row_number() == n())

# Old question order (without the 400er and 500er PageNames)
NumQuest.Stu$PageName <- factor(NumQuest.Stu$PageName, levels = c("1",	"2",	"201",	"55",	"52",	"53",	"54",	"51",	"202", "3",	"4",	"4A",	"6",	"11",	"12",	"13",	"16",	"14",	"18",	"19",	
                                                                  "20",	"21",	"22",	"203",	"23",	"25",	"26",	"27",	"28",	"29",	"30",	"30A",	"30B",	"30C",	"30D",	"30E",	
                                                                  "31",	"32",	"33",	"34",	"204",	"35",	"36",	"37",	"38",	"39",	"40",	"42",	"41",	
                                                                  "205",	"206",	"207",	"208",	"209",	"210",	"211",	"47",	"47B",	"47D",	"47E",	"47H",	"47F",	"47G",	
                                                                  "212",	"213",	"214",	"215",	"216",	"58",	"61",	"62",	"63",	"64"))

NumQuest.Stu <- arrange(NumQuest.Stu, PageName)
NumQuest.Stu$NumQuestions.Next.Break <- lead(NumQuest.Stu$NumQuestions)
NumQuest.Stu$TopicSection.Next <- lead(NumQuest.Stu$TopicSection)
NumQuest.Stu$TopicSection.Prev <- lag(NumQuest.Stu$TopicSection)
NumQuest.Stu$NewTopic.Next.Break <- ifelse(NumQuest.Stu$TopicSection == NumQuest.Stu$TopicSection.Next, 0, 1)
NumQuest.Stu$NewTopic.Prev.Break <- ifelse(NumQuest.Stu$TopicSection == NumQuest.Stu$TopicSection.Prev, 0, 1)
NumQuest.Stu$PageName.Next.Break <- lead(NumQuest.Stu$PageName)
NumQuest.Stu$PageName.Prev.Break <- lag(NumQuest.Stu$PageName)

Num.Quest <- rbind(NumQuest.Fac, NumQuest.Stu) %>%
  dplyr::select(SampType.Survey, PageName, NumQuestions.Next.Break, NewTopic.Next.Break, NewTopic.Prev.Break, PageName.Next.Break, PageName.Prev.Break)

Num.Quest[is.na(Num.Quest)] <- 0


page.ID <- left_join(page.ID, Num.Quest, by = c("SampType.Survey", "PageName"))
#Replace the values where it is NA or the page is breakoff 
page.ID$NumQuestions.Next <- ifelse(page.ID$BreakoffQuestion == 1 | is.na(page.ID$NumQuestions.Next), page.ID$NumQuestions.Next.Break, page.ID$NumQuestions.Next)

page.ID$NewTopic.Next <- ifelse(page.ID$BreakoffQuestion == 1 | is.na(page.ID$NewTopic.Next), page.ID$NewTopic.Next.Break, page.ID$NewTopic.Next)
page.ID$NewTopic.Prev <- ifelse(page.ID$BreakoffQuestion == 1 | is.na(page.ID$NewTopic.Prev), page.ID$NewTopic.Prev.Break, page.ID$NewTopic.Prev)

page.ID$PageName.Next <- ifelse(page.ID$BreakoffQuestion == 1 | is.na(page.ID$PageName.Next), page.ID$PageName.Next.Break, page.ID$PageName.Next)
page.ID$PageName.Prev <- ifelse(page.ID$BreakoffQuestion == 1 | is.na(page.ID$PageName.Prev), page.ID$PageName.Prev.Break, page.ID$PageName.Prev)

# Make variable which page was actually seen next (depends on the action next or previous)
page.ID$NumQuestions.NextSeen <- ifelse(page.ID$Action == "Next" , page.ID$NumQuestions.Next, page.ID$NumQuestions.Prev)
page.ID$NewTopic.NextSeen <- as.factor(ifelse(page.ID$Action == "Next" , page.ID$NewTopic.Next, page.ID$NewTopic.Prev))
page.ID$PageName.NextSeen <- ifelse(page.ID$Action == "Next" , page.ID$PageName.Next, page.ID$PageName.Prev)
