###############################################################################
### Data Analysis ###
### F. Mittereder & B. T. West, 2021, JSSAM "A dynamic survival modeling approach to the prediction of web survey breakoff"
### last modified 02/21/2021
############################################################################### 

# Data is not publicly available. Please contact http://graham.umich.edu/campus/scip/materials#contact for data access and questions. 

library(nnet)
library(tidyr)
library(stats)
library(pROC)
library(ggplot2)
library(plotROC)
library(png)
library(cowplot)
library(caret)
library(dplyr)
library(survey)

# Cox models
library(survival)
library(risksetROC)
library(survminer)
library(My.stepwise)
library(timereg)
# Marignal effects
library(ggeffects)

load("page.ID.RData")


########################
# Descriptive analyses #
########################


# page variant variables: frequencies

Breakoff.f <- table(page.ID$BreakoffQuestion, page.ID$Year)
Breakoff.p <- round((prop.table(Breakoff.f, 2)*100), 2)
Breakoff <- cbind(Breakoff.f[,1], Breakoff.p[,1], Breakoff.f[,2], Breakoff.p[,2])
rownames(Breakoff) <- c("Non-breakoff page", "Breakoff page")

Mobile.f <- table(page.ID$Mobile, page.ID$Year)
Mobile.p <- round((prop.table(Mobile.f, 2)*100), 2)
Mobile <- cbind(Mobile.f[,1], Mobile.p[,1], Mobile.f[,2], Mobile.p[,2])
rownames(Mobile) <- c("Non mobile", "Mobile")

Session.f <- table(page.ID$NewSession, page.ID$Year)
Session.p <- round((prop.table(Session.f, 2)*100), 2)
Session <- cbind(Session.f[,1], Session.p[,1], Session.f[,2], Session.p[,2])
rownames(Session) <- c("Continue session", "Start new session")

Previous.f <- table(page.ID$Previous, page.ID$Year)
Previous.p <- round((prop.table(Previous.f, 2)*100), 2)
Previous <- cbind(Previous.f[,1], Previous.p[,1], Previous.f[,2], Previous.p[,2])
rownames(Previous) <- c("Next button", "Previous button")


page.ID$Scrolled.HV <- as.factor(ifelse(page.ID$VerticalScroll == 0 & page.ID$HorizontalScroll == 0, 0,
                                        ifelse(page.ID$VerticalScroll == 1 & page.ID$HorizontalScroll == 0, 1,
                                               ifelse(page.ID$VerticalScroll == 0 & page.ID$HorizontalScroll == 1, 2,
                                                      ifelse(page.ID$VerticalScroll == 1 & page.ID$HorizontalScroll == 1, 3, NA)))))

Scrolled.f <- table(page.ID$Scrolled.HV, page.ID$Year)
Scrolled.p <- round((prop.table(Scrolled.f, 2)*100), 2)
Scrolled <- cbind(Scrolled.f[,1], Scrolled.p[,1], Scrolled.f[,2], Scrolled.p[,2])
rownames(Scrolled) <- c("Not scrolled", "Vertical", "Horizontal", "Both")

NewTopic.f <- table(page.ID$NewTopic.NextSeen, page.ID$Year)
NewTopic.p <- round((prop.table(NewTopic.f, 2)*100), 2)
NewTopic <- cbind(NewTopic.f[,1], NewTopic.p[,1], NewTopic.f[,2], NewTopic.p[,2])
rownames(NewTopic) <- c("Continue topic section", "Begin new topic section")

Topic.f <- table(page.ID$TopicSection, page.ID$Year)
Topic.p <- round((prop.table(Topic.f, 2)*100), 2)
Topic <- cbind(Topic.f[,1], Topic.p[,1], Topic.f[,2], Topic.p[,2])
rownames(Topic) <- c("Introduction", "Transportation", "Conservation", "Environment", "Food", "Climate", "General sustainability", 
                     "Sustainability at U of M", "Demographics")


Resp.Para.Freq_Pagelevel <- rbind(Breakoff, Topic, NewTopic, Mobile, Session, Previous)
Resp.Para.Freq_Pagelevel[,c(1,3)] <- prettyNum(Resp.Para.Freq_Pagelevel[,c(1,3)], big.mark = ",")
Resp.Para.Freq_Pagelevel[,c(2,4)] <- percentParantheses(as.numeric(Resp.Para.Freq_Pagelevel[,c(2,4)]))

colnames(Resp.Para.Freq_Pagelevel) <- c("Freq.", "Perc.", "Freq.", "Perc.")


# page variant summary
RelItem <- tapply((page.ID$ItemNR.Rate*100), page.ID$Year, summary)
QuestionTime <- tapply(page.ID$QuestionTime, page.ID$Year, summary)
RespTime.Change <- tapply(page.ID$RT.Change, page.ID$Year, summary)
NumQues <- tapply(page.ID$NumQuestions, page.ID$Year, summary)
NumScrol <- tapply(page.ID$NumScrolls, page.ID$Year, summary)
AnsVar <- tapply(page.ID$AnswerVariance, page.ID$Year, summary)

Resp.Para.Summary_Pagelevel <- rbind(NumQues$`2014`, RelItem$`2014`, AnsVar$`2014`, NumScrol$`2014`, QuestionTime$`2014`, RespTime.Change$`2014`,   
                                     NumQues$`2015`, RelItem$`2015`, AnsVar$`2015`, NumScrol$`2015`, QuestionTime$`2015`, RespTime.Change$`2015`)

Resp.Para.Summary_Pagelevel[,c(1:6)] <- round.0.coeff(Resp.Para.Summary_Pagelevel[,c(1:6)], rounding.number =  2)
Resp.Para.Summary_Pagelevel[,c(1:6)] <- prettyNum(Resp.Para.Summary_Pagelevel[,c(1:6)], big.mark = ",")
colnames(Resp.Para.Summary_Pagelevel) <- c("Min.", "25% quantile", "Median", "Mean", "75% quantile", "Max.")

rownames(Resp.Para.Summary_Pagelevel) <- rep(c( "Number of question items per page", "Item nonresponse rate (in %)", "Answer variability", "Number of scrolls per page",
                                                "Question response time (in ms)", "Response time change" ), 2)

# how often horizontal scrolling?
prop.table(table(page.ID[page.ID$Scrolled == 1, ]$HorizontalScroll, page.ID[page.ID$Scrolled == 1, ]$VerticalScroll))

###############################
# Show example for RT change: #
Example.RT <- dplyr::select(page.ID[page.ID$SID == "100094" & page.ID$Year == "2015" & page.ID$PageCount <=10, ], SID, PageCount, NumQuestions, QuestionTime, zQuestionTime, RT.Change, RT.Change.Mean3)
Example.RT[, c(4:7)] <- round(Example.RT[, c(4:7)], 2)

# When/which question do people break off
Breakoff.Fac <- page.ID %>%
  filter(SampType.Survey == "Faculty") %>%
  group_by(Year, PageName) %>%
  summarize(BreakoffNb = sum(as.numeric(as.character(BreakoffQuestion)))) %>%
  ungroup() 

Breakoff.Fac$PageName <- factor(Breakoff.Fac$PageName, levels = c("1",	"2",	"3",	"4",	"4A",	"6",	"7",	"9",	"8",
                                                                  "11",	"12",	"13",	"14",	"15", "101", "16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"25",	"26",	"27",	"28",	"29",
                                                                  "30",	"30A",	"30B",	"30C",	"30D",	"30E",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",
                                                                  "40",	"41",	"42",	"43",	"43A",	"44",	"44A",	"45",	"47",	"47A",	"47B",	"47C",	"47D",	"47E",	"47F",	"47I",	"49",
                                                                  "50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	"61",	"62",	"63",	"64"))

Page.labels.fac <- c(1:75)
Page.Breakoffs.Fac <- ggplot(Breakoff.Fac, aes(x = factor(PageName), y = BreakoffNb, group = Year)) +
  geom_line(aes(color=Year)) +
  geom_point(aes(color=Year)) +
  scale_x_discrete(labels = Page.labels.fac) +
  # scale_x_discrete(breaks = PageName[c(T,F,F)]) +
  scale_color_manual("Survey year", values = c(col.2014, col.2015)) +
  labs(y = "Total breakoffs", x = "Survey page") +
  guides(color = guide_legend("Survey year")) +
  theme.survive +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# number of pages in 2014 2015
table(table(unique(Breakoff.Fac[Breakoff.Fac$Year == "2014",]$PageName)))
table(table(unique(Breakoff.Fac[Breakoff.Fac$Year == "2015",]$PageName)))

Breakoff.Stu <- page.ID %>%
  filter(SampType.Survey == "Student") %>%
  group_by(Year, PageName) %>%
  summarize(BreakoffNb = sum(as.numeric(as.character(BreakoffQuestion)))) %>%
  ungroup() 

Breakoff.Stu$PageName <- factor(Breakoff.Stu$PageName, levels = c("1",	"2",	"201",	"55",	"52",	"53",	"54",	"51",	"202", "3",	"4",	"4A",	"6",	"11",	"12",	"13",	"16",	"14",	"18",	"19",
                                                                  "20",	"21",	"22",	"203",	"23",	"25",	"26",	"27",	"28",	"29",	"30",	"30A",	"30B",	"30C",	"30D",	"30E",
                                                                  "31",	"32",	"33",	"34",	"204",	"35",	"36",	"37",	"38",	"39",	"40",	"42",	"41",
                                                                  "205",	"206",	"207",	"208",	"209",	"210",	"211",	"47",	"47B",	"47D",	"47E",	"47H",	"47F",	"47G",
                                                                  "212",	"213",	"214",	"215",	"216",	"58",	"61",	"62",	"63",	"64"))
Page.labels.stu <- c(1:73)
Page.Breakoffs.Stu <- ggplot(Breakoff.Stu, aes(x = factor(PageName), y = BreakoffNb, group = Year)) +
  geom_line(aes(color=Year)) +
  geom_point(aes(color=Year)) +
  scale_x_discrete(labels = Page.labels.stu) +
  # scale_x_discrete(breaks = PageName[c(T,F,F)]) +
  scale_color_manual("Survey year", values = c(col.2014, col.2015)) +
  labs(y = "Total breakoffs", x = "Survey page") +
  guides(color = guide_legend("Survey year")) +
  theme.survive +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#For paper:
Page.Breakoffs.Fac.paper <- ggplot(data=Breakoff.Fac, aes(x=factor(PageName), y=BreakoffNb)) +
  geom_line(aes(group = Year, linetype=Year, color=Year), size = 1.3) +
  geom_point(aes(color=Year), size = 2.5) +
  scale_color_manual(values=c("2014"=col.2014, "2015"=col.2015)) +
  scale_x_discrete(labels = Page.labels.fac) +
  ylim(0, 155) +
  labs(y = "Total breakoffs for faculty/staff", x = "Survey page") +
  guides(linetype = guide_legend("Survey year"), color = guide_legend("Survey year")) +
  theme.survive  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # ANNOTATION
  # Intro
  annotate(
    geom = "curve", x = 3, y = 40, xend = 1, yend = 31,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 3, y = 40, label = "Intro page", hjust = "left") +
  # Consent
  annotate(
    geom = "curve", x = 4, y = 30, xend = 2, yend = 25,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 4, y = 30, label = "Consent page", hjust = "left") 


Page.Breakoffs.Stu.paper <- ggplot(data=Breakoff.Stu,aes(x=factor(PageName),y=BreakoffNb)) +
  geom_line(aes(group = Year, linetype=Year, color = Year), size = 1.3) +
  geom_point(aes(color = Year), size = 2.5) +
  scale_color_manual(values=c("2014"=col.2014, "2015"=col.2015)) +
  scale_x_discrete(labels = Page.labels.stu) +
  ylim(0, 155) +
  labs(y = "Total breakoffs for students", x = "Survey page") +
  guides(color = guide_legend("Survey year"), linetype = guide_legend("Survey year")) +
  theme.survive  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # ANNOTATION
  # Intro
  annotate(
    geom = "curve", x = 5, y = 120, xend = 1, yend = 150,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = 5, y = 120, xend = 1, yend = 110,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 5, y = 120, label = "Intro page", hjust = "left") +
  # Consent
  annotate(
    geom = "curve", x = 7, y = 45, xend = 2, yend = 50,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = 7, y = 45, xend = 2, yend = 37,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 7, y = 45, label = "Consent page", hjust = "left")
Page.Breakoffs.Stu.paper


#####################
# Survival Analyses #
#####################


# Survival curves
surv.fit.Student <- survfit(Surv(PageCountMax, Breakoff) ~ SampType.Survey, data = case.ID)
surv.plot.Student  <- ggsurvplot(surv.fit.Student,
                                 data = case.ID,
                                 conf.int = TRUE,
                                 legend = "bottom",
                                 legend.title = "University affiliation",
                                 legend.labs = c("Faculty/staff", "Student"),
                                 xlab = "Number of pages seen",
                                 ylim = c(0.7,1),
                                 linetype = 1,
                                 censor.size = 5,
                                 censor.shape = 124, 
                                 palette = c(col.cat1, col.cat2)) 


# Cox models
page.ID$Start <- page.ID$PageCount-1
page.ID$Stop <- page.ID$PageCount

page.ID$TopicSection <- as.factor(ifelse(page.ID$TopicSection == "Intro", 0, 
                                         ifelse(page.ID$TopicSection == "A", 1, 
                                                ifelse(page.ID$TopicSection == "B", 2, 
                                                       ifelse(page.ID$TopicSection == "C", 3,
                                                              ifelse(page.ID$TopicSection == "D", 4, 
                                                                     ifelse(page.ID$TopicSection == "E", 5,
                                                                            ifelse(page.ID$TopicSection == "F", 6,
                                                                                   ifelse(page.ID$TopicSection == "G", 7, 8)))))))))
page.ID$TopicNotIntro <- as.factor(ifelse(page.ID$TopicSection == 0, 0, 1))

page.ID$Gender <- as.factor(ifelse(page.ID$Gender == "M", 0, 1))
page.ID$Race <- as.factor(ifelse(page.ID$Race == "White", 0, 
                                 ifelse(page.ID$Race == "Asian", 1,
                                        ifelse(page.ID$Race == "Black", 2,
                                               ifelse(page.ID$Race == "Hispanic", 3,
                                                      ifelse(page.ID$Race == "Other", 4, 5))))))
page.ID$Race2 <- as.factor(ifelse(page.ID$Race == 0, 0, 1))

page.ID$SampType.Survey <- as.factor(ifelse(page.ID$SampType.Survey == "Faculty", 0, 1))

# for interaction
page.ID$SampTypePanel <- as.factor(ifelse(page.ID$SampType.Survey == 0, 0, 
                                          ifelse(page.ID$SampType.Survey == 1 & page.ID$Panel == 0, 1, 
                                                 ifelse(page.ID$SampType.Survey == 1 & page.ID$Panel == 1, 2, NA))))

page.ID$QuestionTime.Slow.Box <- factor(page.ID$QuestionTime.Slow.Box, levels = c("0", "-1", "1"))

page.ID <- dplyr::mutate_each(page.ID, funs(factor), NewTopicSection, Panel, History.Type, REM, Mobile, NewSession, Previous)



# Page dependent covariates (by years)
cox.full.14 <- coxph(Surv(Start, Stop, BreakoffQuestion) ~
                       TopicSection +
                       NewTopic.NextSeen +
                       NumQuestions +
                       NumQuestions.NextSeen +
                       
                       Gender +
                       Race +
                       SampType.Survey +
                       Panel +
                       
                       History.Type +
                       REM + 
                       Mobile +
                       NewSession +
                       Previous +
                       ItemNR.Rate +
                       AnswerVariance +
                       NumScrolls +
                       QuestionTime.Slow.Box + 
                       RT.Change.Mean3.Speed + RT.Change.Mean3.Slow,
                     data = page.ID[page.ID$Year == "2014", ], method="efron")
summary(cox.full.14)
gof(cox.full.14)


plot(ggpredict(cox.full.14, terms = c("History.Type"), type = "cumhaz", typical = "zero"))

cox.full.15 <- coxph(Surv(Start, Stop, BreakoffQuestion) ~
                       TopicSection +
                       NewTopic.NextSeen +
                       NumQuestions +
                       NumQuestions.NextSeen +
                       
                       Gender +
                       Race +
                       SampType.Survey +
                       Panel +
                       
                       History.Type +
                       REM + 
                       Mobile +
                       NewSession +
                       Previous +
                       ItemNR.Rate +
                       AnswerVariance +
                       NumScrolls +
                       QuestionTime.Slow.Box + 
                       RT.Change.Mean3.Speed + RT.Change.Mean3.Slow,
                     data = page.ID[page.ID$Year == "2015",] , method="efron")
summary(cox.full.15)



# check for time dependent coefficients (by year)
zph.cox.full.14 <- cox.zph(cox.full.14)
zph.cox.full.15 <- cox.zph(cox.full.15)


# Add time depenent variables (by year):
page.ID$Time <- page.ID$PageCount-1

page.ID$Female_time = (page.ID$Gender == 1)*page.ID$Time
page.ID$Student_time = (page.ID$SampType.Survey == 1)*page.ID$Time
page.ID$Panel_time = (page.ID$Panel == 1)*page.ID$Time
page.ID$Mobile_time = (page.ID$Mobile == 1)*page.ID$Time
page.ID$Previous_time = (page.ID$Previous == 1)*page.ID$Time
page.ID$RT.Change.Mean3.Slow_time = page.ID$RT.Change.Mean3.Slow *page.ID$Time
page.ID$RT.Change.Mean3.Speed_time = page.ID$RT.Change.Mean3.Speed *page.ID$Time

page.ID = mutate(page.ID,
                 Start = as.numeric(as.character(Start)),
                 Stop = as.numeric(as.character(Stop)),
                 BreakoffQuestion = as.numeric(as.character(BreakoffQuestion)),
                 TopicSection = factor(TopicSection, levels = c(0,1,2,3,4,5,6,7,8)),
                 NewTopic.NextSeen = factor(NewTopic.NextSeen, levels = c(0,1)),
                 NumQuestions = as.numeric(as.character(NumQuestions)),
                 NumQuestions.NextSeen = as.numeric(as.character(NumQuestions.NextSeen)),
                 Gender = factor(Gender, levels = c(0,1)),
                 Race = factor(Race, levels = c(0,1,2,3,4,5)),
                 SampType.Survey = factor(SampType.Survey, levels = c(0,1)),
                 Panel = factor(Panel, levels = c(0,1)),
                 History.Type = factor(History.Type, levels = c(0,1,2)),
                 REM = factor(REM, levels = c(0,1)),
                 Mobile = factor(Mobile, levels = c(0,1)),
                 NewSession = factor(NewSession, levels = c(0,1)),
                 Previous = factor(Previous, levels = c(0,1)),
                 ItemNR.Rate = as.numeric(as.character(ItemNR.Rate)),
                 AnswerVariance = as.numeric(as.character(AnswerVariance)),
                 NumScrolls = as.numeric(as.character(NumScrolls)),
                 QuestionTime.Slow.Box = factor(QuestionTime.Slow.Box, levels = c(0,-1,1)),
                 RT.Change.Mean3.Slow = as.numeric(as.character(RT.Change.Mean3.Slow)),
                 RT.Change.Mean3.Speed = as.numeric(as.character(RT.Change.Mean3.Speed)),
                 Female_time = as.numeric(as.character(Female_time)),
                 Student_time = as.numeric(as.character(Student_time)),
                 Panel_time = as.numeric(as.character(Panel_time)),
                 Mobile_time = as.numeric(as.character(Mobile_time)),
                 Previous_time = as.numeric(as.character(Previous_time)))

#2014
cox.full.time.14 <- coxph(Surv(Start, Stop, BreakoffQuestion) ~
                            TopicSection +
                            NewTopic.NextSeen +
                            NumQuestions +
                            NumQuestions.NextSeen +
                            
                            Gender +
                            Race +
                            SampType.Survey +
                            Panel +
                            
                            History.Type +
                            REM + 
                            Mobile +
                            NewSession +
                            Previous +
                            ItemNR.Rate +
                            AnswerVariance +
                            QuestionTime.Slow.Box + 
                            RT.Change.Mean3.Speed + RT.Change.Mean3.Slow +
                            # RT.Change.Mean3.Abs +
                            
                            Female_time + Student_time + Panel_time + Mobile_time + Previous_time,
                          data = page.ID[page.ID$Year == "2014", ], method="efron")
summary(cox.full.time.14)
summary(cox.full.14) # COMPARE

#2015
cox.full.time.15 <- coxph(Surv(Start, Stop, BreakoffQuestion) ~
                            TopicSection +
                            NewTopic.NextSeen +
                            NumQuestions +
                            NumQuestions.NextSeen +
                            
                            Gender +
                            Race +
                            SampType.Survey +
                            Panel +
                            
                            History.Type +
                            REM + 
                            Mobile +
                            NewSession +
                            Previous +
                            ItemNR.Rate +
                            AnswerVariance +
                            NumScrolls +
                            QuestionTime.Slow.Box + 
                            RT.Change.Mean3.Speed + RT.Change.Mean3.Slow + 
                            
                            Female_time + Student_time + Panel_time + Mobile_time + Previous_time,
                          data = page.ID[page.ID$Year == "2015",] , method="efron")
summary(cox.full.time.15)
summary(cox.full.15) # COMPARE

# Plot #
########
plot(ggpredict(cox.full.time.14, terms = c("SampType.Survey")))
plot(ggpredict(cox.full.time.14, terms = c("Gender", "Female_time")))
# Predicted survival probablities



#####################################
# Prediction predict 2015 with 2014:
page.ID$Training <- ifelse(page.ID$Year == "2014", 1, 0)
# model with significant variables in at least one year: cox.full.time.table only
cox.train.time = cox.full.time.14

summary(cox.train.time)

page.ID$BreakoffQuestion <- as.numeric(as.character(page.ID$BreakoffQuestion))

# evaluate risk:
page.ID$risk <- 0
page.ID[page.ID$Training == 0, ]$risk<- predict(cox.train.time, page.ID[page.ID$Training == 0, ], type=c("expected"))

# Plot ROCs
ROC.k <- ggplot(page.ID[page.ID$Training == 0, ], aes(d=BreakoffQuestion, m = risk)) +
  geom_roc(n.cuts = 0, size = 1) +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  # ggtitle("Survey year 2014") +
  geom_abline(intercept = 0, slope = 1, color = "grey70") +
  theme.ggplot.simple +
  theme.roc

ROC.Student <- ggplot(page.ID[page.ID$Training == 0, ], aes(d=BreakoffQuestion, m = risk, color = SampType.Survey)) +
  geom_roc(n.cuts = 0, size = 1) +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  # ggtitle("ROC by U of M affiliation") +
  geom_abline(intercept = 0, slope = 1, color = "grey70") +
  theme.ggplot.simple +
  theme.roc +
  scale_color_manual("Affiliation", labels = c("Faculty/staff", "Student"), 
                     values = c(col.Intro, col.Qnr))

ROC.Mobile <- ggplot(page.ID[page.ID$Training == 0, ], aes(d=BreakoffQuestion, m = risk, color = Mobile)) +
  geom_roc(n.cuts = 0, size = 1) +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  # ggtitle("ROC by answering device") +
  geom_abline(intercept = 0, slope = 1, color = "grey70") +
  theme.ggplot.simple +
  theme.roc +
  scale_color_brewer("Device", labels = c("Non-mobile", "Mobile"), 
                     palette="Set2")

page.ID$Samp.Mob <- ifelse(page.ID$SampType.Survey == 0 & page.ID$Mobile == 0, "FacNonMob",
                           ifelse(page.ID$SampType.Survey == 0 & page.ID$Mobile == 1, "FacMob",
                                  ifelse(page.ID$SampType.Survey == 1 & page.ID$Mobile == 0, "StuNonMob", "StuMob")))
page.ID$Samp.Mob <- factor(page.ID$Samp.Mob, levels = c("FacNonMob", "FacMob", "StuNonMob", "StuMob"))

ROC.Samp.Mob <- ggplot(page.ID[page.ID$Training == 0, ], aes(d=BreakoffQuestion, m = risk, color = Samp.Mob)) +
  geom_roc(n.cuts = 0, size = 1) +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  # ggtitle("ROC by affiliation*device") +
  geom_abline(intercept = 0, slope = 1, color = "grey70") +
  theme.ggplot.simple +
  theme.roc +
  scale_color_manual("Affiliation*device", 
                     labels = c("Faculty non-mobile", "Faculty mobile", "Student non-mobile", "Student mobile"), 
                     values = c(col.Qnr, col.Intro, col.cat1, col.cat2)) +
  guides(color = guide_legend(nrow=2))

ROC.Topic <- ggplot(page.ID[page.ID$Training == 0, ], aes(d=BreakoffQuestion, m = risk, color = TopicSection)) +
  geom_roc(n.cuts = 0, size = 1) +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  # ggtitle("ROC by topic section") +
  geom_abline(intercept = 0, slope = 1, color = "grey70") +
  theme.ggplot.simple +
  theme.roc +
  scale_color_discrete("Topic", 
                       labels = c("Intro", "A", "B", "C", "D", "E", "F", "G", "H"))

ROC.all <- plot_grid(ROC.k, ROC.Student, ROC.Mobile, ROC.Samp.Mob, ROC.Topic, labels = "AUTO", ncol = 2, nrow = 3)
save_plot("Graphics/05_ROC_all.png", ROC.all, ncol = 2, nrow = 2)

#Baseline (when all is 0, no breakoff ever)
page.ID$predict <- 0
ConfMx.Base <- confusionMatrix(as.factor(page.ID[page.ID$Training == 0, ]$predict), as.factor(page.ID[page.ID$Training == 0, ]$BreakoffQuestion), positive = "1")


# 1 Threshold:
k <- c(rep(0, 1))
cox.roc.train <- list()
AUC <- length(k)

cox.roc.train <- roc(page.ID[page.ID$Training == 0 , ]$BreakoffQuestion ~  page.ID[page.ID$Training == 0 , ]$risk,
                     data = page.ID[page.ID$Training == 0 , ])
k <- coords(cox.roc.train, "best", ret = "threshold", best.method="youden")
AUC <- as.numeric(cox.roc.train$auc)

page.ID$predict <- 0
page.ID[page.ID$Training == 0, ]$predict <- ifelse(page.ID[page.ID$Training == 0, ]$risk > k, 1, 0)
ConfMx <- confusionMatrix(as.factor(page.ID[page.ID$Training == 0, ]$predict), as.factor(page.ID[page.ID$Training == 0, ]$BreakoffQuestion), positive = "1")


Conf.Mx.All1 <- as.matrix(ConfMx$byClass[c(1, 2, 5)])
Conf.Mx.All1 <- rbind(Conf.Mx.All1, as.matrix(ConfMx$overall[c(1,2)]))

# Threshold by student/facutly:
SampType <- unique(page.ID$SampType.Survey)
k.SampType <- c(rep(0, length(SampType)))
cox.roc.train.SampType <- list()
AUC.SampType <- length(k.SampType)

for(i in 1:length(k.SampType)){
  cox.roc.train.SampType[[i]] <- roc(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i], ]$BreakoffQuestion ~
                                       page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i], ]$risk,
                                     data = page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i], ])
  k.SampType[i] <- coords(cox.roc.train.SampType[[i]], "best", ret = "threshold", best.method="youden")
  AUC.SampType[i] <- as.numeric(cox.roc.train.SampType[[i]]$auc)
}

page.ID$predict.SampType <- 0
for(i in 1:length(k.SampType)){
  page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i], ]$predict.SampType <- 
    ifelse(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i], ]$risk > k.SampType[i], 1, 0)
}

page.ID$predict.SampType <- factor(page.ID$predict.SampType, levels = c(0,1))
page.ID$BreakoffQuestion <- as.factor(page.ID$BreakoffQuestion)

ConfMx.SampType <- confusionMatrix(page.ID[page.ID$Training == 0, ]$predict.SampType, page.ID[page.ID$Training == 0, ]$BreakoffQuestion, positive = "1")
ConfMx.SampType

Conf.Mx.All2 <- as.matrix(ConfMx.SampType$byClass[c(1, 2, 5)])
Conf.Mx.All2 <- rbind(Conf.Mx.All2, as.matrix(ConfMx.SampType$overall[c(1,2)]))



#for paper
AUC.SampType

ConfMx.SampType.Fac <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0, ]$predict.SampType, 
                                       page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.SampType.Fac$byClass[c(1, 2, 5)])
as.matrix(ConfMx.SampType.Fac$overall[c(1,2)])

ConfMx.SampType.Stu <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1, ]$predict.SampType, 
                                       page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.SampType.Stu$byClass[c(1, 2, 5)])
as.matrix(ConfMx.SampType.Stu$overall[c(1,2)])



#Threshold for mobile/nonmobile
Mobile <- unique(page.ID$Mobile)
k.Mobile <- c(rep(0, length(Mobile)))
cox.roc.train.Mobile <- list()
AUC.Mobile <- length(k.Mobile)

for(i in 1:length(k.Mobile)){
  cox.roc.train.Mobile[[i]] <- roc(page.ID[page.ID$Training == 0 & page.ID$Mobile == Mobile[i], ]$BreakoffQuestion ~
                                     page.ID[page.ID$Training == 0 & page.ID$Mobile == Mobile[i], ]$risk,
                                   data = page.ID[page.ID$Training == 0 & page.ID$Mobile == Mobile[i], ])
  k.Mobile[i] <- coords(cox.roc.train.Mobile[[i]], "best", ret = "threshold", best.method="youden")
  AUC.Mobile[i] <- as.numeric(cox.roc.train.Mobile[[i]]$auc)
}

page.ID$predict.Mobile <- 0
for(i in 1:length(k.Mobile)){
  page.ID[page.ID$Training == 0 & page.ID$Mobile == Mobile[i], ]$predict.Mobile <- ifelse(page.ID[page.ID$Training == 0 & page.ID$Mobile == Mobile[i], ]$risk > k.Mobile[i], 1, 0)
}

page.ID$predict.Mobile <- factor(page.ID$predict.Mobile, levels = c(0,1))
ConfMx.Mobile <- confusionMatrix(page.ID[page.ID$Training == 0, ]$predict.Mobile, page.ID[page.ID$Training == 0, ]$BreakoffQuestion, positive = "1")
ConfMx.Mobile

Conf.Mx.All3 <- as.matrix(ConfMx.Mobile$byClass[c(1, 2, 5)])
Conf.Mx.All3 <- rbind(Conf.Mx.All3, as.matrix(ConfMx.Mobile$overall[c(1,2)]))

AUC.Mobile

#For paper
ConfMx.Mobile.nonmob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$Mobile == 0, ]$predict.Mobile, 
                                        page.ID[page.ID$Training == 0 & page.ID$Mobile == 0, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.nonmob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.nonmob$overall[c(1,2)])

ConfMx.Mobile.mob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$Mobile == 1, ]$predict.Mobile, 
                                     page.ID[page.ID$Training == 0 & page.ID$Mobile == 1, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.mob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.mob$overall[c(1,2)])





# Threshold by student/facutly*Mobile/non mobile:
SampType <- unique(page.ID$SampType.Survey)
Mobile <- unique(page.ID$Mobile)
k.SampType.Mob <- cbind(SampType,Mobile)
cox.roc.train.SampType.Mob <- list()
AUC.SampType.Mob <- cbind(SampType,Mobile)

for(i in 1:length(SampType)){
  for(j in 1:length(Mobile)){
    cox.roc.train.SampType.Mob[[i]] <- roc(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i] & page.ID$Mobile == Mobile[j], ]$BreakoffQuestion ~
                                             page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i] & page.ID$Mobile == Mobile[j], ]$risk,
                                           data = page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i] & page.ID$Mobile == Mobile[j], ])
    k.SampType.Mob[i,j] <- coords(cox.roc.train.SampType.Mob[[i]], "best", ret = "threshold", best.method="youden")
    AUC.SampType.Mob[i,j] <- as.numeric(cox.roc.train.SampType.Mob[[i]]$auc)
  }
}


page.ID$predict.SampType.Mob <- 0
for(i in 1:2){
  for(j in 1:2){
    page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i] & page.ID$Mobile == Mobile[j], ]$predict.SampType.Mob <- 
      ifelse(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == SampType[i] & page.ID$Mobile == Mobile[j], ]$risk > k.SampType.Mob[i,j], 1, 0)
  }
}

page.ID$predict.SampType.Mob <- factor(page.ID$predict.SampType.Mob, levels = c(0,1))
ConfMx.SampType.Mob <- confusionMatrix(page.ID[page.ID$Training == 0, ]$predict.SampType.Mob, page.ID[page.ID$Training == 0, ]$BreakoffQuestion, positive = "1")
ConfMx.SampType.Mob

Conf.Mx.All4 <- as.matrix(ConfMx.SampType.Mob$byClass[c(1, 2, 5)])
Conf.Mx.All4 <- rbind(Conf.Mx.All4, as.matrix(ConfMx.SampType.Mob$overall[c(1,2)]))

# for paper
AUC.SampType.Mob

ConfMx.Mobile.Fac.nonmob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0 & page.ID$Mobile == 0, ]$predict.Mobile, 
                                            page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0 & page.ID$Mobile == 0, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.Fac.nonmob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.Fac.nonmob$overall[c(1,2)])

ConfMx.Mobile.Fac.mob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0 & page.ID$Mobile == 1, ]$predict.Mobile, 
                                         page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 0 & page.ID$Mobile == 1, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.Fac.mob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.Fac.mob$overall[c(1,2)])

ConfMx.Mobile.Stu.nonmob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1 & page.ID$Mobile == 0, ]$predict.Mobile, 
                                            page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1 & page.ID$Mobile == 0, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.Stu.nonmob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.Stu.nonmob$overall[c(1,2)])

ConfMx.Mobile.Stu.mob <- confusionMatrix(page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1 & page.ID$Mobile == 1, ]$predict.Mobile, 
                                         page.ID[page.ID$Training == 0 & page.ID$SampType.Survey == 1 & page.ID$Mobile == 1, ]$BreakoffQuestion, positive = "1")
as.matrix(ConfMx.Mobile.Stu.mob$byClass[c(1, 2, 5)])
as.matrix(ConfMx.Mobile.Stu.mob$overall[c(1,2)])


# Threshold by topic
TopicSection <- unique(page.ID$TopicSection)
k.TopicSection <- c(rep(0, length(TopicSection)))
cox.roc.train.TopicSection <- list()
AUC.TopicSection <- length(k.TopicSection)

for(i in 1:length(k.TopicSection)){
  cox.roc.train.TopicSection[[i]] <- roc(page.ID[page.ID$Training == 0 & page.ID$TopicSection == TopicSection[i], ]$BreakoffQuestion ~
                                           page.ID[page.ID$Training == 0 & page.ID$TopicSection == TopicSection[i], ]$risk,
                                         data = page.ID[page.ID$Training == 0 & page.ID$TopicSection == TopicSection[i], ])
  k.TopicSection[i] <- coords(cox.roc.train.TopicSection[[i]], "best", ret = "threshold", best.method="youden")
  AUC.TopicSection[i] <- as.numeric(cox.roc.train.TopicSection[[i]]$auc)
}

page.ID$predict.TopicSection <- 0
for(i in 1:length(k.TopicSection)){
  page.ID[page.ID$Training == 0 & page.ID$TopicSection == TopicSection[i], ]$predict.TopicSection <- ifelse(page.ID[page.ID$Training == 0 & page.ID$TopicSection == TopicSection[i], ]$risk > k.TopicSection[i], 1, 0)
}

page.ID$predict.TopicSection <- factor(page.ID$predict.TopicSection, levels = c(0,1))
ConfMx.TopicSection <- confusionMatrix(page.ID[page.ID$Training == 0, ]$predict.TopicSection, page.ID[page.ID$Training == 0, ]$BreakoffQuestion, positive = "1")
ConfMx.TopicSection

Conf.Mx.All5 <- as.matrix(ConfMx.TopicSection$byClass[c(1, 2, 5)])
Conf.Mx.All5 <- rbind(Conf.Mx.All5, as.matrix(ConfMx.TopicSection$overall[c(1,2)]))




