# Author: Tony Huiquan Zhang
# Date: 2018-08-28
# Paper Title: "The Rise of the Princelings in China: Career Advantages and Collective Elite Reproduction"
# Journal Title: Journal of East Asian Studies
# Publication Date: TBD
# the following code is fully functional under Windows 10 64 bit
# with R version 3.4.3 (2017-11-30)
# Please email tonyhuiquanzhang@gmail.com if you have questions regarding the present project.
# Please kindly cite the current paper properly if you use the data/codes provided by the author.

##### 001: Load libraries and fonts #####
library(xlsx)
library(readxl)
library(data.table)
library(scales)
library(ggplot2)
library(ggthemes)
library(foreign)
library(qdap)
library(dplyr)
library(car)
library(effects)
library(lme4)
library(predictmeans)
library(pscl) 
library(timeDate)
library(lubridate)
library(texreg)

# adding windows fonts #
windowsFonts(Arial=windowsFont("TT Arial"))
windowsFonts(Century=windowsFont("TT Century"))

##### 002: Read into Data #####
Prince = xlsx::read.xlsx("YOUR_FILE_PATH/CPD_reproducible_20180827.xlsx", 
                         1, encoding="UTF-8", header = T)
TOTAL  = xlsx::read.xlsx("YOUR_FILE_PATH/CC_FM_AM_LIST_1945_2012.xlsx",
                         1, encoding="UTF-8", header = T)

# prepare TOTAL (the Central Committee Member List)
TOTAL$head = 1
TOTAL$BirthYear = as.numeric(as.character(TOTAL$BirthYear))
TOTAL$AgeEnter  = as.numeric(as.character(TOTAL$AgeEnter))
TOTAL$Category  = as.character(TOTAL$Category)
TOTAL$NAME      = as.character(TOTAL$NAME)
TOTAL$Meeting   = as.numeric(as.character(TOTAL$Meeting))
TOTAL$Meeting2  = floor(TOTAL$Meeting)

# prepare Prince (the Tai Zi Dang List)
sapply(Prince, class)
Prince$K_Birthyear = as.numeric(as.character(Prince$K_Birthyear))
Prince$K_Deathyear = as.numeric(as.character(Prince$K_Deathyear))
Prince$K_Rankyear  = as.numeric(as.character(Prince$K_Rankyear))
Prince$F_Birthyear = as.numeric(as.character(Prince$F_Birthyear))
Prince$F_Deathyear = as.numeric(as.character(Prince$F_Deathyear))
Prince$F_Rankyear  = as.numeric(as.character(Prince$F_Rankyear))
Prince$K_Name = as.character(Prince$K_Name)
Prince$F_Name = as.character(Prince$F_Name)

##### 003: Chinese Princeling Dataset (Prince) descriptives #####
# These are codes reproducing results for Table 1. 

# GENDER #
dist_tab(Prince$Sex)

#$dataframe
#interval freq cum.freq percent cum.percent
#1       男  233      233   79.52       79.52
#2       女   60      293   20.48      100.00

# birth year #
mean(Prince$K_Birthyear, na.rm=T) # [1] 1944.901
sd(Prince$K_Birthyear, na.rm=T)   # [1] 10.46843

# children's ranks #
dist_tab(Prince$K_Rank)
# interval freq cum.freq percent cum.percent
# 1        1   23       23    7.85        7.85 # vice pre
# 2        2  120      143   40.96       48.81 # pre
# 3        3   85      228   29.01       77.82 # vice prov
# 4        4   37      265   12.63       90.44 # prov
# 5        5   22      287    7.51       97.95 # vice national
# 6        6    6      293    2.05      100.00 # national

# parent/child relation #
Prince$F_Relation = trimws(as.character(Prince$F_Relation))
Prince$F_Relation = factor(Prince$F_Relation)
Prince$F_Relation2 = ifelse(Prince$F_Relation %in% c("父", "母"), "1.Parents", "3.Other")
Prince$F_Relation2 = ifelse(Prince$F_Relation %in% c("岳父", "家翁/公公", 
                                                     "叔", "姑父", "姨夫"), 
                            "2.Parent-in-law/Uncle/Aunt", 
                            Prince$F_Relation2)
#
Prince$F_Relation2 = factor(Prince$F_Relation2)
dist_tab(Prince$F_Relation2)
# > dist_tab(Prince$F_Relation2)
# interval freq cum.freq percent cum.percent
# 1                  1.Parents  219      219   74.74       74.74
# 2 2.Parent-in-law/Uncle/Aunt   51      270   17.41       92.15
# 3                    3.Other   23      293    7.85      100.00


# parental birth year #
mean(Prince$F_Birthyear)
sd(Prince$F_Birthyear)
# > mean(Prince$F_Birthyear)
# [1] 1908.87
# > sd(Prince$F_Birthyear)
# [1] 12.9259

# death year
mean(Prince$F_Deathyear, na.rm=T)
sd(Prince$F_Deathyear, na.rm=T)
# > mean(Prince$F_Deathyear, na.rm=T)
# [1] 1985.5
# > sd(Prince$F_Deathyear, na.rm=T)
# [1] 17.26837

#
dist_tab(Prince$F_Rank)

##### 004: data cleaning: Prince #####

#

# Sex Recoding #
dist_tab(Prince$Sex)
Prince$Female = 1
Prince = within(Prince, {Female [Sex=="男"] = 0})
dist_tab(Prince$Female)

# calculate Rank Age #
Prince$K_RankAge = Prince$K_Rankyear - Prince$K_Birthyear
dist_tab(Prince$K_RankAge)
Prince$F_RankAge = Prince$F_Rankyear - Prince$F_Birthyear
dist_tab(Prince$F_RankAge)
dist_tab(Prince$F_Rank)
dist_tab(Prince$K_Rank)

#
mean(Prince$K_RankAge[Prince$K_Rank==1], na.rm=T)
mean(Prince$K_RankAge[Prince$K_Rank==2], na.rm=T)
mean(Prince$K_RankAge[Prince$K_Rank==3], na.rm=T)
mean(Prince$K_RankAge[Prince$K_Rank==4], na.rm=T)
mean(Prince$K_RankAge[Prince$K_Rank==5], na.rm=T)
mean(Prince$K_RankAge[Prince$K_Rank==6], na.rm=T) 

# how father's life overlaps with kids career.
Prince$F_Deathyear1 = ifelse(is.na(Prince$F_Deathyear)==F, Prince$F_Deathyear, 2012)
Prince$Overlap = Prince$F_Deathyear1 - Prince$K_Birthyear
dist_tab(Prince$Overlap)

# outputting the subsetted variables: Cleaned CPD data ready for analyses #
myvars = c("CaseID", "Sex", "F_Relation2",
           "K_Name", "K_Rank", "K_Birthyear", "K_Deathyear",
           "K_Rankyear", "Female", "K_military", "K_RankAge",
           "F_RankAge",
           "F_Name", "F_Rank", "F_Birthyear", "F_Deathyear",
           "F_Rankyear", "Overlap")
Prince = Prince[,myvars]

##### 005: Knowing Central Committee Better #####

# a user-defined function cleaning spaces/brackets
qingli = function(d)
{d = gsub("[:space:]", "[:blank:]", d, fixed = TRUE)
d = gsub(" ", "", d, fixed = TRUE) 
d = gsub("　", "", d, fixed = TRUE)
d = gsub(",", "", d, fixed = TRUE)
d = gsub("（", "(", d, fixed = TRUE)
d = gsub("）", ")", d, fixed = TRUE)}
TOTAL$NAME1 = qingli(TOTAL$NAME)
TOTAL$NAME1 = genX(TOTAL$NAME1, "(", ")")

#
dist_tab(TOTAL$NAME1) # Central committee 2060 NAMES
length(unique(TOTAL$NAME1))


TZD_total = intersect(c(Prince$K_Name), c(TOTAL$NAME1)) 
TZD_total # 55 names - however Yang Junsheng is a different individual; 
# Liao Chengzhi is himself a 1st generation leader to be excluded.
TOTAL$TZD = ifelse(TOTAL$NAME1 %in% TZD_total, 1,0) #
toexclude = c("杨俊生", "廖承志") # 
TOTAL$TZD = ifelse(TOTAL$NAME1 %in% toexclude, 0, TOTAL$TZD) #
rm(TZD_total)
rm(toexclude)
rm(qingli)
rm(myvars)

# Calculating the number of Tai Zi Dang in Central Committee
dist_tab(TOTAL$TZD)  # 157 out of 4525 entries
length(unique(TOTAL$NAME1))
length(unique(TOTAL$NAME1 [TOTAL$TZD ==1])) # 53 out of 2060 names


# FIGURE 3, 4, 5 are all based on 50 princelings#
# with Li Baohua, Tang Wensheng, Huang Zhizhen removed #
# Table 2 contains 75 counts, different from 50 #
# because it is based on people-entrance 人次   #

# HOW MANY TIMES TZD HAS SERVED in CENTRAL COMMITTEE?
dist_tab(TOTAL$NAME1  [TOTAL$TZD ==1 & 
                       TOTAL$Category %in% 
                       c("3.中央委员", "4.中央候补委员")])
# interval freq cum.freq percent cum.percent
# 1        1   12       12   22.64       22.64
# 2        2   20       32   37.74       60.38
# 3        3    9       41   16.98       77.36
# 4        4    9       50   16.98       94.34
# 5        5    3       53    5.66      100.00
# 53 for now, 
# "李保华、黄知真、唐闻生" will be  removed later since they are pre-11 CCP NC

# clean the environment #
rm(t1)

##### 006: TABLE 2: T-TEST of Age First Entering CC ##### 

# Count the first occurrence - AGE FIRST ENTER #
sapply(TOTAL, class)
TOTAL           = TOTAL[order(TOTAL$TotalOrder),]
TOTAL$NAME      = as.character(TOTAL$NAME)
TOTAL$BirthYear = as.numeric(as.character(TOTAL$BirthYear))
TOTAL$AgeEnter  = as.numeric(as.character(TOTAL$AgeEnter))
TOTAL$YearEnter = as.numeric(as.character(TOTAL$YearEnter))
TOTAL$Meeting   = as.numeric(as.character(TOTAL$Meeting))
TOTAL$Meeting2  = floor(TOTAL$Meeting)
TOTAL$Order     = as.numeric(as.character(TOTAL$Order))
TOTAL$TotalOrder= as.numeric(as.character(TOTAL$TotalOrder))

#
TOTAL1 = TOTAL[TOTAL$Category=="1.政治局委员",]
TOTAL2 = TOTAL[TOTAL$Category=="2.政治局候补委员",]
TOTAL6 = TOTAL[TOTAL$Category=="1.政治局委员"|TOTAL$Category=="2.政治局候补委员",]

#
TOTAL3 = TOTAL[TOTAL$Category=="3.中央委员",]
TOTAL4 = TOTAL[TOTAL$Category=="4.中央候补委员",]
TOTAL5 = TOTAL[TOTAL$Category=="3.中央委员"|TOTAL$Category=="4.中央候补委员",]

# 初次进入中央委员的年龄。需要排除前十次大会已经有的人。
# Excluding 1973 or before;
# Retaining only 1977 or later.
NAME_pre10 = c(TOTAL$NAME1 [TOTAL$Meeting<=10]) # 1512
NAME_pre10 = unique(NAME_pre10) # 779 names
NAME_pre10

# Table 2, Row 1
# 初次进入中央候补委员的年龄 ZHONG YANG HOUBU WEIYUAN
# age of first entering CC alternative member
TOTAL4 = TOTAL4 [with(TOTAL4, order(Order)), ] 
TOTAL4.first = TOTAL4 [!duplicated(TOTAL4$NAME1), ]
TOTAL4.first = TOTAL4.first [!TOTAL4.first$NAME1 %in% NAME_pre10,]
TOTAL4.first = TOTAL4.first [TOTAL4.first$Meeting>=11,] # 12 and after. this is the best.
a = mean(TOTAL4.first$AgeEnter [TOTAL4.first$TZD==0]) # 51.85375
b = mean(TOTAL4.first$AgeEnter [TOTAL4.first$TZD==1]) # 51.375
# > a
# [1] 51.85375
# > b
# [1] 51.375
#> a - b
# [1] 0.47875

#
length(TOTAL4.first$AgeEnter [TOTAL4.first$TZD==0]) # 800
length(TOTAL4.first$AgeEnter [TOTAL4.first$TZD==1]) # 24
t.test(TOTAL4.first$AgeEnter [TOTAL4.first$TZD==0],
       TOTAL4.first$AgeEnter [TOTAL4.first$TZD==1],
       alternative = c("greater"), mu = 0, conf.level = 0.95) 
# t = 0.44054, df = 24.655, p-value = 0.3317

# Table 2, Row 2
# CC Full Members
TOTAL3 = TOTAL3 [with(TOTAL3, order(Order)), ] 
TOTAL3.first = TOTAL3 [!duplicated(TOTAL3$NAME1), ]
TOTAL3.first = TOTAL3.first [!TOTAL3.first$NAME1 %in% NAME_pre10,]
TOTAL3.first = TOTAL3.first [TOTAL3.first$Meeting>=11,] # 12 and after. this is the best.
a = mean(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==0], na.rm=T) # 57.23
b = mean(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==1], na.rm=T) # 56.34
# > a
# [1] 57.24079
# > b
# [1] 56.275
# > a - b
# [1] 0.9657932
# > length(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==0]) # 726
# [1] 706
# > length(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==1]) # 41
# [1] 40
# length(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==0]) # 726
# length(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==1]) # 41
t.test(TOTAL3.first$AgeEnter [TOTAL3.first$TZD==0],
       TOTAL3.first$AgeEnter [TOTAL3.first$TZD==1],
       alternative = c("greater"), 
       mu = 0, conf.level = 0.95) 
# t = 1.4218, df = 44.737, p-value = 0.08101

# Table 2, Row 3
# ZhengZhiJu, PolitBuro AM + FM - 
# age at first entering PB AM/FM
# 初次进入政治局、候补委员的年龄
TOTAL6 = TOTAL6 [with(TOTAL6, order(Order)), ] 
TOTAL6.first = TOTAL6 [!duplicated(TOTAL6$NAME1), ]
TOTAL6.first = TOTAL6.first [!TOTAL6.first$NAME1 %in% NAME_pre10,]
TOTAL6.first = TOTAL6.first [TOTAL6.first$Meeting>=11,] # 12 and after. this is the best.
a= mean(TOTAL6.first$AgeEnter  [TOTAL6.first$TZD==0]) #61.10465
b= mean(TOTAL6.first$AgeEnter  [TOTAL6.first$TZD==1]) #61.10465
a-b
length(TOTAL6.first$AgeEnter [TOTAL6.first$TZD==0])
length(TOTAL6.first$AgeEnter [TOTAL6.first$TZD==1])
t.test(TOTAL6.first$AgeEnter [TOTAL6.first$TZD==0],
       TOTAL6.first$AgeEnter [TOTAL6.first$TZD==1],
       alternative = c("greater"), mu = 0, conf.level = 0.95)  
# t = 1.3336, df = 16.589, p-value = 0.1002

##### 007: Generating table, excluding Zhong Gu Wei Wei Yuan #####
dist_tab(TOTAL$Category)
TOTAL$Category2 = as.numeric(factor(TOTAL$Category))
#
TOTAL1234 = TOTAL[TOTAL$Category!="5.中顾委委员",]
TOTAL1234 = TOTAL1234 [with(TOTAL1234, order(Order)), ] 
TOTAL1234 = TOTAL1234[TOTAL1234$Meeting2>7,]
head(TOTAL1234)
TOTAL1234 = TOTAL1234[!(TOTAL1234$NAME1 %in% NAME_pre10),]
head(TOTAL1234)

# TOTAL1234.first means all the central committee members,
# exlucding pre-1977 individuals
# only containing the very first entry.
TOTAL1234.first = TOTAL1234 [!duplicated(TOTAL1234$NAME1), ]
TOTAL1234.first = TOTAL1234.first [!TOTAL1234.first$NAME1 %in% NAME_pre10,]
head(TOTAL1234.first)
names(TOTAL1234.first)

xtabs(~TOTAL1234.first$Category+TOTAL1234.first$TZD) # 1281 names
# FIGURE 3 data #
# > xtabs(~TOTAL1234.first$Category+TOTAL1234.first$TZD) # 1281 names
# TOTAL1234.first$TZD
# TOTAL1234.first$Category   0   1
# 1.政治局委员      59  10
# 2.政治局候补委员   5   1
# 3.中央委员       401  22
# 4.中央候补委员   766  17

#

##### 008: Figure 4, counting times served in CC #####

## dcast: tranform data - long to wide shape ## 
df_persons = dcast(TOTAL1234, 
                   NAME1 + BirthYear + TZD ~ Meeting2, 
                   value.var = "Category2", 
                   fun.aggregate = mean)

# removing duplicated cases
df_persons = df_persons[!duplicated(df_persons$NAME1),] # from 1282 to 1281
toexclude = c("杨俊生", "廖承志")
df_persons$TZD = ifelse(df_persons$NAME1 %in% toexclude, 
                        0, df_persons$TZD) #
df_persons[is.nan(df_persons)] = NA
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] = round(df[,nums], digits = digits)
  (df) }
df_persons = round_df(df_persons, digits=1) # leave one digit for those tho promote during a meeting.
df_persons

##### 009: Plotting figure 3,4,5; Calculate Chi-Squares #####
# FIGURE 3 #
xtabs(~TOTAL1234.first$Category+TOTAL1234.first$TZD)
#> xtabs(~TOTAL1234.first$Category+TOTAL1234.first$TZD)
#TOTAL1234.first$TZD
#TOTAL1234.first$Category   0   1
#1.政治局委员      59  10
#2.政治局候补委员   5   1
#3.中央委员       401  22
#4.中央候补委员   766  17
x = matrix(c(766, 401, 64, 17, 22, 11), ncol = 2)
chisq.test(x)
x = data.table(x)
x = rename(x, c("V1"="Non-Princeling", "V2"="Princeling"))
x = setattr(x, "row.names", c("CC AM", "CC FM", "PB AM & FM"))
chisq.test(x)
datm = melt(cbind(x, ind = rownames(x)), id.vars = c('ind'))
datm1 = aggregate(datm$value, by=list(variable=datm$variable),
                  FUN=sum, na.rm=T)
datm = merge(datm, datm1, by="variable")
datm$ratio = datm$value/datm$x
datm$ratio1 = 100*round(datm$ratio, 4)
datm$variable = ifelse(datm$variable=="V1", "Non-Princelings", "Princelings")

# windows()
ggplot(datm, aes(x = variable, y = value, 
                fill = factor(ind, levels=c("PB AM & FM", "CC FM", "CC AM")))) +
  geom_bar(position = "fill", stat = "identity")+
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percent", x="Identity", fill="Rank")+
  geom_text(position = "fill", aes(label=paste0(ratio1, "%, ", value), 
                                   vjust=1.3), 
            size = 6, family="Century",  color = "white")+
  annotate("text", x = 1, y = .1, colour = "white", size = 6, 
           label = "Chi-Sq = 31.33, df = 2\np-value = 0.000",
           fontface="bold.italic", family="Century") +
  scale_fill_manual(values=c("grey21", 
                             "grey71", 
                             "grey41"))+ theme_bw()+
  theme(plot.title = element_text(lineheight=2, 
                                  hjust=.5, 
                                  vjust=1.5),
        title = element_text(size=rel(1.5), 
                             family="Century"), 
        axis.text.x = element_text(size=rel(1.7), 
                                   family="Century"),
        axis.text.y = element_text(size=rel(1.7), 
                                   family="Century"),
        legend.direction="vertical",
        legend.background = element_rect(fill="grey96", 
                                         size=.5, linetype="dotted"),
        legend.title = element_text(size=14, lineheight=1, hjust=.5, 
                                    vjust=1.5),
        legend.text = element_text(size=rel(1.2), family="Century"),
        legend.key = element_rect(size = 0.2, color = 'grey16'),
        legend.key.size = unit(1.3, "cm"))

## Figure 4 ##
Vars = c(seq(4,11,1))
df_persons$times = rowSums(!is.na(df_persons[,Vars]))
df_persons$times = apply(df_persons[,Vars], 
                         MARGIN = 1, 
                         FUN = function(x) length(x[!is.na(x)]))
dist_tab(df_persons$times)
df_persons$times2 = NA
df_persons$times2 = ifelse(df_persons$times> 4, 4, df_persons$times)
xtabs(~df_persons$times2+df_persons$TZD) 
chisq.test(xtabs(~df_persons$times2+df_persons$TZD))
x = matrix(c(604, 386, 160, 81, 12, 19, 8, 11), ncol = 2)
x = data.table(x)
x = rename(x, c("V1"="Non-Princeling", "V2"="Princeling"))
x = setattr(x, "row.names", c("1 Term", "2 Terms", "3 Terms", "4 Terms or More"))
datm = melt(cbind(x, ind = rownames(x)), id.vars = c('ind'))
datm1 = aggregate(datm$value, by=list(variable=datm$variable), FUN=sum, na.rm=T)
datm = merge(datm, datm1, by="variable")
datm$ratio = datm$value/datm$x
datm$ratio1 = 100*round(datm$ratio, 4)
datm$variable = ifelse(datm$variable=="V1", "Non-Princelings", "Princelings")

# windows()
ggplot(datm,aes(x = variable, y = value, 
                fill=factor(ind, levels=c("4 Terms or More", 
                                          "3 Terms",
                                          "2 Terms", "1 Term")))) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percent", x="Identity", fill="Number of Terms")+
  geom_text(position = "fill", 
            aes(label=paste0(ratio1, "%, ", value), vjust=1.3), 
            colour = "white", size = 6, family="Century")+
  annotate("text", x = 1, y = .1, 
           label = "Chi-Sq = 23.19, df = 3\np-value = 0.000", 
           colour = "white", size = 6, family="Century", 
           fontface="bold.italic") +
  scale_fill_manual(values=c("black", "grey21", "grey71", 
                             "grey41"))+
  theme_bw()+
  theme(plot.title = element_text(lineheight=2, 
                                  hjust=.5, 
                                  vjust=1.5),
        title = element_text(size=rel(1.5), 
                             family="Century"), 
        axis.text.x = element_text(size=rel(1.7), 
                                   family="Century"),
        axis.text.y = element_text(size=rel(1.7), 
                                   family="Century"),
        legend.direction="vertical",
        legend.background = element_rect(fill="grey96", 
                                         size=.5, linetype="dotted"),
        legend.title = element_text(size=14, lineheight=1, hjust=.5, 
                                    vjust=1.5),
        legend.text = element_text(size=rel(1.2), family="Century"),
        legend.key = element_rect(size = 0.2, color = 'grey16'),
        legend.key.size = unit(1.3, "cm"))

##### 010: FIGURE 5: chi-2 promotion #####
#
maxFun=function(x){max(na.omit(x))} 
df_persons$MAX = apply(df_persons[,Vars], 1, maxFun)
minFun=function(x){min(na.omit(x))} 
df_persons$MIN = apply(df_persons[,Vars], 1, minFun)

# people who got promoted: as defined by who has a change in status during CC history
df_persons$change = df_persons$MAX - df_persons$MIN
df_persons$change2 = ifelse(df_persons$change>0, 1, 0)
dist_tab(df_persons$change2)

#   Figure 5 promoted or not #
x = xtabs(~df_persons$change2+df_persons$TZD)
x = matrix(c(939, 292, 33, 17), ncol = 2)
chisq.test(x)
x = data.table(x)
x = rename(x, c("V1"="Non-Princeling", "V2"="Princeling"))
x = setattr(x, "row.names", c("Not Promoted", "Promoted"))
datm = melt(cbind(x, ind = rownames(x)), id.vars = c('ind'))
datm1 = aggregate(datm$value, by=list(variable=datm$variable),
                  FUN=sum, na.rm=T)
datm = merge(datm, datm1, by="variable")
datm$ratio = datm$value/datm$x
datm$ratio1 = 100*round(datm$ratio, 4)
datm$variable = ifelse(datm$variable=="V1", "Non-Princelings", "Princelings")
datm = datm[order(-ind),]

ggplot(datm,aes(x = variable, y = value, 
                fill=factor(ind, levels=c("Promoted",
                                          "Not Promoted")))) +
  geom_bar(position = "fill", stat = "identity")+ scale_y_continuous(labels = percent_format()) +
  labs(y = "Percent", x="Identity", fill="Promotion Status")+
  geom_text(position = "fill", 
            aes(label=paste0(ratio1, "%, ", value), vjust=1.3),
            colour = "white",  size = 6, family="Century")+
  annotate("text", x = 1, y = .1, 
           label = "Chi-Sq = 2.24, df = 1\np-value = 0.134", 
           colour = "white",  size =6, 
           fontface="bold.italic", family="Century") +
  scale_fill_manual(values=c("grey21", 
                             "grey51"))+ theme_bw()+
  theme(plot.title = element_text(lineheight=2, 
                                  hjust=.5, 
                                  vjust=1.5),
        title = element_text(size=rel(1.5), 
                             family="Century"), 
        axis.text.x = element_text(size=rel(1.7), 
                                   family="Century"),
        axis.text.y = element_text(size=rel(1.7), 
                                   family="Century"),
        legend.direction="vertical",
        legend.background = element_rect(fill="grey96", 
                                         size=.5, linetype="dotted"),
        legend.title = element_text(size=14, lineheight=1, hjust=.5, 
                                    vjust=1.5),
        legend.text = element_text(size=rel(1.2), family="Century"),
        legend.key = element_rect(size = 0.2, color = 'grey16'),
        legend.key.size = unit(1.3, "cm"))
 
##### 011: FIGURE 2 GGPLOT #####
# Figure 2 #
TOTAL5$Meeting = as.integer(floor(as.numeric(TOTAL5$Meeting)))
ggplot(TOTAL5, aes(as.factor(Meeting), fill=factor(TZD))) + 
  geom_bar(position='identity') + #position: stack, dodge, identity
  scale_x_discrete(breaks=seq(1, 18, 1))+
  scale_y_continuous(breaks=seq(0, 400, 50))+
  scale_fill_manual(values = c("dark grey", "black"),
                    labels=c("None-Princeling", "Princeling"))  +
  labs(y='Number of CC FM&AM Members', 
       x='The Number of CCP National Congress') +
  theme_bw()+
  theme(legend.position=c(0.15, 0.88),
        plot.title = element_text(lineheight=2, 
                                  hjust=.5, 
                                  vjust=1.5),
        title = element_text(size=rel(1.5), 
                             family="Century"), 
        axis.text.x = element_text(size=rel(1.7), 
                                   family="Century"),
        axis.text.y = element_text(size=rel(1.7), 
                                   family="Century"),
        legend.direction="vertical",
        legend.background = element_rect(fill="grey96", 
                                         size=.5, linetype="dotted"),
        legend.title=element_blank(), 
        legend.text = element_text(size=rel(1.2), family="Century"),
        legend.key = element_rect(size = 0.1, color = 'white'),
        legend.key.size = unit(1.3, "cm"))

##### 012: Average Rank_Age by Parental Rank (Figure 6) #####
rm(list=setdiff(ls(), "Prince"))
dist_tab(Prince$K_Rank)
dist_tab(Prince$F_Rank)
Prince$K_RankAge = Prince$K_Rankyear - Prince$K_Birthyear
Prince1 = subset(Prince, select=c(CaseID, Sex, 
                                  K_Rank, K_RankAge, K_Rankyear, 
                                  F_Rank, F_Rankyear))
Prince1 = within(Prince1, {
  K_Rank [K_Rank=="1"] = "2"
  K_Rank [K_Rank=="6"] = "5"
  F_Rank [F_Rank=="1"] = NA
  F_Rank [F_Rank=="6"] = "5"
})
Prince1 = na.exclude(Prince1)

library(dplyr)
df.summary2 = Prince1 %>% 
  group_by(as.character(F_Rank), as.character(K_Rank)) %>%
  summarise(sd = sd(K_RankAge, na.rm=T),
    mean = mean(K_RankAge, na.rm=T))

df.summary2 = data.frame(df.summary2)
df.summary2
names(df.summary2) = c("F_Rank", "K_Rank", "sd", "mean")
ggplot(df.summary2,aes(x = K_Rank, y = mean, ymin = mean-sd, ymax = mean+sd))  +
  geom_point(size = 2.5) + geom_linerange(size =0.5) +
  geom_hline(aes(yintercept = 1)) +  facet_wrap(~F_Rank)
# 
# 
windows()
ggplot(df.summary2, aes(x = K_Rank, y = mean,
                        shape = factor(F_Rank),
                        color = factor(F_Rank),
                        linetype = factor(F_Rank))) +
  geom_pointrange(aes(ymin = mean-sd, ymax = mean+sd),
    position = position_dodge(0.33),
    size=0.8)+coord_flip()+
  guides(size = "none", colour = "none",
         linetype = guide_legend(override.aes = list(size=rel(1))),
         shape = guide_legend(override.aes = list(size=rel(1))))+
  theme_bw()+
  scale_color_manual(labels = c("Vice-Provincial", "Provincial",
                                "Vice-National", "National"),
    values = c("grey31", "grey61", "grey51", "grey41"), name = "Parental Rank")+
  scale_linetype_manual(labels = c("Vice-Provincial", "Provincial",
                                "Vice-National", "National"),
                     values = c(11, 12, 13, 14), name = "Parental Rank")+
  scale_shape_manual(labels = c("Vice-Provincial", "Provincial",
                              "Vice-National", "National"),
                   values = c(1, 2, 3, 4), name = "Parental Rank")+
  scale_x_discrete(labels= c("Prefectural", "Vice-Provincial", "Provincial",
                             "Vice-National & National"))+
  # ggtitle("Parental Rank and Children's Entry Age at Each Level") + 
  labs(y='Entry Age', x='Level of Chilren') +
  theme(plot.title = element_text(lineheight=2, hjust=.5, vjust=1.5),
        title = element_text(size=rel(1.1), family="Century"), # controlling the title, xylabs
        axis.text.x = element_text(size=rel(1.3), 
                                   family="Century"),
        axis.text.y = element_text(size=rel(1.3), 
                                   family="Century"),
        legend.direction="vertical",
        legend.background = element_rect(fill="grey86", 
                                         size=.5, linetype="dotted"),
        legend.key = element_rect(size = 0.1, color = 'white'),
        legend.title = element_text(size=12, lineheight=1, hjust=.5, vjust=1.5),
        legend.text = element_text(size=rel(1.1), family="Century"),
        legend.position=c(0.82, 0.28),
        legend.key.size = unit(1, "cm"))

# Figure 6 completed #
rm(df.summary2)
rm(Prince1)

##### 013: POLR models and TABLE 3 #####
Prince1 = subset(Prince, select = c(K_Name, Female, K_military,  
                                    K_Rank, K_Birthyear, K_Rankyear,
                                    K_RankAge, F_Name, F_Relation2,
                                    Overlap, F_Rank, F_Birthyear))
Prince2 = Prince1 [is.na(Prince1$K_RankAge)==F,]

#
Prince2 = within(Prince2, {
  K_Rank [K_Rank=="1"] = "2"
  K_Rank [K_Rank=="6"] = "5"
  F_Rank [F_Rank=="1"] = NA
  F_Rank [F_Rank=="6"] = "5"
})
dist_tab(Prince2$F_Rank)
dist_tab(Prince2$K_Rank)

#
Prince2 = Prince2 [!is.na(Prince2$F_Rank),]
Prince3 = Prince2 [is.na(Prince2$K_RankAge)==F & 
                     Prince2$K_Birthyear < 1950 & 
                     Prince2$K_Birthyear >1930,] # only those who retired.
Prince3 = na.exclude(Prince3) 
dist_tab(Prince3$F_Rank)
dist_tab(Prince3$K_Rank)
sapply(Prince3, class)
Prince3$Female = as.factor(Prince3$Female)
Prince3$F_Relation2 = as.factor(Prince3$F_Relation2)
Prince3$K_Rank  = as.factor(as.character(Prince3$K_Rank))
Prince3$F_Rank  = as.factor(as.character(Prince3$F_Rank))
Prince3$K_Rank  = as.character(Prince3$K_Rank)
Prince3$F_Rank  = as.character(Prince3$F_Rank)
dist_tab(Prince3$F_Relation2)
dist_tab(Prince3$F_Rank)
dist_tab(Prince3$K_Rank)
Prince3 = within(Prince3, {
  F_Rank [F_Rank=="5"] = "1.National"
  F_Rank [F_Rank=="4"] = "2.Vice-National"
  F_Rank [F_Rank=="3"] = "3.Provincial"
  F_Rank [F_Rank=="2"] = "4.Vice-Provincial"
  K_Rank [K_Rank=="5"] = "4.National"
  K_Rank [K_Rank=="4"] = "3.Vice-National"
  K_Rank [K_Rank=="3"] = "2.Provincial"
  K_Rank [K_Rank=="2"] = "1.Vice-Provincial"
})
Prince3$K_Rank  = as.factor(as.character(Prince3$K_Rank))
Prince3$F_Rank  = as.factor(as.character(Prince3$F_Rank))

# these are necessary, because original factor contain non-NA blanks.
# warning due to blank: design appears 
# to be rank-deficient, so dropping some coefs
library(MASS) # polr function is in MASS
model1 = polr(K_Rank~Female+K_military+F_Relation2, data=Prince3, Hess=T)
model2 = polr(K_Rank~Female+K_military+F_Relation2+F_Rank, data=Prince3, Hess=T)
model3 = polr(K_Rank~Female+K_military+F_Relation2+F_Rank+Overlap, data=Prince3, Hess=T)
summary(model3)
exp(coef(model3))
plot(allEffects(model3))
plot(Effect(c("F_Rank"), model3), multiline=T)
plot(Effect(c("Overlap"), model3), multiline=T)
htmlreg(list(model1, model2, model3), file = "D:/POLR-prince.html")

##### 014: ggplo2 effect plot (FIGURE 7) #####
eff1 = effect('F_Rank', mod=model3,confidence.level = 0.95)
eff2 = as.data.frame(eff1)
eff2
head(eff2)
library(tidyr)
x  = gather(eff2, K_level,  prob, prob.X1.Vice.Provincial:prob.X4.National, factor_key=T)
x2 = gather(eff2, K_level, probl, L.prob.X1.Vice.Provincial:L.prob.X4.National, factor_key=T)
x3 = gather(eff2, K_level, probu, U.prob.X1.Vice.Provincial:U.prob.X4.National, factor_key=T)
head(x)
head(x2)
head(x3)
x  = subset(x,  select=c(F_Rank, K_level, prob))
x2 = subset(x2, select=c(probl))
x3 = subset(x3, select=c(probu))
x4 = cbind(x, x2, x3)
x4
# windows()
ggplot(x4, aes(factor(F_Rank), prob, color=factor(K_level), 
              shape=factor(K_level), 
              group=factor(K_level))) + 
  labs(y='Probability of Achievement', 
       x='Parental Rank')+
  # ggtitle("Parental Effects on Children's Achieved Highest Ranks") +
  geom_errorbar(aes(ymin=probl, ymax=probu), width=0.08)+
  geom_line(aes(size=0.03, linetype=factor(K_level))) +
  geom_point(aes(size =1))+ 
  scale_color_manual(values = c("grey1","grey45" , "grey11", "grey21"),
                     name="Princeling's Highest \nAchieved Rank ",
                     labels=c("Vice-Provincial", "Provincial", "Vice-National", "National"))  +
  scale_shape_manual(values = c(12, 17, 16, 15),
                     name="Princeling's Highest \nAchieved Rank ",
                     labels=c("Vice-Provincial", "Provincial", "Vice-National", "National"))  +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"),
                        name="Princeling's Highest \nAchieved Rank ",
                        labels=c("Vice-Provincial", "Provincial", "Vice-National", "National")) +
  guides(size = "none", colour = "none",
         linetype = guide_legend(override.aes = list(size=rel(3))),
         shape = guide_legend(override.aes = list(size=rel(1))))+
  theme_bw()+
  theme(plot.title = element_text(lineheight=2, hjust=.5, vjust=1.5),
        title = element_text(size=rel(1.5), family="Century"), # controlling the title, xylabs
        axis.text.x = element_text(size=rel(1.5), family="Century"),
        axis.text.y = element_text(size=rel(1.5), family="Century"),
        legend.direction="vertical",
        legend.title = element_text(size=14, lineheight=1, hjust=.5, vjust=1.5),
        legend.text = element_text(size=rel(1.2), family="Century"),
        legend.key = element_rect(size = 0.2, color = 'grey66'),
        legend.key.size = unit(1.4, "cm"))

##### 015: Qualitative Sources Attached. #####

# 部分参考书目 #
# 1	陈昊苏、陈小鲁、陈丹淮、陈珊珊，2001，《回忆父亲陈毅——纪念陈毅同志百年诞辰》，北京：华夏出版社。
# 2	陈小津，2009，《我的"文革"岁月》，北京：中央文献出版社。
# 3	邓榕，2000，《我的父亲邓小平：“文革”岁月》，北京：中央文献出版社。
# 4	杜魏华、王宜秋，2000，《在苏联长大的红色后代》，北京：世界知识出版社。
# 5	罗点点（罗峪平），1999，《红色家族档案：罗瑞卿女儿的点点记忆》，海口：南海出版公司。
# 6	滕叙兖，2010，《开国元勋的子女们：哈军工高干子女传记》，广州：广东人民出版社。
# 7	肖伟俐，2007，《帅府家风》，北京：中共党史出版社。
# 8	张胜，2008，《从战争中走来：两代军人的对话》，北京：中国青年出版社。（张爱萍将军之子）
# 9	阎明复，2015，《阎明复回忆录》，北京：人民出版社。
# 10	顾宝孜，2012，《周恩来和他的孩子们》， 南京：江苏人民出版社。
# 11	张春桥，2015，《张春桥狱中家书》，香港：香港中文大学出版社。 
# 12	陈云家风编辑组，2005，《陈云家风: 于若木及陈云子女访谈录》，北京：新华出版社。
# 13	朱敏、顾保孜，1996，《我的父亲朱德》，沈阳：辽宁人民出版社。
# 14	李星华，1981，《回忆我的父亲李大钊》，上海：上海文艺出版社。
# 15	李敏、王桂苡，2000，《我的父亲毛泽东》，沈阳：辽宁人民出版社。
# 16	罗东进、黄瑶，2003，《我的父亲罗荣桓》，沈阳：辽宁人民出版社。
# 17	郭庶英，2004，《我的父亲郭沫若》，沈阳：辽宁人民出版社。
# 18	林豆豆、舒雲，2012，《林豆豆口述: 林彪女兒"九一三"事件見證人》，香港：明鏡出版社。
# 19	“胡乔木传”编写组，2015，《胡乔木传》，北京：人民出版社。
# 20	宋绍明，2011，《王震传奇》，北京：长征出版社。
# 21	徐开福，1995，《许世友的晚年岁月》，南京：江苏人民出版社。
# 22	黄峥，2006，《王光美访谈录》，北京：中央文献出版社。
# 23	牛小梅，2012，《我的父亲牛书申》，北京：中央文献出版社。
# 24	宋任穷，1996，《宋任穷回忆录》，北京：解放军出版社。
# 25	中共中央党史研究室，2009，《宋任穷纪念文集》，北京：中共党史出版社。
# 26	李一氓纪念文集 Front Cover 中华书局. 编辑部 中华书局, 2002
# 27	张霖之纪念文集 Front Cover 申云浦, 许法 煤炭工业出版社, 1992
# 28	黄克诚纪念文集 Front Cover 黄克诚纪念文集编委会 湖南人民出版社, 2002
# 29	紫陽千古: 趙紫陽紀念文集. 續編 Front Cover 趙紫陽，吳國光，張偉國，鮑樸 太平洋世紀出版社, 2006
# 30	《回忆黄知真》中央文献出版社，2001年出版。
# 31	‘怀念任仲夷’编委会，2007，《怀念任仲夷》，广州：广东人民出版社。
# 32	我的爷爷任弼时  任继宁 2005#


## 分类方法	
# 定性文本资料按照以下大类进行分类、编码。	
# A.	人力资本 / 教育和工作机会。包含幼儿园入托、小学、初中、高中、大学、出国留学等教育机会的优势；包含专业方向选择、“分配”、“回城”、初入职场、及其后的升迁机会等。
# B.	社会资本 / 关系网。包含与其他中共高层领袖人物的交往；家庭之间的交往；姻亲关系（极重要！）。
# C.	文化资本 / 惯习。包含对高层政治生活、相关规则知识、政治斗争方式方法，高层派系生态等的掌握。包括高干子弟具备、而平民所不具备的培养环境，例如藏书、电影（特别是所谓特供文化产品、禁书、外国译制片等）、旅行、出国留学、与精英文人的交往等。
# D.	政治运动 / 共同生命历程、人生经历及其后续影响。
# E.	其他暂时无法清晰归入上述类别的、基于集中供给制、高干身份的优势；例如居住条件、营养条件、医疗条件，等等。

## 文段编码方法	
# 材料大类 - 书号 – 材料编号（仅作区分用、按入选本库总先后顺序） – （可能应用的其他分类标签）。例如：A - 2 – 001 – B （最后一位是可选代码）	
# 代表 {“人力资本” – 《陈小津回忆录》 – 总第001号材料 – 同时可以支持“社会资本”论述}	

## 核心定性资料（候选引文）	
# 【引用 & 已翻译】A-2-010-BC  “陈丕显论接班人”	
# 平常像哪家的孩子不听话、耍点儿小脾气这类小事，我们的父辈们都格外关心，经常相互“补台”。……在上海老一辈的子女中，我父亲幽默风趣，和蔼可亲，喜欢与晚辈们聊个天，开开玩笑，大院里的孩子们都愿意亲近他。1965年，毛泽东发表了关于培养革命事业接班人问题的重要讲话，号召全党都要关心培养青年一代，但主要内容当时尚未对社会公布。我父亲想通过与部分子部子弟座谈的形式，切实关心一下身边孩子们的成长情况，于是他在百忙之中挤出时间，于1966年1月27日召集康平路大院里的部分大学生、中学生座谈。……在座谈中，我父亲绘声绘色地讲了他小时候当儿童团长、站岗、放哨、慰劳红军、拾牛粪等革命经历，亲切地勉励孩子们要听毛主席的话，“好好学习，天天向上”，“身体好，学习好、工作好”，要接革命的班。他深情地说：“将来上海市委第一书记不是我，上海市市长也不是荻秋同志，总是你们。不是指你们哪个人，是指你们这一代……要有做接班人的思想准备。”弹指一挥间，30多年过去了，昔日曾唧唧喳喳地聚集在上海康平路小礼堂聆听我父亲讲话的这一代人，现已纷纷进人中年，在各行各业接了革命的班，其中真的出现了一批省部级高级领导干部。	
# “My father was hilarious and congenial, and all the kids living in the courtyard enjoyed being around him. In 1965, Mao gave an important speech about ‘cultivate the revolutionary successors’, calling the entire CCP to care and educate the youth – but the key points of this speech have not been released to the public yet. My father planned to meet with some cadres’ children, to care about their growth. Therefore, he managed to spare some time from his heavy workloads. On Jan 27th, 1966, he invited the university and middle school students in the Kangping Rd Courtyard to meet with him. In the meeting, my father (…) warmly encouraged the children to follow Mao’s words such as, ‘learn for better every day,' ‘exercise well, learn well and work well,' and become successors of the revolution. He said, ‘in the future, the First Secretary of Shanghai CCP committee won’t be me, the mayor won’t be Comrade Cao Diqiu; it will be you. Not any specific person, it is your generation. You need to be ready for that.’ Time flies. After more than 30 years, the group of children which once sat in the Kangping Rd Hall and heard my father had reached their middle age. They did become the successors in various occupations and positions, among which there are a group of provincial-level senior cadres.”	
# A-5-015 & A-5-018-BE 罗点点谈高干子女教育环境	
# “六一”幼儿园像后来的“八一”学校、“十一”学校一样是在供给制度下专门为革命干部的子女开办的，由苏联专家设计和管理。革命胜利后的中国大陆人对苏联人和苏联文化充满了崇拜和羡慕，多少有点像改革开放后对美国人和美国文化的态度。……因为父母实在忙，没有时间管我们，就把我和姐姐送到北京西郊的“十一”小学。 这所学校是专为干部子弟开办的。由军队出钱，主要接收军队的干部子女。和东华门小学、东交民巷小学比较起来，这里漂亮得多。院子宽大，遍植花草。教室明亮通敞，桌椅不仅配套，而且随着孩子的身高而大小不同。学生们住校，食宿全由学校管理。听说原来衣服也是学校发的，后来，部队的干部不实行供给制，实行工资制了，衣服才改由家长提供。但是我们班里还有一些同学的衣服是由公家发给的，他们是一些烈士或者在执行特殊任务的人的子女。我们只知道，后者的父母是一些需要隐姓埋名的人。	
# A-7-025-B  陈毅女儿陈姗姗（又名丛军）文革中1972选派留学英国 ##	
# 丛军（陈毅女儿陈珊珊）……1974年经周恩来批准，被选派到英国伦敦经济学院学习语言，改革开放后又到美国攻读国际关系，拿到硕士学位。她先后在外交部当过翻译、司长，还出任过驻爱沙尼亚大使，是我国为数不多的女大使之一……1972年，好运再次关照到了丛军。“文革”浩劫，让人才断档了。根据周恩来的指示，外交部受命把“文革”前在当时外院附中和外语学校学习了三年的老初三学生招回来，精选一些人，送到英国留学，要求自然是根红苗正的好青年。 已经当了四年兵的丛军幸运地名列其中。 我问：“这个是不是因为家庭的关系？” “是这样的，当时，姬鹏飞外长的夫人许寒冰是外交部干部司的司长，她到医院来看望我母亲。……那次许阿姨来跟我母亲说，现在有这么一个机会，总理的指示，翻译还是要培养的，现在都停课闹革命了，也没有大学了，就想把过去在外语附中学习的老初三学生招回来，送到国外再深造一下，将来回来后就到外交部当翻译。你这个女儿是外语附中的，符合这个条件。”病中的张茜听到这个消息，很是高兴，也有些担忧，她对许寒冰说：她先征求一下周总理和邓大姐的意见，再答复。 丛军说：“我妈妈当时就给邓大姐打电话，她说过去有规定高干子弟不能出国留学，现在有这么一个机会，就想征求一下大姐和总理的意见。后来邓大姐跟总理商量以后，就回了一个电话。邓大姐说，恩来说是有这种规定，但现在情况不一样了，陈老总去世了，外交部让姗姗去学，这个是可以考虑的。意思就是说我父亲已经去世了，现在也不是什么高干了。后来我妈妈说，既然大姐和总理都说没问题，那她就同意就把我送去了。”事后，丛军才知道，他们是在“文革”期间送出去的第一批学生，是红色中国在经历浩劫的同时，特意选择派出的苗子，他们的任务不单单是学习，他们还是中国保持与世界交流的一点火种和希望。 在他们这批负笈西游的学子中，有今天我们熟悉的一些从事外事、外交的优秀人物：中国外交部部长杨洁篪、驻美大使周文重、外交部副部长、驻联合国大使王光亚，以及后去的原商务部副部长龙永图等。	

# 【引用 & 已翻译】 编号：A-6-103 哈军工 学生构成 ##	
# 开国元勋的子女们 哈军工高干子女传记 - 滕叙兖 (1)	
# 当年在哈军工有个不成文的约定俗成：“高干子女”的概念一般是指家长是地方副省级以上、中央副部级以上	

# 【引用 & 已翻译】A-2-037-BD 胡耀邦 陈小津 贺平 工作调动 写条子	
# 我改日又拜见了耀邦伯伯一次，想请他写张条子帮我解决工作调动的事。耀邦伯伯是湖南人，他曾下放到湘潭当过地委书记，与湖南省当时的领导很熟悉。碰巧的是，那天贺平也在，而且也是因为工作调动的事来找耀邦帮忙。贺平的父亲贺彪早年是江湖赤卫队队员、红二方面军的卫生部长，解放后担任了原中央卫生部副部长……耀邦伯伯对我们俩说：“好啊，我给你俩写个条。”耀邦伯伯的条子，还是写给湖南省委领导万达，请他帮助解决我和贺平的工作调动问题……1972年春，我的北京之行收获很大。我不但见到了耀邦、粟裕、曾山、姬鹏飞、周惠等我父亲的老领导、老战友们，还从他们那里了解了政治动向，得到了无私的帮助和温暖，受到了教育与指点。	
# “Days later, I visited Uncle Hu Yaobang again, hoping to ask for his handwriting note to help me find another job. Uncle Hu Yaobang is a Hunan province native and once served the CCP secretary in Xiangtan City, Hunan. He’s familiar with all Hunan provincial leaders at that time. What a coincidence – on that day of my visit, He Ping was there too. He Ping’s father, He Biao, a Red Army veteran and medical chief of the 2nd Red Army, served as the vice minister of Ministry of Health after 1949. (…) Uncle Hu Yaobang told both He Ping and me, ‘alright, I will write a note for you two.’ His note was addressed to Hunan provincial leader, Wan Da, indicating that He Ping and myself need job re-arrangement. (…) My Beijing trip in 1972 was very productive. Not only did I met Hu Yaobang, Su Yu, Zeng Shan, Ji Pengfei, and Zhou Hui, my father’s long time superiors and friends, but also did I know rich information and trends in top politics. I got their unsparing help and care, and I learned greatly from their guidance.”	

## 供给制下高度集中的居住环境和基于此的社会关系网络	
# 【引用】B-2-002-D 我永远不会忘记万寿路招待所北面那一字排开的五栋小楼，这里曾经给于了我亲爱的父辈们一个舔伤的窝，一个喘息的避风港。他们是：陈丕显，宋任穷，叶飞，曾志，江华，江渭清，廖志高，欧阳钦，曾生，谢振华。	
# 【引用】B-3-003-D 我们的家在怀仁堂旁边，一个小胡同，从南到北前后四个院子。一院李富春，二院谭震林，三院邓小平，四院陈毅。原本，四个副总理，四户人家，大人们既是老战友又是老同志，关系亲密。小孩子们更是从小一起长大，像兄弟姐妹一样相处甚欢。	
# 【引用】B-3-004-C 在父亲的亲自安排下，罗帅住进了东交民巷的房子，而我们家则在后来搬进了中南海。“文革”以前，东交民巷这个院子里住着罗荣桓元帅、贺龙元帅、最高人民检察院检察长张鼎丞等四户人家。	
# 【引用】D-2-001-BE 康平路165号的市委大院宿舍区共有8栋二层小楼，由东向西分别住着我父亲、柯庆施、魏文伯、曹荻秋及四家的工作人员。	#

##  高层姻亲、太子党间婚恋关系网。	
# 【暂未引用 & 已翻译】 [VIP] B-2-036-D 叶飞家族 胡耀邦家族 安子文 姻亲关系 ##	
# 我到北京后没几天，叶飞的大女儿叶小楠就带我去了耀邦伯伯家。小楠与中央组织部原部长安子文的女儿安黎是清华的同学。1968年，安黎告诉小楠，她有个男朋友是胡耀邦的长子胡德平，让小楠陪她一起去见见胡德平，并请她帮助参谋参谋。就这样，小楠就知道耀邦家的地址，以后又去过多次，与耀邦也熟悉起来。小毛是北大的学生，她到清华看姐姐小楠，也认识了安黎，后来也多次到过耀邦家。这都是1969年5月耀邦到河南黄湖“五七”干校之前的事。1971年冬，耀邦回到北京，到家里来拜访的那些被打倒、受迫害的老干部及其家属子女们很多。小楠告诉我，耀邦伯伯有一次见到她时，曾经问起她：“你和阿丕家有往来吗？”可见他十分惦念我父亲的情况。小楠见到耀邦伯伯时说：“耀邦叔叔，我给你带来一个年轻人，他想见见你。”耀邦伯伯看到了我，指着我说：“这是谁家的娃娃？”小楠没说话。我有点犹豫，也没有马上说我是谁。突然，耀邦两眼一亮，手一指，说了一句：“你是阿丕的儿子！”	
# “Several days after my arrival in Beijing, Ye Fei’s eldest daughter, Ye Xiaonan brought me to Ungle Hu Yaobang’s house. Ye Xiaonan and An Li (the daughter of An Ziwen, minister of the CCP Organization Department) were classmates in Tsinghua University. In 1968, An Li told Ye Xiaonan her boyfriend was Hu Deping, the eldest son of Hu Yaobang. An Li asked Ye Xiaonan to meet with Hu Deping and to provide some advice and suggestions about this relationship. After this, Ye Xiaonan knew the address of Hu family and visited there many times, becoming more familiar with Uncle Hu Yaobang. (…) Ye Xiaonan told me, Uncle Hu Yaobang once asked her whether she had connections with my family. It shows how much Uncle Hu cared about my father. (When we visit,) Ye Xiaonan said to Uncle Hu Yaobang, ‘Uncle Hu, I brought you a young man who wants to see you.’ Uncle Hu pointed at me as he saw me, asking her ‘whose son is this?’ Ye Xiaonan didn’t reply. I hesitated for a bit and did not reply immediately. Suddenly, Uncle Hu blinked his eyes and pointed at me again – he said, ‘You are A-Pi’s son!’” (译者注)	

## 编号：[VIP] A-7-025-B  陈毅 陈姗姗（又名丛军） 1972 留学英国 ##	
# 人物：陈毅 陈姗姗 姬鹏飞 许寒冰 周恩来 邓颖超	
# 关键字：教育 1972 留学 英国	
# 2006年8月30日，我前往北京市广渠门外大街的广泉小区，采访刚刚从美国回来的丛军（陈毅女儿陈珊珊），她是回国休假，同时也是为了儿子结婚才回来的。 她少时就读于北京外语学院附中，“文革”期间，曾到部队的军医学校学过医，1974年经周恩来批准，被选派到英国伦敦经济学院学习语言，改革开放后又到美国攻读国际关系，拿到硕士学位。她先后在外交部当过翻译、司长，还出任过驻爱沙尼亚大使，是我国为数不多的女大使之一……	
# 她希望我能继承她的这些专长，可我不太愿意。当时我爸爸就说，国家很需要外语人才，女孩子文静一点，学点外语不错。他说我们外交部也很需要外语人才，正好现在外语学院开始办附中，你就去考这个附中吧。我觉得我爸这个决定特好。” 这场文艺和外语的讨论，最后还是父亲说了算。丛军如愿进入外语学院附中读书。并影响到她的一生。 为了帮助女儿学好外语，陈毅专门给丛军买来了英国广播电台英语讲座唱片《林格风》和《基础英语》教科书。40多年前他的这些行为，即使放在今天，也足以和当今许多望子成龙，望女成凤的家长相媲美，足见他对女儿的呵护与器重。	
# 1972年，好运再次关照到了丛军。 “文革”浩劫，让人才断档了。根据周恩来的指示，外交部受命把“文革”前在当时外院附中和外语学校学习了三年的老初三学生招回来，精选一些人，送到英国留学，要求自然是根红苗正的好青年。 已经当了四年兵的丛军幸运地名列其中。 我问：“这个是不是因为家庭的关系？” “是这样的，当时，姬鹏飞外长的夫人许寒冰是外交部干部司的司长，她到医院来看望我母亲。父亲去世不久，我母亲就查出是癌症，住在301医院。那次许阿姨来跟我母亲说，现在有这么一个机会，总理的指示，翻译还是要培养的，现在都停课闹革命了，也没有大学了，就想把过去在外语附中学习的老初三学生招回来，送到国外再深造一下，将来回来后就到外交部当翻译。你这个女儿是外语附中的，符合这个条件。” 病中的张茜听到这个消息，很是高兴，也有些担忧，她对许寒冰说：她先征求一下周总理和邓大姐的意见，再答复。 丛军说：“我妈妈当时就给邓大姐打电话，她说过去有规定高干子弟不能出国留学，现在有这么一个机会，就想征求一下大姐和总理的意见。后来邓大姐跟总理商量以后，就回了一个电话。邓大姐说，恩来说是有这种规定，但现在情况不一样了，陈老总去世了，外交部让姗姗去学，这个是可以考虑的。意思就是说我父亲已经去世了，现在也不是什么高干了。后来我妈妈说，既然大姐和总理都说没问题，那她就同意就把我送去了。” 事后，丛军才知道，他们是在“文革”期间送出去的第一批学生，是红色中国在经历浩劫的同时，特意选择派出的苗子，他们的任务不单单是学习，他们还是中国保持与世界交流的一点火种和希望。 在他们这批负笈西游的学子中，有今天我们熟悉的一些从事外事、外交的优秀人物：中国外交部部长杨洁篪、驻美大使周文重、外交部副部长、驻联合国大使王光亚，以及后去的原商务部副部长龙永图等。	

## 编号：A-7-026   陈昊苏 北京市副市长 ##	
# 人物：陈毅 陈昊苏 关键字：职业发展 中科大	
# 陈昊苏瘦高，戴眼镜，穿一套灰色西服，可能是工作的需要，他的胸前别着一枚显眼的国徽。 陈昊苏相貌与父亲陈毅的相貌反差较大，但他的工作经历却和父亲惊人地相似，不同的只是地位的高低。 陈昊苏就读于中国科技大学，毕业后参军，后又从部队调到地方，先后担任团中央书记处书记，北京市副市长，国家广电部副部长，现任对外友协会长。他工作的经历和父亲一样，先学工，再参军，后从政，再搞外交。因而，在陈昊苏身上有着太多陈毅的影子	
