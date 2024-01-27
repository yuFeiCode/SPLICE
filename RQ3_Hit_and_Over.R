#2023-12-16
library(tidyverse)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(caret)
library(reshape2)
library(car)
library(carData)
library(pROC)
library(effsize)
library(ScottKnottESD)
library(dplyr)
library(tibble)


save.fig.dir = './RQ3_result/Veen_plot_data/'

dir.create(file.path(save.fig.dir), showWarnings = FALSE)

preprocess <- function(x, reverse){
  colnames(x) <- c("variable","value")
  tmp <- do.call(cbind, split(x, x$variable))
  tmp <- tmp[, grep("value", names(tmp))]
  names(tmp) <- gsub(".value", "", names(tmp))
  df <- tmp
  ranking <- NULL
  
  if(reverse == TRUE)
  { 
    ranking <- (max(sk_esd(df)$group)-sk_esd(df)$group) +1 
  }
  else
  { 
    ranking <- sk_esd(df)$group 
  }
  
  x$rank <- paste("Rank",ranking[as.character(gsub("-", ".", x$variable))])
  return(x)
}

get.top.k.tokens = function(df, k)
{
  top.k <- df %>% filter( is.comment.line=="False"  & file.level.ground.truth=="True" & prediction.label=="True" ) %>%
    group_by(test, filename) %>% top_n(k, token.attention.score) %>% select("project","train","test","filename","token") %>% distinct()
  
  top.k$flag = 'topk'
  
  return(top.k)
}


prediction_dir = './Baseline-result/DeepLineDP/output/prediction/DeepLineDP/within-release/'

all_files = list.files(prediction_dir)

df_all <- NULL

for(f in all_files)
{
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}


CEandNFCdir = "./Baseline-result/GLANCE/result/Glance_MD_full_threshold/line_result/test/"

all_CEandNF_files = list.files(CEandNFCdir)

df_CEandNF_all <- NULL

for(f in all_CEandNF_files)
{
  df <- read.csv(paste0(CEandNFCdir, f))
  df$test = str_split_fixed(f, "-result", 2)[,1]
  df_CEandNF_all  <- rbind(df_CEandNF_all, df)
}

df_CEandNF_all = select(df_CEandNF_all, "predicted_buggy_lines", "predicted_buggy_line_numbers","rank", "functioncall", "controlelements", "test")
names(df_CEandNF_all) = c("filename", "line.number", "rank", "NFC", "CE", "test")
df_CEandNF_all$filename = str_split_fixed(df_CEandNF_all$filename, ":", 2)[,1]

# ---------------- Code for RQ3 -----------------------#
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, file.level.ground.truth == "True" & prediction.prob >= 0.5 &  is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)

#Force attention score of comment line is 0
df_all[df_all$is.comment.line == "True",]$token.attention.score = 0

tmp.top.k = get.top.k.tokens(df_all, 1500)

merged_df_all = merge(df_all, tmp.top.k, by=c('project', 'train', 'test', 'filename', 'token'), all.x = TRUE)

merged_df_all[is.na(merged_df_all$flag),]$token.attention.score = 0

## use top-k tokens 
sum_line_attn = merged_df_all %>% filter(file.level.ground.truth == "True" & prediction.label == "True" ) %>% group_by(test, filename,is.comment.line, file.level.ground.truth, prediction.label, line.number, line.level.ground.truth) %>%
  summarize(attention_score = sum(token.attention.score), num_tokens = n())

sum_line_attn = merge(sum_line_attn, df_CEandNF_all, by=c('test', 'filename', 'line.number'))

sorted_SPLICE_F = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score*num_tokens*NFC, .by_group=TRUE) %>% mutate(order = row_number())

sorted_DeepLineDP = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score, .by_group=TRUE) %>% mutate(order = row_number())


########### Calculate the hit and over of TP, calculate the hit and over of TN; ############
########### report the results on each version, and report the overall results of all versions combined together #########
computeHitOver = function(baseSet, newSet){
  intersection = intersect(baseSet, newSet)
  diff = setdiff(newSet, baseSet)
  hit = nrow(intersection) / nrow(baseSet)
  over = nrow(diff) / nrow(baseSet)
  
  result = data.frame(hit = hit, over = over)
  return (result)
}

getHitOverResult = function(rel, baseList, newList){  
  TP_base = getTP(baseList) 
  TN_base = getTN(baseList)
  
  TP_new = getTP(newList) 
  TN_new = getTN(newList) 
  
  TP_result = computeHitOver(TP_base, TP_new)
  TN_result = computeHitOver(TN_base, TN_new)
  
  result = data.frame(test=rel, TP.hit = TP_result$hit, TP.over = TP_result$over, TN.hit = TN_result$hit, TN.over = TN_result$over)
  
  return(result)
}


getTP = function(lineList){  
  TP = lineList %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2 & line.level.ground.truth=="True") %>% select(test, filename, line.number)
  
  return(TP)
}

getTN = function(lineList){  
  TN = lineList %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort > 0.2 & line.level.ground.truth=="False") %>% select(test, filename, line.number)
  
  return(TN)
}


all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0', 
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2', 
                      'hive-0.12.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')

glance.md.result.dir = './Baseline-result/GLANCE/result/BASE-Glance-MD/line_result/test/'
n.gram.result.dir = './Baseline-result/ngram/n_gram_result/'
linedp.result.dir = './Baseline-result/GLANCE/result/MIT-LineDP/line_result/test/'
PMD.result.dir = './Baseline-result/PMD/PMD_result/'

glance.md.hit.over = NULL
ngram.hit.over = NULL
deeplinedp.hit.over = NULL
linedp.hit.over = NULL
PMD.hit.over = NULL

glance.md.result.all = NULL
ngram.result.all = NULL
linedp.result.all = NULL
PMD.result.all = NULL

for(rel in all_eval_releases)
{  
  newList = sorted_SPLICE_F[sorted_SPLICE_F$test==rel, ]
  
  cur.df.file = filter(line.ground.truth, test==rel)
  cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
  
  
  glance.md.result = read.csv(paste0(glance.md.result.dir,rel,'-result.csv'))
  glance.md.result$filename = str_split_fixed(glance.md.result$predicted_buggy_lines,":", 2)[,1]
  glance.md.result = select(glance.md.result,'filename',"predicted_buggy_line_numbers","rank")
  names(glance.md.result) = c("filename", "line.number", "rank")
  glance.md.result = merge(glance.md.result, cur.df.file, by=c("filename", "line.number")) %>% mutate(test = rel) 
  glance.md.result.all = rbind(glance.md.result.all, glance.md.result)
  
  baseList = glance.md.result %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
  temp = getHitOverResult(rel, baseList, newList)
  glance.md.hit.over  = rbind(glance.md.hit.over, temp )
  
  
  PMD.result = read.csv(paste0(PMD.result.dir,rel,'-line-lvl-result.txt'),quote="")
  PMD.result$PMD_prediction_result <- ifelse(PMD.result$PMD_prediction_result == "False", 0, 1)
  PMD.result = PMD.result %>% group_by(filename) %>% arrange(-PMD_prediction_result, Priority, .by_group = TRUE) %>% mutate(rank = row_number())
  PMD.result = select(PMD.result,'filename','line_number','rank')
  names(PMD.result) = c('filename','line.number','rank')
  PMD.result = merge(PMD.result, cur.df.file, by=c("filename", "line.number")) %>% mutate(test = rel) 
  PMD.result.all = rbind(PMD.result.all, PMD.result)
  
  baseList = PMD.result %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
  temp = getHitOverResult(rel, baseList, newList)
  PMD.hit.over = rbind(PMD.hit.over, temp)
  
 
  ngram.result = read.csv(paste0(n.gram.result.dir,rel,'-line-lvl-result.txt'), sep = "\t", quote = "")
  ngram.result = select(ngram.result, "file.name", "line.number",  "line.score")
  ngram.result = distinct(ngram.result)
  ngram.result = ngram.result %>% group_by(file.name) %>% arrange(-line.score, .by_group = TRUE) %>% mutate(rank = row_number())
  ngram.result = select(ngram.result,'file.name','line.number','rank')
  names(ngram.result) = c('filename','line.number','rank')
  ngram.result = merge(ngram.result, cur.df.file, by=c("filename", "line.number")) %>% mutate(test = rel) 
  ngram.result.all = rbind(ngram.result.all, ngram.result)
  
  baseList = ngram.result %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
  temp = getHitOverResult(rel, baseList, newList)
  ngram.hit.over = rbind(ngram.hit.over, temp )
  
  
  linedp.result = read.csv(paste0(linedp.result.dir,rel,'-result.csv'))
  linedp.result = select(linedp.result,'predicted_buggy_lines','rank')
  linedp.result$file.name = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,1]
  linedp.result$line.numbers = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,2]
  linedp.result$line.numbers = as.numeric(linedp.result$line.numbers)
  linedp.result = select(linedp.result,'file.name','line.numbers','rank')
  names(linedp.result) = c("filename", "line.number",'rank')
  linedp.result = merge(linedp.result, cur.df.file, by=c('filename','line.number')) %>% mutate(test = rel) 
  linedp.result.all = rbind(linedp.result.all, linedp.result)
  
  baseList = linedp.result %>% group_by(test,filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())
  temp = getHitOverResult(rel,baseList, newList)
  linedp.hit.over = rbind(linedp.hit.over, temp)
  
  
  baseList = sorted_DeepLineDP[sorted_DeepLineDP$test==rel, ]
  temp = getHitOverResult(rel, baseList, newList)
  deeplinedp.hit.over = rbind(deeplinedp.hit.over, temp)
  
  print(paste0('finished ', rel))
}

####GLANCE-MD hit and over#### 
glance.md.baseList = glance.md.result.all %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
temp = getHitOverResult("allprojects", glance.md.baseList, sorted_SPLICE_F)
glance.md.hit.over = rbind(glance.md.hit.over, temp)

####PMD hit and over#####
PMD.baseList = PMD.result.all %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
temp = getHitOverResult("allprojects", PMD.baseList, sorted_SPLICE_F)
PMD.hit.over = rbind(PMD.hit.over, temp)


####ngram hit and over####
ngram.baseList = ngram.result.all %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
temp = getHitOverResult("allprojects", ngram.baseList, sorted_SPLICE_F)
ngram.hit.over = rbind(ngram.hit.over, temp)


####linedp hit and over####
linedp.baseList = linedp.result.all %>% group_by(test, filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number()) 
temp = getHitOverResult("allprojects", linedp.baseList, sorted_SPLICE_F)
linedp.hit.over = rbind(linedp.hit.over, temp)


####DeepLineDP hit and over####
deepLineDP.baseList = sorted_DeepLineDP
temp = getHitOverResult("allprojects", deepLineDP.baseList , sorted_SPLICE_F)
deeplinedp.hit.over = rbind(deeplinedp.hit.over, temp)


#### RQ3_result ####
write.csv(glance.md.hit.over, file = './RQ3_result/GLANCE-MD.csv')
write.csv(PMD.hit.over, file = './RQ3_result/PMD.csv')
write.csv(ngram.hit.over, file = './RQ3_result/N-gram.csv')
write.csv(deeplinedp.hit.over, file = './RQ3_result/DeepLineDP.csv')
write.csv(linedp.hit.over, file = './RQ3_result/LineDP.csv')

########### prepare data for veen-figure plot  ###########

TP = getTP(glance.md.baseList)
TN = getTN(glance.md.baseList)
write.table(TP, paste0(save.fig.dir,"glancemd-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"glancemd-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

TP = getTP(sorted_SPLICE_F)
TN = getTN(sorted_SPLICE_F)
write.table(TP, paste0(save.fig.dir,"splice-F-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"splice-F-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

TP = getTP(sorted_DeepLineDP)
TN = getTN(sorted_DeepLineDP)
write.table(TP, paste0(save.fig.dir,"deeplinedp-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"deeplinedp-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

TP = getTP(PMD.baseList)
TN = getTN(PMD.baseList)
write.table(TP, paste0(save.fig.dir,"PMD-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"PMD-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

TP = getTP(ngram.baseList)
TN = getTN(ngram.baseList)
write.table(TP, paste0(save.fig.dir,"ngram-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"ngram-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

TP = getTP(linedp.baseList)
TN = getTN(linedp.baseList)
write.table(TP, paste0(save.fig.dir,"linedp-TP.csv"), sep=",", col.names = FALSE, row.names = FALSE)
write.table(TN, paste0(save.fig.dir,"linedp-TN.csv"), sep=",", col.names = FALSE, row.names = FALSE)

