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

save.fig.dir = './RQ1_result/RQ1_figure/'

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

# ---------------- Code for RQ2 -----------------------#
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, file.level.ground.truth == "True" & prediction.prob >= 0.5 &  is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)

get.line.metrics.result = function(baseline.df, cur.df.file)
{
  
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  
  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
  
  IFA = sorted %>%  group_by(filename)  %>% filter(line.level.ground.truth == "True") %>% top_n(1, -order)
  IFA = IFA%>% arrange(filename)
  ifa.list = IFA$order - 1
  
  total_true = sorted %>%  group_by(filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
  total_true = total_true %>% filter(total_true > 0)
  total_true = total_true%>% arrange(filename)
  
  recall20LOC = sorted %>% group_by(filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>%
    summarize(correct_pred = sum(line.level.ground.truth == "True")) %>%
    merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)
  recall20LOC = recall20LOC %>% arrange(filename)
  recall.list = recall20LOC$recall20LOC
  
  effort20Recall = sorted %>% merge(total_true)  %>% group_by(filename) %>% arrange(order, .by_group=TRUE) %>% mutate    (cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth ==      "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0))     %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))
  effort20Recall = effort20Recall %>% arrange(filename)
  effort.list = effort20Recall$effort20Recall
  
  fpa = sorted %>% merge(total_true) %>% group_by(filename) %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )
  fpa = fpa %>% arrange(filename)
  fpa.list = fpa$FPA
  
  top5 = sorted %>% merge(total_true) %>% group_by(filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())
  top5 = top5 %>% arrange(filename)
  top5.list = top5$top5
  
  top10 = sorted %>% merge(total_true)  %>% group_by(filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
  top10 = top10 %>% arrange(filename)
  top10.list = top10$top10
  
  result.df = data.frame(IFA$filename, ifa.list, recall.list, effort.list, fpa.list, top5.list, top10.list, IFA$totalSLOC)
  
  return(result.df)
}

all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0', 
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2', 
                      'hive-0.12.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')

glance.ea.result.dir = './Baseline-result/GLANCE/result/BASE-Glance-EA/line_result/test/'
glance.md.result.dir = './Baseline-result/GLANCE/result/BASE-Glance-MD/line_result/test/'
glance.lr.result.dir = './Baseline-result/GLANCE/result/BASE-Glance-LR/line_result/test/'

n.gram.result.dir = './ngram/n_gram_result/'
linedp.result.dir = './Baseline-result/GLANCE/result/MIT-LineDP/line_result/test/'
PMD.result.dir = './PMD/PMD_result/'  

glance.ea.result.df = NULL
glance.md.result.df = NULL
glance.lr.result.df = NULL
n.gram.result.df = NULL
linedp.result.df = NULL
PMD.result.df = NULL


## get result from baseline
for(rel in all_eval_releases)
{  
  
  glance.ea.result = read.csv(paste0(glance.ea.result.dir,rel,'-result.csv'))
  glance.ea.result = select(glance.ea.result, "predicted_buggy_lines", "predicted_buggy_line_numbers","rank")
  glance.ea.result$filename = str_split_fixed(glance.ea.result$predicted_buggy_lines,":", 2)[,1]
  glance.ea.result = select(glance.ea.result,'filename',"predicted_buggy_line_numbers","rank")
  names(glance.ea.result) = c("filename", "line.number", "rank")

  glance.md.result = read.csv(paste0(glance.md.result.dir,rel,'-result.csv'))
  glance.md.result = select(glance.md.result, "predicted_buggy_lines", "predicted_buggy_line_numbers","rank")
  glance.md.result$filename = str_split_fixed(glance.md.result$predicted_buggy_lines,":", 2)[,1]
  glance.md.result = select(glance.md.result,'filename',"predicted_buggy_line_numbers","rank")
  names(glance.md.result) = c("filename", "line.number", "rank")
  
  glance.lr.result = read.csv(paste0(glance.lr.result.dir,rel,'-result.csv'))
  glance.lr.result = select(glance.lr.result, "predicted_buggy_lines", "predicted_buggy_line_numbers","rank")
  glance.lr.result$filename = str_split_fixed(glance.lr.result$predicted_buggy_lines,":", 2)[,1]
  glance.lr.result = select(glance.lr.result,'filename',"predicted_buggy_line_numbers","rank")
  names(glance.lr.result) = c("filename", "line.number", "rank")
  
  n.gram.result = read.csv(paste0(n.gram.result.dir,rel,'-line-lvl-result.txt'), sep = "\t", quote = "")
  n.gram.result = select(n.gram.result, "file.name", "line.number",  "line.score")
  n.gram.result = distinct(n.gram.result)
  n.gram.result = n.gram.result %>% group_by(file.name) %>% arrange(-line.score, .by_group = TRUE) %>% mutate(rank = row_number())
  n.gram.result = select(n.gram.result,'file.name','line.number','rank')
  names(n.gram.result) = c('filename','line.number','rank')
  
  linedp.result = read.csv(paste0(linedp.result.dir,rel,'-result.csv'))
  linedp.result = select(linedp.result,'predicted_buggy_lines','rank')
  linedp.result$file.name = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,1]
  linedp.result$line.numbers = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,2]
  linedp.result = select(linedp.result,'file.name','line.numbers','rank')
  names(linedp.result) = c("filename", "line.number",'rank')
  
  PMD.result = read.csv(paste0(PMD.result.dir,rel,'-line-lvl-result.txt'),quote="")
  PMD.result$PMD_prediction_result <- ifelse(PMD.result$PMD_prediction_result == "False", 0, 1)
  PMD.result = PMD.result %>% group_by(filename) %>% arrange(-PMD_prediction_result, Priority, .by_group = TRUE) %>% mutate(rank = row_number())
  PMD.result = select(PMD.result,'filename','line_number','rank')
  names(PMD.result) = c('filename','line.number','rank')
  
  cur.df.file = filter(line.ground.truth, test==rel)
  cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
  
  glance.ea.eval.result = get.line.metrics.result(glance.ea.result, cur.df.file) %>% mutate(test=rel)
  glance.md.eval.result = get.line.metrics.result(glance.md.result, cur.df.file) %>% mutate(test=rel)
  glance.lr.eval.result = get.line.metrics.result(glance.lr.result, cur.df.file) %>% mutate(test=rel)
  n.gram.eval.result = get.line.metrics.result(n.gram.result,cur.df.file) %>% mutate(test=rel)
  PMD.eval.result = get.line.metrics.result(PMD.result,cur.df.file) %>% mutate(test=rel)
  linedp.eval.result = get.line.metrics.result(linedp.result,cur.df.file) %>% mutate(test=rel)
  
  glance.ea.result.df = rbind(glance.ea.result.df, glance.ea.eval.result)
  glance.md.result.df = rbind(glance.md.result.df, glance.md.eval.result)
  glance.lr.result.df = rbind(glance.lr.result.df, glance.lr.eval.result)
  n.gram.result.df = rbind(n.gram.result.df,n.gram.eval.result)
  PMD.result.df = rbind(PMD.result.df,PMD.eval.result)
  linedp.result.df = rbind(linedp.result.df,linedp.eval.result)
  
  print(paste0('finished ', rel))
}
######DeepLineDP and boosting SPLICE ######

#Force attention score of comment line is 0
df_all[df_all$is.comment.line == "True",]$token.attention.score = 0

tmp.top.k = get.top.k.tokens(df_all, 1500)

merged_df_all = merge(df_all, tmp.top.k, by=c('project', 'train', 'test', 'filename', 'token'), all.x = TRUE)

merged_df_all[is.na(merged_df_all$flag),]$token.attention.score = 0

## use top-k tokens 
sum_line_attn = merged_df_all %>% filter(file.level.ground.truth == "True" & prediction.label == "True" ) %>% group_by(test, filename,is.comment.line, file.level.ground.truth, prediction.label, line.number, line.level.ground.truth) %>%
  summarize(attention_score = sum(token.attention.score), num_tokens = n())

sum_line_attn = merge(sum_line_attn, df_CEandNF_all, by=c('test', 'filename', 'line.number'))

sorted = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_SPLICE_S = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-CE, -NFC, -attention_score/num_tokens, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
sorted_SPLICE_G = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename)  %>% mutate(order = rank)%>% mutate(totalSLOC = n())
sorted_SPLICE_F = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score*num_tokens*NFC, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

## get result from DeepLineDP
# calculate IFA
IFA = sorted %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_SPLICE_S = sorted_SPLICE_S %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_SPLICE_G = sorted_SPLICE_G %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_SPLICE_F = sorted_SPLICE_F %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)


IFA = IFA %>% arrange(test, filename)
IFA_SPLICE_S = IFA_SPLICE_S %>% arrange(test, filename)
IFA_SPLICE_G = IFA_SPLICE_G %>% arrange(test, filename)
IFA_SPLICE_F = IFA_SPLICE_F %>% arrange(test, filename)

total_true = sorted %>% group_by(test, filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
total_true = total_true %>% filter(total_true > 0)
total_true = total_true %>% arrange(test, filename)


recall20LOC = sorted %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>%
  summarize(correct_pred = sum(line.level.ground.truth == "True")) %>%
  merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_SPLICE_S = sorted_SPLICE_S %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)
recall20LOC_SPLICE_G = sorted_SPLICE_G %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)
recall20LOC_SPLICE_F = sorted_SPLICE_F %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)


recall20LOC = recall20LOC %>% arrange(test, filename)
recall20LOC_SPLICE_S = recall20LOC_SPLICE_S %>% arrange(test, filename)
recall20LOC_SPLICE_G = recall20LOC_SPLICE_G %>% arrange(test, filename)
recall20LOC_SPLICE_F = recall20LOC_SPLICE_F %>% arrange(test, filename)


effort20Recall = sorted %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))
effort20Recall_SPLICE_S = sorted_SPLICE_S %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))
effort20Recall_SPLICE_G = sorted_SPLICE_G %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))
effort20Recall_SPLICE_F = sorted_SPLICE_F %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall = effort20Recall %>% arrange(test, filename)
effort20Recall_SPLICE_S = effort20Recall_SPLICE_S %>% arrange(test, filename)
effort20Recall_SPLICE_G = effort20Recall_SPLICE_G %>% arrange(test, filename)
effort20Recall_SPLICE_F = effort20Recall_SPLICE_F %>% arrange(test, filename)

FPA = sorted %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )
FPA_SPLICE_S = sorted_SPLICE_S %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )
FPA_SPLICE_G = sorted_SPLICE_G %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )
FPA_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA = FPA %>% arrange(test, filename)
FPA_SPLICE_S = FPA_SPLICE_S %>% arrange(test, filename)
FPA_SPLICE_G = FPA_SPLICE_G %>% arrange(test, filename)
FPA_SPLICE_F = FPA_SPLICE_F %>% arrange(test, filename)


top5 = sorted %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())
top5_SPLICE_S = sorted_SPLICE_S %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())
top5_SPLICE_G = sorted_SPLICE_G %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())
top5_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5 = top5 %>% arrange(test, filename)
top5_SPLICE_S = top5_SPLICE_S %>% arrange(test, filename)
top5_SPLICE_G = top5_SPLICE_G %>% arrange(test, filename)
top5_SPLICE_F = top5_SPLICE_F %>% arrange(test, filename)

top10 = sorted %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_SPLICE_S = sorted_SPLICE_S %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_SPLICE_G = sorted_SPLICE_G %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())

top10 = top10 %>% arrange(test, filename)
top10_SPLICE_S = top10_SPLICE_S %>% arrange(test, filename)
top10_SPLICE_G = top10_SPLICE_G %>% arrange(test, filename)
top10_SPLICE_F = top10_SPLICE_F %>% arrange(test, filename)

## prepare data for plotting
deeplinedp.ifa = IFA$order - 1
deeplinedp.recall = recall20LOC$recall20LOC
deeplinedp.effort = effort20Recall$effort20Recall
deeplinedp.fpa= FPA$FPA
deeplinedp.top5= top5$top5
deeplinedp.top10= top10$top10

deepline.dp.line.result = data.frame(IFA$filename, deeplinedp.ifa, deeplinedp.recall, deeplinedp.effort, deeplinedp.fpa, IFA$test, deeplinedp.top5, deeplinedp.top10, IFA$totalSLOC)

splice_S.ifa = IFA_SPLICE_S$order - 1
splice_S.recall = recall20LOC_SPLICE_S$recall20LOC
splice_S.effort = effort20Recall_SPLICE_S$effort20Recall
splice_S.fpa = FPA_SPLICE_S$FPA
splice_S.top5= top5_SPLICE_S$top5
splice_S.top10= top10_SPLICE_S$top10

splice_S.line.result = data.frame(IFA_SPLICE_S$filename, splice_S.ifa, splice_S.recall, splice_S.effort, splice_S.fpa, IFA_SPLICE_S$test, splice_S.top5, splice_S.top10, IFA_SPLICE_S$totalSLOC)


splice_G.ifa = IFA_SPLICE_G$order - 1
splice_G.recall = recall20LOC_SPLICE_G$recall20LOC
splice_G.effort = effort20Recall_SPLICE_G$effort20Recall
splice_G.fpa = FPA_SPLICE_G$FPA
splice_G.top5= top5_SPLICE_G$top5
splice_G.top10= top10_SPLICE_G$top10

splice_G.line.result = data.frame(IFA_SPLICE_G$filename, splice_G.ifa, splice_G.recall, splice_G.effort, splice_G.fpa, splice_G.top5, splice_G.top10, IFA_SPLICE_G$test, IFA_SPLICE_G$totalSLOC)

splice_F.ifa = IFA_SPLICE_F$order - 1
splice_F.recall = recall20LOC_SPLICE_F$recall20LOC
splice_F.effort = effort20Recall_SPLICE_F$effort20Recall
splice_F.fpa = FPA_SPLICE_F$FPA
splice_F.top5= top5_SPLICE_F$top5
splice_F.top10= top10_SPLICE_F$top10

splice_F.line.result = data.frame(IFA_SPLICE_F$filename, splice_F.ifa, splice_F.recall, splice_F.effort, splice_F.fpa, IFA_SPLICE_F$test, splice_F.top5, splice_F.top10, IFA_SPLICE_F$totalSLOC)

sum_glance.lr.result.df = glance.lr.result.df %>%   summarize(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_glance.ea.result.df = glance.ea.result.df %>%   summarize(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_glance.md.result.df = glance.md.result.df %>%   summarize(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_deepline.dp.line.result = deepline.dp.line.result %>%   summarize(IFA=median(deeplinedp.ifa),recall=median(deeplinedp.recall),effort=median(deeplinedp.effort), fpa=median(deeplinedp.fpa), top5=mean(deeplinedp.top5), top10=mean(deeplinedp.top10),.by=IFA.test)
sum_n.gram.result.df = n.gram.result.df %>% summarise(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_linedp.result.df = linedp.result.df %>% summarise(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_PMD.result.df = PMD.result.df %>% summarise(IFA=median(ifa.list),recall=median(recall.list),effort=median(effort.list), fpa=median(fpa.list), top5=mean(top5.list), top10=mean(top10.list), .by=test)
sum_splice_S.line.result = splice_S.line.result %>%   summarize(IFA=median(splice_S.ifa),recall=median(splice_S.recall),effort=median(splice_S.effort), fpa=median(splice_S.fpa), top5=mean(splice_S.top5), top10=mean(splice_S.top10), .by=IFA_SPLICE_S.test)
sum_splice_G.line.result = splice_G.line.result %>%   summarize(IFA=median(splice_G.ifa),recall=median(splice_G.recall),effort=median(splice_G.effort), fpa =median(splice_G.fpa), top5=mean(splice_G.top5), top10=mean(splice_G.top10),  .by=IFA_SPLICE_G.test)
sum_splice_F.line.result = splice_F.line.result %>%   summarize(IFA=median(splice_F.ifa),recall=median(splice_F.recall),effort=median(splice_F.effort), fpa=median(splice_F.fpa), top5=mean(splice_F.top5), top10=mean(splice_F.top10), .by=IFA_SPLICE_F.test)

names(sum_glance.lr.result.df) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_glance.ea.result.df) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_glance.md.result.df)  = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_n.gram.result.df)  = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_linedp.result.df) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_PMD.result.df) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_deepline.dp.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_splice_S.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_splice_G.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_splice_F.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")

#### get data results of models ####
write.csv(sum_splice_S.line.result, file = './RQ1_result/SPLICE-S.csv')
write.csv(sum_splice_G.line.result, file = './RQ1_result/SPLICE-G.csv')
write.csv(sum_splice_F.line.result, file = './RQ1_result/SPLICE-F.csv')
write.csv(sum_glance.lr.result.df, file = './RQ1_result/GLANCE-LR.csv')
write.csv(sum_glance.ea.result.df, file = './RQ1_result/GLANCE-EA.csv')
write.csv(sum_glance.md.result.df, file = './RQ1_result/GLANCE-MD.csv')
write.csv(sum_n.gram.result.df, file = './RQ1_result/N-gram.csv')
write.csv(sum_linedp.result.df, file = './RQ1_result/LineDP.csv')
write.csv(sum_PMD.result.df, file = './RQ1_result/PMD.csv')
write.csv(sum_deepline.dp.line.result, file = './RQ1_result/DeepLineDP.csv')

sum_glance.lr.result.df$technique = 'GLANCE-LR'
sum_glance.ea.result.df$technique = 'GLANCE-EA'
sum_glance.md.result.df$technique = 'GLANCE-MD'
sum_n.gram.result.df$technique = 'N-Gram'
sum_linedp.result.df$technique = 'LineDP'
sum_PMD.result.df$technique = 'PMD'
sum_deepline.dp.line.result$technique = 'DeepLineDP'
sum_splice_S.line.result$technique = 'SPLICE-S'
sum_splice_G.line.result$technique = 'SPLICE-G'
sum_splice_F.line.result$technique = 'SPLICE-F'


all.line.result = rbind(sum_glance.lr.result.df, sum_glance.ea.result.df, sum_glance.md.result.df, sum_n.gram.result.df, sum_PMD.result.df,sum_linedp.result.df,sum_deepline.dp.line.result, sum_splice_S.line.result,sum_splice_G.line.result, sum_splice_F.line.result)

recall.result.df = select(all.line.result, c('technique', 'Recall20%LOC'))
ifa.result.df = select(all.line.result, c('technique', 'IFA'))
effort.result.df = select(all.line.result, c('technique', 'Effort@20%Recall'))
fpa.result.df = select(all.line.result, c('technique', 'FPA'))
top5.result.df = select(all.line.result, c('technique', 'top5'))
top10.result.df = select(all.line.result, c('technique', 'top10'))

recall.result.df = preprocess(recall.result.df, FALSE)
ifa.result.df = preprocess(ifa.result.df, TRUE)
effort.result.df = preprocess(effort.result.df, TRUE)
fpa.result.df = preprocess(fpa.result.df, FALSE)
top5.result.df = preprocess(top5.result.df, FALSE)
top10.result.df = preprocess(top10.result.df, FALSE)


ggplot(recall.result.df, aes(x=reorder(variable, -value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("Recall@Top20%LOC") + xlab("") + theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir,"file-Recall@Top20LOC.pdf"),width=7,height=2.5)

ggplot(effort.result.df, aes(x=reorder(variable, value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("Effort@Top20%Recall") + xlab("")+ theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir,"file-Effort@Top20Recall.pdf"),width=7,height=2.5)

ggplot(ifa.result.df, aes(x=reorder(variable, value, FUN=median), y=value)) + geom_boxplot()  +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + coord_cartesian(ylim=c(0,125)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("IFA") + xlab("")+ theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir, "file-IFA.pdf"),width=7,height=2.5)

ggplot(fpa.result.df, aes(x=reorder(variable, -value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("FPA") + xlab("") + theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir, "file-FPA.pdf"),width=7,height=2.5)

ggplot(top5.result.df, aes(x=reorder(variable, -value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("Top5") + xlab("") + theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir, "file-top5.pdf"),width=7,height=2.5)

ggplot(top10.result.df, aes(x=reorder(variable, -value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("Top10") + xlab("") + theme(axis.text.x=element_text(angle = -60, hjust = 0))
ggsave(paste0(save.fig.dir, "file-top10.pdf"),width=7,height=2.5)


