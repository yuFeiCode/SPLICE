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

save.fig.dir = './Dis_result/'

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


line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, file.level.ground.truth == "True" & prediction.prob >= 0.5 &  is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)
######DeepLineDP和改进版SPLICE的预测性能计算######

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

sorted_SPLICE_F = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score*num_tokens*NFC, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

total_true = sorted_SPLICE_F %>% group_by(test, filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
total_true = total_true %>% filter(total_true > 0)


total_true = total_true %>% arrange(test, filename)

############## 不同threshold下的SPLICE-F的recall ######################
Get.recall = function(sorted_splice_f, threshold){
  
  recall20LOC_SPLICE_F = sorted_splice_f %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= threshold) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

  recall20LOC_SPLICE_F = recall20LOC_SPLICE_F %>% arrange(test, filename)
  
  splice_F.recall = recall20LOC_SPLICE_F$recall20LOC
  
  splice_F.line.result = data.frame(recall20LOC_SPLICE_F$test, splice_F.recall)
  
  sum_splice.F.result = splice_F.line.result %>% summarise(recall = median(splice_F.recall), .by = recall20LOC_SPLICE_F.test)
  names(sum_splice.F.result) = c('release', paste0(threshold*100,'%'))

  sum_splice.F.result = sum_splice.F.result %>% arrange(release)
  
  return (sum_splice.F.result)
}
############## 不同threshold下的SPLICE-F的effort ######################
Get.effort = function(sorted_splice_f, threshold){
  
  effort20Recall_SPLICE_F = sorted_splice_f %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= threshold) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= threshold)/n(), max(class) ))

  effort20Recall_SPLICE_F = effort20Recall_SPLICE_F %>% arrange(test, filename)
  
  splice_F.effort = effort20Recall_SPLICE_F$effort20Recall
  
  splice_F.line.result = data.frame(effort20Recall_SPLICE_F$test, splice_F.effort)
  
  sum_splice.F.result = splice_F.line.result %>% summarise(effort = median(splice_F.effort), .by = effort20Recall_SPLICE_F.test)
  names(sum_splice.F.result) = c('release', paste0(threshold*100,'%'))
  

  sum_splice.F.result = sum_splice.F.result %>% arrange(release)
  
  return (sum_splice.F.result)
}

####步长改为10% #####
####recall@top x%   x = 10, 20,  30,  40,  50,  60,  70,  80,  90, 100####

all.threshold = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0', 
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2', 
                      'hive-0.12.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')


splice.f.recall.result = data.frame(release = all_eval_releases)
splice.f.effort.result = data.frame(release = all_eval_releases)
deeplinedp.recall.result = data.frame(release = all_eval_releases)
deeplinedp.effort.result = data.frame(release = all_eval_releases)


for (x in all.threshold){
  
  temp.splice.f.recall.result = Get.recall(sorted_SPLICE_F, x)
  temp.splice.f.effort.result = Get.effort(sorted_SPLICE_F, x)
  
  temp.deeplinedp.recall.result = Get.recall(sorted, x)
  temp.deeplinedp.effort.result = Get.effort(sorted, x)
  
  splice.f.recall.result = merge(splice.f.recall.result, temp.splice.f.recall.result, by = 'release')
  splice.f.effort.result = merge(splice.f.effort.result, temp.splice.f.effort.result, by = 'release')
  
  deeplinedp.recall.result = merge(deeplinedp.recall.result, temp.deeplinedp.recall.result, .by = 'release')
  deeplinedp.effort.result = merge(deeplinedp.effort.result, temp.deeplinedp.effort.result, .by = 'release')
  
  print(paste0('finished ', x*100, '%Recall and ', x*100, '%Effort'))
}


########################################### GLANCE-MD########################################################
glance.md.get.recall.result = function(baseline.df, cur.df.file, threshold, release)
{
  
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  
  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
  
  total_true = sorted %>%  group_by(filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
  
  total_true = total_true %>% filter(total_true > 0)

  total_true = total_true%>% arrange(filename)
  
  recall20LOC = sorted %>% group_by(filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= threshold) %>%
    summarize(correct_pred = sum(line.level.ground.truth == "True")) %>%
    merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)
  
  recall20LOC = recall20LOC %>% arrange(filename)
  
  recall.list = recall20LOC$recall20LOC
  
  sum_recall = data.frame(release = release, recall.list)
  
  sum_recall = sum_recall %>% summarise(recall = median(recall.list), .by = release)
  
  names(sum_recall) = c('release',paste0('recall',threshold*100,'%'))
  
  return(sum_recall)
}

glance.md.get.effort.result = function(baseline.df, cur.df.file, threshold, release)
{
  
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  

  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
  

  total_true = sorted %>%  group_by(filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
  

  total_true = total_true %>% filter(total_true > 0)

  total_true = total_true%>% arrange(filename)
  
  effort20Recall = sorted %>% merge(total_true)  %>% group_by(filename) %>% arrange(order, .by_group=TRUE) %>% mutate    (cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth ==      "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= threshold) ~ order/n(),TRUE ~ 0))     %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= threshold)/n(), max(class) ))
  

  effort20Recall = effort20Recall %>% arrange(filename)
  
  effort.list = effort20Recall$effort20Recall
  
  sum_effort = data.frame(release = release, effort.list)
  
  sum_effort = sum_effort %>% summarise(effort = median(effort.list), .by = release)
  
  names(sum_effort) = c('release',paste0('recall',threshold*100,'%'))
  
  return(sum_effort)
}


glance.md.result.dir = './Baseline-result/GLANCE/result/BASE-Glance-MD/line_result/test/'

glance.md.recall.result = NULL
glance.md.effort.result = NULL

for(rel in all_eval_releases)
{  
  
  glance.md.result = read.csv(paste0(glance.md.result.dir,rel,'-result.csv'))
  glance.md.result = select(glance.md.result, "predicted_buggy_lines", "predicted_buggy_line_numbers","rank")
  glance.md.result$filename = str_split_fixed(glance.md.result$predicted_buggy_lines,":", 2)[,1]
  glance.md.result = select(glance.md.result,'filename',"predicted_buggy_line_numbers","rank")
  names(glance.md.result) = c("filename", "line.number", "rank")
  
  cur.df.file = filter(line.ground.truth, test==rel)
  cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
  
  single.release.recall.result = data.frame(release = rel)
  single.release.effort.result = data.frame(release = rel)
  
  for (x in all.threshold){
    
    temp.glance.md.recall.result = glance.md.get.recall.result(glance.md.result, cur.df.file, x, rel)
    temp.glance.md.effort.result = glance.md.get.effort.result(glance.md.result, cur.df.file, x, rel)
    
    single.release.recall.result = merge(single.release.recall.result, temp.glance.md.recall.result)
    single.release.effort.result = merge(single.release.effort.result, temp.glance.md.effort.result)
    
    
    print(paste0('finished ', rel,' ', x*100, '%Recall and ', x*100, '%Effort'))
  }
  
  glance.md.recall.result = rbind(glance.md.recall.result, single.release.recall.result)
  glance.md.effort.result = rbind(glance.md.effort.result, single.release.effort.result)
  
  print(paste0('finished ', rel))
}

# 附录内容，各个模型在不同threshold上的recall,effort指标（详细数据，14个release）
write.csv(glance.md.recall.result, file = './Dis_result/GLANCE-MD_all_different_threshold_recall.csv')
write.csv(glance.md.effort.result, file = './Dis_result/GLANCE-MD_all_different_threshold_effort.csv')
write.csv(deeplinedp.recall.result, file = './Dis_result/DeepLineDP_all_different_threshold_recall.csv')
write.csv(deeplinedp.effort.result, file = './Dis_result/DeepLineDP_all_different_threshold_effort.csv')
write.csv(splice.f.recall.result, file = './Dis_result/SPLICE-F_all_different_threshold_recall.csv')
write.csv(splice.f.effort.result, file = './Dis_result/SPLICE-F_all_different_threshold_effort.csv')

#######  绘制 origin 折线图做数据准备  #######
########################################### median recall ##########################################
glance.md.median.project = data.frame(matrix(nrow = 0, ncol = ncol(glance.md.recall.result)))
medians = sapply(glance.md.recall.result[,-1,drop = FALSE], median)
glance.md.median.project = rbind(glance.md.median.project,c('GLANCE-MD',medians))
colnames(glance.md.median.project) = colnames(glance.md.recall.result)
glance.md = gather(glance.md.median.project, key = "threshold", value = "GLANCE-MD")
glance.md = glance.md[-1, , drop = FALSE]

deeplinedp.median.project = data.frame(matrix(nrow = 0, ncol = ncol(deeplinedp.recall.result)))
medians = sapply(deeplinedp.recall.result[,-1,drop = FALSE], median)
deeplinedp.median.project = rbind(deeplinedp.median.project,c('DeepLineDP',medians))
colnames(deeplinedp.median.project) = colnames(deeplinedp.recall.result)
dp = gather(deeplinedp.median.project, key = "threshold", value = "DeepLineDP")
dp = dp[-1, , drop = FALSE]

splice.f.median.project = data.frame(matrix(nrow = 0, ncol = ncol(splice.f.recall.result)))
medians = sapply(splice.f.recall.result[,-1,drop = FALSE], median)
splice.f.median.project = rbind(splice.f.median.project,c('SPLICE-F',medians))
colnames(splice.f.median.project) = colnames(splice.f.recall.result)
splice.f = gather(splice.f.median.project, key = "threshold", value = "SPLICE-F")
splice.f = splice.f[-1, , drop = FALSE]

glance.md$DeepLineDP = dp$DeepLineDP
glance.md$SPLICE_F = splice.m$`SPLICE-F`
write.csv(glance.md, file = './Dis_result/all_different_threshold_median_recall.csv')

####################################### median effort #########################################
glance.md.median.effort = data.frame(matrix(nrow = 0, ncol = ncol(glance.md.effort.result)))
medians = sapply(glance.md.effort.result[,-1,drop = FALSE], median)
glance.md.median.effort = rbind(glance.md.median.effort,c('GLANCE-MD',medians))
colnames(glance.md.median.effort) = colnames(glance.md.effort.result)
glance.md = gather(glance.md.median.effort, key = "threshold", value = "GLANCE-MD")
glance.md = glance.md[-1, , drop = FALSE]

deeplinedp.median.effort = data.frame(matrix(nrow = 0, ncol = ncol(deeplinedp.effort.result)))
medians = sapply(deeplinedp.effort.result[,-1,drop = FALSE], median)
deeplinedp.median.effort = rbind(deeplinedp.median.effort,c('DeepLineDP',medians))
colnames(deeplinedp.median.effort) = colnames(deeplinedp.effort.result)
dp = gather(deeplinedp.median.effort, key = "threshold", value = "DeepLineDP")
dp = dp[-1, , drop = FALSE]

splice.f.median.effort = data.frame(matrix(nrow = 0, ncol = ncol(splice.f.effort.result)))
medians = sapply(splice.f.effort.result[,-1,drop = FALSE], median)
splice.f.median.effort = rbind(splice.f.median.effort,c('SPLICE-F',medians))
colnames(splice.f.median.effort) = colnames(splice.f.recall.result)
splice.f = gather(splice.f.median.effort, key = "threshold", value = "SPLICE-F")
splice.f = splice.f[-1, , drop = FALSE]

glance.md$DeepLineDP = dp$DeepLineDP
glance.md$SPLICE_F = splice.f$`SPLICE-F`

write.csv(glance.md, file = './Dis_result/all_different_threshold_median_effort.csv')

################################## mean recall #############################################
glance.md.mean.recall = data.frame(matrix(nrow = 0, ncol = ncol(glance.md.recall.result)))
means = sapply(glance.md.recall.result[,-1,drop = FALSE], mean)
glance.md.mean.recall = rbind(glance.md.mean.recall,c('GLANCE-MD',means))
colnames(glance.md.mean.recall) = colnames(glance.md.recall.result)
glance.md = gather(glance.md.mean.recall, key = "threshold", value = "GLANCE-MD")
glance.md = glance.md[-1, , drop = FALSE]

deeplinedp.mean.recall = data.frame(matrix(nrow = 0, ncol = ncol(deeplinedp.recall.result)))
means = sapply(deeplinedp.recall.result[,-1,drop = FALSE], mean)
deeplinedp.mean.recall = rbind(deeplinedp.mean.recall,c('DeepLineDP',means))
colnames(deeplinedp.mean.recall) = colnames(deeplinedp.recall.result)
dp = gather(deeplinedp.mean.recall, key = "threshold", value = "DeepLineDP")
dp = dp[-1, , drop = FALSE]

splice.f.mean.recall = data.frame(matrix(nrow = 0, ncol = ncol(splice.f.recall.result)))
means = sapply(splice.f.recall.result[,-1,drop = FALSE], mean)
splice.f.mean.recall = rbind(splice.f.mean.recall,c('SPLICE-F',means))
colnames(splice.f.mean.recall) = colnames(splice.f.recall.result)
splice.f = gather(splice.f.mean.recall, key = "threshold", value = "SPLICE-F")
splice.f = splice.f[-1, , drop = FALSE]

glance.md$DeepLineDP = dp$DeepLineDP
glance.md$SPLICE_F = splice.f$`SPLICE-F`

write.csv(glance.md, file = './Dis_result/all_different_threshold_mean_recall.csv')

################################## mean effort ############################################
glance.md.mean.effort = data.frame(matrix(nrow = 0, ncol = ncol(glance.md.effort.result)))
means = sapply(glance.md.effort.result[,-1,drop = FALSE], mean)
glance.md.mean.effort = rbind(glance.md.mean.effort,c('GLANCE-MD',means))
colnames(glance.md.mean.effort) = colnames(glance.md.effort.result)
glance.md = gather(glance.md.mean.effort, key = "threshold", value = "GLANCE-MD")
glance.md = glance.md[-1, , drop = FALSE]

deeplinedp.mean.effort = data.frame(matrix(nrow = 0, ncol = ncol(deeplinedp.effort.result)))
means = sapply(deeplinedp.effort.result[,-1,drop = FALSE], mean)
deeplinedp.mean.effort = rbind(deeplinedp.mean.effort,c('DeepLineDP',means))
colnames(deeplinedp.mean.effort) = colnames(deeplinedp.recall.result)
dp = gather(deeplinedp.mean.effort, key = "threshold", value = "DeepLineDP")
dp = dp[-1, , drop = FALSE]

splice.f.mean.effort = data.frame(matrix(nrow = 0, ncol = ncol(splice.f.effort.result)))
means = sapply(splice.f.effort.result[,-1,drop = FALSE], mean)
splice.f.mean.effort = rbind(splice.m.mean.effort,c('SPLICE-F',means))
colnames(splice.f.mean.effort) = colnames(splice.f.recall.result)
splice.f = gather(splice.f.mean.effort, key = "threshold", value = "SPLICE-F")
splice.f = splice.f[-1, , drop = FALSE]

glance.md$DeepLineDP = dp$DeepLineDP
glance.md$SPLICE_F = splice.f$`SPLICE-F`

write.csv(glance.md, file = './Dis_result/all_different_threshold_mean_effort.csv')
