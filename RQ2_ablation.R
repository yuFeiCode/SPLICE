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

save.fig.dir = './RQ2_result/'
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
  groupname = x$variable %>% gsub("\\+", ".", .)  %>% gsub("-", ".", .)
  x$rank <- paste("Rank",ranking[as.character(groupname)])
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

df_CEandNF_all = select(df_CEandNF_all, "predicted_buggy_lines", "predicted_buggy_line_numbers","predicted_buggy_score", "rank", "functioncall", "controlelements", "test")
names(df_CEandNF_all) = c("filename", "line.number", "GLANCEscore", "rank", "NFC", "CE", "test")
df_CEandNF_all$filename = str_split_fixed(df_CEandNF_all$filename, ":", 2)[,1]

# ---------------- Code for ablation study -----------------------#

## prepare data for baseline
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, file.level.ground.truth == "True" & prediction.prob >= 0.5 &  is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)


###### Calculation of predictive performance of SPLICE-F and its components ######

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

sorted_without_attention = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-num_tokens*NFC, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_without_NFC = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score*num_tokens, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_without_NT = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score*NFC, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_attention = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-attention_score, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_NFC = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-NFC, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

sorted_NT = sum_line_attn %>% filter(is.comment.line== "False") %>% group_by(test, filename) %>%  arrange(-num_tokens, .by_group=TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())



## get result from DeepLineDP
# calculate IFA
IFA_SPLICE_F = sorted_SPLICE_F %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_without_attention = sorted_without_attention %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_without_NFC = sorted_without_NFC %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_without_NT = sorted_without_NT %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_attention = sorted_attention %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_NFC = sorted_NFC %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)
IFA_NT = sorted_NT %>% filter(line.level.ground.truth == "True") %>% group_by(test, filename) %>% top_n(1, -order)


IFA_SPLICE_F = IFA_SPLICE_F %>% arrange(test, filename)
IFA_without_attention = IFA_without_attention %>% arrange(test, filename)
IFA_without_NFC = IFA_without_NFC %>% arrange(test, filename)
IFA_without_NT = IFA_without_NT %>% arrange(test, filename)
IFA_attention = IFA_attention %>% arrange(test, filename)
IFA_NFC = IFA_NFC %>% arrange(test, filename)
IFA_NT = IFA_NT %>% arrange(test, filename)


total_true = sorted %>% group_by(test, filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
total_true = total_true %>% filter(total_true > 0)
total_true = total_true %>% arrange(test, filename)

# calculate Recall20%LOC
recall20LOC_SPLICE_F = sorted_SPLICE_F %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_without_attention = sorted_without_attention %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_without_NFC = sorted_without_NFC %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_without_NT = sorted_without_NT %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_attention = sorted_attention %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_NFC = sorted_NFC %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)

recall20LOC_NT = sorted_NT %>% group_by(test, filename) %>% mutate(effort = round(order/n(),digits = 2 )) %>% filter(effort <= 0.2) %>% summarize(correct_pred = sum(line.level.ground.truth == "True")) %>% merge(total_true) %>% mutate(recall20LOC = correct_pred/total_true)


recall20LOC_SPLICE_F = recall20LOC_SPLICE_F %>% arrange(test, filename)
recall20LOC_without_attention = recall20LOC_without_attention %>% arrange(test, filename)
recall20LOC_without_NFC = recall20LOC_without_NFC %>% arrange(test, filename)
recall20LOC_without_NT = recall20LOC_without_NT %>% arrange(test, filename)
recall20LOC_attention = recall20LOC_attention %>% arrange(test, filename)
recall20LOC_NFC = recall20LOC_NFC %>% arrange(test, filename)
recall20LOC_NT = recall20LOC_NT %>% arrange(test, filename)


# calculate Effort20%Recall

effort20Recall_SPLICE_F = sorted_SPLICE_F %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_without_attention = sorted_without_attention %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_without_NFC = sorted_without_NFC %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_without_NT = sorted_without_NT %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_attention = sorted_attention %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_NFC = sorted_NFC %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))

effort20Recall_NT = sorted_NT %>% merge(total_true)  %>% group_by(test, filename) %>% arrange(order, .by_group=TRUE) %>% mutate(cummulative_correct_pred = cumsum  (line.level.ground.truth == "True"), recall = round(cumsum(line.level.ground.truth == "True")/total_true, digits = 2)) %>% mutate(class = case_when((line.level.ground.truth == 'True' & recall <= 0.2) ~ order/n(),TRUE ~ 0)) %>%  summarize(effort20Recall = if_else(max(class)==0, sum(recall <= 0.2)/n(), max(class) ))



effort20Recall_SPLICE_F = effort20Recall_SPLICE_F %>% arrange(test, filename)
effort20Recall_without_attention = effort20Recall_without_attention %>% arrange(test, filename)
effort20Recall_without_NFC = effort20Recall_without_NFC %>% arrange(test, filename)
effort20Recall_without_NT = effort20Recall_without_NT %>% arrange(test, filename)
effort20Recall_attention = effort20Recall_attention %>% arrange(test, filename)
effort20Recall_NFC = effort20Recall_NFC %>% arrange(test, filename)
effort20Recall_NT = effort20Recall_NT %>% arrange(test, filename)


#added FPA：fault-percentile-average
FPA_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_without_attention = sorted_without_attention %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_without_NFC = sorted_without_NFC %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_without_NT = sorted_without_NT %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_attention = sorted_attention %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_NFC = sorted_NFC %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

FPA_NT = sorted_NT %>% merge(total_true) %>% group_by(test, filename)  %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )

 
FPA_SPLICE_F = FPA_SPLICE_F %>% arrange(test, filename)
FPA_without_attention = FPA_without_attention %>% arrange(test, filename)
FPA_without_NFC = FPA_without_NFC %>% arrange(test, filename)
FPA_without_NT = FPA_without_NT %>% arrange(test, filename)
FPA_attention = FPA_attention %>% arrange(test, filename)
FPA_NFC = FPA_NFC %>% arrange(test, filename)
FPA_NT = FPA_NT %>% arrange(test, filename)
 
 
top5_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_without_attention = sorted_without_attention %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_without_NFC = sorted_without_NFC %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_without_NT = sorted_without_NT %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_attention = sorted_attention %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_NFC = sorted_NFC %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

top5_NT = sorted_NT %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())

 
top5_SPLICE_F = top5_SPLICE_F %>% arrange(test, filename)
top5_without_attention = top5_without_attention %>% arrange(test, filename)
top5_without_NFC = top5_without_NFC %>% arrange(test, filename)
top5_without_NT = top5_without_NT %>% arrange(test, filename)
top5_attention = top5_attention %>% arrange(test, filename)
top5_NFC = top5_NFC %>% arrange(test, filename)
top5_NT = top5_NT %>% arrange(test, filename)
 
 
top10_SPLICE_F = sorted_SPLICE_F %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_without_attention = sorted_without_attention %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_without_NFC = sorted_without_NFC %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_without_NT = sorted_without_NT %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_attention = sorted_attention %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_NFC = sorted_NFC %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())
top10_NT = sorted_NT %>% merge(total_true) %>% group_by(test, filename) %>% filter(order <= 10)  %>%  summarize(top10 = sum(line.level.ground.truth == "True")/n())

 
top10_SPLICE_F = top10_SPLICE_F %>% arrange(test, filename)
top10_without_attention = top10_without_attention %>% arrange(test, filename)
top10_without_NFC = top10_without_NFC %>% arrange(test, filename)
top10_without_NT = top10_without_NT %>% arrange(test, filename)
top10_attention = top10_attention %>% arrange(test, filename)
top10_NFC = top10_NFC %>% arrange(test, filename)
top10_NT = top10_NT %>% arrange(test, filename)


## prepare data result ##
splice_F.ifa = IFA_SPLICE_F$order - 1
splice_F.recall = recall20LOC_SPLICE_F$recall20LOC
splice_F.effort = effort20Recall_SPLICE_F$effort20Recall
splice_F.fpa = FPA_SPLICE_F$FPA
splice_F.top5= top5_SPLICE_F$top5
splice_F.top10= top10_SPLICE_F$top10
splice_F.line.result = data.frame(IFA_SPLICE_F$filename, splice_F.ifa, splice_F.recall, splice_F.effort, splice_F.fpa, IFA_SPLICE_F$test, splice_F.top5, splice_F.top10, IFA_SPLICE_F$totalSLOC)

 
without_attention.ifa = IFA_without_attention$order - 1
without_attention.recall = recall20LOC_without_attention$recall20LOC
without_attention.effort = effort20Recall_without_attention$effort20Recall
without_attention.fpa = FPA_without_attention$FPA
without_attention.top5= top5_without_attention$top5
without_attention.top10= top10_without_attention$top10
without_attention.line.result = data.frame(IFA_without_attention$filename, without_attention.ifa, without_attention.recall, without_attention.effort, without_attention.fpa, IFA_without_attention$test, without_attention.top5, without_attention.top10, IFA_without_attention$totalSLOC)


without_NFC.ifa = IFA_without_NFC$order - 1
without_NFC.recall = recall20LOC_without_NFC$recall20LOC
without_NFC.effort = effort20Recall_without_NFC$effort20Recall
without_NFC.fpa = FPA_without_NFC$FPA
without_NFC.top5= top5_without_NFC$top5
without_NFC.top10= top10_without_NFC$top10
without_NFC.line.result = data.frame(IFA_without_NFC$filename, without_NFC.ifa, without_NFC.recall, without_NFC.effort, without_NFC.fpa, IFA_without_NFC$test, without_NFC.top5, without_NFC.top10, IFA_without_NFC$totalSLOC)


without_NT.ifa = IFA_without_NT$order - 1
without_NT.recall = recall20LOC_without_NT$recall20LOC
without_NT.effort = effort20Recall_without_NT$effort20Recall
without_NT.fpa = FPA_without_NT$FPA
without_NT.top5= top5_without_NT$top5
without_NT.top10= top10_without_NT$top10
without_NT.line.result = data.frame(IFA_without_NT$filename, without_NT.ifa, without_NT.recall, without_NT.effort, without_NT.fpa, IFA_without_NT$test, without_NT.top5, without_NT.top10, IFA_without_NT$totalSLOC)

attention.ifa = IFA_attention$order - 1
attention.recall = recall20LOC_attention$recall20LOC
attention.effort = effort20Recall_attention$effort20Recall
attention.fpa = FPA_attention$FPA
attention.top5= top5_attention$top5
attention.top10= top10_attention$top10
attention.line.result = data.frame(IFA_attention$filename, attention.ifa, attention.recall, attention.effort, attention.fpa, IFA_attention$test, attention.top5, attention.top10, IFA_attention$totalSLOC)

NFC.ifa = IFA_NFC$order - 1
NFC.recall = recall20LOC_NFC$recall20LOC
NFC.effort = effort20Recall_NFC$effort20Recall
NFC.fpa = FPA_NFC$FPA
NFC.top5= top5_NFC$top5
NFC.top10= top10_NFC$top10
NFC.line.result = data.frame(IFA_NFC$filename, NFC.ifa, NFC.recall, NFC.effort, NFC.fpa, IFA_NFC$test, NFC.top5, NFC.top10, IFA_NFC$totalSLOC)

NT.ifa = IFA_NT$order - 1
NT.recall = recall20LOC_NT$recall20LOC
NT.effort = effort20Recall_NT$effort20Recall
NT.fpa = FPA_NT$FPA
NT.top5= top5_NT$top5
NT.top10= top10_NT$top10
NT.line.result = data.frame(IFA_NT$filename, NT.ifa, NT.recall, NT.effort, NT.fpa, IFA_NT$test, NT.top5, NT.top10, IFA_NT$totalSLOC)


sum_splice_F.line.result = splice_F.line.result %>%   summarize(IFA=median(splice_F.ifa),recall=median(splice_F.recall),effort=median(splice_F.effort), fpa=median(splice_F.fpa), top5=mean(splice_F.top5), top10=mean(splice_F.top10), .by=IFA_SPLICE_F.test)

sum_without_attention.line.result = without_attention.line.result %>%   summarize(IFA=median(without_attention.ifa),recall=median(without_attention.recall),effort=median(without_attention.effort), fpa=median(without_attention.fpa), top5=mean(without_attention.top5), top10=mean(without_attention.top10), .by=IFA_without_attention.test)

sum_without_NFC.line.result = without_NFC.line.result %>%   summarize(IFA=median(without_NFC.ifa),recall=median(without_NFC.recall),effort=median(without_NFC.effort), fpa=median(without_NFC.fpa), top5=mean(without_NFC.top5), top10=mean(without_NFC.top10), .by=IFA_without_NFC.test)

sum_without_NT.line.result = without_NT.line.result %>%   summarize(IFA=median(without_NT.ifa),recall=median(without_NT.recall),effort=median(without_NT.effort), fpa=median(without_NT.fpa), top5=mean(without_NT.top5), top10=mean(without_NT.top10), .by=IFA_without_NT.test)

sum_attention.line.result = attention.line.result %>%   summarize(IFA=median(attention.ifa),recall=median(attention.recall),effort=median(attention.effort), fpa=median(attention.fpa), top5=mean(attention.top5), top10=mean(attention.top10), .by=IFA_attention.test)
 
sum_NFC.line.result = NFC.line.result %>%   summarize(IFA=median(NFC.ifa),recall=median(NFC.recall),effort=median(NFC.effort), fpa=median(NFC.fpa), top5=mean(NFC.top5), top10=mean(NFC.top10), .by=IFA_NFC.test)

sum_NT.line.result = NT.line.result %>%   summarize(IFA=median(NT.ifa),recall=median(NT.recall),effort=median(NT.effort), fpa=median(NT.fpa), top5=mean(NT.top5), top10=mean(NT.top10), .by=IFA_NT.test)



names(sum_splice_F.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_without_attention.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_without_NFC.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_without_NT.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_attention.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_NFC.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")
names(sum_NT.line.result) = c("release", "IFA", "Recall20%LOC", "Effort@20%Recall", "FPA", "top5", "top10")

 
sum_splice_F.line.result$technique = 'SPLICE-F'
sum_without_attention.line.result$technique = 'NFC+NT'
sum_without_NFC.line.result$technique = 'AttentionScore+NT'
sum_without_NT.line.result$technique = 'AttentionScore+NFC'
sum_attention.line.result$technique = 'AttentionScore'
sum_NFC.line.result$technique = 'NFC'
sum_NT.line.result$technique = 'NT'


all.line.result = rbind(sum_splice_F.line.result, sum_without_attention.line.result, sum_without_NFC.line.result, sum_without_NT.line.result, sum_attention.line.result, sum_NFC.line.result, sum_NT.line.result)

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


recall.median = recall.result.df %>% group_by(variable)  %>% summarize(recall = median(value) )
effort.median = effort.result.df %>% group_by(variable)  %>% summarize(effort = median(value) )
ifa.median = ifa.result.df %>% group_by(variable)  %>% summarize(ifa = median(value) )
fpa.median = fpa.result.df %>% group_by(variable)  %>% summarize(fpa = median(value) )
top5.median = top5.result.df %>% group_by(variable)  %>% summarize(top5 = median(value) )
top10.median = top10.result.df %>% group_by(variable)  %>% summarize(top10 = median(value) )
allMedian = data.frame(recall.median,  effort=effort.median$effort, ifa=ifa.median$ifa, fpa=fpa.median$fpa, top5=top5.median$top5, top10=top10.median$top10)

recall.mean = recall.result.df %>% group_by(variable)  %>% summarize(recall = mean(value) )
effort.mean = effort.result.df %>% group_by(variable)  %>% summarize(effort = mean(value) )
ifa.mean = ifa.result.df %>% group_by(variable)  %>% summarize(ifa = mean(value) )
fpa.mean = fpa.result.df %>% group_by(variable)  %>% summarize(fpa = mean(value) )
top5.mean = top5.result.df %>% group_by(variable)  %>% summarize(top5 = mean(value) )
top10.mean = top10.result.df %>% group_by(variable)  %>% summarize(top10 = mean(value) )
allMean = data.frame(recall.mean,  effort=effort.mean$effort, ifa=ifa.mean$ifa, fpa=fpa.mean$fpa, top5=top5.mean$top5, top10=top10.mean$top10)

write.csv(file=paste0(save.fig.dir, "ablation-median.csv"), allMedian)
write.csv(file=paste0(save.fig.dir, "ablation-mean.csv"), allMean)


 
