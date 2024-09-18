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

save.fig.dir = 'D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/major_revision/figures/RQ1_figures/'

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


prediction_dir = 'D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/within-release/'


all_files = list.files(prediction_dir)

df_all <- NULL

for(f in all_files)
{
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}

CEandNFCdir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/Glance_MD_full_threshold/line_result/test/"

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

# ---------------- Code for RQ1 -----------------------#
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, file.level.ground.truth == "True" & prediction.prob >= 0.5 &  is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)


all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0', 
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2', 
                      'hive-0.12.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')


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

sorted = sum_line_attn %>% filter(is.comment.line== "False") %>% filter( NFC > 0) %>% group_by(test, filename) %>% 
  arrange(-attention_score, .by_group=TRUE) %>% 
  mutate(order = row_number())%>% mutate(totalSLOC = n()) %>% 
  select(test,filename,line.number,attention_score,rank,NFC,order,totalSLOC) %>% 
  mutate(unique_attention_scores = n_distinct(attention_score)) %>% mutate(differentiate_rate = round(unique_attention_scores / totalSLOC, digits = 3)) %>%
  distinct(test, filename, differentiate_rate)


sorted_SPLICE_G = sum_line_attn %>% filter(is.comment.line== "False") %>% filter( NFC > 0) %>% group_by(test, filename)  %>% 
  mutate(order = rank)%>% 
  mutate(totalSLOC = n())%>%
  mutate(unique_attention_scores = n_distinct(num_tokens*(NFC + 1))) %>% mutate(differentiate_rate = round(unique_attention_scores / totalSLOC, digits = 3)) %>%
  distinct(test, filename, differentiate_rate)

sorted_SPLICE_F = sum_line_attn %>% filter(is.comment.line== "False") %>% filter( NFC > 0) %>% group_by(test, filename) %>%  filter(NFC>0) %>%
  arrange(-attention_score*num_tokens*NFC, .by_group=TRUE) %>% 
  mutate(order = row_number())%>% mutate(totalSLOC = n())%>%
  mutate(unique_attention_scores = n_distinct(attention_score*num_tokens*NFC)) %>% mutate(differentiate_rate = round(unique_attention_scores / totalSLOC, digits = 3)) %>%
  distinct(test, filename, differentiate_rate)


sum_deepline.dp.line.result = sorted %>% group_by(test) %>% summarise(differentiate_ability = median(differentiate_rate))
sum_splice_G.line.result = sorted_SPLICE_G %>% group_by(test) %>% summarise(differentiate_ability = median(differentiate_rate))
sum_splice_F.line.result = sorted_SPLICE_F %>% group_by(test) %>% summarise(differentiate_ability = median(differentiate_rate))


names(sum_deepline.dp.line.result) = c("release", "differentiate")
names(sum_splice_G.line.result) = c("release", "differentiate")
names(sum_splice_F.line.result) = c("release", "differentiate")


sum_deepline.dp.line.result$technique = 'DeepLineDP'
sum_splice_G.line.result$technique = 'GLANCE'
sum_splice_F.line.result$technique = 'SPLICE-F'


all.line.result = rbind(sum_deepline.dp.line.result,sum_splice_G.line.result, sum_splice_F.line.result)

differentiate.result.df = select(all.line.result, c('technique', 'differentiate'))


differentiate.result.df = preprocess(differentiate.result.df, FALSE)



ggplot(differentiate.result.df, aes(x=reorder(variable, -value, FUN=median), y=value)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 1.5, color = "red", position = position_dodge(0.75)) + facet_grid(~rank, drop=TRUE, scales = "free", space = "free") + ylab("Discriminative power") + xlab("") + theme(axis.text.x=element_text(angle = -60, hjust = 0))

ggsave(paste0(save.fig.dir, "Discriminative power.pdf"),width=7,height=2.5)

