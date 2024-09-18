# The replication kit of Experiment


## Datasets

The datasets are obtained from Wattanakriengkrai et. al. The datasets contain 32 software releases across 9 software projects. The [datasets](https://github.com/awsm-research/line-level-defect-prediction) that we used in our experiment can be found in this github .

The file-level datasets (in the File-level directory) contain the following columns

- `File`: A file name of source code
- `Bug`: A label indicating whether source code is clean or defective
- `SRC`: A content in source code file

The line-level datasets (in the Line-level directory) contain the following columns

- `File`: A file name of source code
- `Line_number`: A line number where source code is modified
- `SRC`: An actual source code that is modified

## Floders Introduction
**Baseline-result** contains all baseline models in our study, it has several subdirectories as follows：

  **DeepLineDP** contains the following directory
  
  - output: This directory contains the following sub-directories:
      - `loss`: This directory stores training and validation loss
      - `model`: This directory stores trained models
      - `prediction`: This directory stores prediction (in CSV files) obtained from the trained models
      - `Word2Vec_mode`l: This directory stores word2vec models of each software project
  - script: This directory contains the following directories and files:
      - `preprocess_data.py`: The source code used to preprocess datasets for file-level model training and evaluation
      - `export_data_for_line_level_baseline.py`: The source code used to prepare data for N-gram and PMD
      - `my_util.py`: The source code used to store utility functions
      - `train_word2vec.py`: The source code used to train word2vec models
      - `DeepLineDP_model.py`: The source code that stores DeepLineDP architecture
      - `train_model.py`: The source code used to train DeepLineDP models
      - `generate_prediction.py`: The source code used to generate prediction (for RQ1-RQ3)
  
  **GLANCE** contains the following directory
  
  - `src`: This folder stores the source code of GLANCE written in Python.

  **ngram**  stores the source code of N-gram written in Java

  **PMD** stores the source code of PMD written in Python 

## Environment Setup

### Python Environment Setup

1.clone the github respository by using the following command:

    git clone <https://github.com/yuFeiCode/LL-SDP.git>

2.download the datasets from the this [github](https://github.com/awsm-research/line-level-defect-prediction) and keep it in `SPLICE/Baseline-result/datasets/original/`

3.use the following command to install required libraries in conda environment

    conda env create -f requirements.yml
    
    conda activate DeepLineDP_env

4.install PyTorch library by following the instruction from this link (the installation instruction may vary based on OS and CUDA version)

### R Environment Setup

Download the following package: `tidyverse`, `gridExtra`, `ModelMetrics`, `caret`, `reshape2`, `pROC`, `effsize`, `ScottKnottESD`

## Execution commands

### **As for DeepLineDP**

1.run the command to prepare data for file-level model training. The output will be stored in`./datasets/preprocessed_data`

    python preprocess_data.py

2.run the command to prepare data for n-gram and PMD
    
    python export_data_for_line_level_baseline.py
    
3.to train Word2Vec models, run the following command:

    python train_word2vec.py <DATASET_NAME>

Where <DATASET_NAME> is one of the following: `activemq`, `camel`, `derby`, `groovy`, `hbase`, `hive`, `jruby`, `lucene, wicket`

4.to train DeepLineDP models, run the following command:

    python train_model.py -dataset <DATASET_NAME>

The trained models will be saved in `./output/model/DeepLineDP/<DATASET_NAME>/`, and the loss will be saved in `../output/loss/DeepLineDP/<DATASET_NAME>-loss_record.csv`

5.to make a prediction of each software release, run the following command:

    python generate_prediction.py -dataset <DATASET_NAME>

The generated output is a csv file which contains the following information:

- `project`: A software project, as specified by <DATASET_NAME>
- `train`: A software release that is used to train DeepLineDP models
- `test`: A software release that is used to make a prediction
- `filename`: A file name of source code
- `file-level-ground-truth`: A label indicating whether source code is clean or defective
- `prediction-prob`: A probability of being a defective file
- `prediction-label`: A prediction indicating whether source code is clean or defective
- `line-number`: A line number of a source code file
- `line-level-ground-truth`: A label indicating whether the line is modified
- `is-comment-line`: A flag indicating whether the line is comment
- `token`: A token in a code line
- `token-attention-score`: An attention score of a token
- `line-attebtion-score`: An attention score of a line

The generated output is stored in `./output/prediction/DeepLineDP/within-release/`

### **As for GLANCE and LineDP**

In order to make it easier to obtain the result of GLANCE and Linedp, you can enter the GLANCE folder and run it according to the following command regulation.

    python main.py

### **As for N-gram**

You can enter the ngram folder and run `n_gram.java`

### **As for PMD**
You need first downlaod the `pmd-dist-7.0.0-rc4-bin` from this [website](https://pmd.github.io/) and keep it in `SPLICE/Baseline-result/PMD/`, then you can get the resut of PMD according the following command regulation.

    python runForPMD.py
   
## Obtaining the Evaluation Result

### **Data preparation**

after you get the result of GLANCE and LineDP, you need create the following new folder:
 
   - `SPLICE/Baseline-result/GLANCE/result/BASE-Glance-EA/line_result/test/`
   
   - `SPLICE/Baseline-result/GLANCE/result/BASE-Glance-MD/line_result/test/`
   
   - `SPLICE/Baseline-result/GLANCE/result/BASE-Glance-LR/line_result/test/`
   
   - `SPLICE/Baseline-result/GLANCE/result/Glance_MD_full_threshold/line_result/test/`
   
   - `SPLICE/Baseline-result/GLANCE/result/MIT-LineDP/line_result/test/`

The new `test` folder contains 14 evaluate output files corresponding to each models. For example, for `SPLICE/Baseline-result/GLANCE/result/BASE-Glance-EA/line_result/test/`, the folder stores the 14 csv files result of `GLANCE-EA` from `SPLICE/Baseline-result/GLANCE/result/BASE-Glance-MD/line_result/` for the respective releases:

    'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0', 'camel-2.10.0', 'camel-2.11.0' , 'derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2', 'hive-0.12.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1', 'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3'


### **Obtaining Result**

Run `RQ1_Compare.R`, `RQ2_Ablation.R`, `RQ3_Hit_and_Over.R` and `Dis-6.2-influence-of-threshold.R`to get the result of RQ1, RQ2, 

RQ3, Dis-6.2 and Dis-6.3 (may run in IDE or by the following command)

    Rscript  RQ1_Compare.R

the result are figures that are sorted in `./RQ1_result`, `./RQ1_result`, `./RQ1_result`, `./Dis_result`

Dis-6.4 result： 
We also conducted additional experiments using datasets  The [datasets](https://github.com/Naplues/BugDet/tree/master/Dataset) that we used in our experiment can be found in this github

the releases we used in Dis-6.4 :

    'ambari-2.4.0', 'ambari-2.5.0', 'ambari-2.6.0', 'ambari-2.7.0', 'calcite-1.15.0', 'calcite-1.16.0', 'calcite-1.17.0', 'calcite-1.18.0', 'groovy-2.4.6', 'groovy-2.4.8', 'groovy-2.5.0', 'groovy-2.5.5', 'mng-3.2.0', 'mng-3.3.0', 'mng-3.5.0', 'mng-3.6.0', 'nifi-0.4.0', 'nifi-1.2.0', 'nifi-1.5.0', 'nifi-1.8.0', 'nutch-1.12', 'nutch-1.13', 'nutch-1.14', 'nutch-1.15', 'zookeeper-3.4.6', 'zookeeper-3.5.1', 'zookeeper-3.5.2', 'zookeeper-3.5.3'

To download the datasets into the ./Discuss/datasets folder and modify the datasets-path in the Baseline-result folder according to different configurations to generate extended results for all baseline results.

## Contact us

**Mail**: zyf@stu.gxnu.edu.cn
