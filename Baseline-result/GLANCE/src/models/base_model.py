# -*- coding:utf-8 -*-

import math

import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

from src.utils.config import USE_CACHE
from src.utils.helper import *
from sklearn import metrics
from sklearn.linear_model import LogisticRegression


class BaseModel(object):
    model_name = 'BaseModel'  # need to be rewrite in subclass

    def __init__(self, train_release: str = '', test_release: str = '', test_result_path='', is_realistic=False):
        # Specific the actual name of each folder or file if any.
        self.result_path = test_result_path if test_result_path != '' else f'{result_path}/{self.model_name}/'

        # Folder path: file_result; line_result; buggy_density
        self.file_level_result_path = f'{self.result_path}file_result/'
        self.line_level_result_path = f'{self.result_path}line_result/'
        self.buggy_density_path = f'{self.result_path}buggy_density/'

        # Evaluation result path
        self.file_level_evaluation_file = f'{self.file_level_result_path}evaluation.csv'
        self.line_level_evaluation_file = f'{self.line_level_result_path}evaluation.csv'
        self.execution_time_file = f'{self.result_path}time.csv'

        # Model configuration info
        self.project_name = train_release.split('-')[0]
        np.random.seed(0)
        self.random_state = 0
        self.threshold_effort = 0.2

        # Model training and test release information
        self.train_release = train_release
        self.test_release = test_release

        # File level classifier
        self.vector = CountVectorizer(lowercase=False, min_df=2)
        self.clf = LogisticRegression(random_state=0)

        # File level data reading
        # Only use data available now as training data
        if is_realistic:
            self.train_text, self.train_text_lines, self.train_label, self.train_filename, self.train_line_numbers= read_file_level_dataset(
                train_release, file_path=f'{root_path}datasets/original/File-level/')
        else:
            self.train_text, self.train_text_lines, self.train_label, self.train_filename, self.train_line_numbers= read_file_level_dataset(
                train_release)      
        self.test_text, self.test_text_lines, self.test_labels, self.test_filename, self.test_line_numbers= read_file_level_dataset(
            test_release)

        # result file path
        self.file_level_result_file = f'{self.file_level_result_path}{self.project_name}/{self.test_release}-result.csv'
        self.line_level_result_file = f'{self.line_level_result_path}{self.project_name}/{self.test_release}-result.csv'
        self.buggy_density_file = f'{self.buggy_density_path}{self.test_release}-density.csv'
        self.commit_buggy_path = f'{dataset_path}{self.test_release.split("-")[0]}'

       
        self.init_file_path()

        
        self.test_pred_labels = []
        self.test_pred_scores = []
        self.test_pred_density = dict()

        
        self.oracle_line_dict, self.oracle_line_set = self.get_oracle_lines()
        self.predicted_buggy_lines = []
        self.predicted_buggy_score = []
        self.predicted_density = []
        
        
        self.rank = []
        self.predicted_buggy_line_numbers = []
        self.file_level_label =[]
        self.functioncall = []  
        self.controlelements = [] 

        
        self.num_total_lines = sum([len(lines) for lines in self.test_text_lines])
        self.num_actual_buggy_lines = len(self.oracle_line_set)  # TP + FN

        # NOTE need to be written in the method of line_level_prediction of subclass
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # TP + FP

    def init_file_path(self):
        
        # Create directory for each folder
        make_path(self.result_path)
        make_path(self.file_level_result_path)
        make_path(self.line_level_result_path)
        make_path(self.buggy_density_path)
        make_path(f'{self.file_level_result_path}{self.project_name}/')
        make_path(f'{self.line_level_result_path}{self.project_name}/')

    def get_oracle_lines(self):
        # get buggy lines information
        oracle_line_dict, oracle_line_list = read_line_level_dataset(self.test_release), set()
        for file_name in oracle_line_dict:
            oracle_line_list.update([f'{file_name}:{line}' for line in oracle_line_dict[file_name]])
        return oracle_line_dict, oracle_line_list

    # ====================================================================================================
    # Buggy files and lines prediction
    # ====================================================================================================
    def file_level_prediction(self):
        """
        NOTE: This method should be implemented by sub class (i.e., Glance)
        """
        print(f"Prediction\t=>\t{self.test_release}")
        if USE_CACHE and os.path.exists(self.file_level_result_file):
            return

        # 2. Convert text feature into numerical feature, classifier
        # Neither perform lowercase, stemming, nor lemmatization. Remove tokens that appear only once
        train_vtr = self.vector.fit_transform(self.train_text)
        test_vtr = self.vector.transform(self.test_text)
        # 3. Predict defective files, test_predictions
        self.clf.fit(train_vtr, self.train_label)

        self.test_pred_labels = self.clf.predict(test_vtr)
        # Obtain the prediction scores of each buggy lines.
        if self.model_name == 'MIT-TMI-SVM':  # NOTE may be removed later
            self.test_pred_scores = np.array([score for score in self.test_pred_labels])
        else:
            self.test_pred_scores = np.array([score[1] for score in self.clf.predict_proba(test_vtr)])

        # Save file level result
        self.save_file_level_result()

    def line_level_prediction(self):
        """
        NOTE: This method must be implemented by sub class
        """
        print(f'Line level prediction for: {self.model_name}')
        pass

    # ====================================================================================================
    # Analyze the predicted buggy files and lines
    # NOTE: The below methods should be inherited rather than reimplemented by sub class.
    # ====================================================================================================
    def analyze_file_level_result(self):
        """
        Checked OK.
        Require: self.test_pred_labels
        Require: self.test_pred_scores
        """
        self.load_file_level_result()

        total_file, identified_file, total_line, identified_line, predicted_file, predicted_line = 0, 0, 0, 0, 0, 0

        for index in range(len(self.test_labels)):
            buggy_line = len(self.test_text_lines[index])
            if self.test_pred_labels[index] == 1:
                predicted_file += 1
                predicted_line += buggy_line

        for index in range(len(self.test_labels)):
            if self.test_labels[index] == 1:
                buggy_line = len(self.oracle_line_dict[self.test_filename[index]])
                if self.test_pred_labels[index] == 1:
                    identified_line += buggy_line
                    identified_file += 1
                total_line += buggy_line
                total_file += 1

        print(f'Buggy file hit info: {identified_file}/{total_file} - {round(identified_file / total_file * 100, 1)}%')
        print(f'Buggy line hit info: {identified_line}/{total_line} - {round(identified_line / total_line * 100, 1)}%')
        print(f'Predicted {predicted_file} buggy files contain {predicted_line} lines')

        # File level evaluation append file
        append_title = True if not os.path.exists(self.file_level_evaluation_file) else False
        title = 'release,precision,recall,f1-score,accuracy,mcc,identified/total files,max identified/total lines\n'
        with open(self.file_level_evaluation_file, 'a') as file:
            file.write(title) if append_title else None
            file.write(f'{self.test_release},'
                       f'{metrics.precision_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.recall_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.f1_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.accuracy_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.matthews_corrcoef(self.test_labels, self.test_pred_labels)},'
                       f'{identified_file}/{total_file},'
                       f'{identified_line}/{total_line},'
                       f'\n')
        return

    def analyze_line_level_result(self):
        """
        Checked OK.
        Require: self.test_pred_labels
        Require: self.predicted_buggy_lines
        Require: self.predicted_buggy_score
        Require: self.predicted_density
        Require: self.num_total_lines
        Require: self.num_actual_buggy_lines
        """
        ################################# Loading file level prediction result #################################
        self.load_file_level_result()
        total_lines_in_defective_files, buggy_lines_in_defective_files = 0, 0
        for index in range(len(self.test_pred_labels)):
            # Statistics in predicted defective files
            if self.test_pred_labels[index] == 1:
                total_lines_in_defective_files += len(self.test_text_lines[index])
                if self.test_labels[index] == 1:
                    buggy_lines_in_defective_files += len(self.oracle_line_dict[self.test_filename[index]])

        # TODO The below assignment statements should be commented 
        # self.num_total_lines = total_lines_in_defective_files  # TP + FP + TN + FN
        # self.num_actual_buggy_lines = buggy_lines_in_defective_files  # TP + FN

        ############################### Loading predicted lines, scores, and density ##############################
        self.load_line_level_result()

        ##################### Classification performance Performance Performance Indicators ################
        tp = len(self.oracle_line_set.intersection(self.predicted_buggy_lines))
        fp = self.num_predict_buggy_lines - tp
        fn = self.num_actual_buggy_lines - tp
        tn = self.num_total_lines - tp - fp - fn
        print(f'Total lines: {self.num_total_lines}\n'
              f'Buggy lines: {self.num_actual_buggy_lines}\n'
              f'Predicted lines: {len(self.predicted_buggy_lines)}\n'
              f'TP: {tp}, FP: {fp}, FN: {fn}, TN: {tn}')

        prec = .0 if tp + fp == .0 else tp / (tp + fp)
        recall = .0 if tp + fn == .0 else tp / (tp + fn)
        far = .0 if fp + tn == 0 else fp / (fp + tn)
        ce = .0 if fn + tn == .0 else fn / (fn + tn)

        d2h = math.sqrt(math.pow(1 - recall, 2) + math.pow(0 - far, 2)) / math.sqrt(2)
        mcc = .0 if tp + fp == .0 or tp + fn == .0 or tn + fp == .0 or tn + fn == .0 else \
            (tp * tn - fp * fn) / math.sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

        x, y = tp + fp, tp
        n, N = self.num_actual_buggy_lines, self.num_total_lines

        ER = .0 if (y * N) == .0 else (y * N - x * n) / (y * N)
        RI = .0 if (x * n) == 0 else (y * N - x * n) / (x * n)

        ################################ Ranking performance Performance Performance Indicators ################################################
        ifa, r_20 = self.rank_strategy()  # Strategy 1

        ################################ Bug hit ratio ################################################
        # buggy_lines_dict = read_dict_from_file(f'{self.commit_buggy_path}/{self.test_release}_commit_buggy_lines.csv')
        buggy_lines_dict = {}  # = read_dict_from_file(f'{self.commit_buggy_path}/{self.test_release}_commit_buggy_lines.csv')
        total_bugs = len(buggy_lines_dict.keys())
        hit_bugs = set() 
        for line in self.predicted_buggy_lines:
            for bug_commit, lines in buggy_lines_dict.items():
                if line in lines:
                    hit_bugs.add(bug_commit)

        ratio = 0 if total_bugs == 0 else round(len(hit_bugs) / total_bugs, 3)

        ################################ Output the evaluation result ################################################
        append_title = True if not os.path.exists(self.line_level_evaluation_file) else False
        title = 'release,precision,recall,far,ce,d2h,mcc,ifa,recall_20,ER,RI,ratio\n'
        with open(self.line_level_evaluation_file, 'a') as file:
            file.write(title) if append_title else None
            file.write(f'{self.test_release},{prec},{recall},{far},{ce},{d2h},{mcc},{ifa},{r_20},{ER},{RI},{ratio}\n')
        return

    def rank_strategy(self):
        """
        Checked OK.
        Two-stage ranking strategy.
        (1) Rank all predicted defective files according to the buggy density;
        (2) Rank all buggy lines according to the score of each line in each file.
        Require: self.test_pred_density
        Require: self.predicted_buggy_lines
        Require: self.predicted_buggy_score
        Require: self.num_total_lines
        :return: ifa, recall_20
        """
        ranked_predicted_buggy_lines = []
        # A list of buggy density for each file in test set.
        test_pred_density = [self.test_pred_density[filename] for filename in self.test_filename]

        # Indices of defective files in descending order according to the prediction density.
        defective_file_index = [i for i in np.argsort(test_pred_density)[::-1] if self.test_pred_labels[i] == 1]

        for i in range(len(defective_file_index)):
            # The filename of predicted defective files in test set.
            defective_filename = self.test_filename[defective_file_index[i]]

            temp_lines, temp_scores = [], []
            for index in range(len(self.predicted_buggy_lines)):
                if self.predicted_buggy_lines[index].startswith(defective_filename):
                    temp_lines.append(self.predicted_buggy_lines[index])
                    temp_scores.append(self.predicted_buggy_score[index])

            # Indices of buggy lines in descending order according to scores in each file.
            sorted_index = np.argsort(temp_scores)[::-1]
            ranked_predicted_buggy_lines.extend(list(np.array(temp_lines)[sorted_index]))

        max_effort = int(self.num_total_lines * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')

        # Calculate the performance values of IFA and Recall@20%.
        return self.get_rank_performance(ranked_predicted_buggy_lines)

    def get_rank_performance(self, ranked_predicted_buggy_lines):
        """
        Require: self.num_total_lines
        Require: self.threshold_effort
        Require: self.actual_buggy_lines
        Require: self.num_actual_buggy_lines
        :param ranked_predicted_buggy_lines:
        :return:
        """
        count, ifa, recall_20, max_effort = 0, 0, 0, int(self.num_total_lines * self.threshold_effort)
        
        for line in ranked_predicted_buggy_lines[:max_effort]:
            if line in self.oracle_line_set:
                ifa = count if ifa == 0 else ifa
                recall_20 += 1
            count += 1
        return ifa, recall_20 / self.num_actual_buggy_lines
    


    # ============================================ File operation ======================================================
    def save_file_level_result(self):
        """
        Checked OK.
        Require: self.test_filename
        Require: self.test_labels
        Require: self.test_pred_labels
        Require: self.test_pred_scores
        Require: self.file_level_result_file
        :return:
        """
        data = {'filename': self.test_filename,
                'oracle': self.test_labels,
                'predicted_label': self.test_pred_labels,
                'predicted_score': self.test_pred_scores}
        data = pd.DataFrame(data, columns=['filename', 'oracle', 'predicted_label', 'predicted_score'])
        data.to_csv(self.file_level_result_file, index=False)

    def save_line_level_GLANCE_result(self):
        """
        Checked OK.
        Require: self.predicted_buggy_lines
        Require: self.predicted_buggy_score
        Require: self.predicted_density
        Require: self.line_level_result_file
        :return:
        """
       
        data = {'predicted_buggy_lines': self.predicted_buggy_lines,
                'predicted_buggy_line_numbers':self.predicted_buggy_line_numbers,
                'predicted_buggy_score': self.predicted_buggy_score,
                'predicted_density': self.predicted_density,  # NOTE may be removed later
                'rank':self.rank,
                'file_level_label':self.file_level_label,
                'functioncall':self.functioncall,
                'controlelements':self.controlelements} 
        data = pd.DataFrame(data, columns=['predicted_buggy_lines', 'predicted_buggy_score', 'predicted_density','predicted_buggy_line_numbers', 'rank','file_level_label','functioncall','controlelements'])
        data.to_csv(self.line_level_result_file, index=False)
    
    def save_line_level_LineDP_result(self):
        """
        Checked OK.
        Require: self.predicted_buggy_lines
        Require: self.predicted_buggy_score
        Require: self.predicted_density
        Require: self.line_level_result_file
        :return:
        """
        
        data = {'predicted_buggy_lines': self.predicted_buggy_lines,
                'predicted_buggy_score': self.predicted_buggy_score,
                'predicted_density': self.predicted_density,  # NOTE may be removed later
                'rank':self.rank,
                } 
        data = pd.DataFrame(data, columns=['predicted_buggy_lines', 'predicted_buggy_score', 'predicted_density', 'rank'])
        data.to_csv(self.line_level_result_file, index=False)

    def save_buggy_density_file(self):
        """
        Checked OK.
        Require: self.predicted_buggy_lines
        Require: self.predicted_buggy_score
        Require: self.predicted_density
        Require: self.line_level_result_file
        :return:
        """
        df = pd.read_csv(self.line_level_result_file)
        self.predicted_buggy_lines = list(df['predicted_buggy_lines'])  # only buggy lines in the defective files

        buggy_density, file_buggy_lines_dict = dict(), dict()
        
        for line in self.predicted_buggy_lines:
            filename = line.strip().split(':')[0]
            if not filename in file_buggy_lines_dict:
                file_buggy_lines_dict[filename] = 1
            else:
                file_buggy_lines_dict[filename] += 1
        
        for index in range(len(self.test_text_lines)):
            filename = self.test_filename[index]
            
            if filename not in file_buggy_lines_dict or len(self.test_text_lines[index]) == 0:
                buggy_density[filename] = 0
            else:
                buggy_density[filename] = file_buggy_lines_dict[filename] / len(self.test_text_lines[index])

        self.test_pred_density = buggy_density

        data = {'test_pred_density': self.test_pred_density}
        data = pd.DataFrame(data, columns=['test_pred_density'])
        data.to_csv(self.buggy_density_file, index=False)

    def load_file_level_result(self):
        # Load file prediction result if no result
        if len(self.test_pred_labels) == 0 or len(self.test_pred_scores) == 0:
            df = pd.read_csv(self.file_level_result_file)
            self.test_pred_labels = np.array(df['predicted_label'])
            self.test_pred_scores = np.array(df['predicted_score'])

    def load_line_level_result(self):
        if len(self.predicted_buggy_lines) == 0:
            df = pd.read_csv(self.line_level_result_file)
            self.predicted_buggy_lines = list(df['predicted_buggy_lines'])  # only buggy lines in the defective files
            self.predicted_buggy_score = list(df['predicted_buggy_score'])
            self.predicted_density = list(df['predicted_density'])
            self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.
            self.save_buggy_density_file()
