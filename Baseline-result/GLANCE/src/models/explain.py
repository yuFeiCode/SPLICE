# -*- coding:utf-8 -*-

import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import LinearSVC
from sklearn.naive_bayes import MultinomialNB
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier

from src.models.base_model import BaseModel
from src.utils.config import USE_CACHE
from src.utils.helper import *

from lime.lime_text import LimeTextExplainer


class LineDP(BaseModel):
    model_name = 'MIT-LineDP'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.vector = CountVectorizer(lowercase=False, min_df=2)
        self.clf = LogisticRegression(random_state=0)

    def line_level_prediction(self):
        """
        Checked OK.
        Require: self.test_pred_labels
        Require: self.test_pred_scores
        :return:
        """
        super(LineDP, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        # Load file prediction result if no result
        if len(self.test_pred_labels) == 0 or len(self.test_pred_scores) == 0:
            df = pd.read_csv(self.file_level_result_file)
            self.test_pred_labels = np.array(df['predicted_label'])
            self.test_pred_scores = np.array(df['predicted_score'])

        print(f'Predicting line level defect prediction of {self.model_name}')
        # Buggy lines 
        predicted_lines, predicted_score, predicted_density,rank = [], [], [], []

        # Indices of defective files in descending order according to scores. Note the rank step may be removed.
        defective_file_index = [i for i in np.argsort(self.test_pred_scores)[::-1] if self.test_pred_labels[i] == 1]

        # Text tokenizer
        tokenizer = self.vector.build_tokenizer()
        c = make_pipeline(self.vector, self.clf)
        # Define an explainer
        explainer = LimeTextExplainer(class_names=['defect', 'non-defect'], random_state=self.random_state)

        # Explain each defective file to predict the buggy lines exist in the file.
        # Process each file according to the order of defective rank list.
        for i in range(len(defective_file_index)):
            print(f'{i}/{len(defective_file_index)}')
            defective_filename = self.test_filename[defective_file_index[i]]
            # Some files are predicted as defective, but they are actually clean (i.e., FP files).
            # These FP files do not exist in the oracle. Therefore, the corresponding values of these files are [].
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            # The code lines list of each corresponding predicted defective file.
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            # ####################################### Core Section #################################################
            # Explain each defective file
            exp = explainer.explain_instance(' '.join(defective_file_line_list), c.predict_proba, num_features=100)
            # Extract top@20 risky tokens with positive scores. maybe less than 20.
            risky_tokens = [x[0] for x in exp.as_list() if x[1] > 0][:20]

            # Count the number of risky tokens occur in each line.
            # The init value for each element of hit_count is [0 0 0 0 0 0 ... 0 0]. Note that line number index from 0.
            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                # Extract all tokens in the line with their original form.
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                # Check whether all risky tokens occurs in the line and count the number.
                for token in tokens_in_line:
                    if token in risky_tokens:
                        hit_count[line_index] += 1

            # ####################################### Core Section #################################################
            # Predicted buggy lines
            sorted_index = np.argsort(-hit_count,kind = 'mergesort').tolist()[:int(len(hit_count))]
            sorted_index = [i for i in sorted_index]
            # predicted_line_numbers.extend([self.test_line_numbers[defective_file_index[i]][h] for h in sorted_index])
            predicted_score.extend([hit_count[i] for i in sorted_index])
            # predicted_lines.extend([f'{defective_filename}:{i+1}' for i in resorted_index]) 
            predicted_lines.extend([f'{defective_filename}:{self.test_line_numbers[defective_file_index[i]][h]}' for h in sorted_index])
            try:
                density = f'{len(np.where(hit_count > 0)) / len(hit_count)}'
            except ZeroDivisionError:
                pass
            predicted_density.extend([density for i in sorted_index])
            rank.extend([i + 1 for i in range(len(sorted_index))]) #rank信息
            # file_label.extend(self.test_labels[defective_file_index[i]] for h in hit_count)
            # rank.extend([i + 1 for i in range(num_of_lines) if hit_count[i] > 0])
            # predicted_line_numbers.extend([self.test_line_numbers[defective_file_index[i]][h] for h in hit_count])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.predicted_density = predicted_density
        # self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.
        
        # self.file_level_label = file_label
        self.rank = rank
        # self.predicted_buggy_line_numbers = predicted_line_numbers

        # Save line level result and buggy density
        self.save_line_level_LineDP_result()
        self.save_buggy_density_file()


#################################### Traditional model interpretation approaches #######################################
class TMI_Model(BaseModel):
    model_name = 'MIT-TMI-Model'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = None

    def line_level_prediction(self):
        """
        Checked OK.
        Require: self.test_pred_labels
        Require: self.test_pred_scores
        :return:
        """
        super(TMI_Model, self).line_level_prediction()
        if USE_CACHE and os.path.exists(self.line_level_result_file):
            return

        # Load file prediction result if no result
        if len(self.test_pred_labels) == 0 or len(self.test_pred_scores) == 0:
            df = pd.read_csv(self.file_level_result_file)
            self.test_pred_labels = np.array(df['predicted_label'])
            self.test_pred_scores = np.array(df['predicted_score'])

        print(f'Predicting line level defect prediction of {self.model_name}')

        # Buggy lines
        predicted_lines, predicted_score, predicted_density = [], [], []

        # Indices of defective files in descending order according to the scores. Note the rank step may be removed.
        defective_file_index = [i for i in np.argsort(self.test_pred_scores)[::-1] if self.test_pred_labels[i] == 1]

        # Text tokenizer
        tokenizer = self.vector.build_tokenizer()

        # ####################################### 获取解释的单词特征 #################################################
        # 特征重要性字典
        feature_weight_dict = dict()
        if self.model_name == 'MIT-TMI-LR' or self.model_name == 'MIT-TMI-SVM' or self.model_name == 'MIT-TMI-MNB':
            # 标准化处理
            std = StandardScaler()
            std_coefficient = std.fit_transform(self.clf.coef_.reshape(-1, 1))
            feature_weight_dict = dict(zip(self.vector.get_feature_names(), std_coefficient.T[0]))

        elif self.model_name == 'MIT-TMI-DT' or self.model_name == 'MIT-TMI-RF':
            feature_weight_dict = dict(zip(self.vector.get_feature_names(), self.clf.feature_importances_.tolist()))
        # 按照重要性排序后的元祖列表
        sorted_feature_weight_dict = sorted(feature_weight_dict.items(), key=lambda kv: (-kv[1], kv[0]))

        # ####################################### 获取解释的单词特征 #################################################

        # Explain each defective file to predict the buggy lines exist in the file.
        # Process each file according to the order of defective rank list.
        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            # Some files are predicted as defective, but they are actually clean (i.e., FP files).
            # These FP files do not exist in the oracle. Therefore, the corresponding values of these files are []
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            # The code lines list of each corresponding predicted defective file
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            # ####################################### Core Section #################################################
            # Explain each defective file
            # Extract top@20 risky tokens with positive scores. maybe less than 20
            risky_tokens = [x[0] for x in sorted_feature_weight_dict if x[1] > 0][:20]

            # Count the number of risky tokens occur in each line.
            # The init value for each element of hit_count is [0 0 0 0 0 0 ... 0 0]. Note that line number index from 0.
            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                # Extract all tokens in the line with their original form.
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                # Check whether all risky tokens occurs in the line and count the number.
                for token in tokens_in_line:
                    if token in risky_tokens:
                        hit_count[line_index] += 1

            # ####################################### Core Section #################################################
            # Predicted buggy lines
            predicted_score.extend([hit_count[i] for i in range(num_of_lines) if hit_count[i] > 0])
            predicted_lines.extend([f'{defective_filename}:{i + 1}' for i in range(num_of_lines) if hit_count[i] > 0])
            density = f'{len(np.where(hit_count > 0)) / len(hit_count)}'
            predicted_density.extend([density for i in range(num_of_lines) if hit_count[i] > 0])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.predicted_density = predicted_density
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.

        # Save line level result
        self.save_line_level_result()
        self.save_buggy_density_file()


class TMI_LR(TMI_Model):
    model_name = 'MIT-TMI-LR'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = LogisticRegression(random_state=self.random_state)


class TMI_SVM(TMI_Model):
    model_name = 'MIT-TMI-SVM'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = LinearSVC(random_state=self.random_state)


class TMI_MNB(TMI_Model):
    model_name = 'MIT-TMI-MNB'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = MultinomialNB()


class TMI_DT(TMI_Model):
    model_name = 'MIT-TMI-DT'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = DecisionTreeClassifier(random_state=self.random_state)


class TMI_RF(TMI_Model):
    model_name = 'MIT-TMI-RF'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        # File level classifier
        self.clf = RandomForestClassifier(random_state=self.random_state)
