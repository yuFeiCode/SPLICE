# -*- coding:utf-8 -*-
from src.utils.config import USE_CACHE
from src.utils.helper import *
from src.models.base_model import BaseModel


class NGram(BaseModel):
    model_name = 'NLP-NGram'

    def __init__(self,train_release: str = '', test_release: str = ''):
        super().__init__(train_release, test_release)
        self.model_file_path = f'{self.result_path}model_files/'
        self.entropy_result_file = f'{self.result_path}entropy_result/{self.project_name}/{self.test_release}-result.csv'

        # remove_path(self.file_level_result_path)

    def file_level_prediction(self):
        # super(Entropy, self).file_level_prediction()
        if USE_CACHE and os.path.exists(self.file_level_result_file):
            return

        self.test_pred_labels = np.ones((len(self.test_labels),), dtype=int)
        self.test_pred_scores = np.ones((len(self.test_labels),), dtype=int)

        # Save file level result
        self.save_file_level_result()

        # """
        # NOTE: This below method is enabled in Dis 3
        # """
        # print(f"Prediction\t=>\t{self.test_release}")
        # if USE_CACHE and os.path.exists(self.file_level_result_file):
        #     return
        #
        # # 2. Convert text feature into numerical feature, classifier
        # # Neither perform lowercase, stemming, nor lemmatization. Remove tokens that appear only once
        # train_vtr = self.vector.fit_transform(self.train_text)
        # test_vtr = self.vector.transform(self.test_text)
        # # 3. Predict defective files, test_predictions
        # self.clf.fit(train_vtr, self.train_label)
        #
        # self.test_pred_labels = self.clf.predict(test_vtr)
        # # Obtain the prediction scores of each buggy lines.
        # self.test_pred_scores = np.array([score[1] for score in self.clf.predict_proba(test_vtr)])
        #
        # # Save file level result
        # self.save_file_level_result()

    def line_level_prediction(self):

        super(NGram, self).line_level_prediction()
        if USE_CACHE and os.path.exists(self.line_level_result_file):
            return

        predicted_lines, predicted_score, predicted_density = [], [], []
        predicted_buggy_files = [self.test_filename[index] for index in range(len(self.test_filename)) if
                                 self.test_pred_labels[index] == 1]

        with open(self.entropy_result_file, 'r') as data:
            for line in data.readlines():
                temp = line.strip().split(',')
                filename = temp[0].split(':')[0]
                if filename in predicted_buggy_files:
                    predicted_lines.append(temp[0])
                    predicted_score.append(temp[1])  # average entropy
                    # predicted_density.append(self.test_pred_density[filename])
                    predicted_density.append(temp[1])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.predicted_density = predicted_density
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.

        # Save line level result and buggy density
        self.save_line_level_result()
        self.save_buggy_density_file()


class NGram_C(NGram):
    model_name = 'NLP-NGram-C'

    def __init__(self, train_release: str = '', test_release: str = ''):
        super().__init__(train_release, test_release)


if __name__ == '__main__':
    pass
