# -*- coding:utf-8 -*-
from xml.dom import minidom

from src.utils.config import USE_CACHE
from src.utils.helper import *
from src.models.base_model import BaseModel


# ############################################ Static Analysis Tool ####################################################
class StaticAnalysisTool(BaseModel):
    model_name = 'SAT'

    def __init__(self, train_release: str = '', test_release: str = ''):
        super().__init__(train_release, test_release)

        self.code_repository_path = f'{root_path}Repository/{self.project_name}/'  # 源代码所在仓库的路径
        self.origin_details_path = ''
        self.origin_final_path = ''
        self.origin_file = ''

        self.file_buggy_dict = dict()

    def load_version_info(self):
        """
        Get version information. commit id, version name, version date, next date, branch. OK.
        """
        commit_version_code, commit_version_branch = {}, {}
        lines = read_data_from_file(f'{root_path}DataCollection/Version/{self.project_name}.csv')
        for line in lines[1:]:
            spices = line.strip().split(",")
            version_code = spices[0]  # 版本commit hash
            version_name = spices[1]  # 版本名称
            version_branch = spices[4]  # 版本所在分支
            commit_version_code[version_name] = version_code
            commit_version_branch[version_name] = version_branch

        return commit_version_code, commit_version_branch

    def read_file_buggy_dict(self):
        print(f'{"=" * 10} Parse bugs of {self.test_release} by CheckStyle tool {"=" * 10}')

        file_buggy_dict = dict()
        lines = read_data_from_file(self.origin_file)
        for line in lines[1:]:
            split = line.strip().split(',')
            if len(split[1].strip()) == 0:
                continue
            file_name = split[0]
            buggy_line_list = file_buggy_dict[file_name] if file_name in file_buggy_dict else []
            # 依次处理每个代码行信息,因为可能包含重复的代码行,所以需要进行去重操作,同时，如果同一行有两个优先级,需要保留最高优先级.
            print(split)
            for v in split[1:]:
                value_candidate = [int(v.split(':')[0]), int(v.split(':')[1])]  # [line_number, priority]

                if len(buggy_line_list) == 0:
                    # 如果该文件还没有对应的buggy行信息,直接插入取出的buggy行 value
                    buggy_line_list.append(value_candidate)
                else:
                    # 判断是否应该插入 value
                    should_insert = True
                    for index in range(len(buggy_line_list)):
                        buggy_line = buggy_line_list[index]
                        # 发现该buggy行已经出现在字典值中,则只进行更新优先级操作,不进行插入操作.
                        if value_candidate[0] == buggy_line[0]:
                            should_insert = False
                            # 如果新buggy的优先级高于已有相同行的优先级,则更新buggy行信息
                            if value_candidate[1] < buggy_line[1]:
                                buggy_line_list[index] = value_candidate
                            break
                    # 插入新的没有在字典值中出现的buggy行信息
                    buggy_line_list.append(value_candidate) if should_insert else None

            file_buggy_dict[file_name] = buggy_line_list
        return file_buggy_dict

    def detect_detailed_result_of_bugs(self):
        """
        Output the detailed warning results by running the cmd command.
        """
        pass

    def file_level_prediction(self):
        if USE_CACHE and os.path.exists(self.file_level_result_file):
            return

        # self.detect_detailed_result_of_bugs() # only run once
        self.test_pred_labels = np.ones((len(self.test_labels),), dtype=int)
        self.test_pred_scores = np.ones((len(self.test_labels),), dtype=int)

        # Save file level result
        self.save_file_level_result()

        """
        NOTE: This below method is enabled in Dis 3
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
        self.test_pred_scores = np.array([score[1] for score in self.clf.predict_proba(test_vtr)])

        # Save file level result
        self.save_file_level_result()

    def line_level_prediction(self):
        super(StaticAnalysisTool, self).line_level_prediction()
        if USE_CACHE and os.path.exists(self.line_level_result_file):
            return

        predicted_lines, predicted_score, predicted_density = [], [], []
        predicted_buggy_files = [self.test_filename[index] for index in range(len(self.test_filename)) if
                                 self.test_pred_labels[index] == 1]

        self.file_buggy_dict = self.read_file_buggy_dict()
        for filename, buggy_line_list in self.file_buggy_dict.items():
            for buggy_line in buggy_line_list:
                if filename in predicted_buggy_files:
                    predicted_lines.append(f'{filename}:{buggy_line[0]}')
                    predicted_score.append(1 / buggy_line[1])  # 优先级的倒数作为预测分数
                    predicted_density.append(1 / buggy_line[1])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.predicted_density = predicted_density
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.

        # Save line level result and buggy density
        self.save_line_level_result()
        self.save_buggy_density_file()


# ################################################ PMD ##########################################################

class PMD(StaticAnalysisTool):
    model_name = 'SAT-PMD'

    def __init__(self, train_release: str = '', test_release: str = ''):
        super().__init__(train_release, test_release)

        if train_release == '' and test_release == '':
            return

        self.origin_details_path = f'{result_path}/{self.model_name}/origin/detailed_result/{self.project_name}/'
        self.origin_final_path = f'{result_path}/{self.model_name}/origin/final_result/{self.project_name}/'
        self.origin_file = f'{self.origin_final_path}{self.test_release}.csv'

        make_path(self.origin_details_path)
        make_path(self.origin_final_path)

        self.rule_list = [
            'category/java/design.xml',
            'category/java/errorprone.xml',
            'category/java/multithreading.xml',
            'category/java/security.xml',
        ]
        # file_buggy_dict[filename] = [[l1, p1], [l2, p2], ..., [ln, pn]]
        self.commit_version_code, self.commit_version_branch = self.load_version_info()
        # !!!!!!! The below code can only run once to speed up the prediction process.
        self.detect_detailed_result_of_bugs()

    def detect_detailed_result_of_bugs(self):
        # 检测某一个项目的具体版本
        print(f'{"=" * 10} Detecting bugs of {self.test_release} by PMD tool {"=" * 10}')

        # 切换到待检测的代码
        os.chdir(self.code_repository_path)

        version_code = self.commit_version_code[self.test_release]
        version_branch = self.commit_version_branch[self.test_release]

        os.system(f'git checkout -f {version_branch}')
        os.system(f'git reset --hard {version_code}')

        cmd_pmd = f'pmd -d {self.code_repository_path} -R {",".join(self.rule_list)} -f csv > {self.origin_file}'
        os.system(cmd_pmd)

    def read_file_buggy_dict(self):
        print(f'{"=" * 10} Parse bugs of {self.test_release} by PMD tool {"=" * 10}')

        file_buggy_dict = dict()
        lines = read_data_from_file(self.origin_file)
        for line in lines[1:]:
            split = line.split(',')
            file_name = split[2].strip('"').replace("\\", "/").replace(self.code_repository_path, "")
            line_number = split[4].strip('"')  # buggy lines
            priority = int(split[3].strip('"'))  # buggy priority
            value = [line_number, priority]
            if file_name not in file_buggy_dict:
                # 字典中没有该文件对应的键值对， 初始化该文件对应的buggy列表
                file_buggy_dict[file_name] = [value]
            else:
                # 字典中已有该文件对应的键值对
                # 取出该文件对应buggy行的信息,判断是否应该插入该buggy行的信息,
                should_insert = True
                for index in range(len(file_buggy_dict[file_name])):
                    buggy_line = file_buggy_dict[file_name][index]
                    # 发现该buggy行已经出现在字典值中,则不插入
                    if value[0] == buggy_line[0]:
                        should_insert = False
                        # 如果新buggy的优先级高于已有相同行的优先级,则更新buggy行信息
                        if value[1] > buggy_line[1]:
                            file_buggy_dict[file_name][index] = value
                        break
                # 插入新的没有在字典值中出现的buggy行信息
                file_buggy_dict[file_name].append(value) if should_insert else None
        return file_buggy_dict

# ################################################ CheckStyle ##########################################################
class CheckStyle(StaticAnalysisTool):
    model_name = 'SAT-CheckStyle'

    def __init__(self, train_release: str = '', test_release: str = ''):
        super().__init__(train_release, test_release)

        if train_release == '' and test_release == '':
            return

        self.origin_details_path = f'{result_path}/{self.model_name}/origin/detailed_result/{self.project_name}/'
        self.origin_final_path = f'{result_path}/{self.model_name}/origin/final_result/{self.project_name}/'
        self.origin_file = f'{self.origin_final_path}{self.test_release}.csv'
        self.tool_path = 'C:/Tools/checkstyle/'

        make_path(self.origin_details_path)
        make_path(self.origin_final_path)

        self.rule_list = ['sun']  # ['sun', 'google']
        self.commit_version_code, self.commit_version_branch = self.load_version_info()
        # !!!!!!! The below code can only run once to speed up the prediction process.
        self.detect_detailed_result_of_bugs()
        self.parse_detailed_results()

    def detect_detailed_result_of_bugs(self):
        # 检测某一个项目的具体版本
        print(f'{"=" * 30} Detecting bugs of {self.test_release} by CheckStyle tool {"=" * 30}')

        # 切换到准备待检测的代码
        os.chdir(self.code_repository_path)
        version_code = self.commit_version_code[self.test_release]
        version_branch = self.commit_version_branch[self.test_release]

        os.system(f'git checkout -f {version_branch}')
        os.system(f'git reset --hard {version_code}')

        # 需要检测的规则
        for check in self.rule_list:
            output_file = f'{self.origin_details_path}/{self.test_release}-{check}.xml'
            config_file = f'{self.tool_path}{check}_checks.xml'  # docs/google_checks.xml
            cmd_cs = f'java -jar {self.tool_path}/checkstyle-8.37-all.jar -c {config_file} -f xml -o {output_file} {self.code_repository_path}'
            os.system(cmd_cs)

            # 解析整个项目的文件时出错
            if not is_legal_file(output_file):
                self.detect_detailed_results_of_bugs_from_single_file()

    def detect_detailed_results_of_bugs_from_single_file(self):
        detail_result_path = f'{self.origin_details_path}/tmp-{self.test_release}/'
        make_path(detail_result_path)

        source_files = export_all_files_in_project(self.code_repository_path)
        for index in range(len(source_files)):
            source_file = source_files[index]
            print(f'Processing {index}/{len(source_files)} {source_file}')

            for check in self.rule_list:
                config_file = f'{self.tool_path}{check}_checks.xml'  # docs/google_checks.xml
                detail_file_path = f'{detail_result_path}{source_file.replace("/", ".")}-{check}.xml'
                source_file_path = self.code_repository_path + source_file
                # command
                cmd_cs = f'java -jar {self.tool_path}checkstyle-8.37-all.jar -c {config_file} -f xml -o {detail_file_path} {source_file_path}'
                os.system(cmd_cs)

    def parse_detailed_results(self):
        """
        Parse detail result to generate final result
        """
        prefix_path = f'{root_path}Repository/{self.project_name}/'

        buggy_dict = []
        for check in self.rule_list:
            xml_file_path = f'{self.origin_details_path}/{self.test_release}-{check}.xml'
            if is_legal_file(xml_file_path):
                buggy_dict.append(parse_project_xml(xml_file_path))
            else:
                buggy_dict.append(self.parse_detailed_results_from_single_file(check))
        text = combine_sun_and_google(prefix_path, buggy_dict)
        save_csv_result(f'{self.origin_final_path}', f'{self.test_release}.csv', text)

    def parse_detailed_results_from_single_file(self, check):
        detail_result_path = f'{self.origin_details_path}/tmp-{self.test_release}/'
        file_buggy_dict = {}
        source_files = export_all_files_in_project(self.code_repository_path)
        for source_file in source_files:
            detail_file_path = f'{detail_result_path}{source_file.replace("/", ".")}-{check}.xml'
            if is_legal_file(detail_file_path):
                file_buggy_dict[source_file] = [line for line in parse_single_file_xml(detail_file_path)]
        return file_buggy_dict


def is_legal_file(path):
    if not os.path.exists(path):
        return False
    lines = read_data_from_file(path)
    return len(lines) > 0 and lines[-1].strip() == '</checkstyle>'


def to_set(buggy_list):
    buggy_set = []
    for buggy in buggy_list:
        if buggy not in buggy_set:
            buggy_set.append(buggy)
    return buggy_set


def parse_project_xml(file_path):
    # 读取原始的bug报告
    DOMTree = minidom.parse(file_path)
    files = DOMTree.documentElement.getElementsByTagName('file')

    file_buggy_dict = {}
    for file in files:
        file_name = file.getAttribute('name')
        buggy_lines = [bug.getAttribute('line') for bug in file.getElementsByTagName('error')]
        file_buggy_dict[file_name] = buggy_lines
    return file_buggy_dict


def parse_single_file_xml(file_path):
    # 读取原始的bug报告
    DOMTree = minidom.parse(file_path)
    body = DOMTree.documentElement.getElementsByTagName('error')
    buggy_lines = [bug.getAttribute('line') for bug in body]
    return to_set(buggy_lines)


def combine_sun_and_google(path, buggy_dict):
    sun_dict, google_dict = buggy_dict[0], dict()
    file_names = sun_dict.keys()

    if len(buggy_dict) == 2:
        google_dict = buggy_dict[1]
        for name_in_google in google_dict.keys():
            if name_in_google not in file_names:
                file_names.append(name_in_google)

    text = ''
    for file_name in file_names:
        lines = []
        if file_name in sun_dict:
            lines += [f'{line_number}:1' for line_number in sun_dict[file_name]]

        if len(buggy_dict) == 2:
            if file_name in google_dict:
                lines += [f'{line_number}:2' for line_number in google_dict[file_name]]

        file_name = file_name.replace("\\", "/").replace(path, "")
        text += f'{file_name},{",".join(lines)}\n'
    return text


if __name__ == '__main__':
    pass
