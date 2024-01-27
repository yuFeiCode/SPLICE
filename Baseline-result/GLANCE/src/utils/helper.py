# -*- coding:utf-8 -*-
import shutil
from warnings import simplefilter

import os
import re
import numpy as np
import pickle


from src.utils.config import PROJECT_RELEASE_LIST

simplefilter(action='ignore', category=FutureWarning)


root_path = r'/root/workspace/zyf/SPLICE/Baseline-result/'  
dataset_string = 'datasets'
result_string = 'result'

dataset_path = f'{root_path}/{dataset_string}/Bug-Info/'
file_level_path = f'{root_path}/{dataset_string}/original/File-level/'
line_level_path = f'{root_path}/{dataset_string}/original/Line-level/'
result_path = f'{root_path}/GLANCE/{result_string}'
file_level_path_suffix = '_ground-truth-files_dataset.csv'
line_level_path_suffix = '_defective_lines_dataset.csv'

def is_comment_line(code_line, comments_list):
    '''
        input
            code_line (string): source code in a line
            comments_list (list): a list that contains every comments
        output
            boolean value
    '''

    code_line = code_line.strip()

    if len(code_line) == 0:
        return False
    elif code_line.startswith('//'):
        return True
    elif code_line in comments_list:
        return True
    
    return False


# ============================================ Project & Release Information ===========================================
def get_project_releases_dict():
    """
    :return: project releases dict: dict[project] = [r1, r2, ..., rn]
    """
    project_releases_dict = {}
    for release in PROJECT_RELEASE_LIST:
        project = release.split('-')[0]
        if project not in project_releases_dict:
            project_releases_dict[project] = [release]
        else:
            project_releases_dict[project].append(release)

    return project_releases_dict


def get_project_list():
    """
    :return: project list: [p1, p2, ..., pn]
    """
    return list(get_project_releases_dict().keys())


def get_all_releases_list():
    """
    :return: release list: [r1, r2, ..., rn]
    """
    return PROJECT_RELEASE_LIST


def get_test_releases_list():
    """
    :return: release list: [r1, r2, ..., rn]
    """
    releases_list = []
    for project, releases in get_project_releases_dict().items():
        releases_list.extend(releases[1:])
    return releases_list


# ============================================== Dataset Information ===========================================
def read_file_level_dataset(release='', file_path=file_level_path):
    """
    :param release:项目名
    :param file_path
    :return:
    """
    if release == '':
        return [], [], [], []
    path = f'{file_path}{release}{file_level_path_suffix}'
    with open(path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()
        # 文件信息索引列表, 每个文件名不一样该语句才没有错误 TODO line.index(line)
        src_file_indices = [lines.index(line) for line in lines if r'.java,true,"' in line or r'.java,false,"' in line]
        # 源文件路径,需要时返回 OK
        src_files = [lines[index].split(',')[0] for index in src_file_indices]
        # 缺陷标记
        string_labels = [lines[index].split(',')[1] for index in src_file_indices]
        numeric_labels = [1 if label == 'true' else 0 for label in string_labels]

        # 行级别的文本语料库
        texts_lines = []
        line_numbers = []
        for i in range(len(src_file_indices)): 
           
            s_index = src_file_indices[i]
            e_index = src_file_indices[i + 1] if i + 1 < len(src_file_indices) else len(lines)

            # xxx 也许需要过滤掉注释行
            temp_lines = lines[s_index:e_index]
            
            all_lines = ''.join(temp_lines)
                       
            # is_comments = []
            code_lines = []
            within_file_line_numbers = []
            
            comments = re.findall(r'(/\*[\s\S]*?\*/)',all_lines,re.DOTALL)
            comments_str = '\n'.join(comments)
            comments_list = comments_str.split('\n')
            
            count = s_index
            
            for line in lines[s_index:e_index]:
                line_without_huanhangfu = line.replace('\n','')
                line_without_huanhangfu = line_without_huanhangfu.strip()
                if(is_comment_line(line_without_huanhangfu,comments_list)):
                    #如果时注释行的话，当前的代码行会被直接改写成空字符串
                    lines[count] = ''
                count = count + 1
                          
            temp_lines[0] = temp_lines[0].split(',')[-1][1:]          
            if(is_comment_line(temp_lines[0],comments_list)):
                lines[s_index] = ''
            
            for index in range(s_index+1,e_index):
                line = lines[index]
                if line.strip() != '':
                    code_lines.append(line)
                    within_file_line_numbers.append(index-s_index+1)
           
            code_lines = code_lines[:-1]
            texts_lines.append(code_lines)
            within_file_line_numbers = within_file_line_numbers[:-1] 
            line_numbers.append(within_file_line_numbers)
            count = 0
       
        texts = [' '.join(line) for line in texts_lines]

        
        return texts, texts_lines, numeric_labels, src_files, line_numbers


def read_line_level_dataset(release=''):
    """
    :param release: 项目名
    :return: 字典：dict[文件名] = [bug行号]
    """
    if release == '':
        return dict()
    path = f'{line_level_path}{release}{line_level_path_suffix}'
    with open(path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()
        file_buggy_lines_dict = {}
        for line in lines[1:]:
            temp = line.split(',', 2)
            file_name, buggy_line_number = temp[0], int(temp[1])
            if file_name not in file_buggy_lines_dict.keys():
                file_buggy_lines_dict[file_name] = [buggy_line_number]
            else:
                file_buggy_lines_dict[file_name].append(buggy_line_number)

    return file_buggy_lines_dict


def dump_pk_result(path, data):
    """
    dump result
    :param path:
    :param data:
    :return:
    """
    with open(path, 'wb') as file:
        pickle.dump(data, file)


def load_pk_result(path):
    with open(path, 'rb') as file:
        data = pickle.load(file)
    return data


def read_data_from_file(path):
    with open(path, 'r', encoding='utf-8', errors="ignore") as fr:
        lines = fr.readlines()
    return lines


def read_dict_from_file(path):
    dict_var = {}
    for line in read_data_from_file(path):
        ss = line.strip().split(sep=':', maxsplit=1)
        dict_var[ss[0]] = ss[1].split('|')
    return dict_var


def save_csv_result(file_path, file_name, data):
    """
    Save result into f{file_path}{file_name}.
    :param file_path: The file location
    :param file_name: The file name
    :param data: The data
    :return:
    """
    make_path(file_path)
    with open(f'{file_path}{file_name}', 'w', encoding='utf-8') as file:
        file.write(data)
    print(f'Result has been saved to {file_path}{file_name} successfully!')


def save_result(file_path, data):
    """
    Save result into file_path.
    :param file_path: The file location
    :param data: The data
    :return:
    """
    with open(f'{file_path}', 'w', encoding='utf-8') as file:
        file.write(data)


def add_value(avg_data, line):
    values = line.split(',')[2:]
    for index in range(len(values)):
        avg_data[index] += float(values[index])
    return avg_data


def eval_ifa(path):
    ifa_dict = {}
    with open(f'{path}result_worst.csv', 'r') as file:
        for line in file.readlines()[1:]:
            project_name = line.split(',')[1].split('-')[0]
            if project_name not in ifa_dict:
                ifa_dict[project_name] = ','.join(line.strip().split(',')[2:])
            else:
                ifa_dict[project_name] += ',' + ','.join(line.strip().split(',')[2:])

    text = ''
    for project_name in ifa_dict:
        text += project_name + ',' + ifa_dict[project_name] + '\n'

    with open(f'{path}result_worst.csv', 'w') as file:
        file.write(text)


def make_path(path):
    """
    Make path is it does not exists
    :param path:
    :return:
    """
    if not os.path.exists(path):
        os.makedirs(path)


def remove_path(path):
    del_list = os.listdir(path)
    for f in del_list:
        file_path = os.path.join(path, f)
        if os.path.isfile(file_path):
            os.remove(file_path)
        elif os.path.isdir(file_path):
            shutil.rmtree(file_path)




def dataset_statistics():
    """
    数据集统计信息
    :return:
    """
    projects = ['ant-ivy', 'commons-compress', 'commons-configuration', 'commons-lang', 'commons-math',
                'commons-net', 'commons-vfs', 'giraph', 'parquet-mr']
    print('project name, #files, #buggy files, ratio, #LOC, #buggy LOC, ratio, #tokens')

    for proj in projects:
        texts, texts_lines, numeric_labels, src_files = read_file_level_dataset(proj)

        file_num = len(texts)
        bug_num = len([l for l in numeric_labels if l == 1])
        file_ratio = bug_num / file_num

        loc = sum([len([line for line in text if not line == ""]) for text in texts_lines])
        bug_lines = sum([len(v) for k, v in read_line_level_dataset(proj).items()])
        line_ratio = bug_lines / loc

        res = (proj, file_num, bug_num, file_ratio, loc, bug_lines, line_ratio)
        print("%s, %d, %d, %f, %d, %d, %f" % res)


def output_box_data_for_metric():
    setting = 'CP'
    mode = 'worst'
    index_dict = {'recall': 2, 'far': 3, 'd2h': 4, 'mcc': 5, 'ce': 6, 'r_20%': 7, 'IFA_mean': 8, 'IFA_median': 9}
    thresholds = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50]

    for metric_name, metric_index in index_dict.items():
        text = ', '.join([str(e) for e in thresholds]) + '\n'
        result_data = []
        for threshold in thresholds:
            path = f'{result_path}{setting}/AccessModel_{threshold}/result_{mode}.csv'
            with open(path, 'r') as file:
                tmp = []
                for line in file.readlines()[1:]:
                    tmp.append(line.split(',')[metric_index])
            result_data.append(tmp)

        for j in range(len(result_data[0])):
            for i in range(len(thresholds)):
                text += result_data[i][j] + ','
            text += '\n'

        with open(f'{result_path}{setting}/threshold_{mode}_{metric_name}.csv', 'w') as file:
            file.write(text)
    print('Finish!')


def transform():
    for release in PROJECT_RELEASE_LIST:
        data = 'filename,#total lines,#buggy lines,label\n'
        text, text_lines, label, filename = read_file_level_dataset(release)
        file_buggy = read_line_level_dataset(release)
        for index in range(len(filename)):
            name = filename[index]
            lines = len([line for line in text_lines[index] if line.strip() != ''])
            buggy = len(file_buggy[name]) if name in file_buggy.keys() else 0
            label = 1 if buggy > 0 else 0
            data += f'{name},{lines},{buggy},{label}\n'

        # save_csv_result(root_path + 'Transform/' + release + '.csv', data)
        make_path(f'{root_path}Transform')
        save_csv_result(f'{root_path}Transform/{release}.csv', data)
        print(release, 'finish')


def export_source_file():
    """
    For Entropy model.
    Export all source files of a project into a folder.
    :return:
    """
    for release in PROJECT_RELEASE_LIST:
        # generate all source files of a release
        release_source_path = root_path + 'Dataset/Source/' + release
        make_path(release_source_path)
        text, text_lines, label, filename = read_file_level_dataset(release)
        for index in range(len(filename)):
            save_csv_result(release_source_path + '/', filename[index].replace('/', '.'), '\n'.join(text_lines[index]))
        print(len(filename), 'in', release, 'finish')


def make_udb_file():
    for release in PROJECT_RELEASE_LIST:
        # generate .udb file
        release_source_path = root_path + 'Dataset/Source/' + release
        release_udb_path = root_path + 'Dataset/UDB/' + release
        print(release_udb_path)
        os.system(f"und create -db {release_udb_path}.udb -languages java c++ python")
        os.system(f"und -db {release_udb_path}.udb add {release_source_path}")
        os.system(f"und -db {release_udb_path} -quiet analyze")


def is_test_file(src):
    """
    Whether the target source file is a test file OK
    :param src:
    :return:
    """
    # return 'src/test/' in src
    # return 'test/' in src or 'tests/' in src or src.endswith('Test.java')
    return False


def is_non_java_file(src):
    """
    Whether the target source file is not a java file OK
    :param src:
    :return:
    """
    return '.java' not in src


def remove_test_or_non_java_file_from_dataset():
    """
    移除数据集中的测试文件和非java文件 OK
    :return:
    """
    for release in PROJECT_RELEASE_LIST:
        # #### remove test file from file level dataset
        t, texts_lines, numeric_labels, src_files = read_file_level_dataset(release, root_path + 'Dataset/Origin_File/')

        new_file_dataset = 'File,Bug,SRC\n'
        for index in range(len(src_files)):
            target_file = src_files[index]
            target_text = texts_lines[index]
            if is_test_file(target_file) or is_non_java_file(target_file):
                continue
            label = 'true' if numeric_labels[index] == 1 else 'false'
            new_file_dataset += f'{target_file},{label},"'
            new_file_dataset += ''.join(target_text)
            new_file_dataset += '"\n'

        out_file = file_level_path + release + file_level_path_suffix
        save_csv_result(out_file, data=new_file_dataset)

        # #### remove test file from line level dataset
        new_line_dataset = 'File,Line_number,SRC\n'
        path = root_path + 'Dataset/Origin_Line/' + release + line_level_path_suffix
        with open(path, 'r', encoding='utf-8', errors='ignore') as file:
            lines = file.readlines()
            for line in lines[1:]:
                if is_test_file(line) or is_non_java_file(line):
                    continue
                new_line_dataset += line
        out_file = line_level_path + release + line_level_path_suffix
        save_csv_result(out_file, data=new_line_dataset)
        print(release, 'finish')


def export_all_files_in_project(path):
    """
    Export all files in a specific root path OK
    :param path:
    :return:
    """
    file_list = []
    for root, dirs, files in os.walk(path):
        for file in files:
            file_path = (root.replace('\\', '/') + '/' + file).replace(path, '')
            if not file_path.endswith('.java') or is_test_file(file_path):
                continue
            file_list.append(file_path)
    return file_list


java_common_tokens = ['public', 'private', 'protected', 'class', 'interface', 'abstract', 'implement', 'extends', 'new',
                      'import', 'package', 'byte', 'char', 'boolean', 'short', 'int', 'float', 'long', 'double', 'void',
                      'null', 'true', 'false', 'if', 'else', 'while', 'for', 'switch', 'case', 'default', 'do', 'break',
                      'continue', 'return', 'instanceof', 'static', 'final', 'super', 'this', 'native', 'strictfp',
                      'synchronized', 'transient', 'volatile', 'catch', 'try', 'finally', 'throw', 'throws', 'enum',
                      'assert', 'const', 'goto',
                      ]


def preprocess_code_line(code, remove_java_common_tokens=False):
    """
    对代码进行预处理 OK
    :param code:
    :param remove_java_common_tokens:
    :return:
    """
    code = code.replace('(', ' ') \
        .replace(')', ' ') \
        .replace('{', ' ') \
        .replace('}', ' ') \
        .replace('[', ' ') \
        .replace(']', ' ') \
        .replace('.', ' ') \
        .replace(':', ' ') \
        .replace(';', ' ') \
        .replace(',', ' ') \
        .replace(' _ ', '_')
    code = re.sub('``.*``', '<STR>', code)
    code = re.sub("'.*'", '<STR>', code)
    code = re.sub('".*"', '<STR>', code)
    code = re.sub('\d+', '<NUM>', code)

    processed_code = code
    if remove_java_common_tokens:
        new_code = ''
        for tok in code.split():
            if tok not in java_common_tokens:
                new_code = new_code + tok + ' '
        processed_code = new_code.strip()
    return processed_code.strip()


def get_bug_number():
    for proj, releases in get_project_releases_dict().items():
        total_bugs = []
        for release in releases:
            commit_buggy_path = f'{root_path}/Dataset/Bug-Info/{release.split("-")[0]}'
            buggy_lines_dict = read_dict_from_file(f'{commit_buggy_path}/{release}_commit_buggy_lines.csv')
            total_bugs.append(len(buggy_lines_dict.keys()))

        print(f'{min(total_bugs)}~{max(total_bugs)}')


def calc_auc(label, pred):
    pos = [i for i in range(len(label)) if label[i] == 1]
    neg = [i for i in range(len(label)) if label[i] == 0]
    # 计算正样本和负样本的索引, 以便索引出之后的概率值
    auc = 0
    for i in pos:
        for j in neg:
            if pred[i] > pred[j]:
                auc += 1
            elif pred[i] == pred[j]:
                auc += 0.5
    return auc / (len(pos) * len(neg))


if __name__ == '__main__':
    # remove_test_or_non_java_file_from_dataset()
    # output_box_data_for_metric()
    # make_source_file()
    # make_udb_file()
    dataset_statistics()
    # print(get_project_list())
    # print(get_project_releases_dict())
    # read_file_level_dataset(get_project_releases_dict()['lucene'][0])
    # read_line_level_dataset(get_project_releases_dict()['lucene'][0])
    # export_source_file()
    # get_bug_number()

    pass
