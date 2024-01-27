import pandas as pd
import numpy as np
import subprocess, re, os, time

from multiprocessing import Pool

from tqdm import tqdm


def get_line_priority_dict(reported_lines):
    line_priority_dict = dict()
    for line in reported_lines:
        split = line.split(':')
        priority = int(split[0].strip('"'))  # buggy priority
        line_number = int(split[1].strip('"')) 
        if line_number not in line_priority_dict:
           
            line_priority_dict[line_number] = priority
        else:
            
            if  line_priority_dict[line_number] > priority:
                line_priority_dict[line_number] = priority

    return line_priority_dict


rule_list = [
            'category/java/design.xml',
            'category/java/errorprone.xml',
            'category/java/multithreading.xml',
            'category/java/security.xml',
        ]

all_eval_releases = ['activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                     'camel-2.10.0','camel-2.11.0', 
                     'derby-10.5.1.1',
                     'groovy-1_6_BETA_2', 
                     'hbase-0.95.2',
                     'hive-0.12.0', 
                     'jruby-1.5.0','jruby-1.7.0.preview1',
                     'lucene-3.0.0','lucene-3.1', 
                     'wicket-1.5.3']

all_dataset_name = ['activemq','camel','derby','groovy','hbase','hive','jruby','lucene','wicket']

# outputfile = f'/home/pmdtest/test.csv'

base_file_dir = '../datasets/PMD_data/'
base_command = f'./pmd-bin-7.0.0-rc4/bin/pmd.dat check -R {",".join(rule_list)} -d '

result_dir = './PMD_result/'

if not os.path.exists(result_dir):
    os.makedirs(result_dir)
    
def run_PMD(rel):
    df_list = []
    java_file_dir = base_file_dir+rel+'/'

    file_list = os.listdir(java_file_dir)
    
    for java_filename in tqdm(file_list):     
        print()
        print()
        print(f'{"=" * 10} {java_filename} {"=" * 10}')
   
        f = open(java_file_dir+java_filename,'r',encoding='utf-8',errors='ignore')
        java_code = f.readlines()

        code_len = len(java_code)

        output = subprocess.getoutput(base_command+java_file_dir+java_filename + f'  -f csv ')
 
        reported_lines = re.findall('.java","\d+","\d+',output)  
        reported_lines = [l.replace('.java","', '').replace('","', ':') for l in reported_lines]
        print("before: lenght of reported_lines = ", len(reported_lines))
        print(reported_lines)

        temp = get_line_priority_dict(reported_lines)
        reported_lines = list(temp.keys())
        reported_priorities = list(temp.values())
        print("after: lenght of reported_lines = ", len(reported_lines))
        print("after: reported_priorities = ", len(reported_priorities))
        print(reported_lines)
        print(reported_priorities)

        line_df = pd.DataFrame()

        line_df['filename'] = [java_filename.replace('_','/')]*code_len
        line_df['test-release'] = [rel]*len(line_df)
        line_df['line_number'] = np.arange(1,code_len+1)
        line_df['PMD_prediction_result'] = line_df['line_number'].isin(reported_lines)
        line_df['Priority'] = 0
        line_df.loc[line_df['line_number'].isin(reported_lines), 'Priority'] = reported_priorities

        df_list.append(line_df)

    final_df = pd.concat(df_list)
    final_df.to_csv(result_dir+rel+'-line-lvl-result.txt',index=False)
    print('finished',rel)

agents = 5
chunksize = 8

with Pool(processes=agents) as pool:
    pool.map(run_PMD, all_eval_releases, chunksize)
