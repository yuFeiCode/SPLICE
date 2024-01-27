# -*-coding:utf-8-*-

import sys

# --------------------------------
# Code Path
# --------------------------------
# This variable should be modified as the correct path when running code in other locations.
CODE_PATH = 'C:/Users/gzq-712/Desktop/Git/CLDP/'
sys.path.append(CODE_PATH)

# --------------------------------
# Testing
# --------------------------------
# is it a test run?
# test runs reduce the dataset to 1 test release
TEST = False

# --------------------------------
# Cached Result
# --------------------------------
# Do we use the cached results? True=yes(False=no) means that speed up the prediction process.
USE_CACHE = False

# --------------------------------
# Dataset project
# --------------------------------


PROJECT_RELEASE_LIST=[
    'activemq-5.0.0','activemq-5.1.0', 'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
    'camel-1.4.0','camel-2.9.0', 'camel-2.10.0', 'camel-2.11.0',
    'groovy-1_5_7', 'groovy-1_6_BETA_1', 'groovy-1_6_BETA_2',
    'hbase-0.94.0','hbase-0.95.0', 'hbase-0.95.2', 
    'hive-0.9.0','hive-0.10.0', 'hive-0.12.0',
    'jruby-1.1','jruby-1.4.0','jruby-1.5.0','jruby-1.7.0.preview1',
    'lucene-2.3.0','lucene-2.9.0', 'lucene-3.0.0', 'lucene-3.1',
    'derby-10.2.1.6','derby-10.3.1.4', 'derby-10.5.1.1',
    'wicket-1.3.0-beta2','wicket-1.3.0-incubating-beta-1','wicket-1.5.3'
]

# --------------------------------
# DO NOT CHANGE FROM HERE ON
# --------------------------------

# Let's change some parameters (i.e., make them smaller) if this is a test run
if TEST:
    PROJECT_RELEASE_LIST = ['ambari-1.2.0', 'ambari-2.1.0', 'ambari-2.2.0', 'ambari-2.4.0']
