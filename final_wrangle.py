
# coding: utf-8

# In[3]:

import pandas as pd
import numpy as np
import sqlite3
import re
from datetime import datetime
    
from gender import gender # This import is required for determining gender

DATABASE_FILE = 'content_digest.db'

LOGS_FILE = 'access.log'


def get_emails(portion=False):
    
    # Choose file
    sql_file = DATABASE_FILE
    
    # Connect
    conn = sqlite3.connect(sql_file)

    # Cursor
    c = conn.cursor()

    # Execute (limit in place for now)
    if portion:
        statement = 'select * from email_content limit 10000;'
    else:
        statement = 'select * from email_content;'
    c.execute(statement)

    # Fetch (and store)
    all_rows = c.fetchall()

    # Commit
    pass

    # Close
    conn.close()

    # Get data into pandas
    frame = pd.DataFrame(all_rows)
    frame.columns = ['content_id','email_id','user_id','article_id','send_time']

    # Housekeeping with datatypes
    date_format = '%Y-%m-%d %H:%M:%S'
    frame['send_time'] = frame['send_time'].apply(lambda x: datetime.strptime(x[:-7], date_format))
    frame['content_id'] = frame['content_id'].apply(np.int64)
    frame['article_id'] = frame['article_id'].apply(np.int64)
    frame['email_id'] = frame['email_id'].apply(np.int64)
    frame['user_id'] = frame['user_id'].apply(np.int64)

    return frame

def get_logs(portion=False):

    # Get file
    input_file = LOGS_FILE

    # Read it
    logs_raw = pd.read_csv(input_file, names=['logs'], header=None)
    logs_series = logs_raw['logs']

    # One small fix up, getting rid of the ampersand
    cleaned_series = logs_series.apply(lambda x: x.replace('&',''))
    
    if portion:
        cleaned_series = cleaned_series[:100]

    # Our key regular expression
    regex = r'\[(\d+\/\w+\/\d+:\d+:\d+:\d+)\]\s\"GET\s.\w+\?\w+=(\d+)\w+=(\d+)\sHTTP/1.1"\s(\d+)\s(\d+)' 

    # Create empty storage lists
    time_list = []
    article_id_list= []
    user_id_list = []
    status_code_list = []
    byte_size_list = []

    # Loop through the logs and pull out known items of interest
    for item in cleaned_series:
        matches = re.search(regex, item)
        try:
            time_list.append(matches.group(1))
            article_id_list.append(matches.group(2))
            user_id_list.append(matches.group(3))
            status_code_list.append(matches.group(4))
            byte_size_list.append(matches.group(5))
        except:
            print "This line was not parseable: ", item
            continue

    # Pandas time
    logs_frame = pd.DataFrame({'time' : time_list, 
                               'article_id': article_id_list, 
                               'user_id': user_id_list,
                              'status_code': status_code_list, 
                               'byte_size': byte_size_list})

    # Housekeeping with datatypes
    log_date_format = '%d/%b/%Y:%H:%M:%S'
    logs_frame['time'] = logs_frame['time'].apply(lambda x: datetime.strptime(x,
                                                                             log_date_format))
    logs_frame['user_id'] = logs_frame['user_id'].apply(np.int64)
    logs_frame['article_id'] = logs_frame['article_id'].apply(np.int64)
    logs_frame['status_code'] = logs_frame['status_code'].apply(np.int64)
    logs_frame['byte_size'] = logs_frame['byte_size'].apply(np.int64)

    return logs_frame

def get_articles():

    # Choose file
    sql_file = DATABASE_FILE

    # Connect
    conn = sqlite3.connect(sql_file)

    # Cursor
    c = conn.cursor()

    # Execute (limit in place for now)
    statement = 'select * from articles;'
    c.execute(statement)

    # Fetch (and store)
    all_rows = c.fetchall()

    # Commit
    pass

    # Close
    conn.close()

    # Get data into pandas
    articles_frame = pd.DataFrame(all_rows)
    articles_frame.columns = ['article_id','author_id','topic_id','type_id','submission_time']
    
    # Housekeeping with datatypes
    date_format = '%Y-%m-%d %H:%M:%S'
    articles_frame['submission_time'] = articles_frame['submission_time'].apply(lambda x: datetime.strptime(x[:-7], date_format))
    articles_frame['author_id'] = articles_frame['author_id'].apply(np.int64)
    articles_frame['topic_id'] = articles_frame['topic_id'].apply(np.int64)
    articles_frame['type_id'] = articles_frame['type_id'].apply(np.int64)

    return articles_frame

def get_topics():
    
    # Choose file
    sql_file = DATABASE_FILE

    # Connect
    conn = sqlite3.connect(sql_file)

    # Cursor
    c = conn.cursor()

    # Execute (limit in place for now)
    statement = 'select * from topics;'
    c.execute(statement)

    # Fetch (and store)
    all_rows = c.fetchall()

    # Commit
    pass

    # Close
    conn.close()

    # Get data into pandas
    topics_frame = pd.DataFrame(all_rows)
    topics_frame.columns = ['topic_id','name']
    
    return topics_frame

def get_users():
    
    # Choose file
    sql_file = DATABASE_FILE

    # Connect
    conn = sqlite3.connect(sql_file)

    # Cursor
    c = conn.cursor()

    # Execute (limit in place for now)
    statement = 'select * from users;'
    c.execute(statement)

    # Fetch (and store)
    all_rows = c.fetchall()

    # Commit
    pass

    # Close
    conn.close()

    # Get data into pandas
    users_frame = pd.DataFrame(all_rows)
    users_frame.columns = ['user_id','email_address']

    regex = '(\w+).\w+@\w+.com'

    first_names = [] 

    for item in users_frame['email_address']:
        matches = re.search(regex, item)
        try:
            first_names.append(matches.group(1).upper())
        except:
            "Regex failed!!!"
    
    gender_list = []

    for item in first_names:
        try:
            gender_list.append(gender[item])
        except:
            gender_list.append('andro')

    users_frame['name'] = pd.Series(first_names)
    users_frame['gender'] = pd.Series(gender_list)
    
    return users_frame

# Quick function to create a boolean for clicks
def easy_split(item):
    if item == 'left_only':
        return False
    elif item == 'both':
        return True
    else:
        return np.nan
    
    
def main():
    
    # Collect all data from sql using dedicated functions
    emails = get_emails(portion=False)
    logs = get_logs(portion=False)
    topics = get_topics()
    articles = get_articles()
    users = get_users()

    # Major merge of the data from emails and logs
    core_frame = pd.merge(emails, logs, on=['user_id','article_id'], how='outer',indicator=True)

    # Some new columns
    core_frame['click'] = core_frame['_merge'].apply(easy_split)
    core_frame['send_date_hour'] = core_frame['send_time'].apply(lambda x: x.replace(minute=0, second=0))
    core_frame['send_date'] = core_frame['send_time'].apply(lambda x: x.date()) 
    
    # Incorporate articles frame
    core_frame = pd.merge(core_frame, articles, on='article_id', how='left')
    
    # Incorporate users frame
    core_frame = pd.merge(core_frame, users, on='user_id', how='left')
    
    # Save to CSV, next up....R
    core_frame.to_csv('core_frame.csv')
    
    return core_frame

if __name__ == '__main__':
    main()

