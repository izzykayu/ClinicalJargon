import pandas as pd
import numpy as np

df = pd.read_csv('/gpfs/scratch/metzgi01/ZS/data/2018-07-05_ZS_withscaled.csv')
#train = df.loc[df['TVT_GROUP'] == 'TRAIN']
#test = df.loc[df['TVT_GROUP'] == 'VAL']
import datetime
#todaysdate = datetime.datetime.now().strftime("%Y-%m-%d")
#print(todaysdate)
import string
import random
import nltk
import re
from textblob import TextBlob
pos_dic = {
    'noun' : ['NN','NNS','NNP','NNPS'],
    'pron' : ['PRP','PRP$','WP','WP$'],
    'verb' : ['VB','VBD','VBG','VBN','VBP','VBZ'],
    'adj' :  ['JJ','JJR','JJS'],
    'adv' : ['RB','RBR','RBS','WRB']
}

# function to check and get the part of speech tag count of a words in a given sentence
def pos_check(x, flag):
    cnt = 0
    try:
        wiki = TextBlob(x)
        for tup in wiki.tags:
            ppo = list(tup)[1]
            if ppo in pos_dic[flag]:
                cnt += 1
    except:
        pass
    return cnt

df['noun_count'] = df['NOTE_TEXT'].apply(lambda x: pos_check(x, 'noun'))
df['verb_count'] = df['NOTE_TEXT'].apply(lambda x: pos_check(x, 'verb'))
df['adj_count'] = df['NOTE_TEXT'].apply(lambda x: pos_check(x, 'adj'))
df['adv_count'] = df['NOTE_TEXT'].apply(lambda x: pos_check(x, 'adv'))
df['pron_count'] = df['NOTE_TEXT'].apply(lambda x: pos_check(x, 'pron'))
#df.to_csv('/work/data/newest_features_dfFull_Jun27.csv', index=False)
print(df[['noun_count', 'verb_count', 'adj_count', 'adv_count', 'pron_count']].head(10))

df.to_csv("/gpfs/scratch/metzgi01/ZS/data/POS_ZS_df_full_Jul5.csv", index=False)
