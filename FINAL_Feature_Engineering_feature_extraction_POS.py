import pandas as pd
import numpy as np

#df = pd.read_csv('/gpfs/scratch/metzgi01/ZS/data/df.csv')
df = pd.read_csv('<PATH-TO-DF>')
#train = df.loc[df['TVT_GROUP'] == 'TRAIN']
#test = df.loc[df['TVT_GROUP'] == 'VAL']
import datetime
todaysdate = datetime.datetime.now().strftime("%Y-%m-%d")
print(todaysdate)
import string
import random
import nltk
import re
from textblob import TextBlob

# part of speech dictionary
pos_dic = {
    'noun' : ['NN','NNS','NNP','NNPS'],
    'pron' : ['PRP','PRP$','WP','WP$'],
    'verb' : ['VB','VBD','VBG','VBN','VBP','VBZ'],
    'adj' :  ['JJ','JJR','JJS'],
    'adv' : ['RB','RBR','RBS','WRB']
}

# function to check and get the part of speech tag count of a words in a given sentence
def pos_count(x, flag):
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

df['noun_count'] = df['NOTE_TEXT'].apply(lambda x: pos_count(x, 'noun'))
df['verb_count'] = df['NOTE_TEXT'].apply(lambda x: pos_count(x, 'verb'))
df['adj_count'] = df['NOTE_TEXT'].apply(lambda x: pos_count(x, 'adj'))
df['adv_count'] = df['NOTE_TEXT'].apply(lambda x: pos_count(x, 'adv'))
df['pron_count'] = df['NOTE_TEXT'].apply(lambda x: pos_count(x, 'pron'))

print(df[['noun_count', 'verb_count', 'adj_count', 'adv_count', 'pron_count']].head(10))

#df.to_csv("/gpfs/scratch/metzgi01/ZS/data/POS_ZS_df_full_" + str(todaysdate) + "_.csv", index=False)
df.to_csv("<OUTPUT-NAME-NEW-DF-WITH-POS-TAG-COUNTS>", index=False)