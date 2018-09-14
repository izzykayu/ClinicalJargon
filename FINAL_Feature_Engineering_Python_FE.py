import pandas as pd
import numpy as np
import string
import re
from nltk.tokenize import word_tokenize

df = pd.read_csv('<PATH-TO-YOUR-DF>')

def normalize(string):
    """
    input txt is a string (the original note)
    returns normalized text with digit replacement
    """
    string = string.lower()
    text = re.sub('\n', ' ', string)
    text = re.sub('\d', 'd', text)
    text = re.sub(' +', ' ', text)
    tokens = word_tokenize(text)
    text = '"' + ' '.join(tokens) + '"'
    return text

df['NORMALIZED_TEXT'] = df['NOTE_TEXT'].apply(lambda x: normalize(x))

df['word_count'] = df['NORMALIZED_TEXT'].apply(lambda x: len(x.split()))

df['raw_char_count'] = df['NOTE_TEXT'].apply(lambda x: len(str(x)))

df['char_count'] = df['NORMALIZED_TEXT'].apply(lambda x: len(str(x)))

df['word_density'] = df['char_count'] / (df['word_count']+1)

def lexical_diversity(my_text_data):
    """
    input is list of text data
    output gives diversity_score
    """
    word_count = len(my_text_data)
    vocab_size = len(set(my_text_data))
    diversity_score = word_count / vocab_size
    return diversity_score

df['lexical_diversity'] = df['NOTE_TEXT'].apply(lambda x: lexical_diversity([wrd for wrd in x.split() if not wrd.isnumeric()]))

df['title_word_count'] = df['NOTE_TEXT'].apply(lambda x: len([wrd for wrd in x.split() if wrd.istitle()]))

# for all upper in the word, examples are NPO, IVF, CHF, etc
df['upper_case_word_count'] = df['NOTE_TEXT'].apply(lambda x: len([wrd for wrd in x.split() if wrd.isupper()]))

df['stopword_count'] = df['NOTE_TEXT'].apply(lambda x: len([wrd for wrd in x.split() if wrd.lower() in stop_words]))

# write out to csv
df.to_csv('<PATH-TO-WHERE-FE-DF>', index=False)

