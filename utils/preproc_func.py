import pandas as pd
import numpy as np
import re
import datetime
import os

# Extract relevant columns of relevant category
def extract_columns(column_patterns, dataset):    
    # Get all colnames
    cols = dataset.columns
    
    # Subset relevant columns
    rel_cols = ['Date']
    rel_cols.extend([col for col in dataset.columns if
                     any(xs.lower() in col.lower() for xs in column_patterns)])
    
    # Return result
    return dataset[rel_cols]

# Plot correlation matrix for given dataset and threshold
def plot_corr_matrix(dataset, threshold):
    # Create plot
    f = plt.figure(figsize=(12, 12))
    plt.matshow(dataset.corr()[dataset.corr().abs() > threshold], fignum=f.number)
    plt.xticks(range(dataset.shape[1]), dataset.columns, fontsize=12, rotation=90)
    plt.yticks(range(dataset.shape[1]), dataset.columns, fontsize=12)
    cb = plt.colorbar()
    cb.ax.tick_params(labelsize=12)
    #plt.title('Correlation Matrix', fontsize=12)
    plt.show();
    
def extract_columns_extra(channel, temperature, channel_type, dataset):
    
    names_1 = set(extract_columns(column_patterns=[channel], dataset=dataset).columns)
    names_2 = set(extract_columns(column_patterns=[temperature], dataset=dataset).columns)
    names_3 = set(extract_columns(column_patterns=[channel_type], dataset=dataset).columns)
    names_4 = set(extract_columns(column_patterns=['cost'], dataset=dataset).columns)
    total_names = (names_1.intersection(names_2)).intersection(names_3)
    cost_names = (names_1.intersection(names_2)).intersection(names_4)
    total_names = total_names - {'date'} - {'Date'}
    cost_names = cost_names - {'date'} - {'Date'}
    total_names = list(total_names)
    cost_names = list(cost_names)
    return([dataset[total_names], dataset[cost_names]])

def extract_columns_extra_2(channel, temperature, channel_type, dataset):
    
    names_1 = set(extract_columns(column_patterns=[channel], dataset=dataset).columns)
    names_2 = set(extract_columns(column_patterns=[temperature], dataset=dataset).columns)
    names_3 = set(extract_columns(column_patterns=[channel_type], dataset=dataset).columns)
    names_4 = set(extract_columns(column_patterns=['cost'], dataset=dataset).columns)
    total_names = (names_1.intersection(names_2)).intersection(names_3)
    cost_names = (names_1.intersection(names_2)).intersection(names_4)
    total_names = total_names - {'date'} - {'Date'}
    cost_names = cost_names - {'date'} - {'Date'}
    total_names = list(total_names)
    cost_names = list(cost_names)
    return([dataset[total_names], dataset[cost_names]])

def get_temp_table(channel, channel_type, dataset):
    temp_table = pd.DataFrame(media['Date'].values, columns=['Date'])
    sum_calc = 0
    for temperature in ['cold', 'hot', 'warm']:
        temp_table[str(channel) + str('_') + str(channel_type) + str('_') + str(temperature)] = extract_columns_extra(channel, temperature, channel_type, media)[0].sum(axis=1)
        temp_table[str(channel) + str('_') + str('cost') + str('_') + str(temperature)] = extract_columns_extra_2(channel, temperature, channel_type, media)[1].sum(axis=1)
        sum_calc = sum_calc + extract_columns_extra(channel, temperature, channel_type, media)[0].sum().sum()
    sum_start = extract_columns_extra(channel, 'total', channel_type, media)[0].sum().sum()
    # print(channel, sum_start, sum_calc)
    return(temp_table)

def get_new_column(channel, temperature, channel_type, dataset):
    column_new = extract_columns_extra(channel, temperature, features_dict[channel][0], dataset)
    channel_check = extract_columns_extra(channel, 'total', features_dict[channel][0], dataset)
    if (column_new.sum().sum() == channel_check.sum().sum()):
        # print(channel, 'ok')
    else:
        # print(channel, 'wrong')
    return(column_new)