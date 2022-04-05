# Import additional functions needed for MMM modelling in Publicis Media
from scipy.optimize import minimize, Bounds, LinearConstraint, curve_fit
from sklearn_pandas import DataFrameMapper, gen_features
from sklearn.linear_model import Lasso, Ridge, LinearRegression, ElasticNet
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, LabelBinarizer
from sklearn.impute import SimpleImputer
from sklearn.metrics import r2_score, mean_squared_error, median_absolute_error
from lmfit import Parameters, Minimizer, report_fit, Parameters
from sklearn.base import BaseEstimator, TransformerMixin
import math
import statsmodels.api as sm
import plotly.graph_objects as go
import pysolnp
import mmm_python
import itertools

from scipy import stats
# General tools to work with tables, numbers and strings
import pandas as pd
import numpy as np
import pickle
import random
import datetime
pd.options.display.max_columns = 500

# Optimisation and modelling modules

# Set option to display more columns in pandas data frame
pd.set_option('display.max_columns', 150)

# Set hyperparameters
RANDOM_STATE = 42


def resp_poly(revenue_table, col):
    xp = list(revenue_table['Investment'].values)
    fp = list(revenue_table[col].values)
#     fp = [np.exp(-1/(x+10**4)) for x in fp]
    min_a1_x, max_a1_x = min(xp), max(xp)
    new_a1_x = list(revenue_table['Investment'].values)
    a1_coefs = np.polyfit(xp, fp, 15)
    return(np.poly1d(a1_coefs))


def get_param_name(name, params):
    if '__lm_coef' in name:
        return('__lm_coef')
    if '__diminish_beta' in name:
        return('__diminish_beta')
    if '__diminish_gamma' in name:
        return('__diminish_gamma')
    if '__lambda_ad' in name:
        return('__lambda_ad')


def get_params_table(params):
    params_data = pd.DataFrame(
        columns=['feach_name', 'coef_name', 'value', 'min_val', 'max_val', 'vary'])

    for col in params.keys():
        coef_name = get_param_name(params[col].name, params)
        feach_name = params[col].name.replace(coef_name, '')
        value = params[col].value
        min_val = params[col].min
        max_val = params[col].max
        vary = params[col].vary
        line = [feach_name, coef_name, value, min_val, max_val, vary]
        row = pd.Series(line, ['feach_name', 'coef_name',
                               'value', 'min_val', 'max_val', 'vary'])
        params_data = params_data.append([row])

    return(params_data)


def neg_exponential_form(grp, beta, gamma, alpha=0, c=0):
    return alpha + gamma*(1 - np.exp(-(beta*grp)))


def exp_just_draw_v2(revenue_table, params_table, product_price=1, period=12, budg=2*10**9, grad=10**6, means=dict(), media_costs=dict()):
    opt_rev_dict = {}
    revenue_coefficients = pd.DataFrame(columns=['media', 'beta', 'gamma'])
    channels = [x for x in revenue_table.drop(
        columns='Investment').columns if x[-5:] != '_ROAS']

    for channel in channels:
        print(channel)
        delt = media_costs[media_costs['media'] == channel]['cost'].values[0]
        revenue_table[channel] = revenue_table[channel]*product_price
        grp = np.array(revenue_table['Investment'].values)
        ydata = np.array(revenue_table[channel].values)

        beta_start = float(params_table[params_table['feach_name']+params_table['coef_name'] == (
            channel+'__diminish_beta')]['value'].values[0])
        gamma_start = float(params_table[params_table['feach_name']+params_table['coef_name'] == (channel+'__diminish_gamma')]['value'].values[0])*float(
            params_table[params_table['feach_name']+params_table['coef_name'] == (channel+'__lm_coef')]['value'].values[0])
        for i in range(-5, 5, 1):
            scale = 10**(i)
            p0 = (beta_start/(scale*delt), gamma_start)
            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='trf', bounds=(
                [0.000000000001, 0.0000000000001], [10000000, 100000000]), maxfev=10000000)
            resp = neg_exponential_form(
                np.array(range(0, budg, gran)), popt[0], popt[1], alpha=0, c=0)
            error = abs(sum(resp)-sum(ydata))/sum(ydata)
            if (error < 10**(-10)):
                break
        revenue_coefficients = revenue_coefficients.append(
            {'media': channel, 'beta': popt[0], 'gamma': popt[1]}, ignore_index=True)
    return revenue_coefficients


def get_nec_coef(channels, params_table, product_price=1, period=12, budg=2*10**9, grad=10**6, means=dict()):
    revenue_coefficients = pd.DataFrame(columns=['media', 'beta', 'gamma'])
    for channel in channels:
        beta_start = float(params_table[params_table['feach_name']+params_table['coef_name'] == (
            channel+'__diminish_beta')]['value'].values[0])
        gamma_start = float(params_table[params_table['feach_name']+params_table['coef_name'] == (channel+'__diminish_gamma')]['value'].values[0])*float(
            params_table[params_table['feach_name']+params_table['coef_name'] == (channel+'__lm_coef')]['value'].values[0])

        revenue_coefficients = revenue_coefficients.append(
            {'media': channel, 'beta': beta_start, 'gamma': gamma_start}, ignore_index=True)
    return revenue_coefficients


def plot_curves(revenue_coefficients, budg=4*10**6, gran=10**4):
    grp = np.array(range(0, budg, gran))
    fig = go.Figure()
    for col in revenue_coefficients['media'].values:
        beta = revenue_coefficients[revenue_coefficients['media']
                                    == col]['beta'].values[0]
        gamma = revenue_coefficients[revenue_coefficients['media']
                                     == col]['gamma'].values[0]
        pred = neg_exponential_form(grp, beta, gamma)

        fig.add_trace(go.Scatter(
            x=grp,
            y=pred*means[dep_var[0]],
            name=col+'_response'
        ))
    fig.update_layout(title='Response curves',
                      xaxis_title='Daily investments (in USD)',
                      yaxis_title='Expected daily respinse in comparison')

    fig.show()


def calc_impact(dataset, decomposition_df, media_costs, product_price):
    impact_table = pd.DataFrame()
    #impact_table['media'] = ad_vars_agg
    impact_table = media_costs.drop(columns=['cost', 'comment'])
    impact_table['Budget'] = impact_table['media'].apply(lambda x: round(
        dataset[rev_vars[x]].sum(), 0))
#     impact_table=impact_table[impact_table['Budget']>=10**6].reset_index(drop=True)
#    impact_table['Budget']=impact_table['Budget'].apply(lambda x: 10**6 if x<10**6 else x)
    impact_table['Impact'] = impact_table['media'].apply(
        lambda x: round(decomposition_df[x].sum(), 0))
#    impact_table['ROAS']=impact_table.apply(lambda x: revenue_table.loc[revenue_table['Investment']==round(x['Budget']/10**6,0)*10**6][x['media']+'_ROAS'].values[0]/10**6, axis=1)
    impact_table['ROAS'] = round(
        (impact_table['Impact']*product_price)/impact_table['Budget'], 1)
    return(impact_table.sort_values('ROAS', ascending=False))


def calc_impact_v2(dataset, decomposition_df, media_costs, product_price):

    impact_table = media_costs.drop(columns=['cost', 'comment'])
    impact_table['Budget'] = impact_table['media'].apply(lambda x: round(dataset[dataset[x[:-7]+'_Spend'] > 0][x[:-7]+'_Spend'].rolling(
        7).sum().mean(), 0) if x in clicks else round(dataset[dataset[x[:-12]+'_Spend'] > 0][x[:-12]+'_Spend'].rolling(7).sum().mean(), 0))
#     impact_table=impact_table[impact_table['Budget']>=10**6].reset_index(drop=True)
#    impact_table['Budget']=impact_table['Budget'].apply(lambda x: 10**6 if x<10**6 else x)
    impact_table['Impact'] = impact_table['media'].apply(lambda x: round(
        decomposition_df[decomposition_df[x] > 0][x].rolling(7).sum().mean(), 0))
#    impact_table['ROAS']=impact_table.apply(lambda x: revenue_table.loc[revenue_table['Investment']==round(x['Budget']/10**6,0)*10**6][x['media']+'_ROAS'].values[0]/10**6, axis=1)
    impact_table['ROAS'] = round(
        (impact_table['Impact']*product_price)/impact_table['Budget'], 1)
    return(impact_table.sort_values('ROAS', ascending=False))


def get_impact(dataset, decomposition_table, years=[2019, 2020], month=12):
    impact_all = pd.DataFrame(ad_vars, columns=['media'])
    for year in years:
        if year != 2020:
            dataset_temp = dataset[dataset['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            decomposition_df = decomposition_table.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            impact_temp = calc_impact(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)})
            impact_all = impact_all.merge(
                impact_temp, on=['media'], how='left')
        if month > 0:
            dataset_temp = dataset[dataset['Date'].apply(lambda x: (
                x.year == year and x.month <= month))].reset_index(drop=True)
            decomposition_df = decomposition_df_sshape.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year and x.month <= month))].reset_index(drop=True)
            impact_temp = calc_impact(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)+'_till_june'})
        impact_all = impact_all.merge(impact_temp, on=['media'], how='left')
    return(impact_all.reset_index(drop=True))

def get_impact_3(dataset, decomposition_table, years=[2019, 2020], month=12):
    impact_all = pd.DataFrame(ad_vars, columns=['media'])
    for year in years:
        if year != 2020:
            dataset_temp = dataset[dataset['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            decomposition_df = decomposition_table.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            impact_temp = calc_impact(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)})
            impact_all = impact_all.merge(
                impact_temp, on=['media'], how='left')
        if month > 0:
            dataset_temp = dataset[dataset['Date'].apply(lambda x: (
                x.year == year and x.month <= month))].reset_index(drop=True)
            
            dataset_agg = pd.DataFrame()
            for col in ad_vars_agg:
                dataset_agg[col] = dataset_temp[[x for x in dataset_temp.columns if col in x]].sum(axis=1)
                
            dataset_agg['Date'] = dataset['Date']
            
            dataset_temp = dataset_agg.copy()
            
            decomposition_df = decomposition_df_sshape.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year and x.month <= month))].reset_index(drop=True)
            
            decomposition_df_agg = pd.DataFrame()
            for col in ad_vars_agg:
                decomposition_df_agg[col] = decomposition_df_temp[[x for x in decomposition_df_temp.columns if col in x]].sum(axis=1)
              
            decomposition_df_agg['Date'] = decomposition_df_temp['Date']
            decomposition_df_agg = decomposition_df_agg.groupby('Date').sum()
            
            
            decomposition_df_temp= decomposition_df_agg.copy()
            
            impact_temp = calc_impact(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)+'_till_june'})
        impact_all = impact_all.merge(impact_temp, on=['media'], how='left')
    return(impact_all.reset_index(drop=True))





def get_impact_v2(dataset, decomposition_table, years=[2019, 2020], month=12):
    impact_all = pd.DataFrame(ad_vars, columns=['media'])
    for year in years:
        if year != 2020:
            dataset_temp = dataset[dataset['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            decomposition_df = decomposition_df_sshape.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year))].reset_index(drop=True)
            impact_temp = calc_impact_v2(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)})
            impact_all = impact_all.merge(
                impact_temp, on=['media'], how='left')
        if month > 0:
            dataset_temp = dataset[dataset['Date'].apply(lambda x: (
                x.year == year and x.month <= month))].reset_index(drop=True)
            decomposition_df = decomposition_df_sshape.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year and x.month <= month))].reset_index(drop=True)
            impact_temp = calc_impact_v2(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)+'_till_june'})
        impact_all = impact_all.merge(impact_temp, on=['media'], how='left')
    return(impact_all.reset_index(drop=True))


def get_impact(dataset, decomposition_table, years=[2019, 2020], month_start = 1 , month_end = 12):
    impact_all = pd.DataFrame(ad_vars, columns=['media'])
    for year in years:
#         if year != 2020:
#             dataset_temp = dataset[dataset['Date'].apply(
#                 lambda x: (x.year == year))].reset_index(drop=True)
#             decomposition_df = decomposition_table.reset_index()
#             decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
#                 lambda x: (x.year == year))].reset_index(drop=True)
#             impact_temp = calc_impact(
#                 dataset_temp, decomposition_df_temp, media_costs, product_price)
#             for col in impact_temp.drop(columns=['media']).columns:
#                 impact_temp = impact_temp.rename(
#                     columns={col: col+'_'+str(year)})
#             impact_all = impact_all.merge(
#                 impact_temp, on=['media'], how='left')
        if month_start > 0:
            dataset_temp = dataset[dataset['Date'].apply(lambda x: (
                x.year == year and x.month <= month_end and x.month >= month_start))].reset_index(drop=True)
            decomposition_df = decomposition_df_sshape.reset_index()
            decomposition_df_temp = decomposition_df[decomposition_df['Date'].apply(
                lambda x: (x.year == year and x.month <= month_end and x.month >= month_start))].reset_index(drop=True)
            impact_temp = calc_impact(
                dataset_temp, decomposition_df_temp, media_costs, product_price)
            for col in impact_temp.drop(columns=['media']).columns:
                impact_temp = impact_temp.rename(
                    columns={col: col+'_'+str(year)})
        impact_all = impact_all.merge(impact_temp, on=['media'], how='left')
    return(impact_all.reset_index(drop=True))


def mroas_calc(rev_table, product_price, temp = 'none'):

    names = [x for x in rev_table.columns if temp in x and '_ROAS' not in x]
    
    if temp == 'none':
        names = [x for x in rev_table.columns if 'Investment' not in x and '_ROAS' not in x]

    revenue_table_temp = rev_table[['Investment'] + names].copy()
    
    for col in names:
        revenue_table_temp[col] = revenue_table_temp[col]*product_price
    
    revenue_table_temp['Investment'] = revenue_table_temp['Investment']/1000000 
    
    
    
    mroas_table = revenue_table_temp.copy()
    one_mroas_table = pd.DataFrame(columns = ['Channel', 'Investment', 'Revenue'])
    for col in names:
        mroas_table[col] = (mroas_table[col] - mroas_table.shift(1)[col])/(mroas_table['Investment'] - mroas_table.shift(1)['Investment'])  
        temp_ind = mroas_table[mroas_table[col]>1].index[-1]
        col_inv = mroas_table[temp_ind - 1:temp_ind]['Investment'].values[0]
        col_rev = revenue_table_temp[revenue_table_temp['Investment'] == col_inv][col].values[0]
        temp_tb = pd.DataFrame([[col, col_inv, col_rev]], columns = ['Channel', 'Investment', 'Revenue'])
        one_mroas_table = pd.concat([one_mroas_table , temp_tb])
    one_mroas_table = one_mroas_table.reset_index(drop = True)

        
        
    return(one_mroas_table)


