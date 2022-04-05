# Author: Vladislav Zamkovoy
# Date created: 17.12.2019

# Date last modified: 06.04.2020
# Last modified by: Asset Karazhay
# JIRA link: MMM Light // коробочное решение для оптимизация трудозатрат на тендеры (питчи)


import pandas as pd
import numpy as np
import statsmodels.api as sm
import math 
from sklearn.base import BaseEstimator, TransformerMixin
from lmfit import Parameters, Minimizer, report_fit, Parameters
from sklearn.metrics import r2_score, mean_squared_error, median_absolute_error

import os
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler, LabelBinarizer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import Lasso, Ridge, LinearRegression, ElasticNet
from sklearn.metrics import r2_score, mean_squared_error, median_absolute_error

from sklearn_pandas import DataFrameMapper, gen_features

from lmfit import Parameters, Minimizer, report_fit, Parameters

from scipy.optimize import minimize, Bounds, LinearConstraint, curve_fit

import random
import plotly.graph_objects as go

def neg_exponential_form_budget(grp, beta, gamma, alpha=0, c=0):
    return alpha + gamma*(1 - np.exp(-(beta*grp)))

def log_form_budget(x, steep, EC50,lm,cap):
    return (cap / (1+np.exp((-steep)*(x/cap-EC50))) - cap/(1+np.exp(steep*EC50)))*lm

def neg_exponential_form(x, beta, gamma, alpha=0, c=0):
    expfunc=[0 for i in range(0,len(x))]
    for i in range(len(x)):
        expfunc[i]=alpha + gamma*(1 - np.exp(-(beta*x[i])))-x[i]
    return np.array(expfunc)

def log_form(x, steep, EC50,lm,budg=10**9,level=1):
    cap = budg
    logfunc = [0 for i in range(0,len(x))]

    for i in range(len(x)):
        logfunc[i] = (cap / (1+np.exp((-steep)*(x[i]/cap-EC50))) - cap/(1+np.exp(steep*EC50)))*lm-x[i]

    return np.array(logfunc)

class DataFrameAttrsEditor(BaseEstimator, TransformerMixin):
    
    def __init__(self):
        """
        Edits raw features
        """
    
    def fit(self, X, y=None):
        return self
    
    def transform(self, X, y=None):
        X_copy = X.copy()
        
        return X_copy
    
def MMM_dataset(dataset, dep_var, ad_vars, market_vars, extra_vars, date):
    base_feach = [x for x in ['baseline', 'seasonality', 'trend'] if x in market_vars]
    print(base_feach)
    columns=[]
    columns.append(date)
    columns=columns+dep_var+ad_vars+extra_vars+[x for x in market_vars if x not in ['baseline', 'seasonality', 'trend']]
    DT = dataset[columns]
    DT = DT.set_index(date)
    DT=DT.copy()
    if 'baseline' in base_feach:
        DT['baseline']=1
    
    #decomposition = sm.tsa.seasonal_decompose(DT[dep_var], model='additive')
    #if 'baseline' in base_feach:
    #    DT['baseline']=1
    if 'seasonality' in base_feach:
        decomposition = sm.tsa.seasonal_decompose(DT[dep_var], model='additive')
        DT['seasonality']=decomposition.seasonal
    #if 'trend' in base_feach:
    #    DT['trend']=decomposition.trend     
    return DT

class DiminishingReturnsEffectTransformer(BaseEstimator, TransformerMixin):
    
    def __init__(self, gamma, beta, lambda_ad):
        """
        Diminishing Returns Effect Transformation
        
        f(x) = gamma * (1 - exp(-beta * x))
        
        Where:
            - gamma is a maximum saturation
            - beta is a saturation rate, beta > 0
        """
        self.gamma = gamma
        self.beta = beta
        self.lambda_ad = lambda_ad
        
    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        X_copy = X.copy()
        
        X_copy = self.gamma * (1 - np.exp(-self.beta * X_copy))
        
        X_copy=AdStock(X_copy, self.lambda_ad)
        return X_copy
    
class LogEffectTransformer(BaseEstimator, TransformerMixin):
    
    def __init__(self, steep, EC50, level, lambda_ad):
        """
        Diminishing Returns Effect Transformation
        
        f(x) = gamma * (1 - exp(-beta * x))
        
        Where:
            - gamma is a maximum saturation
            - beta is a saturation rate, beta > 0
        """
        
        self.steep = steep
        self.EC50 = EC50
        self.level = level
        self.lambda_ad = lambda_ad
        
    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        X_copy = X.copy()
        
        X_copy=LogFunc(X_copy, self.steep, self.EC50, self.level)
        
        X_copy=AdStock(X_copy, self.lambda_ad)
        return X_copy    
    
    
def get_data_pipeline(params,  
                      cols2impute, cols2diminish, cols2log):
    
    # Imputation
    feature_imputer = [
        ([col], SimpleImputer(strategy='constant', fill_value=0))
        for col in cols2impute
    ]
    
    # Diminishing Returns Effect
    feature_diminisher = []
    for col in cols2diminish:
        col_transformer = DiminishingReturnsEffectTransformer(
            gamma=params[col + '__diminish_gamma'], 
            beta=params[col + '__diminish_beta'],
            lambda_ad=params[col + '__lambda_ad']
        )
        feature_diminisher.append((col, col_transformer))

    # Log Effect
    feature_log = []
    for col in cols2log:
        col_transformer = LogEffectTransformer(
            steep=params[col + '__log_steep'], 
            EC50=params[col + '__log_EC50'],
            level=params[col + '__log_level'],
            lambda_ad=params[col + '__lambda_ad']
        )
        feature_log.append((col, col_transformer))
    
    # Data Pipeline
    default_args = {
        'input_df': True, 
        'df_out': True, 
        'default': None
    }
    data_pipeline = Pipeline([
        ('feature_editor', DataFrameAttrsEditor()),
        ('feature_imputer', DataFrameMapper(feature_imputer, **default_args)),
        ('feature_diminisher', DataFrameMapper(feature_diminisher, **default_args)),
        ('feature_log', DataFrameMapper(feature_log, **default_args))
    ])
    
    return data_pipeline

def get_prediction(params, X, cols2impute, cols2diminish, cols2log):

    # Building data pipeline
    data_pipeline = get_data_pipeline(
        params=params,
        cols2impute=cols2impute, 
        cols2diminish=cols2diminish,
        cols2log=cols2log
    )
    
    # X_train transformation
    X_prep = data_pipeline.fit_transform(X)
    
    # Prediction
    coefs = []
    for col in X_prep.columns:
        coefs.append(params[col + '__lm_coef'])
    
    y_pred = X_prep.values.dot(coefs)
    
    return y_pred

def lmfit_objective(params, X, y, cols2impute, cols2diminish, cols2log):
    
    # Building prediction
    y_pred = get_prediction(params, X, cols2impute, cols2diminish, cols2log)
    
    return (y_pred - y).values

def AdStock(x, lmb=0):
    adstock = x 
    adstock[0] = (1-lmb) * x[0]

    for i in range(1, len(x)):
        adstock[i] = (1-lmb) * x[i] + lmb * adstock[i-1]  

    return adstock    
	
	




def decomposition(decomposition_df):
    colors=['aliceblue', 'antiquewhite', 'aqua', 'aquamarine', 'azure',
    'beige', 'bisque', 'black', 'blanchedalmond', 'blue',
    'blueviolet', 'brown', 'burlywood', 'cadetblue',
    'chartreuse', 'chocolate', 'coral', 'cornflowerblue',
    'cornsilk', 'crimson', 'cyan', 'darkblue', 'darkcyan',
    'darkgoldenrod', 'darkgray', 'darkgrey', 'darkgreen',
    'darkkhaki', 'darkmagenta', 'darkolivegreen', 'darkorange',
    'darkorchid', 'darkred', 'darksalmon', 'darkseagreen',
    'darkslateblue', 'darkslategray', 'darkslategrey',
    'darkturquoise', 'darkviolet', 'deeppink', 'deepskyblue',
    'dimgray', 'dimgrey', 'dodgerblue', 'firebrick',
    'floralwhite', 'forestgreen', 'fuchsia', 'gainsboro',
    'ghostwhite', 'gold', 'goldenrod', 'gray', 'grey', 'green',
    'greenyellow', 'honeydew', 'hotpink', 'indianred', 'indigo',
    'ivory', 'khaki', 'lavender', 'lavenderblush', 'lawngreen',
    'lemonchiffon', 'lightblue', 'lightcoral', 'lightcyan',
    'lightgoldenrodyellow', 'lightgray', 'lightgrey',
    'lightgreen', 'lightpink', 'lightsalmon', 'lightseagreen',
    'lightskyblue', 'lightslategray', 'lightslategrey',
    'lightsteelblue', 'lightyellow', 'lime', 'limegreen',
    'linen', 'magenta', 'maroon', 'mediumaquamarine',
    'mediumblue', 'mediumorchid', 'mediumpurple',
    'mediumseagreen', 'mediumslateblue', 'mediumspringgreen',
    'mediumturquoise', 'mediumvioletred', 'midnightblue',
    'mintcream', 'mistyrose', 'moccasin', 'navajowhite', 'navy',
    'oldlace', 'olive', 'olivedrab', 'orange', 'orangered',
    'orchid', 'palegoldenrod', 'palegreen', 'paleturquoise',
    'palevioletred', 'papayawhip', 'peachpuff', 'peru', 'pink',
    'plum', 'powderblue', 'purple', 'red', 'rosybrown',
    'royalblue', 'saddlebrown', 'salmon', 'sandybrown',
    'seagreen', 'seashell', 'sienna', 'silver', 'skyblue',
    'slateblue', 'slategray', 'slategrey', 'snow', 'springgreen',
    'steelblue', 'tan', 'teal', 'thistle', 'tomato', 'turquoise',
    'violet', 'wheat', 'white', 'whitesmoke', 'yellow',
    'yellowgreen']
    

def decomposition(decomposition_df):
    colors=['aliceblue', 'antiquewhite', 'aqua', 'aquamarine', 'azure',
    'beige', 'bisque', 'black', 'blanchedalmond', 'blue',
    'blueviolet', 'brown', 'burlywood', 'cadetblue',
    'chartreuse', 'chocolate', 'coral', 'cornflowerblue',
    'cornsilk', 'crimson', 'cyan', 'darkblue', 'darkcyan',
    'darkgoldenrod', 'darkgray', 'darkgrey', 'darkgreen',
    'darkkhaki', 'darkmagenta', 'darkolivegreen', 'darkorange',
    'darkorchid', 'darkred', 'darksalmon', 'darkseagreen',
    'darkslateblue', 'darkslategray', 'darkslategrey',
    'darkturquoise', 'darkviolet', 'deeppink', 'deepskyblue',
    'dimgray', 'dimgrey', 'dodgerblue', 'firebrick',
    'floralwhite', 'forestgreen', 'fuchsia', 'gainsboro',
    'ghostwhite', 'gold', 'goldenrod', 'gray', 'grey', 'green',
    'greenyellow', 'honeydew', 'hotpink', 'indianred', 'indigo',
    'ivory', 'khaki', 'lavender', 'lavenderblush', 'lawngreen',
    'lemonchiffon', 'lightblue', 'lightcoral', 'lightcyan',
    'lightgoldenrodyellow', 'lightgray', 'lightgrey',
    'lightgreen', 'lightpink', 'lightsalmon', 'lightseagreen',
    'lightskyblue', 'lightslategray', 'lightslategrey',
    'lightsteelblue', 'lightyellow', 'lime', 'limegreen',
    'linen', 'magenta', 'maroon', 'mediumaquamarine',
    'mediumblue', 'mediumorchid', 'mediumpurple',
    'mediumseagreen', 'mediumslateblue', 'mediumspringgreen',
    'mediumturquoise', 'mediumvioletred', 'midnightblue',
    'mintcream', 'mistyrose', 'moccasin', 'navajowhite', 'navy',
    'oldlace', 'olive', 'olivedrab', 'orange', 'orangered',
    'orchid', 'palegoldenrod', 'palegreen', 'paleturquoise',
    'palevioletred', 'papayawhip', 'peachpuff', 'peru', 'pink',
    'plum', 'powderblue', 'purple', 'red', 'rosybrown',
    'royalblue', 'saddlebrown', 'salmon', 'sandybrown',
    'seagreen', 'seashell', 'sienna', 'silver', 'skyblue',
    'slateblue', 'slategray', 'slategrey', 'snow', 'springgreen',
    'steelblue', 'tan', 'teal', 'thistle', 'tomato', 'turquoise',
    'violet', 'wheat', 'white', 'whitesmoke', 'yellow',
    'yellowgreen']


    
    fit=decomposition_df['fitted']
    act=decomposition_df['actual']
    data_help=decomposition_df.drop(columns=['fitted', 'actual'])
    col_both=[]
    col_pos=[]
    col_neg=[]
    for col in data_help:
        if col not in ['actual', 'fitted']:
            if (np.sign(data_help[col].max())-np.sign(data_help[col].min()))==2:
                col_both.append(col)
            else:
                if data_help[col].max()>0:
                    col_pos.append(col)
                else:
                    col_neg.append(col)
                    
                    
                    

    data_help=decomposition_df
    base=decomposition_df['baseline']
    col_h='baseline'

    fig = go.Figure()



    fig.add_trace(go.Scatter(x=decomposition_df.index, y=data_help[col_h].values, fill='tozeroy',
                            mode='none', name=col_h, stackgroup='two', fillcolor=random.choice(colors)  # override default markers+lines
                            ))

    for col in data_help.drop(columns=['baseline']).columns:

        if col in col_pos:
            fig.add_trace(go.Scatter(x=decomposition_df.index, y=data_help[col].values, fill='tonexty',
                            mode='none', name=col, stackgroup='two', fillcolor=random.choice(colors)   # override default markers+lines
                            ))



        if col in col_neg: 
            fig.add_trace(go.Scatter(x=decomposition_df.index, y=data_help[col].values, fill='tonexty',
                            mode='none', name=col,stackgroup='one', fillcolor=random.choice(colors)  # override default markers+lines
                            ))  


        if col in col_both:
            color=random_num = random.choice(colors)  
            fig.add_trace(go.Scatter(x=decomposition_df.index, y=data_help[col].clip(lower=0).values, fill='tonexty',
                            mode='none', name=col, stackgroup='two', fillcolor=color   # override default markers+lines
                            )) 

            fig.add_trace(go.Scatter(x=decomposition_df.index, y=data_help[col].clip(upper=0).values, fill='tonexty',
                            mode='none', name=col, stackgroup='one', fillcolor=color, showlegend=False   # override default markers+lines
                            )) 



    fig.add_trace(go.Scatter(
        x=decomposition_df.index,
        y=fit.values,
        name='Fitted', line_color="crimson"
    ))

    fig.add_trace(go.Scatter(
        x=decomposition_df.index,
        y=act.values,
        name='Actual', line_color="green"
    ))
    
    fig.update_layout(title='Decomposition of factors by its input into dependent variable',
                   xaxis_title='period',
                   yaxis_title='data')
	
	
	
    fig.show()
    
    fig = go.Figure()
    
    fig.add_trace(go.Scatter(
    x=decomposition_df.index,
    y=decomposition_df['actual'],
    name='Actual', line_color="green"
    ))

    fig.add_trace(go.Scatter(
    x=decomposition_df.index,
    y=decomposition_df['fitted'],
    name='Fitted', line_color="crimson"
    ))
    fig.update_layout(title='Actual and fitted values of the model',
                   xaxis_title='period',
                   yaxis_title='data')
    
    fig.show()

	
	
	
def model_dt(result, X_train, y_train, cols2impute, cols2diminish, cols2log):
    best_params=result.params.valuesdict()
    y_pred = get_prediction(best_params, X_train, cols2impute, cols2diminish, cols2log)

    best_data_pipeline = get_data_pipeline(
        params=best_params, 
        cols2impute=cols2impute, 
        cols2diminish=cols2diminish,
        cols2log=cols2log
    )

    # X_train transformation
    X_train_prep = best_data_pipeline.fit_transform(X_train)

    best_coefs = []
    for col in X_train_prep.columns:
        best_coefs.append(best_params[col + '__lm_coef'])

    target_df = pd.DataFrame(
        data={
            'actual': y_train,
            'fitted': y_pred
        },
        index=y_train.index
    )
    decomposition_df = pd.concat((target_df, X_train_prep * best_coefs), axis=1)

    return(decomposition_df)
	
	
	
def model(params,cols2impute,cols2diminish,cols2log,X_train,y_train, method, maxiter=200):
    data_pipeline = get_data_pipeline(
        params=params,
        cols2impute=cols2impute, 
        cols2diminish=cols2diminish,
        cols2log=cols2log
        )

    minner = Minimizer(
        userfcn=lmfit_objective,

        params=params,
        fcn_args=(X_train, y_train,cols2impute,cols2diminish, cols2log)
    )


    result = minner.minimize(method=method, **{'options': {'verbose': 3, 'maxiter': maxiter}})
    #result = minner.minimize(method=method)
    return(result)
	
	


def ARL(x,beta=0.01,gamma=1,a=0.3,steep=1,EC50=.5,level=1,lag=0,adb='exp',lag_na=True,window=np.nan,last=np.nan):
    arl = x
  #response curve
    if steep!=0:
        if adb=='s-shape':
            arl = LogFunc(arl,steep,EC50,level)
        if adb=='exp':
            arl = NegExp(arl, beta, gamma, alpha=0, c=0)
    
  #adstock
    arl = AdStock(arl,a)
    return arl

  #lags
#     if lag < 0:
#         print("????????????? ???????? lag, ???????? ???????? ???? ??????  ?? ???????!")
#     arl = shift(arl,-round(lag))

#   #NA ?????? ?? ????  
#     if lag_na==False:
#         arl[np.isnan(arl)] = 0

#     if np.isnan(last):
#         last = arl.shape[0]

#     if np.isnan(window):
#         window = last

#     arl = arl[(last-window):last]

    return arl
	
	
def shift_one(x, n):
    if n > 0:
        return np.concatenate((x[n:], np.full(n, np.nan)))[:x.shape[0]]
    elif n < 0:
        return np.concatenate((np.full(np.abs(n), np.nan), x[:n]))[:x.shape[0]]
    else:
        return x

def shift(x, shift_by):
#     return np.stack([shift_one(x, shift_by[i]) for i in range(len(shift_by))], axis=1)
    return np.stack([shift_one(x, shift_by)], axis=1)
	
	
	
def AdBudg(x, steep, EC50=0.5,level=1):
    cap = level #capacity
    adbudg = x

    for i in range(len(x)):
        if (x[i]==0):
            adbudg[i]=0
        else:
            adbudg[i] = cap / (1+(x[i]/(cap*EC50))**(-steep))

    return adbudg
	
def NegExp(grp, beta, gamma, alpha=0, c=0):
    return [(alpha + gamma*(1 - np.exp(-beta*x))) for x in grp]

def LogFunc(x, steep, EC50,level,budg=10**9):
    cap = level #capacity
    logfunc = [0 for i in range(0,len(x))]

    for i in range(len(x)):
        logfunc[i] = (cap / (1+np.exp((-steep)*(x[i]/cap-EC50))) - cap/(1+np.exp(steep*EC50)))

    return np.array(logfunc)
	
	
def direct_response_curves(budg,best_params,media_costs,model,period=12, gran=10**6): 
    investment=range(0,budg, gran)
    #investment=range(0,budg,10**3)
    revenue_table=pd.DataFrame()
    if model=='exp':
        for chanel in media_costs.media.values:
            rev=[]
            lambda_coef=best_params[chanel+'__lambda_ad']
            beta_coef=best_params[chanel+'__diminish_beta']
            gamma_coef=best_params[chanel+'__diminish_gamma']
            lm_coef=best_params[chanel+'__lm_coef']
            if gamma_coef>0:
                for inst in investment:
                    a=inst/period/media_costs[media_costs['media']==chanel].cost.values[0]
                    rev.append(sum(ARL([a for x in range(0,period)],a=lambda_coef , beta=beta_coef, gamma=gamma_coef, adb=model)))
                rev=[i*lm_coef for i in rev]
                revenue_table[chanel]=rev
            else:
                for inst in investment:
                    a=inst/period/media_costs[media_costs['media']==chanel].cost.values[0]
                    rev.append(-sum(ARL([a for x in range(0,period)],a=lambda_coef , beta=beta_coef, gamma=gamma_coef, adb=model)))
                rev=[i*lm_coef for i in rev]
                revenue_table[chanel]=rev
    if model=='s-shape':
        for chanel in media_costs.media.values:
            rev=[]
            steep_coef=best_params[chanel+'__log_steep'] 
            EC50_coef=best_params[chanel+'__log_EC50']
            level_coef=best_params[chanel+'__log_level'] 
            lambda_coef=best_params[chanel+'__lambda_ad']
            lm_coef=best_params[chanel+'__lm_coef']
            for inst in investment:
                a=inst/period/media_costs[media_costs['media']==chanel].cost.values[0]
                rev.append(sum(ARL([a for x in range(0,period)],a=lambda_coef, beta=0.1, gamma=1, steep=steep_coef, EC50=EC50_coef, level=level_coef, adb=model)))
            rev=[i*lm_coef for i in rev]
            revenue_table[chanel]=rev
    return revenue_table
    
	
def response_curves_plot(revenue_table):
    fig = go.Figure()

    for col in revenue_table.drop(columns='Investment').columns:
        if col[-5:]!='_ROAS':
            fig.add_trace(go.Scatter(
                #x=list(range(0,revenue_table.shape[0])),
                x=revenue_table['Investment'].values,
                y=revenue_table[col].values,
                name=col+'_response'
            ))
    fig.update_layout(title='Response curves',
                       xaxis_title='Daily investments (in USD)',
                       yaxis_title='Expected daily response (Mobile registrations)')
    fig.show()
    
	
def ROAS_curves_plot(revenue_table, a=1):
    fig = go.Figure()
    for col in revenue_table.drop(columns='Investment').columns:
        if col[-5:]!='_ROAS':
            fig.add_trace(go.Scatter(
                x=revenue_table['Investment'].values,
                y=revenue_table[col+'_ROAS'].values/a,
                name=col+'_ROAS'
            ))
	
    fig.update_layout(title='ROAS curves',
                   xaxis_title='Daily investments (in USD)',
                   yaxis_title='Expected daily ROAS in comparison')
	
    fig.show()
	
def ROAS_curves_plot_opt(revenue_table, budg, product_price):
    investment=range(0,budg,10**6)
    revenue_table['Investment']=investment
    for col in revenue_table.drop(columns='Investment'):
        revenue_table[col+'_ROAS']=revenue_table[col]*product_price-revenue_table['Investment']
        #revenue_table[col+'_ROAS'].plot()

# Get coefficients of revenue function for every channel 
def MMM_revenue_coefficients(response_curves):
    results = []
    # Extract Revenues
    response_curves_coef=response_curves.copy()
    for col in response_curves_coef[[x for x in response_curves_coef.columns if 'roas' in x.lower()]].columns:
        response_curves_coef[col]=response_curves_coef[col]+response_curves_coef['Investment']
    
    ROAS = pd.concat([response_curves_coef['Investment'],
                    (response_curves_coef[[x for x in response_curves_coef.columns if 'roas' in x.lower()]])],
                   axis=1)

    # Create a table of coefficients allowing to fit the data
    channel_list = ROAS.columns.tolist()[1:]
    
    for channel in channel_list:
        
        grp = np.array(ROAS['Investment'])        
        ydata = np.array(ROAS[channel])
        p0 = (0.00000001,ROAS[channel].mean())
        
        popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)
        
        results.append(popt)
        
    results_df = pd.DataFrame(results, columns=['beta', 'gamma'])
    channels_series = pd.Series([x.replace('_ROAS', '') for x in channel_list], name='media')
        
    return pd.concat([channels_series, results_df], axis=1)


def MMM_revenue_coefficients_v2(params, media_costs, product_price, rev_vars, model):
    pd_pars = pd.DataFrame([(p.name, p.value) for p in params.values()], columns=('name', 'best-fit value'))
    
    pd_pars=pd_pars.T.reset_index(drop=True)
    pd_pars.columns = pd_pars.iloc[0]
    pd_pars.drop(pd_pars.index[0], inplace=True)
    
    revenue_coefficients=pd.DataFrame(list(rev_vars.keys()), columns=['media'])
    if model=='exp':
        for col in rev_vars:
            revenue_coefficients['beta']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__diminish_beta'].values[0])/12/media_costs[media_costs['media']==col].cost.values[0]

            revenue_coefficients['gamma']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__diminish_gamma'].values[0])*product_price*12
    if model=='s-shape':
        for col in rev_vars:
#             steep_start=steep*period*media_costs[media_costs['media']==chanel].cost.values[0]
#             EC50_start=EC50/period/media_costs[media_costs['media']==chanel].cost.values[0]
#             lm_start=lm/period/product_price  
            
            revenue_coefficients['lm_coef']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__lm_coef'].values[0])*12*product_price 
            
            revenue_coefficients['EC50']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__log_EC50'].values[0])*12*media_costs[media_costs['media']==col].cost.values[0]
            
            revenue_coefficients['steep']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__log_steep'].values[0])/12/media_costs[media_costs['media']==col].cost.values[0]
            
            revenue_coefficients['cap']=revenue_coefficients['media'].apply(lambda x: pd_pars[x+'__log_level'].values[0])

    return revenue_coefficients




# Plot budget split
def optimal_budget_plot(optimised_budget):
	# x axis
	x = optimised_budget['budget']

	fig = go.Figure()

	media_cols = list(optimised_budget.drop(columns=['budget', 'profit']))

	# plot first media variable
	fig.add_trace(go.Scatter(
						    x=x,
						    y=optimised_budget[media_cols[0]],
						    mode='lines',
						    line=dict(width=0.5),
						    name=media_cols[0],
						    stackgroup='one',
						    groupnorm='percent'
						))
    
#     plot rest of media variables
	for col in media_cols[1:]:
		fig.add_trace(go.Scatter(
                                x=x,
                                y=optimised_budget[col],
                                mode='lines',
                                line=dict(width=0.5),
                                name=col,
                                stackgroup='one'
                            ))

#     set axis and labes
	fig.update_layout(
                    title='Optimal Budget Split',
                    xaxis_title='Budget',
                    yaxis_title='Budget Split Percentage',
                    showlegend=True,
                    xaxis=dict(
                                tickmode='auto',
                                nticks=10,
                                ticksuffix=' Rub.'),
                    yaxis=dict(
                                type='linear',
                                range=[1, 100],
                                ticksuffix='%')
                    )

	fig.show()
    
def optimize_budget(x):
    
    revenue = []
    
    # Calculate revenues for all channels -> sum it up
    for i in range(len(media_vars)):
      
        # Calculate revenue for the channel
        revenue.append(mmm_pyton.neg_exponential_form(budget * x[i],
                                            revenue_coefficients['beta'][i],
                                            revenue_coefficients['gamma'][i])
                      )
    revenue = sum(revenue)
      
    # Set optimisation function
    return -(revenue - budget)

def optimum_func():   
    optimum = minimize(
                    optimize_budget,
                    x0,
                    method='trust-constr',
                    constraints=cons_f,
                    bounds=bounds
                                  )    
    return(optimum)

def exp_response_curves_draw(opt_rev_dict,media_costs,best_params,product_price,period=12,budg=2*10**9): 
    for chanel in media_costs.media.values:
        
        invest_opt=opt_rev_dict[chanel][0]
        profit_max=opt_rev_dict[chanel][1]
        
        grp=np.array([0,0.99*invest_opt,invest_opt,1.01*invest_opt])
        ydata=np.array([0,0.99*profit_max,profit_max,0.99*profit_max])
        for i in range(0,10,1):
            scale=10**(-i)
            gamma_0=scale*profit_max
            p0=(0.0000000001,gamma_0)
            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)

            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max):
                break

            scale=10**(i)
            gamma_0=scale*profit_max
            p0=(0.00000001,gamma_0)
            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)

            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max):
                break

            if (i==9):  
                print('Cant fit curve, enter new values')
        
        beta=popt[0]
        gamma=popt[1]     
    
        gamma_start=gamma/product_price/period
        beta_start=beta*period*media_costs[media_costs['media']==chanel].cost.values[0]
        
        
        best_params.add(
            name=chanel + '__lambda_ad',
            value=0.1,
             min=0.1,
             max=1,
        )  
    
        best_params.add(
            name=chanel + '__diminish_beta',
            value=beta_start,
             min=0.0,
             max=10,
        )  
        
        best_params.add(
            name=chanel + '__diminish_gamma',
            value=gamma_start,
             min=0,
             max=10,
        )  
        
        best_params.add(
            name=chanel + '__lm_coef',
            value=1,
            vary=False
        )  
        
    return best_params



def log_response_curves_draw(opt_rev_dict,media_costs,best_params,product_price,cap_dict,period=12, budg=2*10**9): 
    for chanel in media_costs.media.values:
        
        invest_opt=opt_rev_dict[chanel][0]
        profit_max=opt_rev_dict[chanel][1]
                                        
        grp_0=np.array([0,0.99*invest_opt,invest_opt,1.01*invest_opt])
        ydata=np.array([0,0.99*profit_max,profit_max,0.99*profit_max])
        
        def log_form_help(x, steep, EC50,lm,cap=cap_dict[chanel],budg=10**9,level=1):
            logfunc = [0 for i in range(0,len(x))]
            for i in range(len(x)):
                logfunc[i] = (cap / (1+np.exp((-steep)*(x[i]/cap-EC50))) - cap/(1+np.exp(steep*EC50)))*lm-x[i]
            return np.array(logfunc)
        
        for i in range(0,10,1):
            steep_0=10**(-i)
            p0=(steep_0/2,0.00005,10000)
            popt, pcov = curve_fit(log_form_help, grp_0, ydata, p0=p0, method='trf', bounds=([0.0,0.0,0.0],[steep_0,1,10**10]), maxfev=10000000)

            resp=log_form_help(np.array(range(0,budg,10**6)), popt[0], popt[1], popt[2],budg=10**9,level=1)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max):
                break

            steep_0=10**(i)
            p0=(steep_0/2,0.00005,10000)
            popt, pcov = curve_fit(log_form_help, grp_0, ydata, p0=p0, method='trf', bounds=([0.0,0.0,0.0],[steep_0,1,10**10]), maxfev=10000000)

            resp=log_form_help(np.array(range(0,budg,10**6)), popt[0], popt[1], popt[2],budg=10**9,level=1)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max):
                break

            if (i==9):  
                print('Cant fit curve, enter new values')
        
        
        steep=popt[0]
        EC50=popt[1]
        lm=popt[2]
        
        
        steep_start=steep*period*media_costs[media_costs['media']==chanel].cost.values[0]
        EC50_start=EC50/period/media_costs[media_costs['media']==chanel].cost.values[0]
        lm_start=lm/period/product_price

        

        best_params.add(
                    name=chanel + '__lambda_ad',
                    value=0.1,
                     min=0.0,
                     max=0.6,
                )  

        best_params.add(
                    name=chanel + '__log_steep',
                    value=steep_start,
                     min=0.0,
                     max=10000000000,
                )  

        best_params.add(
                    name=chanel + '__log_EC50',
                    value=EC50_start,
                     min=0.0,
                     max=1,
                )  

        best_params.add(
                    name=chanel + '__lm_coef',
                    value=lm_start,
                    min=0.0,
                    max=10000000
                ) 

        best_params.add(
                    name=chanel + '__log_level',
                    value=cap_dict[chanel],
                    vary=False
                ) 
        
    return best_params

def log_form_help(x, steep, EC50,lm,cap,budg=10**9,level=1):
    logfunc = [0 for i in range(0,len(x))]

    for i in range(len(x)):
        logfunc[i] = (cap / (1+np.exp((-steep)*(x[i]/cap-EC50))) - cap/(1+np.exp(steep*EC50)))*lm-x[i]

    return logfunc

def opt_rev_plot(revenue_table):
    fig = go.Figure()
    fig.add_trace(go.Scatter(
            x=revenue_table['budget'].values,
            y=(revenue_table['profit'].values),
            name='optimal curve'
    ))
	
    fig.update_layout(title='Optimal ROAS curve',
                   xaxis_title='Yearly investments (mln rub)',
                   yaxis_title='Expected yearly ROAS in comparison')
	
    fig.show()
    
    
def exp_just_draw(opt_rev_dict,period=12,budg=2*10**9): 
    revenue_coefficients=pd.DataFrame(columns=['media','beta', 'gamma'])
    for chanel in opt_rev_dict.keys():
        grp=np.array([0])
        ydata=np.array([0])

        for j in range(1,len(opt_rev_dict[chanel])):
            grp=np.append(grp, opt_rev_dict[chanel][j][0])
            ydata=np.append(ydata, opt_rev_dict[chanel][j][1])
        
        invest_opt=opt_rev_dict[chanel][0][0]
        profit_max=opt_rev_dict[chanel][0][1]
        
        grp=np.append(grp,[0.99*invest_opt,invest_opt,1.01*invest_opt])
        ydata=np.append(ydata, [0.99*profit_max,profit_max,0.99*profit_max])
        
        
        for i in range(0,10,1):
            scale=10**(-i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)
            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            scale=10**(i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)

            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            if (i==9):  
                print('Cant fit curve, enter new values')
        
        revenue_coefficients=revenue_coefficients.append({'media':chanel,'beta':popt[0],'gamma':popt[1]}, ignore_index=True)
    return revenue_coefficients

def exp_just_draw_v2(revenue_table,period=12,budg=2*10**9): 
    opt_rev_dict={}
    revenue_coefficients=pd.DataFrame(columns=['media','beta', 'gamma'])
    chanels=[x for x in revenue_table.drop(columns='Investment').columns if x[-5:]=='_ROAS']
    for col in chanels:
        opt_rev_dict[col]=[[int(revenue_table[col].idxmax()*1000000),int(revenue_table[col].max())]]
    
    for chanel in opt_rev_dict.keys():
        grp=np.array([0])
        ydata=np.array([0])

        for j in range(1,len(opt_rev_dict[chanel])):
            grp=np.append(grp, opt_rev_dict[chanel][j][0])
            ydata=np.append(ydata, opt_rev_dict[chanel][j][1])
        
        invest_opt=opt_rev_dict[chanel][0][0]
        profit_max=opt_rev_dict[chanel][0][1]
        
        grp=np.append(grp,[0.99*invest_opt,invest_opt,1.01*invest_opt])
        ydata=np.append(ydata, [0.99*profit_max,profit_max,0.99*profit_max])
        
        
        for i in range(0,10,1):
            scale=10**(-i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)
            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            scale=10**(i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)

            popt, pcov = curve_fit(neg_exponential_form, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            if (i==9):  
                print('Cant fit curve, enter new values')
        
        revenue_coefficients=revenue_coefficients.append({'media':chanel,'beta':popt[0],'gamma':popt[1]}, ignore_index=True)
    return revenue_coefficients

def exp_just_draw_v3(revenue_table,period=12,budg=2*10**9): 
    opt_rev_dict={}
    revenue_coefficients=pd.DataFrame(columns=['media','beta', 'gamma'])
    chanels=[x for x in revenue_table.drop(columns='Investment').columns if x[-5:]!='_ROAS']
    for col in chanels:
        opt_rev_dict[col]=[[int(revenue_table[col].idxmax()*1000000),int(revenue_table[col].max())]]
    
    for chanel in opt_rev_dict.keys():
        grp=np.array([0])
        ydata=np.array([0])

        for j in range(1,len(opt_rev_dict[chanel])):
            grp=np.append(grp, opt_rev_dict[chanel][j][0])
            ydata=np.append(ydata, opt_rev_dict[chanel][j][1])
        
        invest_opt=opt_rev_dict[chanel][0][0]
        profit_max=opt_rev_dict[chanel][0][1]
        
        grp=np.append(grp,[0.99*invest_opt,invest_opt,1.01*invest_opt])
        ydata=np.append(ydata, [0.99*profit_max,profit_max,0.99*profit_max])
        
        
        for i in range(0,10,1):
            scale=10**(-i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)
            popt, pcov = curve_fit(neg_exponential_form_budget, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form_budget(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            scale=10**(i)
            gamma_0=scale*profit_max
            p0=(0.000000001,gamma_0)

            popt, pcov = curve_fit(neg_exponential_form_budget, grp, ydata, p0=p0, method='lm', maxfev=10000000)
            resp=neg_exponential_form_budget(np.array(range(0,budg,10**6)), popt[0], popt[1], alpha=0, c=0)

            if (resp.argmax() in range(int(invest_opt/10**6)-10,int(invest_opt/10**6)+11,1)) and (resp.max()>0.8*profit_max) and (resp.max()<1.2*profit_max) and (resp.min()<-1):
                break

            if (i==9):  
                print('Cant fit curve, enter new values')
        
        revenue_coefficients=revenue_coefficients.append({'media':chanel,'beta':popt[0],'gamma':popt[1]}, ignore_index=True)
    return revenue_coefficients