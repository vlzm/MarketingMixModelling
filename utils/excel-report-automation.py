import xlsxwriter

HOLIDAYS = [
    'Christmas',
    'Independence_Day',
    'Independence_Day_Sales',
    'Labor_Day',
    'Memorial_Day',
    'New_Year',
    'Sales_Drop',
    'Washington_Birthday',
    'Post_stimulus_weekend'
]

MACRO = [
    'NASDAQ_Opens',
#     'SP500_Open',
    'CPI',
    'Consumer_Sentiment_Index',
    'Total_HA_Clicked'
#     'Unemployed'
]

BLACK_FRIDAY = [
    'BF_sales', 
    'BF_sales_2020', 
#     'Black_friday', 
    'Post_Black_Friday', 
    'Pre_Black_Friday_Weekend',
    'Black_friday_2019_29',
    'Black_friday_2019_30',
    'Black_friday_2020_27',
    'Black_friday_2020_28'
]

def mape(true, pred):
    return ((true - pred) / true).abs().sum() / true.shape[0]

def calculate_metrics(decomposition):
    true = decomposition['actual']
    pred = decomposition['fitted']

    return (
        ('R2', f'{round(r2_score(true, pred)*100, 1)}%'),
        ('MAPE', round(mape(true, pred), 2)),
        ('Weekly MAPE', round(mape(
            true.resample('W').sum(),
            pred.resample('W').sum()
        ), 2))
    )

def get_responses(revenue_table):
    def get_response_by_temperature(revenue_table, temperature):
        return revenue_table.set_index('Investment').filter(like=temperature).rename(
            # Remove 'Impressions_[TEMP]' from column name
            columns=lambda x: '_'.join(x.split('_')[:-2])
        )
    
    return (
        (
            temperature[1:],
            get_response_by_temperature(revenue_table, temperature)
        )
        for temperature in ['_hot', '_warm', '_cold', 'COOP']
    )

def mod_fit_to_excel(decomposition_df_sshape):
        model_fit_sheet = decomposition_df_sshape[['actual','fitted']]

        model_fit_sheet.to_excel(writer, sheet_name='model_fit', float_format='%.2f')
        workbook  = writer.book
        worksheet = writer.sheets['model_fit']

    # Create a new chart object.
        chart = workbook.add_chart({'type': 'line'})
        
    # Add a series to the chart.
        for i, col in enumerate(model_fit_sheet.columns):
            chart.add_series({
                'categories': ['model_fit', 1, 0, model_fit_sheet.shape[0], 0],
                'values': ['model_fit', 1, i+1, model_fit_sheet.shape[0], i+1]
            })

    # Configure the chart axes.
        chart.set_x_axis({'name': 'Date', 'position_axis': 'on_tick'})
        chart.set_y_axis({'name': 'Units', 'major_gridlines': {'visible': False}})

    # Turn off chart legend. It is on by default in Excel.
        chart.set_legend({'position': 'none'})
        chart.set_size({'width': 1200, 'height': 800})
        worksheet.set_column(0, 0, 20)
        worksheet.set_column(4, 4, 12)
    # Calculate metrics
        for i, (name, metric) in enumerate(calculate_metrics(decomposition_df_sshape)):
            worksheet.write(i, 4, name)
            worksheet.write(i, 5, metric)
            
        worksheet.insert_chart('H1', chart)

def mod_validation_to_excel(decomposition_df_sshape, validation):
        model_fit_sheet = decomposition_df_sshape[['actual','fitted']].copy()
        model_fit_sheet.fitted[validation.index] = np.nan
        model_fit_sheet = pd.concat([model_fit_sheet, validation], axis=1)
        
        model_fit_sheet.to_excel(writer, sheet_name='model_CV', float_format='%.2f')
        workbook  = writer.book
        worksheet = writer.sheets['model_CV']

    # Create a new chart object.
        chart = workbook.add_chart({'type': 'line'})
        
    # Add a series to the chart.
        for i, col in enumerate(model_fit_sheet.columns):
            if i < 2:
                chart.add_series({
                    'categories': ['model_CV', 1, 0, model_fit_sheet.shape[0], 0],
                    'values': ['model_CV', 1, i+1, model_fit_sheet.shape[0], i+1]
                })
            else:
                chart.add_series({
                    'categories': ['model_CV', 1, 0, model_fit_sheet.shape[0], 0],
                    'values': ['model_CV', 1, i+1, model_fit_sheet.shape[0], i+1],
                    'line': {'dash_type': 'long_dash'}
                })

    # Configure the chart axes.
        chart.set_x_axis({'name': 'Date', 'position_axis': 'on_tick'})
        chart.set_y_axis({'name': 'Units', 'major_gridlines': {'visible': False}})

    # Turn off chart legend. It is on by default in Excel.
        chart.set_legend({'position': 'none'})
        chart.set_size({'width': 1200, 'height': 800})
        worksheet.set_column(0, 0, 20)
        worksheet.set_column(4, 4, 12)
            
        worksheet.insert_chart('H1', chart)

def add_response_curves(revenue_table):
    workbook = writer.book
    
    for temperature, response in get_responses(revenue_table):
        if temperature == 'OOP':
            sheet_name = f'response_curves_C{temperature}'
        else:
            sheet_name = f'response_curves_{temperature}'
        
        response.to_excel(writer, sheet_name=sheet_name)
            
        chart = workbook.add_chart({'type': 'line'})
        
        for i, name in enumerate(response.columns):
            chart.add_series({
                'categories': [sheet_name, 1, 0, response.shape[0], 0],
                'values': [sheet_name, 1, i+1, response.shape[0], i+1],
                'name': name
            })
            
        chart.set_x_axis({'name': 'Weekly Investments(USD)', 'position_axis': 'on_tick'})
        chart.set_y_axis({'name': 'Units', 'major_gridlines': {'visible': False}})
        chart.set_size({'width': 900, 'height': 500})
        writer.sheets[sheet_name].insert_chart('M1', chart)

def add_media_impacts(roas_table, roas_agg):
    stats = roas_table.set_index('media')
    agg_stats = roas_agg.set_index('media')
    percent_fmt = writer.book.add_format({'num_format': '0.00%'})
    
    startcol = 0
    for stat_type in ['Budget', 'Impact', 'ROAS']:
        idx = pd.MultiIndex.from_product([[stat_type], ['Overall', 'Cold', 'Warm', 'Hot'], ['2019', '2020', '2021']])
#         idx = pd.MultiIndex.from_product([[stat_type], ['Overall', 'Cold', 'Warm', 'Hot'], ['2019/2020']])
        
        df = agg_stats.round(1).filter(like=stat_type).rename(lambda x: '_'.join(x.split('_')[:-1]))
        
        for temperature in ['_cold', '_warm', '_hot']:
            df = pd.concat([df, stats.filter(like=temperature, axis=0).filter(like=stat_type).rename(lambda x: '_'.join(x.split('_')[:-2]))], axis=1)
            
        df = pd.DataFrame(df.values, index=df.index, columns=idx).replace([np.inf, np.nan], 0)
        
        if stat_type in ['Budget', 'Impact']:
            df.loc['Total'] = df.sum()
            
        df.to_excel(writer, sheet_name='media_impacts_abs', startrow=0, startcol=startcol)
        
        if stat_type in ['Budget', 'Impact']:
            df = df[:-1]
        else:
            df.to_excel(writer, sheet_name='media_impacts_%', startrow=0, startcol=startcol)
            continue
            
        # Fix an empty row problem in xlsxwriter when using Multiindex
        writer.sheets['media_impacts_abs'].set_row(3, options={'hidden': True})

        for temperature in ['Cold', 'Warm', 'Hot', 'Overall']:
            df[stat_type][temperature] = (df[stat_type][temperature] / df[stat_type]['Overall'].sum())
        
        if stat_type in ['Budget', 'Impact']:
            df.loc['Total'] = df.sum()
            
        df.to_excel(writer, sheet_name='media_impacts_%', startrow=0, startcol=startcol)
        writer.sheets['media_impacts_%'].set_column(startcol, startcol + df.shape[1], cell_format=percent_fmt)
        
        startcol += 14
        
    # Fix an empty row problem in xlsxwriter when using Multiindex
    writer.sheets['media_impacts_%'].set_row(3, options={'hidden': True})

def add_decomposition_table(decomposition):
    COOP = decomposition.filter(like='COOP')
    COMP = decomposition.filter(like='COMP')
    covid = decomposition.filter(like='covid').drop(['Pre_covid_int', 'Post_covid_int'], axis=1)
#     impressions = decomposition.filter(like='Impressions')
    impressions = pd.concat([decomposition.filter(like='Impressions'), decomposition.filter(like='Clicks')], axis=1)
    month_seasonality = decomposition.filter(like='Month')

    ranges = {
        '2019': pd.date_range('01-01-2019', '12-31-2019'),
        '2020': pd.date_range('01-01-2020', '12-31-2020'),
        '2021': pd.date_range('01-01-2021', '01-31-2021')
#         '2019/2020': pd.date_range('02-24-2019', '02-23-2020'),
    }
    
    startcol = 0
    impacts = pd.concat([
        pd.DataFrame().from_dict({
            'media': impressions.loc[interval].sum().sum(),
            'COOP': COOP.loc[interval].sum().sum(),
            'COMP': COMP.loc[interval].sum().sum(),
            'average_price': decomposition['average_price'].loc[interval].sum().sum(),
            'macro factors': decomposition[MACRO].loc[interval].sum().sum(),
            'black_friday': decomposition[BLACK_FRIDAY].loc[interval].sum().sum(),
            'holidays': decomposition[HOLIDAYS].loc[interval].sum().sum(),
            'daily_seasonality': covid.loc[interval].sum().sum(),
            'monthly_seasonality': month_seasonality.loc[interval].sum().sum(),
#             'baseline': decomposition['baseline'].loc[interval].sum().sum(),
            'baseline': decomposition[['Post_covid_int', 'Pre_covid_int']].loc[interval].sum().sum(),
            'total': decomposition['fitted'].loc[interval].sum(),
        }, columns=[name], orient='index')
        for name, interval in ranges.items()
    ], axis=1)
    impacts.to_excel(writer, sheet_name='all_impacts_abs', float_format='%.f', startrow=0, startcol=startcol)
    
    percents = (impacts / impacts.loc['total'])
    percents.to_excel(writer, sheet_name='all_impacts_%', startrow=0, startcol=startcol)
    percent_fmt = writer.book.add_format({'num_format': '0.00%'})
    writer.sheets['all_impacts_%'].set_column(startcol, startcol + (len(ranges)+1), cell_format=percent_fmt)
    
    startcol += (len(ranges)+1) + 1
    
    media_impacts = pd.concat([
        impressions.loc[interval].sum().rename(name)
        for name, interval in ranges.items()
    ], axis=1)
    media_impacts.to_excel(writer, sheet_name='all_impacts_abs', float_format='%.f', startrow=0, startcol=startcol)
    
    media_percents = (media_impacts / impacts.loc['total'])
    media_percents.to_excel(writer, sheet_name='all_impacts_%', startrow=0, startcol=startcol)
    writer.sheets['all_impacts_%'].set_column(startcol, startcol + (len(ranges)+1), cell_format=percent_fmt)

    startcol += (len(ranges)+1) + 1
    
    coop_impacts = pd.concat([
        COOP.loc[interval].sum().rename(name)
        for name, interval in ranges.items()
    ], axis=1)
    coop_impacts.to_excel(writer, sheet_name='all_impacts_abs', float_format='%.f', startrow=0, startcol=startcol)
    
    coop_percents = (coop_impacts / impacts.loc['total'])
    coop_percents.to_excel(writer, sheet_name='all_impacts_%', startrow=0, startcol=startcol)
    writer.sheets['all_impacts_%'].set_column(startcol, startcol + (len(ranges)+1), cell_format=percent_fmt)
    
    startcol += (len(ranges)+1) + 1
    
    comp_impacts = pd.concat([
        COMP.loc[interval].sum().rename(name)
        for name, interval in ranges.items()
    ], axis=1)
    comp_impacts.to_excel(writer, sheet_name='all_impacts_abs', float_format='%.f', startrow=0, startcol=startcol)
    
    comp_percents = (comp_impacts / impacts.loc['total'])
    comp_percents.to_excel(writer, sheet_name='all_impacts_%', startrow=0, startcol=startcol)
    writer.sheets['all_impacts_%'].set_column(startcol, startcol + (len(ranges)+1), cell_format=percent_fmt)
    
    startcol += (len(ranges)+1) + 1
    
    macro_impacts = pd.concat([
        decomposition[MACRO].loc[interval].sum().rename(name)
        for name, interval in ranges.items()
    ], axis=1)
    macro_impacts.to_excel(writer, sheet_name='all_impacts_abs', float_format='%.f', startrow=0, startcol=startcol)
    
    macro_percents = (macro_impacts / impacts.loc['total'])
    macro_percents.to_excel(writer, sheet_name='all_impacts_%', startrow=0, startcol=startcol)
    writer.sheets['all_impacts_%'].set_column(startcol, startcol + (len(ranges)+1), cell_format=percent_fmt)

def add_params(parameters):
    param_names = ['__lm_coef', '__lambda_ad', '__diminish_beta', '__diminish_gamma']

    df = pd.DataFrame(parameters.valuesdict().items(), columns=['name', 'value']).set_index('name')

    df = pd.concat([
        df.filter(like=param, axis=0).rename(index=lambda x: x.replace(param, '')).rename(columns={'value': param})
        for param in param_names
    ], axis=1)
    
    df.sort_values('__lambda_ad').to_excel(writer, sheet_name='parameters', startrow=0, startcol=0)

def add_coop_roas(coop_roas_table):
    coop_roas_table.to_excel(writer, sheet_name='COOP_roas', startrow=0, startcol=0)
    
with pd.ExcelWriter('1ew_results.xlsx') as writer:
    mod_fit_to_excel(decomposition_df_sshape)
#     mod_validation_to_excel(decomposition_df_sshape, validation)
#     add_media_decomposition_table(decomposition_df_sshape)
    add_decomposition_table(decomposition_df_sshape)
    add_media_impacts(roas_table, roas_agg)
    add_coop_roas(coop_roas_table.set_index('media'))
    add_response_curves(revenue_table[:50])
    add_params(params_sshape)