#!/apps/anaconda3-4.3.0/bin/python3.6

import glob

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt


def get_academics(number_ind, aca_authors, new_frame):
    not_string = False
    academia_rows = []

    for i in number_ind:
        row = new_frame.iloc[i]
        for j in row:
            if type(j) == str:
                s = sorted(j.replace(',', '').strip().split())
                if len(s) > 1:
                    for j in aca_authors:
                        if s == j:
                            academia_rows.append(i)
                            break
            else:
                not_string = True
                break
            
        if not_string == True:
            continue

    return academia_rows

def create_frames(aca_df, nonaca_df, orig_df, number_ind, rows):
    for i in number_ind[:100]:
        if i in rows:
            aca_df = aca_df.append(orig_df.iloc[i])
        else:
            nonaca_df = nonaca_df.append(orig_df.iloc[i])

    return (aca_df, nonaca_df)

def plot_freq(df, title_name):
    counts = {'D' : 0, 'E' : 0, 'F' : 0, 'G' : 0, 'H' : 0, 'I' : 0, 'J' : 0, 'K' : 0,
          'L' : 0, 'O' : 0} 

    jels = df.values['JEL Code']

    for i in jels:
        if type(i) != float:
            jc = i.split()
            for j in jc:
                if j in counts:
                    counts += 1


    #plot adapted from: https://stackoverflow.com/questions/28931224/adding-value-labels-on-a-matplotlib-bar-chart

    data = [(i,j) for i,j in counts.items()]
    data = sorted(data, key=lambda x : x[0])

    frequencies = [i[1] for i in data]
    x_labels = [i[0] for i in data]

    freq_series = pd.Series.from_array(frequencies) 

    plt.figure(figsize=(12, 8))
    ax = freq_series.plot(kind='bar')
    ax.set_title(title_name + ': ' + 'JEL Code Frequency')
    ax.set_xlabel("JEL Code")
    ax.set_ylabel("Frequency")
    ax.set_xticklabels(x_labels, rotation=0)

    rects = ax.patches

    bar_labels = frequencies

    for rect, label in zip(rects, bar_labels):
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width() / 2, height + 1, label, ha='center', va='bottom')

    plt.savefig('//apptemp/scratch/pl2669/authors/plots/' + title_name.lower() + '.png')


if __name__ == '__main__':
    workingpapers_english = pd.read_csv('//apptemp/scratch/pl2669/grid_model_workingpapers/full_english/english_workingpapers_final.csv', encoding='utf-8-sig')
    journals_english = pd.read_csv('//apptemp/scratch/pl2669/grid_model_journals/full_english/english_journals_final.csv', encoding='utf-8-sig')


    workingpapers_english.drop('Unnamed: 0', axis=1, inplace=True)
    journals_english.drop('Unnamed: 0', axis=1, inplace=True)

    workingpapers_english = workingpapers_english.rename(columns={'4' : 'JEL Code'})
    journals_english = journals_english.rename(columns={'3' : 'JEL Code'})

    authors_df = pd.read_pickle('academics_nonacademics.pkl')

    authors = authors_df['Name'].values
    academic = authors_df['Academic (1)'].values

    academic_authors = [sorted(authors[i].split()) for i in range(len(authors)) if academic[i] == 1]

    wp_new = workingpapers_english
    j_new = journals_english

    wp_new.drop(['0', '1', '2', '3', 'Combined', '4'], axis=1, inplace=True)
    j_new.drop(['0', '1', '2', 'Combined', '3'], axis=1, inplace=True)

    number_indices_wp = [i for i in range(0, 1111145)]
    number_indices_j = [i for i in range(0, 1412131)]

    not_string = False
    academia_rows_wp = []

    rows_wp = get_academics(number_indices_wp, academic_authors, wp_new)
    rows_j = get_academics(number_indices_j, academic_authors, j_new)

    academia_df = pd.DataFrame()
    nonacademia_df = pd.DataFrame()

    aca, nonaca = create_frames(academia_df, nonacademia_df, workingpapers_english, number_indices_wp)
    updated_aca, updated_nonaca = create_frames(aca, nonaca, journals_english, number_indices_j)

    updated_aca.drop('4', axis=1, inplace=True)
    updated_aca.to_pickle('//apptemp/scratch/pl2669/authors/dataframes/academia.pkl')
    updated_aca.to_csv('//apptemp/scratch/pl2669/authors/dataframes/academia.csv', encoding='utf-8-sig')

    updated_nonaca.drop('4', axis=1, inplace=True)
    updated_nonaca.to_pickle('//apptemp/scratch/pl2669/authors/dataframes/nonacademia.pkl')
    updated_nonaca.to_csv('//apptemp/scratch/pl2669/authors/dataframes/nonacademia.csv', encoding='utf-8-sig')

    plot_freq(updated_aca, 'Academic')
    plot_freq(updated_nonaca, 'Non-Academic')
