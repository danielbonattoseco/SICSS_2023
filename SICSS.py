# -*- coding: utf-8 -*-
"""
Created on Wed Jul 12 16:10:25 2023

@author: danie
"""
#CARREGAMENTO DE DADOS E BIBLIOTECAS

import pandas as pd
import seaborn as sns
import matplotlib.dates as mdates
from unidecode import unidecode
import nltk
import os
import warnings

warnings.filterwarnings("ignore")

#######################
file = 'SICSS Bolsonaro FB 2 Semestre 2022'
#######################

print('Carregando dados...')
dados = pd.read_csv('./%s.csv'%file)
#%%
print('Processando dados...')

#CONVERSÃO DADOS
dados['created_date'] = pd.to_datetime(dados['Post Created Date'])
created_date = dados['created_date'].dt.date

#CONTAGEM DE PUBLICAÇÕES POR DOA
num_days = created_date.value_counts()
num_days = pd.DataFrame(num_days)
num_days.reset_index(inplace=True)
num_days = num_days.sort_values(by='created_date',ascending=False)
num_days['DateAsNumber'] = num_days['index'].apply(mdates.date2num)

#IDENTIFICAÇÃO X DIAS COM MAIOR VOLUME DE PUBLICAÇÕES
x = 20
top_values = num_days.head(x).sort_values(by='index',ascending=True)

#%% GERAÇÃO E PREPROCESSAMENTO DE CORPUS PARA REDES SEMANTICAS 

textos_posts = dados[['Message','created_date']]
textos_posts.set_index('created_date',inplace=True)
textos_posts['Message'] = textos_posts['Message'].apply(lambda value: str(value))

print('   Removendo pontuação...')
textos_posts['Message Normalizado'] = textos_posts['Message'].apply(unidecode)
print('   Lowercase...')
textos_posts['Message Normalizado'] = textos_posts['Message Normalizado'].str.lower()
print('   Removendo espaçamento extra...')
textos_posts['Message Normalizado'] = textos_posts['Message Normalizado'].str.strip()
print('   Removendo quebras de linha...')
textos_posts['Message Normalizado'] = textos_posts['Message Normalizado'].str.replace('[^\w\s]','')
print('   Removendo links...')
textos_posts['Message Normalizado'] = textos_posts['Message Normalizado'].apply(lambda x: ' '.join([word 
                                                                                      for word 
                                                                                      in x.split() 
                                                                                      if word.startswith('http') == False]))
print('   Removendo stopwords...')
stopwords = nltk.corpus.stopwords.words('portuguese')
stopwords.extend(['nao', 'ja', 'ate'])
textos_posts['Message Normalizado'] = textos_posts['Message Normalizado'].apply(lambda x: ' '.join([word for word in x.split() if word not in stopwords]))

#%% SEGMENTAÇÃO DE CORPUS POR SEMANA

textos_posts_semana = textos_posts.groupby(pd.Grouper(freq='W')).agg({'Message Normalizado': '\n'.join})

print('Gerando corpus para redes semânticas...')

if not os.path.isdir("./Narrativas Semana"):
    os.mkdir("./Narrativas Semana")

for i, row in textos_posts_semana.iterrows():
    if not os.path.isdir("./Narrativas Semana/%s"%i.strftime("%Y%m%d")):
        os.mkdir("./Narrativas Semana/%s"%i.strftime("%Y%m%d"))

    with open("./Narrativas Semana/%s/%s.txt"%(i.strftime("%Y%m%d"),
                                               i.strftime("%Y%m%d")), "w", encoding="utf-8") as text_file:
        text_file.write(row['Message Normalizado'])

#%% PLOTS DE FREQUENCIA

print('Plotando figuras...')

if not os.path.isdir("./Plots"):
    os.mkdir("./Plots")

#LINE/SCATTERPLOT
sns.set(rc = {'figure.figsize':(20,7)})
sns.set(font_scale=1.5)

ax = sns.lineplot(x=num_days['index'],
                  y=num_days['created_date'])
ax = sns.scatterplot(x=top_values['index'], 
                     y=top_values['created_date'],
                     s=50,
                     color='red')
ax.set(xlabel='Data da Publicação', 
       ylabel='Quantidade de Publicações',
       title='Volume de publicações mencionando Bolsonaro por dia')

ax.figure.savefig('./Plots/%s_volume_publicacoes.png'%(file))

#REGRESSÃO LOGÍSTICA
for order in [1,3,6]:
    lm = sns.lmplot(x = 'DateAsNumber', 
                y = 'created_date',  
                ci = None, 
                data = num_days,
                aspect=2.8, 
                height=6,
                order=order)
    lm.ax.xaxis.set_major_formatter(mdates.DateFormatter('%y-%m-%d'))
    lm.ax.set(xlabel='Data da Publicação', 
           ylabel='Quantidade de Publicações',
           title='\nLinha de tendência de publicações mencionando Bolsonaro por dia')
    
    lm.figure.savefig('./Plots/%s_curva_tendência_order_%i.png'%(file, 
                                                                 order))

#%% IDENTIFICAÇÃO DE TOP10 PERFIS COM MAIOR VOLUME DE PUBLICAÇÕES POR MÊS

dados_autores = dados[['User Name', 'Page Category', 'Post Created Date', 'Total Interactions']]
dados_autores['Post Created Date'] = pd.to_datetime(dados['Post Created Date'])

dados_top10 = pd.DataFrame()

for ano in [2022,2023]:
    for mes in list(range(1,13)):
        dados_autores_filtrado = dados_autores[(dados_autores['Post Created Date'].dt.month == mes) & 
                                               (dados_autores['Post Created Date'].dt.year == ano)]
        print(len(dados_autores_filtrado))
        principais = dados_autores_filtrado['User Name'].value_counts()
        top10 = principais.head(10)
        for item in top10.iteritems():
            dados_top10.loc[item[0],'%i-%i'%(mes,ano)] = item[1]

dados_top10.reset_index(inplace=True)
dados_top10.rename(columns={'index':'Conta'},
                   inplace=True)
dados_top10.to_excel('./%s_top10_contas_publicacoes.xlsx'%file)
