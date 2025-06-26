###########################################################
# Janeiro 2024 #
# Bernard Costa #
# #
# Informacoes sobre Desenvolvimento: #
# <Ambiente (SublimeText/Python 3.6 IDLE)> #
# <Sistema Operacional (Ubuntu)> #
# calcula a prporcao e autocorrelacao das cadeias
###########################################################
import numpy as np
import random
import csv

def read_csv_columns(filename):
    with open(filename, newline='', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)
        columns = []
        
        for row in reader:
            if not columns:
                columns = [[] for _ in row]  # Initialize lists for each column
            for i, value in enumerate(row):
                columns[i].append(value)
    
    return columns

def convert_columns(columns):
  i=0
  while (i<len(columns)):
    j=0
    while (j<len(columns[i])):
        if columns[i][j]=='1':
            columns[i][j]=1
        elif columns[i][j]=='-1':
            columns[i][j]=-1
        j+=1
    i+=1
  return columns

#Mean response 
def mean_response(cadeia):
    mean = 1/2 + 1/(2*len(cadeia))*sum(cadeia)
    return mean

#Accuracy 
def accuracy_response(cadeia1,cadeia2):
  accuracy = 1/2 + 1/(2*len(cadeia1))*sum(multiplicacao_pos(cadeia1,cadeia2,0))
  return accuracy

def multiplicacao_pos(cadeia1,cadeia2,lag):
  i=0
  multiplicacao = []
  while (i < (len(cadeia1)-lag)):
    multiplicacao+= [cadeia1[i+lag]*cadeia2[i]]
    i+=1
  return multiplicacao

#multiplica duas cadeia com leg negativo
def multiplicacao_neg(cadeia1,cadeia2,lag):
  i=0
  multiplicacao = []
  while (i < (len(cadeia1)-lag)):
    multiplicacao+= [cadeia1[i]*cadeia2[i+lag]]
    i+=1
  return multiplicacao

#probability of shift
def shift_response(cadeia1):
  i=1
  shift=0
  while (i<len(cadeia1)-1):
    if (cadeia1[i]!=cadeia1[i+1]):
      shift+=1
    i+=1
  return shift/len(cadeia1)

#probability of shift, win
def shift_win_response(cadeia1,cadeia2):
  i=0
  count=0
  shift=0
  while (i<len(cadeia1)-1):
    if (cadeia1[i]==cadeia2[i]):
      count+=1
    if (cadeia1[i]==cadeia2[i] and cadeia2[i]!=cadeia2[i+1]):
      shift+=1
    i+=1
  return shift/count

#probability of shift, lose
def shift_lose_response(cadeia1,cadeia2):
  i=0
  count=0
  shift=0
  while (i<len(cadeia1)-1):
    if (cadeia1[i]!=cadeia2[i]):
      count+=1
    if (cadeia1[i]!=cadeia2[i] and cadeia2[i]!=cadeia2[i+1]):
      shift+=1
    i+=1
  return shift/count

#auto-correlation 
def correlation(cadeia1,cadeia2,lag):
    if(lag<=0):
        auto_neg = 1/(len(cadeia1)-lag)*sum(multiplicacao_neg(cadeia1,cadeia2,-lag))
        return auto_neg
    else:
        auto_pos = 1/(len(cadeia1)-lag)*sum(multiplicacao_pos(cadeia1,cadeia2,lag))
        return auto_pos


def main():

  filename = "comportamento_surrogados_trans.csv"  # Change this to your CSV file name
  columns = convert_columns(read_csv_columns(filename))

  output_file = open('saida_final' + '.csv', 'w', newline='')
  output_file.write('comportamento')
  output_file.write(' \t')
  output_file.write('cadeia')
  output_file.write(' \t')
  output_file.write('tipo')
  output_file.write(' \t')
  output_file.write('mean_response')
  output_file.write(' \t')
  output_file.write('accuracy')
  output_file.write(' \t')
  output_file.write('pr(shift)')
  output_file.write(' \t')
  output_file.write('pr(shift|win)')
  output_file.write(' \t')
  output_file.write('pr(shift|lose)')
  output_file.write(' \t')
  output_file.write('lag')
  output_file.write(' \t')
  output_file.write('auto_correlation')
  output_file.write(' \t')
  output_file.write('cross_correlation')
  output_file.write(' \t')
  output_file.write('\n')

  i = 0
  while (i<len(columns)-1):
    comportamento = columns[i+1][0]
    cadeia = columns[i+1][3]
    tipo = columns[i+1][2]
    proporcao = mean_response(columns[i][4:len(columns[0])])
    acuracia = accuracy_response(columns[i][4:len(columns[0])],columns[i+1][4:len(columns[0])])
    shift=shift_response(columns[i][4:len(columns[i])])
    win=shift_win_response(columns[i][4:len(columns[i])],columns[i+1][4:len(columns[i])])
    lose=shift_lose_response(columns[i][4:len(columns[i])],columns[i+1][4:len(columns[i])])
    

    j=-5
    lag = 6
    while(j<lag):
        output_file.write(str(comportamento))
        output_file.write(' \t')
        output_file.write(str(cadeia))
        output_file.write(' \t')
        output_file.write(str(tipo))
        output_file.write(' \t')
        if(j==0):
            output_file.write(str(proporcao))
            output_file.write(' \t')
            output_file.write(str(acuracia))
            output_file.write(' \t')
            output_file.write(str(shift))
            output_file.write(' \t')
            output_file.write(str(win))
            output_file.write(' \t')
            output_file.write(str(lose))
            output_file.write(' \t')
        else:
            output_file.write(str('N/A'))
            output_file.write(' \t')
            output_file.write(str('N/A'))
            output_file.write(' \t')
            output_file.write(str('N/A'))
            output_file.write(' \t')
            output_file.write(str('N/A'))
            output_file.write(' \t')
            output_file.write(str('N/A'))
            output_file.write(' \t')
        output_file.write(str(j))
        output_file.write(' \t')
        auto=correlation(columns[i][4:len(columns[i])],columns[i][4:len(columns[i])],j)
        output_file.write(str(auto))
        output_file.write(' \t')
        cross=correlation(columns[i][4:len(columns[i])],columns[i+1][4:len(columns[i])],j)
        output_file.write(str(cross))
        output_file.write(' \t')
        output_file.write('\n')
        j+=1
    i+=1
  output_file.close()










  #   output_file.write(str(comportamento))
  #   output_file.write(' \t')
  #   output_file.write(str(cadeia))
  #   output_file.write(' \t')
  #   output_file.write(str(tipo))
  #   output_file.write(' \t')
  #   output_file.write(str(proporcao))
  #   output_file.write(' \t')
  #   if(i%2==0):
  #       output_file.write(str('N/A'))
  #       output_file.write(' \t')
  #   else:
  #       output_file.write(str(acuracia))
  #       output_file.write(' \t')
  #   output_file.write(str(shift))
  #   output_file.write(' \t')
  #   if(i%2==0):
  #       output_file.write(str('N/A'))
  #       output_file.write(' \t')
  #   else:
  #       output_file.write(str(win))
  #       output_file.write(' \t')
  #   if(i%2==0):
  #       output_file.write(str('N/A'))
  #       output_file.write(' \t')
  #   else:
  #       output_file.write(str(lose))
  #       output_file.write(' \t')
  #   output_file.write(' \t')
  #   output_file.write(' \t')
  #   output_file.write('\n')
  #   i+=1
  # output_file.close()





main ()