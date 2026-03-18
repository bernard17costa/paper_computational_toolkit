###########################################################
# January 2025 #
# Bernard Costa #
# #
# Development Information: #
# <Environment (SublimeText/Python 3.6 IDLE)> #
# <Operational System (Ubuntu)>
#
# Description:
# This script processes the output of experimental data
# ("2_3_collected_data.csv") and computes:
#
# 1. Basic behavioral metrics:
#    - Mean response
#    - Accuracy
#    - Shift probabilities (overall, win, lose)
#
# 2. Temporal structure:
#    - Auto-correlation
#    - Cross-correlation (input vs response)
#
# 3. Recovers Markov structure:
#    - M0
#    - M2
#
# It also:
# - Cleans raw CSV formatting artifacts ([], spaces)
# - Transposes the dataset (row ↔ column)
# - Outputs 3 processed CSV files:
#     sim_output1.csv → behavioral + correlation metrics
#     sim_output2.csv → M0 structure
#     sim_output3.csv → M2 structure
###########################################################
import numpy as np
import random
import csv
import string
import os
import shutil

# --------------------------------------------------------
# CSV UTILITIES
# --------------------------------------------------------

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

# --------------------------------------------------------
# BEHAVIORAL METRICS
# --------------------------------------------------------

#Mean response 
def mean_response(seq):
  mean = 1/2 + 1/(2*len(seq))*sum(seq)
  return mean

#Accuracy 
def accuracy_response(seq1,seq2):
  accuracy = 1/2 + 1/(2*len(seq1))*sum(multi_pos(seq1,seq2,0))
  return accuracy

# --------------------------------------------------------
# TEMPORAL OPERATIONS
# --------------------------------------------------------

#multiplies two sequences with zero or a positive leg
def multi_pos(seq1,seq2,lag):
  i=0
  multiply = []
  while (i < (len(seq1)-lag)):
    multiply+= [seq1[i+lag]*seq2[i]]
    i+=1
  return multiply

#multiplies two sequences with a negative leg
def multi_neg(seq1,seq2,lag):
  i=0
  multiply = []
  while (i < (len(seq1)-lag)):
    multiply+= [seq1[i]*seq2[i+lag]]
    i+=1
  return multiply

# --------------------------------------------------------
# SHIFT METRICS
# --------------------------------------------------------

#probability of shift
def shift_response(seq1):
  i=1
  shift=0
  while (i<len(seq1)-1):
    if (seq1[i]!=seq1[i+1]):
      shift+=1
    i+=1
  return shift/len(seq1)

#probability of shift, win
def shift_win_response(seq1,seq2):
  i=0
  count=0
  shift=0
  while (i<len(seq1)-1):
    if (seq1[i]==seq2[i]):
      count+=1
    if (seq1[i]==seq2[i] and seq2[i]!=seq2[i+1]):
      shift+=1
    i+=1
  return shift/count

#probability of shift, lose
def shift_lose_response(seq1,seq2):
  i=0
  count=0
  shift=0
  while (i<len(seq1)-1):
    if (seq1[i]!=seq2[i]):
      count+=1
    if (seq1[i]!=seq2[i] and seq2[i]!=seq2[i+1]):
      shift+=1
    i+=1
  return shift/count

#correlation 
def correlation(seq1,seq2,lag):
    if(lag<=0):
        auto_neg = 1/(len(seq1)-lag)*sum(multi_neg(seq1,seq2,-lag))
        return auto_neg
    else:
        auto_pos = 1/(len(seq1)-lag)*sum(multi_pos(seq1,seq2,lag))
        return auto_pos

# --------------------------------------------------------
# MARKOV STRUCTURE
# --------------------------------------------------------

def markovM0(seq1,seq2):
  i=0
  sumS0 = 0
  count=0
  while (i<len(seq1)):
    count+=1
    if seq2[i] == 1:
        sumS0 +=1
    i+=1
  return sumS0/count

def markovM2(seq1,seq2):
  i=0
  sumS0 = 0
  sumS1 = 0
  sumS2 = 0
  sumS3 = 0
  count0 = 1
  count1 = 1
  count2 = 1
  count3 = 1
  while (i<(len(seq1)-2)):
    if seq1[i]==1 and seq1[i+1]==1:
      count0+=1
    elif seq1[i]==1 and seq1[i+1]==-1:
      count1+=1
    elif seq1[i]==-1 and seq1[i+1]==1:
      count2+=1
    elif seq1[i]==-1 and seq1[i+1]==-1:
      count3+=1
    if seq1[i]==1 and seq1[i+1]==1 and seq2[i+2]==1:
      sumS0+=1
    elif seq1[i]==1 and seq1[i+1]==-1 and seq2[i+2]==-1:
      sumS1+=1
    elif seq1[i]==-1 and seq1[i+1]==1 and seq2[i+2]==1:
      sumS2+=1
    elif seq1[i]==-1 and seq1[i+1]==-1 and seq2[i+2]==1:
      sumS3+=1
    i+=1
  return [sumS0/count0,1-(sumS1/count1),sumS2/count2,sumS3/count3]

# --------------------------------------------------------
# MAIN PIPELINE
# --------------------------------------------------------

def main():

  filename = "2_3_collected_data.csv"
  columns = convert_columns(read_csv_columns(filename))

  output_file = open('2_3_data_output1' + '.csv', 'w', newline='')
  output_file.write('behaviour')
  output_file.write(' \t')
  output_file.write('sequence')
  output_file.write(' \t')
  output_file.write('type')
  output_file.write(' \t')
  output_file.write('mean_response')
  output_file.write(' \t')
  output_file.write('accuracy')
  output_file.write(' \t')
  output_file.write('pr_shift')
  output_file.write(' \t')
  output_file.write('pr_shift_win')
  output_file.write(' \t')
  output_file.write('pr_shift_lose')
  output_file.write(' \t')
  output_file.write('lag')
  output_file.write(' \t')
  output_file.write('auto_correlation')
  output_file.write(' \t')
  output_file.write('cross_correlation')
  output_file.write(' \t')
  output_file.write('\n')

  i = 0
  while (i<len(columns)):
    behaviour = columns[i][0]
    sequence = columns[i][3]
    type = columns[i][2]
    mean = mean_response(columns[i][4:len(columns[0])])
    accuracy = accuracy_response(columns[i-1][4:len(columns[0])],columns[i][4:len(columns[0])])
    shift=shift_response(columns[i][4:len(columns[i])])
    win=shift_win_response(columns[i-1][4:len(columns[i])],columns[i][4:len(columns[i])])
    lose=shift_lose_response(columns[i-1][4:len(columns[i])],columns[i][4:len(columns[i])])
    

    j=-5
    lag = 6
    while(j<lag):
        output_file.write(str(behaviour))
        output_file.write(' \t')
        output_file.write(str(sequence))
        output_file.write(' \t')
        output_file.write(str(type))
        output_file.write(' \t')
        if(j==0):
            output_file.write(str(mean))
            output_file.write(' \t')
            if(i % 2 == 0):
              output_file.write(str('N/A'))
              output_file.write(' \t')
              output_file.write(str('N/A'))
              output_file.write(' \t')
              output_file.write(str('N/A'))
              output_file.write(' \t')
              output_file.write(str('N/A'))
              output_file.write(' \t')
            else:
              output_file.write(str(accuracy))
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
        cross=correlation(columns[i-1][4:len(columns[i])],columns[i][4:len(columns[i])],j)
        output_file.write(str(cross))
        output_file.write(' \t')
        output_file.write('\n')
        j+=1
    i+=1
  output_file.close()

  filename = "2_3_collected_data.csv"
  columns = convert_columns(read_csv_columns(filename))

  output_file = open('2_3_data_output2' + '.csv', 'w', newline='')
  output_file.write('behaviour')
  output_file.write(' \t')
  output_file.write('sequence')
  output_file.write(' \t')
  output_file.write('type')
  output_file.write(' \t')
  output_file.write('element')
  output_file.write(' \t')
  output_file.write('rec')
  output_file.write(' \t')
  output_file.write('\n')

  i = 0
  while (i<len(columns)-1):
    behaviour = columns[i+1][0]
    sequence = columns[i+1][3]
    type = columns[i+1][2]
    markov0_cross = markovM0(columns[i][4:len(columns[0])],columns[i+1][4:len(columns[0])])

    output_file.write(str(behaviour))
    output_file.write(' \t')
    output_file.write(str(sequence))
    output_file.write(' \t')
    output_file.write(str(type))
    output_file.write(' \t')
    output_file.write('M0_0')
    output_file.write(' \t')
    output_file.write(str(markov0_cross))
    output_file.write(' \t')
    output_file.write('\n')

    i+=2
  output_file.close()

  filename = "2_3_collected_data.csv"  # Change this to your CSV file name
  columns = convert_columns(read_csv_columns(filename))

  output_file = open('2_3_data_output3' + '.csv', 'w', newline='')
  output_file.write('behaviour')
  output_file.write(' \t')
  output_file.write('sequence')
  output_file.write(' \t')
  output_file.write('type')
  output_file.write(' \t')
  output_file.write('element')
  output_file.write(' \t')
  output_file.write('rec')
  output_file.write(' \t')
  output_file.write('\n')

  i = 0
  while (i<len(columns)-1):
    j=0
    while(j<4):
      behaviour = columns[i+1][0]
      sequence = columns[i+1][3]
      type = columns[i+1][2]
      markov2_cross = markovM2(columns[i][4:len(columns[0])],columns[i+1][4:len(columns[0])])
      output_file.write(str(behaviour))
      output_file.write(' \t')
      output_file.write(str(sequence))
      output_file.write(' \t')
      output_file.write(str(type))
      output_file.write(' \t')
      output_file.write('M2_'+str(j+1))
      output_file.write(' \t')
      output_file.write(str(markov2_cross[j]))
      output_file.write(' \t')
      output_file.write('\n')
      j+=1
    i+=2
  output_file.close()
main ()