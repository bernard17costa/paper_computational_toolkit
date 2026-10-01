###########################################################
# Janeiro 2025 #
# Bernard Costa #
# #
# Informacoes sobre Desenvolvimento: #
# <Ambiente (SublimeText/Python 3.6 IDLE)> #
# <Sistema Operacional (Ubuntu)> #
###########################################################
import numpy as np
import random
import csv
import os
import shutil

def transpose_csv(input_file, output_file):
	if os.path.isdir('temp'):
	    shutil.rmtree('temp')
	    os.makedirs('temp')
	else:
		os.makedirs('temp')
	with open(input_file, 'r') as infile, open(output_file, 'w', newline='') as outfile:
		reader = csv.reader(infile)
		writer = csv.writer(outfile)
		writer.writerows(zip(*reader))


def delete():
  input_file = open('temp/2_2_collected_data_surrogate1.csv', 'r')
  output_file = open('temp/2_2_collected_data_surrogate2.csv', 'w')
  data = csv.reader(input_file)
  writer = csv.writer(output_file)
  specials = '['

  for line in data:
    line = [value.replace(specials, '') for value in line]
    writer.writerow(line)

  input_file.close()
  output_file.close()

  input_file = open('temp/2_2_collected_data_surrogate2.csv', 'r')
  output_file = open('temp/2_2_collected_data_surrogate3.csv', 'w')
  data = csv.reader(input_file)
  writer = csv.writer(output_file)
  specials = ']'

  for line in data:
    line = [value.replace(specials, '') for value in line]
    writer.writerow(line)

  input_file.close()
  output_file.close() 

  input_file = open('temp/2_2_collected_data_surrogate3.csv', 'r')
  output_file = open('2_2_collected_data_surrogate.csv', 'w')
  data = csv.reader(input_file)
  writer = csv.writer(output_file)
  specials = ' '

  for line in data:
    line = [value.replace(specials, '') for value in line]
    writer.writerow(line)

  input_file.close()
  output_file.close() 

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

def main():
	filename = "2_2_collected_data.csv"  # Change this to your CSV file name
	columns = convert_columns(read_csv_columns(filename))
	output_file = open('2_2_collected_data_surrogate' + '.csv', 'w', newline='')

	i=0
	while(i<len(columns)-1):
		comp=columns[i][4:len(columns[0])]
		user=columns[i+1][4:len(columns[0])]
		local=columns[i][0]
		name=columns[i][1]
		type=columns[i][2]
		type_1=columns[i+1][2]
		group=columns[i][3]
		output_file.write(str(local)+','+str(name)+','+str(type)+','+str(group)+','+str(comp))
		output_file.write(' \n')
		output_file.write(str(local)+','+str(name)+','+str(type_1)+','+str(group)+','+str(user))
		output_file.write(' \n')
		j=0
		while(j<100):
			output_file.write(str(local)+','+str(name)+','+str(type)+','+str(group)+','+str(comp))
			output_file.write(' \n')
			random.shuffle(user)
			output_file.write(str(local)+','+str(name)+',SURROGATE,'+str(group)+','+str(user))
			output_file.write(' \n')
			j+=1
		i+=2
	output_file.close()


	transpose_csv('2_2_collected_data_surrogate.csv', 'temp/2_2_collected_data_surrogate1.csv')
	delete()

main()