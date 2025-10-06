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
tamanho_cadeia = 10.0
descarte=1
p = 0.67

#gera cadeia M0 do computador
def gerador_cadeia_M0():
    i = 0
    cadeia_markov = []
    while (i < tamanho_cadeia):
      markov = random.random()
      if (markov < p):
        cadeia_markov+= [1]
      else:
        cadeia_markov+= [-1]
      i+=1
    return cadeia_markov

#gera cadeia M2 do computador
def gerador_cadeia_M2():
  i = 2
  cadeia_markov = [-1,-1]
  while (i < tamanho_cadeia):
    markov = random.random()
    if (cadeia_markov[i-2] == -1 and cadeia_markov[i-1] == -1 and markov < 1):
      cadeia_markov+= [1]
    elif (cadeia_markov[i-2] == -1 and cadeia_markov[i-1] == 1 and markov < 1):
      cadeia_markov+= [1]
    elif (cadeia_markov[i-2] == 1 and cadeia_markov[i-1] == -1 and markov < 0):
      cadeia_markov+= [0]
    elif (cadeia_markov[i-2] == 1 and cadeia_markov[i-1] == 1 and markov < 0.67):
      cadeia_markov+= [1]
    else:
      cadeia_markov+= [-1]
    i+=1
  return cadeia_markov

#descarta os 20 primeiros
def descarta(cadeia):
  cadeia_nova=[]
  cadeia_nova=cadeia[descarte:len(cadeia)]
  return cadeia_nova

#comportamentos
#PROBABILITY MATCHING
def matcher():
    i = 0
    cadeia_markov = []
    while (i < tamanho_cadeia-descarte):
      markov = random.random()
      if (markov < p):
        cadeia_markov+= [1]
      else:
        cadeia_markov+= [-1]
      i+=1
    return cadeia_markov

#alternation
def alternation():
    i = 0
    cadeia_markov = []
    while (i < tamanho_cadeia-descarte):
      markov = random.random()
      if (i % 2 == 0):
        cadeia_markov+= [1]
      elif (markov<0.95):
        cadeia_markov+= [-1]
      else:
        cadeia_markov+= [1]        
      i+=1
    return cadeia_markov

#persevaration
def persevaration():
    i = 0
    cadeia_markov = []
    while (i < tamanho_cadeia):
      markov = random.random()
      if (markov < 0.95):
        cadeia_markov+= [1]
      else:
        cadeia_markov+= [-1]
      i+=1
    return cadeia_markov

def wsls(cadeia):
  i=0
  cadeia_markov = [-1]
  while (i < len(cadeia)-1):
    markov = random.random()
    if (cadeia[i]==cadeia_markov[i] and markov>0.10):
      cadeia_markov+= [cadeia_markov[i]]
    elif (cadeia[i]!=cadeia_markov[i] and cadeia_markov[i]==-1 and markov>0.10):
      cadeia_markov+= [1]
    else:
      cadeia_markov+= [-1]
    i+=1
  return cadeia,cadeia_markov


def obs(cadeia):
  i=0
  cadeia_markov = [-1,-1]
  while (i < len(cadeia)-2):
    markov = random.random()
    if cadeia[i]==1 and cadeia[i+1]==1 and markov>0.05:
      cadeia_markov+=[1]
    elif cadeia[i]==1 and cadeia[i+1]==-1 and markov>0.05:
      cadeia_markov+=[-1]
    elif cadeia[i]==-1 and cadeia[i+1]==1 and markov>0.05:
      cadeia_markov+=[1]
    elif cadeia[i]==-1 and cadeia[i+1]==-1 and markov>0.05:
      cadeia_markov+=[1]
    elif(markov>0.50):
      cadeia_markov+=[1]
    else:
      cadeia_markov+=[-1]
    i+=1
  return cadeia,cadeia_markov
  
def main():

    f = open('behaviors_surrogates.csv','w')
    i=0
    markovM0 = []
    markovM2 = []
    while(i<50):

      markovM0 = descarta(gerador_cadeia_M0())
      markovM2 = descarta(gerador_cadeia_M2())
      print("PROBABILITY MATCHING,comp,INPUT,M0,",markovM0, file=f)
      prob_match_1 = matcher()
      print("PROBABILITY MATCHING,comp,OUTPUT,M0,",prob_match_1, file=f)
      print("PROBABILITY MATCHING,comp,INPUT,M2,",markovM2, file=f)
      prob_match_2 = matcher()
      print("PROBABILITY MATCHING,comp,OUTPUT,M2,",prob_match_2, file=f)

      markovM0 = descarta(gerador_cadeia_M0())
      markovM2 = descarta(gerador_cadeia_M2())
      print("ALTERNATION,comp,INPUT,M0,",markovM0, file=f)
      ALTERNATION_1 = alternation()
      print("ALTERNATION,comp,OUTPUT,M0,",ALTERNATION_1, file=f)
      print("ALTERNATION,comp,INPUT,M2,",markovM2, file=f)
      ALTERNATION_2 = alternation()
      print("ALTERNATION,comp,OUTPUT,M2,",ALTERNATION_2, file=f)

      markovM0 = descarta(gerador_cadeia_M0())
      markovM2 = descarta(gerador_cadeia_M2())
      print("PERSEVERATION,comp,INPUT,M0,",markovM0, file=f)
      perseveration_1 = persevaration()
      print("PERSEVERATION,comp,OUTPUT,M0,",perseveration_1, file=f)
      print("PERSEVERATION,comp,INPUT,M2,",markovM2, file=f)
      perseveration_2 = persevaration()
      print("PERSEVERATION,comp,OUTPUT,M2,",perseveration_2, file=f)

      markovM0 = descarta(gerador_cadeia_M0())
      markovM2 = descarta(gerador_cadeia_M2())
      print("WSLS,comp,INPUT,M0,",markovM0, file=f)
      win_stay_M0 = wsls(markovM0)
      print("WSLS,comp,OUTPUT,M0,",win_stay_M0[1], file=f)
      print("WSLS,comp,INPUT,M2,",markovM2, file=f)
      win_stay_M2 = wsls(markovM2)
      print("WSLS,comp,OUTPUT,M2,",win_stay_M2[1], file=f)

      markovM0 = descarta(gerador_cadeia_M0())
      markovM2 = descarta(gerador_cadeia_M2())
      print("IO,comp,INPUT,M0,",markovM0, file=f)
      IO_M0 = obs(markovM0)
      print("IO,comp,OUTPUT,M0,",IO_M0[1], file=f)
      print("IO,comp,INPUT,M2,",markovM2, file=f)
      IO_M2 = obs(markovM2)
      print("IO,comp,OUTPUT,M2,",IO_M2[1], file=f)          
      
      j=0
      while (j<100):
        print("PROBABILITY MATCHING,comp,INPUT,M0,",markovM0, file=f)
        random.shuffle(prob_match_1)
        print("PROBABILITY MATCHING,comp,SURROGATE,M0,",prob_match_1, file=f)
        print("PROBABILITY MATCHING,comp,INPUT,M2,",markovM2, file=f)
        random.shuffle(prob_match_2)
        print("PROBABILITY MATCHING,comp,SURROGATE,M2,",prob_match_2, file=f)

        print("ALTERNATION,comp,INPUT,M0,",markovM0, file=f)
        random.shuffle(ALTERNATION_1)
        print("ALTERNATION,comp,SURROGATE,M0,",ALTERNATION_1, file=f)
        print("ALTERNATION,comp,INPUT,M2,",markovM2, file=f)
        random.shuffle(ALTERNATION_2)
        print("ALTERNATION,comp,SURROGATE,M2,",ALTERNATION_2, file=f)

        print("PERSEVERATION,comp,INPUT,M0,",markovM0, file=f)
        random.shuffle(perseveration_1)
        print("PERSEVERATION,comp,SURROGATE,M0,",perseveration_1, file=f)
        print("PERSEVERATION,comp,INPUT,M2,",markovM2, file=f)
        random.shuffle(perseveration_2)
        print("PERSEVERATION,comp,SURROGATE,M2,",perseveration_2, file=f)

        print("WSLS,comp,INPUT,M0,",win_stay_M0[0], file=f)
        random.shuffle(win_stay_M0[1])
        print("WSLS,comp,SURROGATE,M0,",win_stay_M0[1], file=f)
        print("WSLS,comp,INPUT,M2,",win_stay_M2[0], file=f)
        random.shuffle(win_stay_M2[1])
        print("WSLS,comp,SURROGATE,M2,",win_stay_M2[1], file=f)

        print("IO,comp,INPUT,M0,",IO_M0[0], file=f)
        random.shuffle(IO_M0[1])
        print("IO,comp,SURROGATE,M0,",IO_M0[1], file=f)
        print("IO,comp,INPUT,M2,",IO_M2[0], file=f)
        random.shuffle(IO_M2[1])
        print("IO,comp,SURROGATE,M2,",IO_M2[1], file=f)

        j+=1
      i+=1
    f.close()

main ()