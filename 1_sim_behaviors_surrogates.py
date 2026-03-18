###########################################################
# January 2025 #
# Bernard Costa #
# #
# Development Information: #
# <Environment (SublimeText/Python 3.6 IDLE)> #
# <Operational System (Ubuntu)>

# Description:
# This script generates synthetic behavioral sequences based on:
# - Markov processes (M0: zeroth-order, M2: second-order)
# - Behavioral strategies (probability matching, alternation, perseveration
#     win-stay/lose-shift and ideal predictor)
# - Surrogate datasets via shuffling
#
# Output:
# A CSV file ("1_sim_behaviors_surrogates_output.csv") containing:
# - Input sequences
# - Model-generated outputs
# - Surrogate (shuffled) sequences
###########################################################
import numpy as np
import random
# -----------------------------
# GLOBAL PARAMETERS
# -----------------------------
# input sequence size
seq_size = 220
# initial sequence part to be dicard
discard=20
# matcher probability
p = 0.67

# -----------------------------
# MARKOV SEQUENCE GENERATORS
# -----------------------------

# generates input sequence M0
def generator_seq_M0():
    i = 0
    seq_markov = []
    while (i < seq_size):
      markov = random.random()
      if (markov < p):
        seq_markov+= [1]
      else:
        seq_markov+= [-1]
      i+=1
    return seq_markov

# generates input sequence M2
def generator_seq_M2():
  i = 2
  seq_markov = [-1,-1]
  while (i < seq_size):
    markov = random.random()
    if (seq_markov[i-2] == -1 and seq_markov[i-1] == -1 and markov < 1):
      seq_markov+= [1]
    elif (seq_markov[i-2] == -1 and seq_markov[i-1] == 1 and markov < 1):
      seq_markov+= [1]
    elif (seq_markov[i-2] == 1 and seq_markov[i-1] == -1 and markov < 0):
      seq_markov+= [0]
    elif (seq_markov[i-2] == 1 and seq_markov[i-1] == 1 and markov < 0.67):
      seq_markov+= [1]
    else:
      seq_markov+= [-1]
    i+=1
  return seq_markov

# -----------------------------
# PREPROCESSING
# -----------------------------

# discard the firt part of the sequence
# number defined in the golbal parameters
def discard_initial(seq):
  seq_new=[]
  seq_new=seq[discard:len(seq)]
  return seq_new

# -----------------------------
# BEHAVIORAL MODELS
# -----------------------------

# PROBABILITY MATCHING
def matcher():
    i = 0
    seq_markov = []
    while (i < seq_size-discard):
      markov = random.random()
      if (markov < p):
        seq_markov+= [1]
      else:
        seq_markov+= [-1]
      i+=1
    return seq_markov

#alternation
def alternation():
    i = 0
    seq_markov = []
    while (i < seq_size):
      markov = random.random()
      if (i % 2 == 0 and markov<0.95):
        seq_markov+= [1]
      elif (i % 2 == 0 and markov>0.95):
        seq_markov+= [-1] 
      elif (i % 2 != 0 and markov>0.95):
        seq_markov+= [1]
      elif (i % 2 != 0 and markov<0.95):
        seq_markov+= [-1]  
      i+=1
    return seq_markov

#persevaration and ideal predictor in M0
def persevaration():
    i = 0
    seq_markov = []
    while (i < seq_size):
      markov = random.random()
      if (markov < 0.95):
        seq_markov+= [1]
      else:
        seq_markov+= [-1]
      i+=1
    return seq_markov

#win-stay/lose-shift
def wsls(seq):
  i=0
  seq_markov = [-1]
  while (i < len(seq)-1):
    markov = random.random()
    if (seq[i] == seq_markov[i] and markov > 0.05):
        seq_markov += [seq_markov[i]] 
    elif (seq[i] != seq_markov[i] and markov > 0.05):
        seq_markov += [-seq_markov[i]] 
    elif (markov < 0.05):
        seq_markov += [random.choice([-1, 1])] 
    i+=1
  return seq,seq_markov

#ideal predictor in M2
def ideal(seq):
  i=0
  seq_markov = [-1,-1]
  while (i < len(seq)-2):
    markov = random.random()
    if seq[i]==1 and seq[i+1]==1 and markov>0.05:
      seq_markov+=[1]
    elif seq[i]==1 and seq[i+1]==-1 and markov>0.05:
      seq_markov+=[-1]
    elif seq[i]==-1 and seq[i+1]==1 and markov>0.05:
      seq_markov+=[1]
    elif seq[i]==-1 and seq[i+1]==-1 and markov>0.05:
      seq_markov+=[1]
    elif(markov>0.50):
      seq_markov+=[1]
    else:
      seq_markov+=[-1]
    i+=1
  return seq,seq_markov

# -----------------------------
# MAIN PIPELINE
# -----------------------------

def main():

    f = open('1_sim_behaviors_surrogates_output.csv','w')
    i=0
    markovM0 = []
    markovM2 = []
    while(i<50):

      markovM0 = discard_initial(generator_seq_M0())
      markovM2 = discard_initial(generator_seq_M2())
      print("PROBABILITY MATCHING,comp,INPUT,M0,",markovM0, file=f)
      prob_match_1 = matcher()
      print("PROBABILITY MATCHING,comp,OUTPUT,M0,",prob_match_1, file=f)
      print("PROBABILITY MATCHING,comp,INPUT,M2,",markovM2, file=f)
      prob_match_2 = matcher()
      print("PROBABILITY MATCHING,comp,OUTPUT,M2,",prob_match_2, file=f)

      markovM0 = discard_initial(generator_seq_M0())
      markovM2 = discard_initial(generator_seq_M2())
      print("ALTERNATION,comp,INPUT,M0,",markovM0, file=f)
      ALTERNATION_1 = alternation()
      print("ALTERNATION,comp,OUTPUT,M0,",ALTERNATION_1, file=f)
      print("ALTERNATION,comp,INPUT,M2,",markovM2, file=f)
      ALTERNATION_2 = alternation()
      print("ALTERNATION,comp,OUTPUT,M2,",ALTERNATION_2, file=f)

      markovM0 = discard_initial(generator_seq_M0())
      markovM2 = discard_initial(generator_seq_M2())
      print("PERSEVERATION,comp,INPUT,M0,",markovM0, file=f)
      perseveration_1 = persevaration()
      print("PERSEVERATION,comp,OUTPUT,M0,",perseveration_1, file=f)
      print("PERSEVERATION,comp,INPUT,M2,",markovM2, file=f)
      perseveration_2 = persevaration()
      print("PERSEVERATION,comp,OUTPUT,M2,",perseveration_2, file=f)

      markovM0 = discard_initial(generator_seq_M0())
      markovM2 = discard_initial(generator_seq_M2())
      print("WSLS,comp,INPUT,M0,",markovM0, file=f)
      win_stay_M0 = wsls(markovM0)
      print("WSLS,comp,OUTPUT,M0,",win_stay_M0[1], file=f)
      print("WSLS,comp,INPUT,M2,",markovM2, file=f)
      win_stay_M2 = wsls(markovM2)
      print("WSLS,comp,OUTPUT,M2,",win_stay_M2[1], file=f)

      markovM0 = discard_initial(generator_seq_M0())
      markovM2 = discard_initial(generator_seq_M2())
      print("IO,comp,INPUT,M0,",markovM0, file=f)
      IO_M0 = persevaration()
      print("IO,comp,OUTPUT,M0,",IO_M0, file=f)
      print("IO,comp,INPUT,M2,",markovM2, file=f)
      IO_M2 = ideal(markovM2)
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

        print("IO,comp,INPUT,M0,",markovM0, file=f)
        random.shuffle(IO_M0)
        print("IO,comp,SURROGATE,M0,",IO_M0, file=f)
        print("IO,comp,INPUT,M2,",IO_M2[0], file=f)
        random.shuffle(IO_M2[1])
        print("IO,comp,SURROGATE,M2,",IO_M2[1], file=f)

        j+=1
      i+=1
    f.close()

main ()
