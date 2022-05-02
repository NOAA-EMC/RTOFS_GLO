#include <cmath>
#include <cstdlib>

// Robert Grumbine 1999-present

// Class for working on/with genetic algorithms.  Begun ca. 2004
//   for education, and then first used in Gulf Stream Finder
// Earliest genetics is 1999, on a genetic programming type idea
//
//  Oldest extant is 9 August 2001
//    -- functions in the include file, but was not created with a class
//  Dec 24 2001 -- added: 
//    -- genetics class itself
//  Jan  8 2002 -- minor mods to types
//  Oct 30 2002 -- add error check for code type
//  Mar 31 2003 -- add transcribe and retroscribe (for read in/out) 
//  May 25 2004 -- change vector to mvector
//  May 10 2005 -- make capable of working with multiobjective optimization
//  Sep 23 2005 -- ifdef an output to VERBOSE only
//  5 Nov 2009 type cast scores in if tests
// 26 Jan 2010 replace type cast with utility function 'nulled' for inter-system robustness


// Define/construct header for genetic algorithms
#ifndef POINTSH
  #include "points.h"
#endif
#ifndef MVECTORH
  #include "mvector.h"
#endif

#ifndef GENESH
  #define GENESH

bool nulled(mvector<float> &scores) ;

//Start bringing over the genetic code segments:
#define FLOAT_TYPE 1
#define INT_TYPE   0

typedef union {
  int ival;
  float fval;
} united;

typedef struct {
  int nbits, type;
  union {
    int ival;
    float fval;
  } base;
  union {
    int ival;
    float fval;
  } top;
} codon;
class genetic_code {
  public:  // Privatize these as critter and species classes are built.
    int ncodes;
    int code_length;
    codon *code;
  public:
    genetic_code();
    genetic_code(int );
    void resize(int );
    void newgene(int, int , int, united, united);
    void write(FILE *);
    void read (FILE *);
};
genetic_code::genetic_code() {
  ncodes = 0;
  code_length = 0;
  code = (codon *) NULL;
}
genetic_code::genetic_code(int n) {
  int i;
  ncodes = n;
  code_length = 0;
  code = new codon[n];
  for (i = 0 ; i < n; i++) {
    code[i].nbits = 0;
  }
} 
void genetic_code::resize(int n) {
  int i;
  this->ncodes = n;
  this->code_length = 0;
  this->code = new codon[n];
  for (i = 0 ; i < n; i++) {
    code[i].nbits = 0;
  }
} 
void genetic_code::newgene(int n, int len, int type, united lower, united upper) {
  int i, tot = 0;

  if (n > ncodes-1) {
    printf("specifying out of range code number %d vs. %d\n",n, ncodes);
    return;
  }
  code[n].nbits = len;
  code[n].type  = type;
  if (type == FLOAT_TYPE ) {
    code[n].base.fval = lower.fval;
    code[n].top.fval = upper.fval;
  }
  else {
    code[n].base.ival = lower.ival;
    code[n].top.ival = upper.ival;
  }
  for (i = 0; i < ncodes; i++) {
    tot += code[i].nbits;
  }
  code_length = tot;
}  

void genetic_code::read(FILE *fin) {
  int i, tmp1, tmp2;

  fscanf(fin, "%d %d ",&tmp1, &tmp2);
  this->ncodes = tmp1;
  this->code_length = tmp2;

  for (i = 0; i < this->ncodes; i++) {
    fscanf(fin, "%d %d",&tmp1,&tmp2); 
    this->code[i].nbits = tmp1;
    this->code[i].type = tmp2;
    if (this->code[i].type == FLOAT_TYPE) {
      fscanf(fin," %f %f ",
        &(this->code[i].base.fval), &(this->code[i].top.fval) );
    }
    else {
      fscanf(fin,"  %d %d ",
        &(this->code[i].base.ival), &(this->code[i].top.ival) );
    }
  }
}
void genetic_code::write(FILE *fout) {
  int i; 
  fprintf(fout, "%d %d ",this->ncodes, this->code_length);
  for (i = 0; i < this->ncodes; i++) {
    if (this->code[i].type == FLOAT_TYPE) {
      fprintf(fout," %d %d %f %f ",
        this->code[i].nbits, this->code[i].type, 
        this->code[i].base.fval, this->code[i].top.fval);
    }
    else {
      fprintf(fout," %d %d %d %d ",
        this->code[i].nbits, this->code[i].type, 
        this->code[i].base.ival, this->code[i].top.ival);
    }
  }
  fprintf(fout,"\n");
}




///////////////////////////////////////////
void transcriber(mvector<int> &genome, mvector<united> &weights, genetic_code &x) ;
void retroscribe(mvector<int> &genome, mvector<united> &weights, genetic_code &x) ;
void showgenes(FILE *fout, mvector<int> &genome, genetic_code &x) ;

// Transcribe a hierarchal genome into a mvector of weights
void transcriber(mvector<int> &genome, mvector<united> &weights, genetic_code &x) {
  int i, k, mul, base, tmp;
  float val;

  #ifdef VERBOSE2
    cout << "entered transcriber\n"; cout.flush();
    genome.printer(stdout);
  #endif
  base = 0;
  for (k = 0 ; k < x.ncodes; k++) {
    mul = 1;
    tmp = 0;
    for (i = 0; i < x.code[k].nbits; i++) {
      tmp += mul*genome[i+base];
      mul *= 2;
    }
    base += x.code[k].nbits;

    if (x.code[k].type == INT_TYPE) {
      tmp = tmp % (x.code[k].top.ival - x.code[k].base.ival + 1);
      tmp += x.code[k].base.ival;
      weights[k].ival = tmp;
      #ifdef VERBOSE2
      printf("ibase top = %d %d %d\n",
                 x.code[k].base.ival, x.code[k].top.ival, tmp);
      #endif
    }
    else {
      #ifdef VERBOSE2
      printf("fbase top = %f %f\n",x.code[k].base.fval, x.code[k].top.fval);
      #endif
      val = (float) tmp / pow(2., x.code[k].nbits) ;
      val *= (x.code[k].top.fval - x.code[k].base.fval);
      val += x.code[k].base.fval;
      weights[k].fval = val;
    }

  }
  #ifdef VERBOSE
  printf("trans %d %f %f\n",weights[0].ival, weights[1].fval, weights[2].fval);
  fflush(stdout);
  #endif

  return ;
}

// Transcribe a mvector of weights back in to a genome
void retroscribe(mvector<int> &genome, mvector<united> &weights, genetic_code &x) {
  int i, k, base, tmp;
  float fdel;

  #ifdef VERBOSE
    cout << "entered retrotranscriber\n"; cout.flush();
  #endif
  genome.resize(x.code_length); 
  base = 0;
  for (k = 0 ; k < x.ncodes; k++) {
    if (x.code[k].type == FLOAT_TYPE) {
      if (weights[k].fval > x.code[k].top.fval) {
        printf("have out of range weight, %f vs top of %f\n",
              weights[k].fval, x.code[k].top.fval);
        exit(4);
      }
      fdel = pow(2.,x.code[k].nbits) * 
            (weights[k].fval - x.code[k].base.fval)/
            (x.code[k].top.fval - x.code[k].base.fval);
      tmp = (int) (0.5 + fdel);
      for (i = 0; i < x.code[k].nbits; i++) {
        genome[i+base] = tmp % 2;
        tmp /= 2;
      }
      base += x.code[k].nbits;
    }
    else {
      cout << "Invoked the INT_TYPE retroscription, which is not yet ready\n"; 
      cout.flush();
      exit(5);
    } 
    #ifdef VERBOSE
      printf("%f  %f %f %d\n",weights[k].fval, x.code[k].top.fval, 
                              x.code[k].base.fval, tmp);
    #endif

  }
  #ifdef VERBOSE2
  genome.printer(stdout);
  #endif

  return ;
}
/////////////////////////////////
void showgenes(FILE *fout, mvector<int> &genome, genetic_code &x) {
  int i;
  mvector<united> weights(x.ncodes);
  transcriber(genome, weights, x);
  for (i = 0; i < weights.xpoints(); i++) {
    if (x.code[i].type == FLOAT_TYPE) {
      #ifdef VERBOSE2
      fprintf(fout, "float ");
      #endif
      fprintf(fout, "%f ",weights[i].fval);
    }
    else {
      #ifdef VERBOSE2
      fprintf(fout, "int ");
      #endif
      fprintf(fout, "%d ",weights[i].ival);
    }
  }
  fprintf(fout, "\n");
  return;
}


//////////////////////////////////////////////////////////////
// Generic genetics
#define PCROSS 0.5
#define PMU
void swap(mvector<int> &x, mvector<int> &y) ;
void order(mvector<mvector<int> > &genomes, mvector<float> &scores);
void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores) ;
void newgenes(mvector<int> &genome) ;
void mutate(mvector<int> &genome) ;
int find_parent(mvector<float> &table, float trand) ;
int crossover(mvector<mvector<int> > &genomes, int p1, int p2, int rep) ;
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) ;
// Multi-objective-capable:
void order(mvector<mvector<int> > &genomes, mvector<mvector<float> > &scores);
void grazer(mvector<mvector<int> > &genomes, mvector<mvector<float> > &scores) ;
void reproducer(mvector<mvector<int> > &genomes, mvector<mvector<float> > &scores) ;


//Swap two genomes:
void swap(mvector<int> &x, mvector<int> &y) {
  mvector<int> tmp(x.xpoints() );

  tmp = y;
  y = x;
  x = tmp;

  return ;
}

// Partial sorting of genomes by score:
void order(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  int i, j;
  float tmp;

  for (i = 0; i < scores.xpoints() - 1; i++) {
    for (j = i+1; j < scores.xpoints() ; j++) {
      if (scores[j] > scores[i]) {
        tmp = scores[j]; scores[j] = scores[i]; scores[i] = tmp;
        swap(genomes[i], genomes[j]);
        continue;
      }
    }
  }

  return;
}

///////////// Grazer 'eats up' the excessively identical genomes
void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  int i, j;
    
  for (i = 0; i < scores.xpoints(); i++) {
    for (j = i+1; j < scores.xpoints(); j++) {
       if (scores[i] == scores[j]) { // might be same genome
         if (genomes[i] == genomes[j]) {
           mutate(genomes[j]);
           scores[j] = 0.0;
           continue;
         }
       } // ifs testing on too-similar genomes
    }
  }
  return ;
}

/////////////////////////
void mutate(mvector<int> &genome) {
  int j;
// PMUTATE = 1 / genome length

  for (j = 0; j < genome.xpoints(); j++) {
    if ((1.0*rand()) /(RAND_MAX+1.0) < 1./genome.xpoints() ) {
      if (genome[j] == 1) { genome[j] = 0; }
      else { genome[j] = 1 ; }
    }
  }
  return;
}
int find_parent(mvector<float> &table, float trand) {
  int i;
  for (i = 0; i < table.xpoints(); i++) {
    if (trand < table[i]) return i;
  }
  return 0;
}
int crossover(mvector<mvector<int> > &genomes, int p1, int p2, int rep) {
  int crosspoint, i;
  crosspoint = (int) (0.5 + ( (float) genomes[p1].xpoints()*rand())/
                             (RAND_MAX + 1.0) );
  for (i = 0; i < genomes[p1].xpoints(); i++) {
    if (i < crosspoint) {
      genomes[rep][i] = genomes[p1][i];
    }
    else {
      genomes[rep][i] = genomes[p2][i];
    }
  }
  return crosspoint;
}

// Conduct the reproduction
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  float average = scores.average(), reqt;
  int i, nbetter;
  float total, running, trand;
  int nparents = 0, parent1, parent2;
  int tries;
  mvector<float> pcross(scores.xpoints() );

  nbetter = 0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > average) nbetter += 1;
  }

  if (nbetter > genomes.xpoints()/2) {
    reqt = average + 0.25 * (scores.maximum() - average) ;
    if (scores[scores.xpoints()/2] > reqt) reqt = scores[scores.xpoints()/2];
  }
  else {
    reqt = average;
  }
  reqt = max(-100.0, reqt);

// Weight roulette wheel by score, high score is better
  total = 0.0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] >= reqt) { total += scores[i]; }
    else {
      scores[i] = 0;
    }
  }
  running = 0.;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > 0.0) {
      nparents += 1;
      running += scores[i] / total;
      pcross[i] = running;
    }
  }
  #ifdef VERBOSE
    printf("nparents = %d\n",nparents);
  #endif

  if (nparents != 1) {
    for (i = 0; i < genomes.xpoints(); i++) {
      if (scores[i] == 0. ) {
        if ((1.0*rand()) /(RAND_MAX+1.0) < PCROSS ) {
          trand = (1.0*rand()) /(RAND_MAX+1.0);
          parent1 = find_parent(pcross, trand);
          tries = 0;
          do {
            tries += 1;
            trand = (1.0*rand()) /(RAND_MAX+1.0);
            parent2 = find_parent(pcross, trand);
          #ifdef VERBOSE
            printf("i %d parent2 parent1 %d %d %d\n",i, parent2, parent1, nparents);
            fflush(stdout);
          #endif
          } while (parent2 == parent1 && tries < 10);
          #ifdef VERBOSE2
            cout << "calling crossover\n"; cout.flush();
          #endif
          crossover(genomes, parent1, parent2, i);
          #ifdef VERBOSE2
            cout << "back from crossover\n"; cout.flush();
          #endif
        }
        else {
          newgenes(genomes[i]);
        }
      }
    }
  }
  // end of horizontal transfer/crossover.  Skip if only 1 parent

  // apply mutation:
  #ifdef VERBOSE
  printf("about to try mutation, reqt = %f\n", reqt); fflush(stdout);
  #endif
  for (i = 0; i < genomes.xpoints(); i++) {
    #ifdef VERBOSE2
    printf("test for mutation on %d\n",i); fflush(stdout);
    #endif
    if (scores[i] <= reqt) {
      #ifdef VERBOSE
        printf("doing mutation on %d\n",i); fflush(stdout);
      #endif
      mutate(genomes[i]);
      #ifdef VERBOSE
        printf("back from mutation %d\n",i); fflush(stdout);
      #endif
      // Note that we don't have to worry about lethal mutations
    }
  }

  return;

}

void newgenes(mvector<int> &genome) {
  int i;
  for (i = 0; i < genome.xpoints(); i++) {
    genome[i] = (int) (0.5 + (1.0*rand()) /(RAND_MAX+1.0) );
  }
  return;
}

//////////////////////////////////////////////
// Multi-objective-capable:
// Partial sorting of genomes by score:
bool dominates(mvector<float> &s1, mvector<float> &s2) ;
int ndominated_by(mvector<mvector<float> > &scores, int ref) ;
int ndominates(mvector<mvector<float> >&scores, int ref) ;

void order(mvector<mvector<int> > &genomes, mvector<mvector<float> > &scores) {
  int i, j, nswap = 0, pass = 0;
  mvector<float> tmp(9000);

  do {
    nswap = 0;
    pass += 1;
    for (i = 0; i < scores.xpoints() - 1; i++) {
      for (j = i+1; j < scores.xpoints() ; j++) {
        if (dominates(scores[j], scores[i])) {
          tmp = scores[j]; scores[j] = scores[i]; scores[i] = tmp;
          swap(genomes[i], genomes[j]);
          nswap += 1;
          //continue;
        }
      }
    }
  }
  while (nswap > 0 && pass < 3);


  return;
}
bool dominates(mvector<float> &s1, mvector<float> &s2) {
  bool res = true;
  for (int i = 0; i < s1.xpoints(); i++) {
    res = res && (s1[i] >= s2[i]);
  }
  return res;
}
int ndominated_by(mvector<mvector<float> > &scores, int ref) {
  int n = 0;
  for (int i = 0; i < scores.xpoints(); i++) {
     if (i != ref) {
       if (dominates(scores[i], scores[ref]) ) n += 1;
     }
  }
  return n;
}
int ndominates(mvector<mvector<float> > &scores, int ref) {
  int n = 0;
  for (int i = 0; i < scores.xpoints(); i++) {
     if (i != ref) {
       if (dominates(scores[ref], scores[i]) ) n += 1;
     }
  }
  return n;
} 

///////////// Grazer 'eats up' the excessively identical genomes
void grazer(mvector<mvector<int> > &genomes, mvector<mvector<float> > &scores) {
  int i, j;

  for (i = 0; i < scores.xpoints(); i++) {
    for (j = i+1; j < scores.xpoints(); j++) {
       if (scores[i] == scores[j]) { // might be same genome
         if (genomes[i] == genomes[j]) {
           mutate(genomes[j]);
           scores[j] = 0.0;
           continue;
         }
       } // ifs testing on too-similar genomes
    }
  }
  return ;
}


void reproducer(mvector<mvector<int> > &genomes, mvector<mvector<float> >&scores) {
  int i;
  mvector<int> doms(genomes.xpoints()), domby(genomes.xpoints());
  mvector<float> pcross(scores.xpoints() );
  int nparents = 0, parent1, parent2;
  int tries;
  float trand = 0., running = 0., total = 0.;

  for (i = 0; i < genomes.xpoints(); i++) {
    doms[i] = ndominates(scores, i);
    domby[i] = ndominated_by(scores, i);
    #ifdef VERBOSE
    printf("i = %d  dominates %d dom_by %d\n",i, doms[i], domby[i]);
    #endif
    if (domby[i] != 0) {
      scores[i] = 0;
      newgenes(genomes[i]);  //Temporary -- fill in with random element
      total += doms[i] + 1;  // add one so that points that are 
                      // nondominating, but also nondominated, can remain
    }
  }

  for (i = 0; i < genomes.xpoints(); i++) {
    //if (! ((float) scores[i] == (float) 0.) ) {
    if (!nulled(scores[i])) {
      nparents += 1;
      running += (float) (1 + doms[i]) / total;
      pcross[i] = running;
    }
  }  
  printf("nparents = %d\n",nparents);

  if (nparents != 1) {
    for (i = 0; i < genomes.xpoints(); i++) { 
      //if ((float) scores[i] == (float) 0. ) {
      if (nulled(scores[i])) {
        if ((1.0*rand()) /(RAND_MAX+1.0) < PCROSS ) {
          trand = (1.0*rand()) /(RAND_MAX+1.0);
          parent1 = find_parent(pcross, trand);
          tries = 0;
          do {
            tries += 1;
            trand = (1.0*rand()) /(RAND_MAX+1.0);
            parent2 = find_parent(pcross, trand);
          } while (parent2 == parent1 && tries < 10);
          crossover(genomes, parent1, parent2, i);
        }
        else {
          newgenes(genomes[i]);
        } 
      }
    }
  }


    // apply mutation:
  for (i = 0; i < genomes.xpoints(); i++) {
    //if ((float) scores[i] == (float) 0. ) {
    if (nulled(scores[i])) {
      mutate(genomes[i]);
    }
  }

  return;

}
// New utility function 26 January 2010
bool nulled(mvector<float> &scores) {
  for (int i = 0; i < scores.xpoints() ; i++) {
    if (scores[i] != (float) 0.0) return false;
  }
  return true;
} 

#endif
