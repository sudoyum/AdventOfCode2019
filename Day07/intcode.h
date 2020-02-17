#ifndef INTCODE_H
#define INTCODE_H

#include <stdbool.h>

#define HALTED 0xFF231
#define START  0xFF232
#define DONE   0xFF233
#define WAIT_INPUT 0xFF234
#define WAIT_OUTPUT 0xFF235

#define FAILURE -1
#define SUCCESS 0
#define DEBUG_PRINT 0

#define INPUTLEN_GUESS 50


typedef struct IntCode {
  char id;
  long long pc;
  int phase;
  int state;

  long long *insts;
  int insts_size;
  int input;
  int input_count;
  long long output;

  //Day07 specific
  bool do_phases;

  long long relative_base;
} IntCode_t;


void run_intcode(IntCode_t *ic);
IntCode_t *init_intcode(char id, long long *insts, int insts_size);

void reinit_intcode(IntCode_t *ic, long long *insts);
void free_intcode(IntCode_t *ic);
int parse_intcode(char *file_name, long long **arr, int *arr_size);
void print_intcode(IntCode_t *ic);

#endif
