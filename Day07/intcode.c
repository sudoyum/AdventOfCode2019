#include "intcode.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void split_line(char *line, long long **arr, int *arr_size) {
  long long *array = (long long *) malloc(sizeof(long long) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int item_num = 0;

  char *tokenized = strtok(line, ",");
  while(NULL != tokenized) {
    array[item_num++] = atol(tokenized);
    if(item_num == array_size) {
      array_size += INPUTLEN_GUESS;
      array = (long long *) realloc(array, sizeof(long long) * array_size);
    }
    tokenized = strtok(NULL, ",");
  }

  *arr = array;
  *arr_size = item_num;
}

int parse_intcode(char *file_name, long long **arr, int *arr_size) {
  FILE *fp;
  size_t len;
  char *line = NULL;
  int line_num = 0;


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  while(-1 != getline(&line, &len, fp)) {
    if(line_num > 1) {
      printf("parse issue. expected one line file\n");
      return FAILURE;
    }
    split_line(line, arr, arr_size);
    line_num++;
  }

  if(line) {
    free(line);
  }

  return SUCCESS;
}

IntCode_t *init_intcode(char id, long long *insts, int insts_size) {
  IntCode_t *ic = (IntCode_t *)malloc(sizeof(IntCode_t));
  ic->insts_size = insts_size;
  ic->insts = (long long *)calloc(insts_size * 1000000, sizeof(long long));
  memcpy(ic->insts, insts, sizeof(long long) * insts_size);
  ic->state = START;
  ic->id = id;
  ic->pc = 0;
  ic->input_count = 0;
  ic->input = 0;
  ic->output = 0;
  ic->do_phases = false;
  ic->relative_base = 0;
  return ic;
}

void reinit_intcode(IntCode_t *ic, long long *insts) {
  memcpy(ic->insts, insts, sizeof(long long) * ic->insts_size);
  ic->state = START;
  ic->input_count = 0;
  ic->pc = 0;
  ic->output = 0;
}

void free_intcode(IntCode_t *ic) {
  free(ic->insts);
  ic->insts = NULL;
  free(ic);
  ic = NULL;
}

void print_intcode(IntCode_t *ic) {
  if(ic == NULL) {
    printf("ic NULL.\n");
  }
  for(int i = 0; i < ic->insts_size - 1; i++) {
    printf("%d,", ic->insts[i]);
  }
  printf("%d\n", ic->insts[ic->insts_size - 1]);
}

void run_intcode(IntCode_t *ic) {

  while(1) {
    if(ic->pc >= ic->insts_size) {
      ic->state = DONE;
      break;
    }

    long long parameter = ic->insts[ic->pc];
    long long param1_mode = 0, param2_mode = 0, param3_mode = 0;
    long long opcode;

    opcode = parameter % 100;
    if(parameter > 99) {
      param1_mode = (parameter/100) % 10;
      param2_mode = (parameter/1000) % 10;
      param3_mode = (parameter/10000) % 10;
    }

    //Handle halt instruction
    if(opcode == 99) {
      ic->state = HALTED;
      break;
    }

    // 0 - position
    // 1 - immediate
    // 2 - relative
    long long param1, param2;
    if(1 == param1_mode) {
      param1 = ic->insts[ic->pc + 1];
    } else if(0 == param1_mode) {
      param1 = ic->insts[ic->insts[ic->pc + 1]];
    } else if(2 == param1_mode) {
      param1 = ic->insts[ic->relative_base + ic->insts[ic->pc + 1]];
    } else {
      printf("FAILURE: unknown param1_mode=%d\n", param2_mode);
    }

    if(1 == param2_mode) {
      param2 = ic->insts[ic->pc + 2];
    } else if(0 == param2_mode) {
      param2 = ic->insts[ic->insts[ic->pc + 2]];
    } else if(2 == param2_mode) {
      param2 = ic->insts[ic->relative_base + ic->insts[ic->pc + 2]];
    } else {
      printf("FAILURE: unknown param2_mode=%d\n", param2_mode);
    }

    switch(opcode) {
      case 1: {
                if(param3_mode == 2) {
                  ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = param1 + param2;
                } else {
                  ic->insts[ic->insts[ic->pc + 3]] = param1 + param2;
                }
                ic->pc += 4;
                break;
              }
      case 2: {
                if(param3_mode == 2) {
                  ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = param1 * param2;
                } else {
                  ic->insts[ic->insts[ic->pc + 3]] = param1 * param2;
                }
                ic->pc += 4;
                break;
              }
      case 3: {
                if(param1_mode == 2) {
                  ic->insts[ic->relative_base + ic->insts[ic->pc + 1]] = (ic->input_count == 0 && ic->do_phases) ? ic->phase: ic->input;
                } else {
                  ic->insts[ic->insts[ic->pc + 1]] = (ic->input_count == 0 && ic->do_phases) ? ic->phase: ic->input;
                }
                ic->input_count++;
                ic->pc += 2;
                break;
              }
      case 4: {
                ic->output = param1;
                ic->pc += 2;
                return;
               }
      case 5: {
                if(param1 != 0) {
                  ic->pc = param2;
                } else {
                  ic->pc += 3;
                }
                break;
               }
      case 6: {
                if(param1 == 0) {
                  ic->pc = param2;
                } else {
                  ic->pc+=3;
                }
                break;
               }
      case 7: {
                if(param1 < param2) {
                  if(param3_mode == 2) {
                    ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = 1;
                  } else {
                    ic->insts[ic->insts[ic->pc + 3]] = 1;
                  }
                } else {
                  if(param3_mode == 2) {
                    ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = 0;
                  } else {
                    ic->insts[ic->insts[ic->pc + 3]] = 0;
                  }
                }
                ic->pc += 4;
                break;
               }
      case 8: {
                if(param1 == param2) {
                  if(param3_mode == 2) {
                    ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = 1;
                  } else {
                    ic->insts[ic->insts[ic->pc + 3]] = 1;
                  }
                } else {
                  if(param3_mode == 2) {
                    ic->insts[ic->relative_base + ic->insts[ic->pc + 3]] = 0;
                  } else {
                    ic->insts[ic->insts[ic->pc + 3]] = 0;
                  }
                }
                ic->pc += 4;
                break;
               }
      case 9: {
                ic->relative_base += param1;
                ic->pc += 2;
                break;
               }
      default: {
                 printf("unknown opcode %d, exiting\n", opcode);
                 exit(EXIT_SUCCESS);
               }
    }
  }
}
