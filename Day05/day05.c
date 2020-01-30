#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

int g_output = 0;

void run_intcode(int *insts, int insts_size, int input_val) {
  int pc = 0;

#if DEBUG_PRINT
  for(int i = 0; i < insts_size - 1; i++) {
    printf("%d,", insts[i]);
  }
  printf("%d\n", insts[insts_size - 1]);
#endif

  while(1) {
    if(pc >= insts_size) {
      break;
    }

    int parameter = insts[pc];
    int mode1 = 0, mode2 = 0, mode3 = 0;
    int opcode;

    opcode = parameter % 100;
    if(parameter > 99) {
      mode1 = (parameter/100) % 10;
      mode2 = (parameter/1000) % 10;
      mode3 = (parameter/10000) % 10;
    }

    //Handle halt instruction
    if(opcode == 99) {
      break;
    }

    int param1, param2;
    if(1 == mode1) {
      param1 = insts[pc + 1];
    } else {
      param1 = insts[insts[pc + 1]];
    }

    if(1 == mode2) {
      param2 = insts[pc + 2];
    } else {
      param2 = insts[insts[pc + 2]];
    }

    switch(opcode) {
      case 1: {
                insts[insts[pc + 3]] = param1 + param2;
                pc += 4;
                break;
              }
      case 2: {
                insts[insts[pc + 3]] = param1 * param2;
                pc += 4;
                break;
              }
      case 3: {
                insts[insts[pc + 1]] = input_val;
                pc += 2;
                break;
              }
      case 4: {
                //g_output = insts[insts[pc + 1]];
                g_output = param1;
                pc += 2;
                break;
               }
      case 5: {
                if(param1 != 0) {
                  pc = param2;
                } else {
                  pc+=3;
                }
                break;
               }
      case 6: {
                if(param1 == 0) {
                  pc = param2;
                } else {
                  pc+=3;
                }
                break;
               }
      case 7: {
                if(param1 < param2) {
                  insts[insts[pc + 3]] = 1;
                } else {
                  insts[insts[pc + 3]] = 0;
                }
                pc += 4;
                break;
               }
      case 8: {
                if(param1 == param2) {
                  insts[insts[pc + 3]] = 1;
                } else {
                  insts[insts[pc + 3]] = 0;
                }
                pc += 4;
                break;
               }
      default: {
                 printf("unknown opcode %d, exiting\n", opcode);
                 exit(EXIT_SUCCESS);
               }
    }
  }
}

void part1(int *insts, int insts_size) {
   int *initial_state = (int *)malloc(sizeof(int) * insts_size);
   memcpy(initial_state, insts, sizeof(int) * insts_size);

   run_intcode(initial_state, insts_size, 1);
   printf("PART1: %d\n", g_output);
   if(initial_state) {
     free(initial_state);
   }
}

void part2(int *insts, int insts_size) {
  int *initial_state = (int *) malloc(sizeof(int) * insts_size);
  memcpy(initial_state, insts, sizeof(int) * insts_size);

  run_intcode(initial_state, insts_size, 5);
  printf("PART2: %d\n", g_output);
  if(initial_state) {
    free(initial_state);
  }
}

void split_line(char *line, int **arr, int *arr_size) {
  int *array = (int *) malloc(sizeof(int) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int item_num = 0;

  char *tokenized = strtok(line, ",");
  while(NULL != tokenized) {
    array[item_num++] = atoi(tokenized);
    if(item_num == array_size) {
      array_size += INPUTLEN_GUESS;
      array = (int *) realloc(array, sizeof(int) * array_size);
    }
    tokenized = strtok(NULL, ",");
  }

  *arr = array;
  *arr_size = item_num;
}

int parse_file(char *file_name, int **arr, int *arr_size) {
  FILE *fp;
  size_t len;
  char *line = NULL;


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  while(-1 != getline(&line, &len, fp)) {
    split_line(line, arr, arr_size);
  }

  return SUCCESS;
}


int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    int *insts;
    int insts_size;
    if(SUCCESS == parse_file(argv[1], &insts, &insts_size)) {
      part1(insts, insts_size);
      part2(insts, insts_size);
      if(insts) {
        free(insts);
        insts = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
