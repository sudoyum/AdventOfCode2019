#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

void run_intcode(int *insts, int insts_size) {
  int pc = 0;
  bool halt_flag = false;

  while(1) {
    if(pc >= insts_size || halt_flag) {
      break;
    }

    int opcode = insts[pc];
    switch(opcode) {
      case 1: {
                insts[insts[pc + 3]] = insts[insts[pc + 1]] + insts[insts[pc + 2]];
                pc += 4;
                break;
              }
      case 2: {
                insts[insts[pc + 3]] = insts[insts[pc + 1]] * insts[insts[pc + 2]];
                pc += 4;
                break;
              }
      case 99: {
                 halt_flag = true;
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
  int *initial_state = (int *) malloc(sizeof(int) * insts_size);
  memcpy(initial_state, insts, sizeof(int) * insts_size);
  initial_state[1] = 12;
  initial_state[2] = 2;

  run_intcode(initial_state, insts_size);
  printf("PART1: %d\n", initial_state[0]);
  if(initial_state) {
    free(initial_state);
  }
}

void part2(int *insts, int insts_size) {
  int *initial_state = (int *) malloc(sizeof(int) * insts_size);

  int noun = 0, verb = 0;
  while(1) {
    memcpy(initial_state, insts, sizeof(int) * insts_size);
    initial_state[1] = noun;
    initial_state[2] = verb;
    run_intcode(initial_state, insts_size);
    if(initial_state[0] == 19690720) {
      break;
    } else {
      noun++;
      noun %= 100;
      if(noun == 0) {
        verb++;
      }
    }
  }

  if(initial_state) {
    free(initial_state);
  }

  printf("PART2: %d\n", noun * 100 + verb);
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
