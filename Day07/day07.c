#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

#define HALTED 0xFF231
#define START  0xFF232
#define DONE   0xFF233
#define WAIT_INPUT 0xFF234
#define WAIT_OUTPUT 0xFF235

typedef struct IntCode {
  char id;
  int pc;
  int phase;
  int state;

  int *insts;
  int insts_size;
  int input;
  int input_count;
  int output;
} IntCode_t;


void run_intcode(IntCode_t *ic) {
#if DEBUG_PRINT
  for(int i = 0; i < ic->insts_size - 1; i++) {
    printf("%d,", ic->insts[i]);
  }
  printf("%d\n", ic->insts[ic->insts_size - 1]);
#endif

  while(1) {
    if(ic->pc >= ic->insts_size) {
      ic->state = DONE;
      break;
    }

    int parameter = ic->insts[ic->pc];
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
      ic->state = HALTED;
      break;
    }

    int param1, param2;
    if(1 == mode1) {
      param1 = ic->insts[ic->pc + 1];
    } else {
      param1 = ic->insts[ic->insts[ic->pc + 1]];
    }

    if(1 == mode2) {
      param2 = ic->insts[ic->pc + 2];
    } else {
      param2 = ic->insts[ic->insts[ic->pc + 2]];
    }

    switch(opcode) {
      case 1: {
                ic->insts[ic->insts[ic->pc + 3]] = param1 + param2;
                ic->pc += 4;
                break;
              }
      case 2: {
                ic->insts[ic->insts[ic->pc + 3]] = param1 * param2;
                ic->pc += 4;
                break;
              }
      case 3: {
                ic->insts[ic->insts[ic->pc + 1]] = (ic->input_count == 0) ? ic->phase: ic->input;
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
                  ic->insts[ic->insts[ic->pc + 3]] = 1;
                } else {
                  ic->insts[ic->insts[ic->pc + 3]] = 0;
                }
                ic->pc += 4;
                break;
               }
      case 8: {
                if(param1 == param2) {
                  ic->insts[ic->insts[ic->pc + 3]] = 1;
                } else {
                  ic->insts[ic->insts[ic->pc + 3]] = 0;
                }
                ic->pc += 4;
                break;
               }
      default: {
                 printf("unknown opcode %d, exiting\n", opcode);
                 exit(EXIT_SUCCESS);
               }
    }
  }
}

int g_phases[120][5];
int g_phase_index = 0;

void swap(int *a, int *b)
{
    int temp;
    temp = *a;
    *a = *b;
    *b = temp;
}

//permutation function
void permutation(int *arr, int start, int end)
{
    if(start==end)
    {
        memcpy(g_phases[g_phase_index++], arr, sizeof(int) * 5);
        return;
    }
    int i;
    for(i=start;i<=end;i++)
    {
        swap((arr+i), (arr+start));
        permutation(arr, start+1, end);
        swap((arr+i), (arr+start));
    }
}

IntCode_t *init_intcode(char id, int *insts, int insts_size) {
  IntCode_t *ic = (IntCode_t *)malloc(sizeof(IntCode_t));
  ic->insts_size = insts_size;
  ic->insts = (int *)malloc(sizeof(int) * insts_size);
  memcpy(ic->insts, insts, sizeof(int) * insts_size);
  ic->state = START;
  ic->id = id;
  ic->pc = 0;
  ic->input_count = 0;
  ic->input = 0;
  ic->output = 0;
  return ic;
}

void reinit_intcode(IntCode_t *ic, int *insts) {
  memcpy(ic->insts, insts, sizeof(int) * ic->insts_size);
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

void part1(int *insts, int insts_size) {
   int *initial_state = (int *)malloc(sizeof(int) * insts_size);
   memcpy(initial_state, insts, sizeof(int) * insts_size);

   IntCode_t *ampA = init_intcode('A', insts, insts_size);
   IntCode_t *ampB = init_intcode('B', insts, insts_size);
   IntCode_t *ampC = init_intcode('C', insts, insts_size);
   IntCode_t *ampD = init_intcode('D', insts, insts_size);
   IntCode_t *ampE = init_intcode('E', insts, insts_size);

   int max = 0;

   int arr[5] = {0, 1, 2, 3, 4};
   g_phase_index = 0;
   permutation(arr, 0, 4);

   for(int i = 0; i < 120; i++) {
     reinit_intcode(ampA, initial_state);
     reinit_intcode(ampB, initial_state);
     reinit_intcode(ampC, initial_state);
     reinit_intcode(ampD, initial_state);
     reinit_intcode(ampE, initial_state);


     ampA->phase = g_phases[i][0];
     run_intcode(ampA);
     ampB->phase = g_phases[i][1];
     ampB->input = ampA->output;
     run_intcode(ampB);
     ampC->phase = g_phases[i][2];
     ampC->input = ampB->output;
     run_intcode(ampC);
     ampD->phase = g_phases[i][3];
     ampD->input = ampC->output;
     run_intcode(ampD);
     ampE->phase = g_phases[i][4];
     ampE->input = ampD->output;
     run_intcode(ampE);

     if(ampE->output > max) {
       max = ampE->output;
     }
   }

   printf("PART1: %d\n", max);
   if(initial_state) {
     free(initial_state);
     initial_state = NULL;
     free_intcode(ampA);
     free_intcode(ampB);
     free_intcode(ampC);
     free_intcode(ampD);
     free_intcode(ampE);
   }
}

void part2(int *insts, int insts_size) {
   int *initial_state = (int *)malloc(sizeof(int) * insts_size);
   memcpy(initial_state, insts, sizeof(int) * insts_size);

   IntCode_t *ampA = init_intcode('A', insts, insts_size);
   IntCode_t *ampB = init_intcode('B', insts, insts_size);
   IntCode_t *ampC = init_intcode('C', insts, insts_size);
   IntCode_t *ampD = init_intcode('D', insts, insts_size);
   IntCode_t *ampE = init_intcode('E', insts, insts_size);

   int max = 0;

   int arr[5] = {5, 6, 7, 8, 9};
   g_phase_index = 0;
   permutation(arr, 0, 4);


   for(int i = 0; i < 120; i++) {
     reinit_intcode(ampA, initial_state);
     reinit_intcode(ampB, initial_state);
     reinit_intcode(ampC, initial_state);
     reinit_intcode(ampD, initial_state);
     reinit_intcode(ampE, initial_state);
     ampA->phase = g_phases[i][0];
     ampB->phase = g_phases[i][1];
     ampC->phase = g_phases[i][2];
     ampD->phase = g_phases[i][3];
     ampE->phase = g_phases[i][4];

     while(1) {
       ampA->input = ampE->output;
       run_intcode(ampA);
       ampB->input = ampA->output;
       run_intcode(ampB);
       ampC->input = ampB->output;
       run_intcode(ampC);
       ampD->input = ampC->output;
       run_intcode(ampD);
       ampE->input = ampD->output;
       run_intcode(ampE);
       if(ampE->state == HALTED) {
         break;
       }
     }

     if(ampE->output > max) {
       max = ampE->output;
     }
   }

   printf("PART2: %d\n", max);
   if(initial_state) {
     free(initial_state);
     initial_state = NULL;
     free_intcode(ampA);
     free_intcode(ampB);
     free_intcode(ampC);
     free_intcode(ampD);
     free_intcode(ampE);
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
