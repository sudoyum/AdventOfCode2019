#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "intcode.h"

#define FAILURE -1
#define SUCCESS 0

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
  if(start == end)
  {
    memcpy(g_phases[g_phase_index++], arr, sizeof(int) * 5);
    return;
  }

  for(int i = start;i <= end; i++)
  {
    swap((arr + i), (arr + start));
    permutation(arr, start + 1, end);
    swap((arr + i), (arr + start));
  }
}

void part1(long long *insts, int insts_size) {
  long long *initial_state = (long long *)malloc(sizeof(long long) * insts_size);
  memcpy(initial_state, insts, sizeof(long long) * insts_size);

  IntCode_t *ampA = init_intcode('A', insts, insts_size);
  IntCode_t *ampB = init_intcode('B', insts, insts_size);
  IntCode_t *ampC = init_intcode('C', insts, insts_size);
  IntCode_t *ampD = init_intcode('D', insts, insts_size);
  IntCode_t *ampE = init_intcode('E', insts, insts_size);

  ampA->do_phases = true;
  ampB->do_phases = true;
  ampC->do_phases = true;
  ampD->do_phases = true;
  ampE->do_phases = true;

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

void part2(long long *insts, int insts_size) {
  long long *initial_state = (long long *)malloc(sizeof(long long) * insts_size);
  memcpy(initial_state, insts, sizeof(long long) * insts_size);

  IntCode_t *ampA = init_intcode('A', insts, insts_size);
  IntCode_t *ampB = init_intcode('B', insts, insts_size);
  IntCode_t *ampC = init_intcode('C', insts, insts_size);
  IntCode_t *ampD = init_intcode('D', insts, insts_size);
  IntCode_t *ampE = init_intcode('E', insts, insts_size);

  ampA->do_phases = true;
  ampB->do_phases = true;
  ampC->do_phases = true;
  ampD->do_phases = true;
  ampE->do_phases = true;

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

int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    long long *insts;
    int insts_size;
    if(SUCCESS == parse_intcode(argv[1], &insts, &insts_size)) {
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
