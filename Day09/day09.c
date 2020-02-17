#include <stdio.h>
#include <stdlib.h>

#include "intcode.h"

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50


void part1(long long *insts, int insts_size) {
  IntCode_t *ic = init_intcode('A', insts, insts_size);
  ic->input = 1;
  run_intcode(ic);
  printf("PART1: %lld\n", ic->output);
  free_intcode(ic);
}

void part2(long long *insts, int insts_size) {
  IntCode_t *ic = init_intcode('A', insts, insts_size);
  ic->input = 2;
  run_intcode(ic);
  printf("PART2: %d\n", ic->output);
  free_intcode(ic);
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
