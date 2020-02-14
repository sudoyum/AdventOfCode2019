#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0

#define NUM_PHASES 100


void print_as_num(int *sig, int sig_size) {
  for(int i = 0; i < sig_size; i++) {
    printf("%d", sig[i]);
  }
}

void print_as_num_off(int *sig, int sig_size, int offset) {
  for(int i = offset; i < offset + sig_size; i++) {
    printf("%d", sig[i]);
  }
}

int get_start_index(int *sig) {
  int offset = 0;

  for(int i = 6; i >=0; i--) {
    int exp = 6 - i;
    offset += sig[i] * (int)pow(10.0, (double)exp);
  }

  return offset;
}

void part1(int *sig_init, int sig_size) {
  int *sig = (int *) malloc(sizeof(int) * sig_size);
  int *temp = (int *) malloc(sizeof(int) * sig_size);

  memcpy(sig, sig_init, sizeof(int) * sig_size);

  #if DEBUG_PRINT
  for(int i = 0; i < sig_size; i++) {
    printf("sig[%d]=%d\n", i, sig[i]);
  }
  #endif

  int pattern[4] = {0, 1, 0, -1};

  int phase = 0;

  while(1) {
    #if DEBUG_PRINT
    printf("phase %d: ", phase);
    print_as_num(sig, sig_size);
    printf("\n");
    #endif

    if(phase >= NUM_PHASES) {
      break;
    }

    for(int i = 0; i < sig_size; i++) {
      int sum = 0;
      for(int j = i; j < sig_size; j++) {
        int index = (i + 2 + j)/(i + 1) - 1;
        sum += sig[j] * pattern[index % 4];
      }
      temp[i] = abs(sum % 10);
    }

    memcpy(sig, temp, sizeof(int) * sig_size);
    phase++;
  }

  if(temp) {
    free(temp);
    temp = NULL;
  }

  printf("PART1: ");
  print_as_num(sig, 8);
  printf("\n");

  if(sig) {
    free(sig);
    sig = NULL;
  }
}

void part2(int *sig_init, int sig_size) {
  int *sig = (int *) malloc(sizeof(int) * sig_size * 10000);

  for(int i = 0; i < 10000; i++) {
     memcpy(sig + i * sig_size, sig_init, sizeof(int) * sig_size);
  }

  int offset = get_start_index(sig_init);

  #if DEBUG_PRINT
  for(int i = 0; i < sig_size; i++) {
    printf("sig[%d]=%d\n", i, sig[i]);
  }
  #endif

  int phase = 0;

  while(1) {
    #if DEBUG_PRINT
    printf("phase %d: ", phase);
    print_as_num(sig, sig_size);
    printf("\n");
    #endif

    if(phase >= NUM_PHASES) {
      break;
    }

    for(int i = sig_size * 10000 - 2; i >= 0; i--) {
      sig[i] = (sig[i] + sig[i + 1]) % 10;
    }
    phase++;
  }

  printf("PART2: ");
  print_as_num_off(sig, 8, offset);
  printf("\n");

  if(sig) {
    free(sig);
    sig = NULL;
  }
}

int parse_file(char *file_name, int **arr, int *arr_size) {
  FILE *fp;
  size_t len;
  char *line = NULL;


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  int *array;
  int array_size;
  int line_num = 0;


  int n_bytes;
  while(-1 != (n_bytes = getline(&line, &len, fp))) {
    if(line_num > 0) {
       printf("input should only be one line, exiting\n");
       return FAILURE;
    }
    array_size = n_bytes - 1;
    array = (int *) malloc(sizeof(int) * array_size);

    for(int i = 0; i < array_size; i++) {
      array[i] = (int)(line[i] - 48);
    }
  }

  if(line) {
    free(line);
    line = NULL;
  }

  *arr = array;
  *arr_size = array_size;

  return SUCCESS;
}

int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    int *sig;
    int sig_size;
    if(SUCCESS == parse_file(argv[1], &sig, &sig_size)) {
      part1(sig , sig_size);
      part2(sig , sig_size);
      if(sig) {
        free(sig);
        sig = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
