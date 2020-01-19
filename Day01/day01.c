#include <stdio.h>
#include <stdlib.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50


int get_fuel(int module) {
  int fuel = module/3 - 2;
  if(fuel <= 0) {
    return 0;
  } else {
    return fuel + get_fuel(fuel);
  }
}

void part1(int *modules, int modules_size) {
  int sum = 0;
  for(int i = 0; i < modules_size; i++) {
    sum += (modules[i]/3 - 2);
  }
  printf("PART1: %d\n", sum);
}

void part2(int *modules, int modules_size) {
  int sum = 0;
  for(int i = 0; i < modules_size; i++) {
    sum += get_fuel(modules[i]);
  }
  printf("PART2: %d\n", sum);
}

int parse_file(char *file_name, int **arr, int *arr_size) {
  FILE *fp;
  size_t len;
  char *line = NULL;


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  int *array = (int *) malloc(sizeof(int) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int line_num = 0;


  while(-1 != getline(&line, &len, fp)) {
    if(line_num == array_size) {
      array_size += INPUTLEN_GUESS;
      array = (int *) realloc(array, sizeof(int) * array_size);
    }
    array[line_num++] = atoi(line);
  }

  *arr = array;
  *arr_size = line_num;

  return SUCCESS;
}


int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    int *modules;
    int modules_size;
    if(SUCCESS == parse_file(argv[1], &modules, &modules_size)) {
      part1(modules, modules_size);
      part2(modules, modules_size);
      if(modules) {
        free(modules);
        modules = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
