#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

void part1(int **plane, int x_max, int y_max) {
  for(int i = 0; i < y_max; i++) {
    for(int j = 0; j < x_max; j++) {
      if(plane[i][j]) {
        printf("#");
      } else {
        printf(".");
      }
    }
    printf("\n");
  }
}

int parse_file(char *file_name, int ***arr, int *x_max, int *y_max) {
  FILE *fp;
  size_t len;
  char *line = NULL;


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  int **plane = (int **) malloc(sizeof(int *) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int line_num = 0;


  while(-1 != getline(&line, &len, fp)) {
    int *row = (int *) malloc(sizeof(int) * INPUTLEN_GUESS);
    *x_max = strlen(line) - 1;
    for(int i = 0; i < strlen(line) - 1; i++) {
       row[i] = '#' == line[i] ? 1: 0;
    }
    plane[line_num++] = row;
  }

  if(line) {
    free(line);
    line = NULL;
  }

  *arr = plane;
  *y_max = line_num;

  return SUCCESS;
}

int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    int **plane = NULL;
    int x_max = 0, y_max = 0;
    if(SUCCESS == parse_file(argv[1], &plane, &x_max, &y_max)) {
      part1(plane, x_max, y_max);
      if(plane) {
        free(plane);
        plane = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
