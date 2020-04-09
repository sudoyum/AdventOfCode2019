#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

typedef struct Point {
  int x;
  int y;
} Point_t;

void print_plane(int **plane, int x_max, int y_max) {
  for(int j = 0; j < y_max; j++) {
    for(int i = 0; i < x_max; i++) {
      if(plane[j][i]) {
        printf("#");
      } else {
        printf(".");
      }
    }
    printf("\n");
  }
}

Point_t *get_asteroids(int **plane, int x_max, int y_max, int num_asteroids) {
  Point_t *asteroids = malloc(sizeof(Point_t) * num_asteroids);
  int index = 0;

  for(int j = 0; j < y_max; j++) {
    for(int i = 0; i < x_max; i++) {
      if(plane[j][i]) {
        asteroids[index].x = i;
        asteroids[index++].y = j;
      }
    }
  }
  return asteroids;
}

int gcd(int n1, int n2) {
  n1 = ( n1 > 0) ? n1 : -n1;
  n2 = ( n2 > 0) ? n2 : -n2;

  while(n1 != n2)
  {
    if(n1 > n2) {
      n1 -= n2;
    } else {
      n2 -= n1;
    }
  }
  return n1;
}

int num_can_see(Point_t *asteroids, int num_asteroids, int **plane, int focus_index) {
  int num_seen = 0;
  for(int i = 0; i < num_asteroids; i++) {
    if(i != focus_index) {
      int dx = asteroids[i].x - asteroids[focus_index].x;
      int dy = asteroids[i].y - asteroids[focus_index].y;
      if(dx == 0) {
        int y_start = MIN(asteroids[i].y, asteroids[focus_index].y);
        int y_end = MAX(asteroids[i].y, asteroids[focus_index].y);
        bool blocked = false;
        for(int y = y_start + 1; y < y_end; y++) {
          if(plane[y][asteroids[i].x]) {
            blocked = true;
            break;
          }
        }
        if(!blocked) {
          num_seen++;
#if DEBUG_PRINT
          printf("dx==0, seen=(%d,%d)\n", asteroids[i].x, asteroids[i].y);
#endif
        }
      } else if(dy == 0) {
        int x_start = MIN(asteroids[i].x, asteroids[focus_index].x);
        int x_end = MAX(asteroids[i].x, asteroids[focus_index].x);
        bool blocked = false;
        for(int x = x_start + 1; x < x_end; x++) {
          if(plane[asteroids[i].y][x]) {
            blocked = true;
            break;
          }
        }
        if(!blocked) {
          num_seen++;
#if DEBUG_PRINT
          printf("dy==0, seen=(%d,%d)\n", asteroids[i].x, asteroids[i].y);
#endif
        }
      } else {
        bool blocked = false;
        int divisor = gcd(dy, dx);
        dy = dy/divisor;
        dx = dx/divisor;
        Point_t start_point;
        start_point.y = asteroids[focus_index].y;
        start_point.x = asteroids[focus_index].x;
        while(1) {
          start_point.y += dy;
          start_point.x += dx;
          if(asteroids[i].x == start_point.x &&
             asteroids[i].y == start_point.y) {
            break;
          } else {
             if(plane[start_point.y][start_point.x]) {
               blocked = true;
               break;
             }
          }
        }
        if(!blocked) {
          num_seen++;
#if DEBUG_PRINT
          printf("dy!=0 dx!=0, seen=(%d,%d)\n", asteroids[i].x, asteroids[i].y);
#endif
        }
      }
    }
  }
  return num_seen;
}

void part1(int **plane, int x_max, int y_max, int num_asteroids) {
  printf("num_asteroids=%d\n", num_asteroids);
  print_plane(plane, x_max, y_max);
  Point_t *asteroids = get_asteroids(plane, x_max, y_max, num_asteroids);
  Point_t best_point = {0, 0};
  int max_seen = -1;

  for(int i = 0; i < num_asteroids; i++) {
    int num_seen = num_can_see(asteroids, num_asteroids, plane, i);
#if DEBUG_PRINT
    printf("DEBUG: (%d, %d) %d detected\n", asteroids[i].x, asteroids[i].y, num_seen);
#endif
    if(num_seen > max_seen) {
      max_seen = num_seen;
      best_point = asteroids[i];
    }
  }

  printf("FINAL: (%d, %d) %d detected\n", best_point.x, best_point.y, max_seen);
  printf("PART1: %d\n", max_seen);
}

int parse_file(char *file_name, int ***arr, int *x_max, int *y_max, int *num_asteroids) {
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
  int num_asters = 0;


  while(-1 != getline(&line, &len, fp)) {
    int *row = (int *) malloc(sizeof(int) * INPUTLEN_GUESS);
    *x_max = strlen(line) - 1;
    for(int i = 0; i < strlen(line) - 1; i++) {
       row[i] = '#' == line[i] ? 1: 0;
       if(row[i]) {
         num_asters++;
       }
    }
    plane[line_num++] = row;
  }

  if(line) {
    free(line);
    line = NULL;
  }

  *arr = plane;
  *y_max = line_num;
  *num_asteroids = num_asters;

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
    int num_asteroids = 0;
    if(SUCCESS == parse_file(argv[1], &plane, &x_max, &y_max, &num_asteroids)) {
      part1(plane, x_max, y_max, num_asteroids);
      if(plane) {
        free(plane);
        plane = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
