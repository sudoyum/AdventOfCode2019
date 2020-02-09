#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#include <regex.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

#define NUM_STEPS 1000

struct Moon {
  int id;
  int x;
  int y;
  int z;
  int vx;
  int vy;
  int vz;
  int ix;
  int iy;
  int iz;
};

typedef struct Moon Moon_t;

void print_moon(Moon_t *moon) {
  printf("pos=<x=%2d, y=%3d, z=%2d>, ", moon->x, moon->y, moon->z);
  printf("vel=<vx=%2d, vy=%2d, vz=%2d>\n", moon->vx, moon->vy, moon->vz);
}

void print_moons(Moon_t *moons, int moons_size) {
  for(int i = 0; i < moons_size; i++) {
    print_moon(&moons[i]);
  }
}

unsigned long gcd(unsigned long a, unsigned long b) {
  while(a != b)
  {
    if(a > b) {
      a -= b;
    } else {
      b -= a;
    }
  }
}

unsigned long lcm(unsigned long a, unsigned long b) {
  return a*b/gcd(a, b);
}

void part1(Moon_t *original_moons, int moons_size) {
  Moon_t *moons = (Moon_t *) malloc(sizeof(Moon_t) * moons_size);
  memcpy(moons, original_moons, sizeof(Moon_t) * moons_size);

  #if DEBUG_PRINT
  printf("step=0:\n", 0);
  print_moons(moons, moons_size);
  #endif

  for(int i = 0; i < NUM_STEPS; i++) {
    //gravity
    for(int j = 0; j < moons_size - 1; j++) {
      for(int k = j + 1; k < moons_size; k++) {
        //printf("(%d,%d)\n", moons[j].id, moons[k].id);
        if(moons[j].x > moons[k].x) {
          moons[j].vx -= 1;
          moons[k].vx += 1;
        } else if(moons[j].x < moons[k].x) {
          moons[j].vx += 1;
          moons[k].vx -= 1;
        }
        if(moons[j].y > moons[k].y) {
          moons[j].vy -= 1;
          moons[k].vy += 1;
        } else if(moons[j].y < moons[k].y) {
          moons[j].vy += 1;
          moons[k].vy -= 1;
        }
        if(moons[j].z > moons[k].z) {
          moons[j].vz -= 1;
          moons[k].vz += 1;
        } else if(moons[j].z < moons[k].z) {
          moons[j].vz += 1;
          moons[k].vz -= 1;
        }
      }
    }
    //position
    for(int p = 0; p < moons_size; p++) {
      moons[p].x += moons[p].vx;
      moons[p].y += moons[p].vy;
      moons[p].z += moons[p].vz;
    }
    #if DEBUG_PRINT
    printf("step=%d:\n", i);
    print_moons(moons, moons_size);
    #endif
  }

  //energy
  int total_energy = 0;
  for(int i = 0; i < moons_size; i++) {
    int potential = 0, kinetic = 0;
    potential += abs(moons[i].x);
    potential += abs(moons[i].y);
    potential += abs(moons[i].z);
    kinetic += abs(moons[i].vx);
    kinetic += abs(moons[i].vy);
    kinetic += abs(moons[i].vz);
    total_energy += potential * kinetic;

    #if DEBUG_PRINT
    printf("%d * %d\n", potential, kinetic);
    #endif
  }

  if(moons) {
    free(moons);
    moons = NULL;
  }

  printf("PART1: %d\n", total_energy);
}

void part2(Moon_t *original_moons, int moons_size) {
  Moon_t *moons = (Moon_t *) malloc(sizeof(Moon_t) * moons_size);
  memcpy(moons, original_moons, sizeof(Moon_t) * moons_size);
  #if DEBUG_PRINT
  printf("step=0:\n", 0);
  print_moons(moons, moons_size);
  #endif

  unsigned long steps = 0;
  bool done = false;
  bool x_found = false, y_found = false, z_found = false;
  unsigned long x_steps = 0, y_steps = 0, z_steps = 0;

  while(!done) {
    int x_same, y_same, z_same;
    for(int j = 0; j < moons_size - 1; j++) {
      x_same = y_same = z_same = 0;
      for(int k = j + 1; k < moons_size; k++) {
        if(!x_found) {
          if(moons[j].x > moons[k].x) {
            moons[j].vx -= 1;
            moons[k].vx += 1;
          } else if(moons[j].x < moons[k].x) {
            moons[j].vx += 1;
            moons[k].vx -= 1;
          }
        }
        if(!y_found) {
          if(moons[j].y > moons[k].y) {
            moons[j].vy -= 1;
            moons[k].vy += 1;
          } else if(moons[j].y < moons[k].y) {
            moons[j].vy += 1;
            moons[k].vy -= 1;
          }
        }
        if(!z_found) {
          if(moons[j].z > moons[k].z) {
            moons[j].vz -= 1;
            moons[k].vz += 1;
          } else if(moons[j].z < moons[k].z) {
            moons[j].vz += 1;
            moons[k].vz -= 1;
          }
        }
      }
    }

    for(int p = 0; p < moons_size; p++) {
      if(!x_found) {
        moons[p].x += moons[p].vx;
        if(moons[p].ix == moons[p].x && 0 == moons[p].vx) {
          x_same++;
        }
      }
      if(!y_found) {
        moons[p].y += moons[p].vy;
        if(moons[p].iy == moons[p].y && 0 == moons[p].vy) {
          y_same++;
        }
      }
      if(!z_found) {
        moons[p].z += moons[p].vz;
        if(moons[p].iz == moons[p].z && 0 == moons[p].vz) {
          z_same++;
        }
      }
    }

    steps++;

    if(!x_found && x_same == moons_size) {
      x_found = true;
      x_steps = steps;
    }

    if(!y_found && y_same == moons_size) {
      y_found = true;
      y_steps = steps;
    }

    if(!z_found && z_same == moons_size) {
      z_found = true;
      z_steps = steps;
    }

    if(x_found && y_found && z_found) {
      done = true;
    }

  }

  unsigned long max = lcm(x_steps, lcm(y_steps, z_steps));

  if(moons) {
    free(moons);
    moons = NULL;
  }

  printf("PART2: %lu\n", max);
}

int parse_file(char *file_name, Moon_t **arr, int *arr_size) {
  int id = 0;
  FILE *fp;
  size_t len;
  char *line = NULL;
  int rc = SUCCESS;

  int num_matches = 4;
  regex_t regex;
  regmatch_t matches[num_matches];

  int reg_comp = regcomp(&regex, "<x=([-0-9]+), y=([-0-9]+), z=([-0-9]+)", REG_EXTENDED);

  if(reg_comp) {
    printf("regex compilation failure\n");
    return FAILURE;
  }

  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  Moon_t *array = (Moon_t *) malloc(sizeof(Moon_t) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int line_num = 0;


  int coordinates[3] = {0};
  while(-1 != getline(&line, &len, fp)) {
    if(line_num == array_size) {
      array_size += INPUTLEN_GUESS;
      array = (Moon_t *) realloc(array, sizeof(Moon_t) * array_size);
    }

    if(0 == regexec(&regex, line, num_matches, matches, 0)) {
      for(int i = 1; i < num_matches; i++) {
        int entry_len = matches[i].rm_eo - matches[i].rm_so;
        char entry[entry_len];
        memset(entry, 0, entry_len);
        strncpy(entry, line + matches[i].rm_so, matches[i].rm_eo - matches[i].rm_so);
        entry[matches[i].rm_eo - matches[i].rm_so] = '\0';
        coordinates[i-1] = atoi(entry);
      }
      array[line_num].ix = array[line_num].x = coordinates[0];
      array[line_num].iy = array[line_num].y = coordinates[1];
      array[line_num].iz = array[line_num].z = coordinates[2];
      array[line_num].vx = 0;
      array[line_num].vy = 0;
      array[line_num].vz = 0;
      array[line_num].id = id++;
    } else {
      printf("regexec failure at line %d: %s", line_num, line);
      rc = FAILURE;
      goto cleanup;
    }
    line_num++;
  }

  *arr = array;
  *arr_size = line_num;

cleanup:
  if(reg_comp == 0) {
    regfree(&regex);
  }

  if(rc == FAILURE && array) {
    free(array);
    array = NULL;
  }
  return rc;
}


int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("usage: ./dayXX input.txt\n");
  } else {
    if(DEBUG_PRINT == 1) {
      printf("filename_arg = %s\n", argv[1]);
    }
    Moon_t *moons;
    int moons_size;
    if(SUCCESS == parse_file(argv[1], &moons, &moons_size)) {
      part1(moons, moons_size);
      part2(moons, moons_size);
      if(moons) {
        free(moons);
        moons = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
