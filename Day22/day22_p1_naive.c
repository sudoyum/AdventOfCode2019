#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_PRINT 0

#define FAILURE -1
#define SUCCESS 0
#define INPUTLEN_GUESS 50

#define INCREMENT 0xFFF1
#define NEW_STACK 0xFFF2
#define CUT_DECK 0xFFF3

#define DECK_SIZE 10007

typedef struct Shuffle {
  int type;
  int val;
} Shuffle_t;

int *temp_cards;

void print_shuffles(Shuffle_t *shuffles, int shuffle_size) {
  for(int i = 0; i < shuffle_size; i++) {
    if(shuffles[i].type == INCREMENT) {
      printf("increment %d\n", shuffles[i].val);
    } else if(shuffles[i].type == NEW_STACK) {
      printf("new stack\n");
    } else if(shuffles[i].type == CUT_DECK) {
      printf("cut %d\n", shuffles[i].val);
    }
  }
}

void print_deck(int *cards) {
  for(int i = 0; i < DECK_SIZE - 1; i++) {
    printf("%d ", cards[i]);
  }
  printf("%d\n", cards[DECK_SIZE - 1]);
}

void new_stack(int *cards) {
  for(int i = 0; i < DECK_SIZE/2; i++) {
    int temp = cards[DECK_SIZE - 1 - i];
    cards[DECK_SIZE - 1 - i] = cards[i];
    cards[i] = temp;
  }
}

void cut_deck(int *cards, int cut_num) {
  if(cut_num >= 0) {
    for(int i = 0; i < DECK_SIZE - cut_num; i++) {
      temp_cards[i] = cards[cut_num + i];
    }
    for(int i = 0; i < cut_num; i++) {
      temp_cards[DECK_SIZE - cut_num + i] = cards[i];
    }
  } else {
    for(int i = 0; i < abs(cut_num); i++) {
      temp_cards[i] = cards[DECK_SIZE + cut_num + i];
    }

    for(int i = 0; i < DECK_SIZE - abs(cut_num); i++) {
      temp_cards[abs(cut_num) + i] = cards[i];
    }
  }
  memcpy(cards, temp_cards, sizeof(int) * DECK_SIZE);
}

void increment(int *cards, int inc_num) {
  temp_cards[0] = cards[0];
  for(int i = 1; i < DECK_SIZE; i++) {
    temp_cards[(inc_num * i) % DECK_SIZE] = cards[i];
  }
  memcpy(cards, temp_cards, sizeof(int) * DECK_SIZE);
}

void part1(Shuffle_t *shuffles, int shuffles_size) {
  temp_cards = malloc(sizeof(int) * DECK_SIZE);
  int *cards = malloc(sizeof(int) * DECK_SIZE);

  for(int i = 0; i < DECK_SIZE; i++) {
    cards[i] = i;
  }

  for(int i = 0; i < shuffles_size; i++) {
    switch(shuffles[i].type) {
      case NEW_STACK: {
                        new_stack(cards);
                        break;
                      }

      case CUT_DECK: {
                        cut_deck(cards, shuffles[i].val);
                        break;
                      }
      case INCREMENT: {
                        increment(cards, shuffles[i].val);
                        break;
                      }
    }
  }

  int found_index = -1;
  for(int i = 0; i < DECK_SIZE; i++) {
    if(cards[i] == 2019) {
      found_index = i;
      break;
    }
  }

  printf("PART1: %d\n", found_index);
}

void part2(void) {
  printf("PART2: %d\n", 0);
}

int get_int_from_regex(regmatch_t match, char *line) {
  int entry_len = match.rm_eo - match.rm_so;
  char entry[entry_len];
  memset(entry, 0, entry_len);
  strncpy(entry, line + match.rm_so, match.rm_eo - match.rm_so);
  entry[match.rm_eo - match.rm_so] = '\0';
  return atoi(entry);
}

int parse_file(char *file_name, Shuffle_t **arr, int *arr_size) {
  FILE *fp;
  size_t len;
  char *line = NULL;

  int num_matches = 2;
  regmatch_t matches[num_matches];
  regex_t regex, regex2, regex3;
  int reg_comp = regcomp(&regex, "deal with increment ([0-9]+)", REG_EXTENDED);
  int reg_comp2 = regcomp(&regex2, "deal into new stack", REG_EXTENDED);
  int reg_comp3 = regcomp(&regex3, "cut ([-0-9]+)", REG_EXTENDED);


  if(NULL == (fp = fopen(file_name, "r"))) {
    printf("failed to open input file %s, exiting\n", file_name);
    return FAILURE;
  }

  Shuffle_t *array = (Shuffle_t *) malloc(sizeof(Shuffle_t) * INPUTLEN_GUESS);
  int array_size = INPUTLEN_GUESS;
  int line_num = 0;


  while(-1 != getline(&line, &len, fp)) {
    if(line_num == array_size) {
      array_size += INPUTLEN_GUESS;
      array = (Shuffle_t *) realloc(array, sizeof(Shuffle_t) * array_size);
    }

    Shuffle_t shuffle = {0, 0};
    if(0 == regexec(&regex, line, num_matches, matches, 0)) {
      shuffle.type = INCREMENT;
      shuffle.val = get_int_from_regex(matches[1], line);
    } else if(0 == regexec(&regex2, line, num_matches, matches, 0)) {
      shuffle.type = NEW_STACK;
    } else if(0 == regexec(&regex3, line, num_matches, matches, 0)) {
      shuffle.type = CUT_DECK;
      shuffle.val = get_int_from_regex(matches[1], line);
    } else {
      printf("Every line should match at least one regex, exiting\n");
      return FAILURE;
    }

    array[line_num++] = shuffle;
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

    Shuffle_t *shuffles;
    int shuffles_size;
    if(SUCCESS == parse_file(argv[1], &shuffles, &shuffles_size)) {
      part1(shuffles, shuffles_size);
      if(shuffles) {
        free(shuffles);
        shuffles = NULL;
      }
    }
  }

  return EXIT_SUCCESS;
}
