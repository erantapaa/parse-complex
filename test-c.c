#include <stdio.h>
#include <math.h>

const double bounds = 2.0;
const int imgSize = 400;

int count[imgSize*imgSize] = { 0 };

int main() {
  char line[1024];
  double rpart, ipart;

  while (1) {
    if (fgets(line, 1024, stdin) == NULL) break;
    sscanf("%lf+%lf*I", line, &rpart, &ipart);
    int row = (ipart / bounds) * 2 * imgSize + 200;
    int col = (rpart / bounds) * 2 * imgSize + 200;
    int x = row*imgSize + col;
    count[x]++;
  }
  int total = 0;
  for (int i = 0; i < imgSize*imgSize; i++) {
    total += count[i];
  }
  printf("total = %d\n", total);
}
