#include <stdio.h>
#include <linux/input.h>

int main(int argc, char** argv) {
  /* EVIOCGID -2146941694 */
  printf("EVIOCGID %d\n", EVIOCGID);
  printf("EVIOCGID %u\n", EVIOCGID);
  printf("EVIOCGVERSION %u\n", EVIOCGVERSION);
  /* EVIOCGRAB 1074021776 */
  printf("EVIOCGRAB %d\n", EVIOCGRAB);
  /* sizeof(struct input_event) 24 */
  printf("sizeof(struct input_event) %d\n", sizeof(struct input_event));
  return 0;
}
