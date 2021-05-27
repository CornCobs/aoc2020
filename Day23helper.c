#include "Day23helper.h"
#include <stdio.h>

#define MAX 1000000

int initial[] = {3,2,7,4,6,5,1,8,9};
int cups[1000001];

long solve(void) {
    int i = 0;
    for (; i < 8; i++) {
        cups[initial[i]] = initial[i+1];
    }
    cups[initial[8]] = 10;
    for (i = 10; i < 1000000; i++) {
        cups[i] = i+1;
    }
    cups[1000000] = 3;

    int current = 3;
    int a, b, c, next, destination;
    for (i = 0; i < 10000000; i++) {
        a = cups[current];
        b = cups[a];
        c = cups[b];
        next = cups[c];
        destination = ((current+MAX - 2) % MAX) + 1;
        while (destination == a || destination == b || destination == c) {
            destination = ((destination+MAX - 2) % MAX) + 1;
        }
        // printf("Current: %d\nNext: %d\nDestination: %d\n", current, next, destination);
        cups[current] = next;
        cups[c] = cups[destination];
        cups[destination] = a;
        current = next;
    }
    printf("%d, %d\n", cups[1], cups[cups[1]]);
    long result = cups[1];
    result *= cups[result];
    return result;
}