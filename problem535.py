#!/usr/bin/env python

from math import sqrt

def main():
    k = 1

    while True:
        j = 0

        while j < k:
            ai = 1

            while ai < j:
                print("%d" % (ai))
                ai += 1

            j  = (j + 1) % k

        k += 1

if __name__ == "__main__":
    main()
