#!/usr/bin/env python

def main():
    j = 1

    while True:
        ai = 1

        while ai < j:
            print("%d" % ai)

            ai += 1

        j += 1

if __name__ == "__main__":
    try:
        main()
    except:
        pass
