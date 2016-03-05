package us.yellosoft.projecteuler;

import it.unimi.dsi.fastutil.longs.LongBigArrays;

public final class Problem535 {
  public static long lsqrt(final long x) {
    return (long) Math.floor(Math.sqrt(x));
  }

  static class S {
    private long n;
    private long maxJ;
    private long[][] s;
    private long i;
    private long j;
    private long r;
    private long h;
    private long y;
    private long t;
    boolean first;

    public S(long n) {
      this.n = n;
      this.maxJ = 1000L + (long) Math.sqrt(n);
      this.s = LongBigArrays.newBigArray(maxJ);
      this.i = 2L;
      this.j = 0L;
      this.r = 0L;
      this.h = 0L;
      this.y = 0L;
      this.t = 0L;
      this.first = true;
    }

    private void append(long y) {
      if (h < maxJ) {
        LongBigArrays.set(s, h, y);
      }
    }

    private void addModOneBillion(long y) {
      t = (t + y) % 1000000000;
    }

    private void crank() {
      if (first) {
        first = false;

        y = 1L;

        append(y);
      } else if (r == 0) {
        y = LongBigArrays.get(s, j);

        append(y);

        long y2 = LongBigArrays.get(s, j + 1);
        j = j + 1;
        r = lsqrt(y2);
      } else {
        y = i;

        append(y);

        i = i + 1;
        r = r - 1;
      }

      addModOneBillion(y);
    }

    public void generate() {
      while (h < n) {
        crank();
        h++;

        if (h % 1000000000 == 0) {
          System.out.println("H: " + h);
        }
      }
    }

    public long value() {
      generate();

      return y;
    }

    public long sumModOneBillion() {
      generate();

      return t;
    }
  }

  public static void test() {
    assert new S(20L).value() == 5L;

    assert new S(1L).sumModOneBillion() == 1L;
    assert new S(20L).sumModOneBillion() == 86L;
    assert new S(1000L).sumModOneBillion() == 364089L;
    assert new S(1000000000L).sumModOneBillion() == 498676527978348241L;

    System.out.println("Passed all assertions");
  }

  public static void main(final String[] args) {
    if (args.length > 0 && args[0].equals("-t")) {
      test();
    } else {
      System.out.println("T(10^18)_(10^9):");
      System.out.println(new S(1000000000000000000L).sumModOneBillion());
    }
  }
}
