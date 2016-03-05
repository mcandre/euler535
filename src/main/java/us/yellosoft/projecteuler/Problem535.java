package us.yellosoft.projecteuler;

import it.unimi.dsi.fastutil.longs.LongBigArrays;

public final class Problem535 {
  public static long lsqrt(final long x) {
    return (long) Math.floor(Math.sqrt(x));
  }

  static class S {
    private long n;
    private long[][] s;
    private long i;
    private long j;
    private long r;
    private long h;
    private long t;
    boolean first;

    public S(long n) {
      this.n = n;
      this.s = LongBigArrays.newBigArray(n);
      this.i = 2L;
      this.j = 0L;
      this.r = 0L;
      this.h = 0L;
      this.t = 0L;
      this.first = true;
    }

    private void append(long y) {
      LongBigArrays.set(s, h, y);
    }

    private void add(long y) {
      t = t + y;
    }

    private void crank() {
      long y;

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

      add(y);
    }

    public void generate() {
      while (h < n) {
        crank();
        h++;
      }
    }

    public long sum() {
      generate();

      return t;
    }
  }

  public static void main(final String[] args) {
    final Long tFirstGiven = 1L;
    System.out.println("T(1)_given:\t" + tFirstGiven);

    final long tFirst = (new S(1)).sum();
    System.out.println("T(1):\t\t" + tFirst);

    final long tTwentyGiven = 86L;
    System.out.println("T(20)_given:\t" + tTwentyGiven);

    final long tTwenty = (new S(20)).sum();
    System.out.println("T(20):\t\t" + tTwenty);

    final long tOneThousandGiven = 364089L;
    System.out.println("T(1000)_given:\t" + tOneThousandGiven);

    final long tOneThousand = (new S(1000)).sum();
    System.out.println("T(1000):\t" + tOneThousand);

    final long tOneBillionGiven = 498676527978348241L;
    System.out.println("T(10^9)_given:\t" + tOneBillionGiven);

    final long tOneBillion = (new S(1000000000)).sum();
    System.out.println("T(10^9):\t" + tOneBillion);

    // final long tOneQuintillion = problem535.stream().limit(1000000000000000000L).reduce(new ModdingSummer(1000000000L)).get();
    // System.out.println("T(10^18) % 10^9:\t" + tOneQuintillion);
  }
}
