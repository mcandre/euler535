package us.yellosoft.projecteuler;

import com.google.common.base.Preconditions;
import com.google.common.math.BigIntegerMath;

import java.math.BigInteger;
import java.math.RoundingMode;

public final class Euler535 {
  /** Utility class */
  private Euler535() {}

  public static BigInteger flSqrt(final BigInteger x) {
    return BigIntegerMath.sqrt(x, RoundingMode.DOWN);
  }

  public static int flLog10(final BigInteger x) {
    return BigIntegerMath.log10(x, RoundingMode.DOWN);
  }

  static final class Track {
    public BigInteger i = BigInteger.ZERO;
    public BigInteger j = BigInteger.ZERO;
    public BigInteger r = BigInteger.ZERO;
    public boolean skip = false;
  }

  public static Track[] teleport(final BigInteger n) {
    BigInteger sum = BigInteger.ZERO;
    int k = 0;

    BigInteger circled = BigInteger.ONE;
    BigInteger uncircled = BigInteger.ZERO;

    Track[] tracks = new Track[5 + flLog10(n)];
    for (int i = 0; i < tracks.length; i++) {
      tracks[i] = new Track();
    }

    tracks[0].i = BigInteger.ONE;

    while (sum.compareTo(n) < 0) {
      tracks[k].i = circled;
      tracks[k].j = uncircled;

      uncircled = uncircled.add(circled);

      if (k != 0) {
        circled = circled.add(sumOfRoots(circled));
      }

      sum = circled.add(uncircled);

      k++;
    }

    Track[] reversed = new Track[k];

    for (int i = 0; i < k; i++) {
      reversed[i] = tracks[k - i - 1];
    }

    return reversed;
  }

  public static BigInteger sumOfRoots(final BigInteger n) {
    BigInteger x, sum = BigInteger.ZERO;
    BigInteger sqrtN = flSqrt(n);

    for (x = BigInteger.ONE; x.compareTo(sqrtN) < 0; x = x.add(BigInteger.ONE)) {
      sum = sum.add(x.multiply(x.multiply(BigInteger.valueOf(2)).add(BigInteger.ONE)));
    }

    return sum.add(x.multiply(n.subtract(x.multiply(x)).add(BigInteger.ONE)));
  }

  public static void run(final Track[] tracks, final BigInteger n) {
    BigInteger m = tracks[0].i.add(tracks[0].j);
    int nextTrack = 0;
    int k;

    while (m.compareTo(n) < 0) {
      k = 0;

      while (tracks[k].skip) {
        tracks[k].skip = false;
        k++;
      }

      if (tracks[k].r.equals(BigInteger.ZERO)) {
        nextTrack = k + 1;

        while (tracks[nextTrack].skip) {
          nextTrack++;
        }

        tracks[k].r = flSqrt(tracks[nextTrack].i.add(BigInteger.ONE));
      }

      // Blink
      if (k == 0) {
        m = m.add(tracks[k].r);
        tracks[k].i = tracks[k].i.add(tracks[k].r);
        tracks[k].r = BigInteger.ZERO;
        tracks[k].skip = true;
      } else {
        m = m.add(BigInteger.ONE);
        tracks[k].r = tracks[k].r.subtract(BigInteger.ONE);
        tracks[k].i = tracks[k].i.add(BigInteger.ONE);
        tracks[k].skip = tracks[k].r.compareTo(BigInteger.ZERO) == 0;
      }
    }

    moonwalk(tracks, n);
  }

  public static void moonwalk(final Track[] tracks, final BigInteger n) {
    BigInteger subtotal = BigInteger.ZERO;

    for (int i = 1; i < tracks.length; i++) {
      subtotal = subtotal.add(tracks[i].i);
    }

    tracks[0].i = n.subtract(subtotal);
  }

  public static int sumOfSums(final Track[] tracks) {
    BigInteger total = BigInteger.ZERO;
    BigInteger i;
    BigInteger subtotal;

    for (Track track : tracks) {
      i = track.i;
      subtotal = i.multiply(i.add(BigInteger.ONE)).divide(BigInteger.valueOf(2));
      total = total.add(subtotal);
    }

    return total.divideAndRemainder(BigInteger.valueOf(1000000000L))[1].intValue();
  }

  public static int t(final BigInteger n) {
    Track[] tracks = teleport(n);
    run(tracks, n);
    return sumOfSums(tracks);
  }

  public static void test() {
    Preconditions.checkArgument(t(BigInteger.ONE) == 1);
    Preconditions.checkArgument(t(BigInteger.valueOf(20)) == 86);
    Preconditions.checkArgument(t(BigInteger.valueOf(1000)) == 364089);
    Preconditions.checkArgument(t(BigInteger.valueOf(1000000000L)) == 978348241);

    System.out.println("Passed all assertions");
  }

  public static void main(final String[] args) {
    if (args.length > 0 && args[0].equals("-t")) {
      test();
    } else {
      System.out.println("T(10^18)_(10^9):");
      System.out.println(t(BigInteger.valueOf(1000000000000000000L)));
    }
  }
}
