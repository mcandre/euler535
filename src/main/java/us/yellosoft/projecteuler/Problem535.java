package us.yellosoft.projecteuler;

import com.google.common.math.BigIntegerMath;

import java.math.BigInteger;
import java.math.RoundingMode;

public final class Problem535 {
  public static BigInteger flSqrt(final BigInteger x) {
    return BigIntegerMath.sqrt(x, RoundingMode.DOWN);
  }

  public static int ceLog10(final BigInteger x) {
    return BigIntegerMath.log10(x, RoundingMode.UP);
  }

  static class Track {
    public boolean drop = false;
    public BigInteger r = BigInteger.ZERO;
    public BigInteger i = BigInteger.ZERO;
    public BigInteger j = BigInteger.ZERO;

    public BigInteger n() {
      return i.add(j);
    }
  }

  static class S {
    public static Track[] teleport(final BigInteger n) {
      BigInteger m = BigInteger.ZERO;
      int k = 0;

      BigInteger circledNums = BigInteger.ONE;
      BigInteger uncircledNums = BigInteger.ZERO;

      Track[] tracks = new Track[5 + ceLog10(n)];
      for (int i = 0; i < tracks.length; i++) {
        tracks[i] = new Track();
      }

      tracks[0].i = BigInteger.ONE;

      BigInteger currentCircledNums = BigInteger.ZERO;

      while (m.compareTo(n) < 0) {
        tracks[k].i = circledNums;
        tracks[k].j = uncircledNums;

        k++;

        circledNums = tracks[k - 1].i;
        uncircledNums = tracks[k - 1].n();
        m = circledNums.add(uncircledNums);

        if (k != 1) {
          currentCircledNums = tracks[k - 1].i;
          circledNums = circledNums.add(sumOfRoots(currentCircledNums));
          m = circledNums.add(uncircledNums);
        }
      }

      Track[] reversed = new Track[k];

      for (int i = 0; i < k; i++) {
        reversed[i] = tracks[k - i - 1];
      }

      return reversed;
    }

    public static BigInteger sumOfRoots(final BigInteger n) {
      BigInteger sum = BigInteger.ZERO;
      BigInteger root = BigInteger.ONE;

      while (root.multiply(root).compareTo(n) <= 0) {
        BigInteger candidate = root.add(BigInteger.ONE);

        if (candidate.multiply(candidate).compareTo(n) > 0) {
          BigInteger q = n.subtract(root.multiply(root)).add(BigInteger.ONE);
          sum = sum.add(q.multiply(root));
        } else {
          sum = sum.add(root.multiply(root.multiply(BigInteger.valueOf(2)).add(BigInteger.ONE)));
        }

        root = root.add(BigInteger.ONE);
      }

      return sum;
    }

    public static void run(Track[] tracks, BigInteger n) {
      BigInteger m = tracks[0].n();
      int nextTrack = 0;
      int k;

      while (m.compareTo(n) < 0) {
        k = 0;

        while (tracks[k].drop) {
          tracks[k].drop = false;
          k++;
        }

        if (tracks[k].r.equals(BigInteger.ZERO)) {
          nextTrack = k + 1;

          while (tracks[nextTrack].drop) {
            nextTrack++;
          }

          tracks[k].r = flSqrt(tracks[nextTrack].i.add(BigInteger.ONE));
        }

        // Blink
        if (k == 0) {
          tracks[k].i = tracks[k].i.add(tracks[k].r);
          m = m.add(tracks[k].r);
          tracks[k].r = BigInteger.ZERO;
        } else {
          tracks[k].r = tracks[k].r.subtract(BigInteger.ONE);
          tracks[k].i = tracks[k].i.add(BigInteger.ONE);
          m = m.add(BigInteger.ONE);
        }

        if (tracks[k].r.compareTo(BigInteger.ZERO) <= 0) {
          tracks[k].drop = true;
        }
      }

      moonwalk(tracks, n);
    }

    public static void moonwalk(Track[] tracks, BigInteger n) {
      BigInteger subtotal = BigInteger.ZERO;

      for (int i = 1; i < tracks.length; i++) {
        subtotal = subtotal.add(tracks[i].i);
      }

      tracks[0].i = n.subtract(subtotal);
    }

    public static int sumOfSums(Track[] tracks) {
      BigInteger total = BigInteger.ZERO;

      for (Track track : tracks) {
        BigInteger n = track.i;
        BigInteger subtotal = n.multiply(n.add(BigInteger.ONE)).divide(BigInteger.valueOf(2));
        total = total.add(subtotal);
      }

      return total.divideAndRemainder(BigInteger.valueOf(1000000000L))[1].intValue();
    }

    public static int t(BigInteger n) {
      Track[] tracks = teleport(n);
      run(tracks, n);
      return sumOfSums(tracks);
    }
  }

  public static void test() {
    assert S.t(BigInteger.ONE) == 1;
    assert S.t(BigInteger.valueOf(20)) == 86;
    assert S.t(BigInteger.valueOf(1000)) == 364089;
    assert S.t(BigInteger.valueOf(1000000000L)) == 978348241;

    System.out.println("Passed all assertions");
  }

  public static void main(final String[] args) {
    if (args.length > 0 && args[0].equals("-t")) {
      test();
    } else {
      System.out.println("T(10^18)_(10^9):");
      System.out.println(S.t(BigInteger.valueOf(1000000000000000000L)));
    }
  }
}
