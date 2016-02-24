package us.yellosoft.projecteuler;

import it.unimi.dsi.fastutil.BigList;
import it.unimi.dsi.fastutil.longs.LongBigArrayBigList;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.stream.Collectors;
import java.util.function.BinaryOperator;

public final class Problem535 implements Iterable<Long> {
  public static long lsqrt(final long x) {
    return (long) Math.floor(Math.sqrt(x));
  }

  static class S implements Iterator<Long> {
    private BigList<Long> s;
    private Long i;
    private Long j;
    private Long r;
    boolean first;

    public S() {
      s = new LongBigArrayBigList();
      s.add(1L);

      i = 2L;
      j = 0L;
      r = 0L;

      first = true;
    }

    @Override
    public boolean hasNext() {
      return true;
    }

    @Override
    public Long next() {
      if (first) {
        first = false;
        return s.get(0L);
      } else if (r == 0) {
        Long y = s.get(j);
        s.add(y);
        Long y2 = s.get(j + 1);
        j = j + 1;
        r = lsqrt(y2);
        return y;
      } else {
        Long y = i;
        s.add(y);
        i = i + 1;
        r = r - 1;
        return y;
      }
    }
  }

  static class Summer implements BinaryOperator<Long> {
    @Override
    public Long apply(final Long t, final Long u) {
      return t + u;
    }
  }

  static class ModdingSummer implements BinaryOperator<Long> {
    private Long modulus;

    public ModdingSummer(final Long modulus) {
      this.modulus = modulus;
    }

    @Override
    public Long apply(final Long t, final Long u) {
      return (t + u) % modulus;
    }
  }

  @Override
  public Iterator<Long> iterator() {
    return new S();
  }

  public Stream<Long> stream() {
    return StreamSupport.stream(spliterator(), false);
  }

  public static void main(final String[] args) {
    final Problem535 problem535 = new Problem535();

    final List<Long> sFirstTwentyGiven = Arrays.asList(new Long[] {
      1L, 1L, 2L, 1L, 3L, 2L, 4L, 1L, 5L, 3L, 6L, 2L, 7L, 8L, 4L, 9L, 1L, 10L, 11L, 5L
      });
    System.out.println("S(1:20)_given:\t" + sFirstTwentyGiven);

    final List<Long> sFirstTwenty = problem535.stream().limit(20).collect(Collectors.toList());
    System.out.println("S(1:20):\t" + sFirstTwenty);

    final Long tFirstGiven = 1L;
    System.out.println("T(1)_given:\t" + tFirstGiven);

    final Summer summer = new Summer();

    final Long tFirst = problem535.stream().limit(1).reduce(summer).get();
    System.out.println("T(1):\t\t" + tFirst);

    final Long tTwentyGiven = 86L;
    System.out.println("T(20)_given:\t" + tTwentyGiven);

    final Long tTwenty = problem535.stream().limit(20).reduce(summer).get();
    System.out.println("T(20):\t\t" + tTwenty);

    final Long tOneThousandGiven = 364089L;
    System.out.println("T(1000)_given:\t" + tOneThousandGiven);

    final Long tOneThousand = problem535.stream().limit(1000).reduce(summer).get();
    System.out.println("T(1000):\t" + tOneThousand);

    final Long tOneBillionGiven = 498676527978348241L;
    System.out.println("T(10^9)_given:\t" + tOneBillionGiven);

    final Long tOneBillion = problem535.stream().limit(1000000000).reduce(summer).get();
    System.out.println("T(10^9):\t" + tOneBillion);
    // ...
  }
}
