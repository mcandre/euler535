package us.yellosoft.projecteuler;

import com.google.common.base.Preconditions;
import com.google.common.math.BigIntegerMath;

import java.math.BigInteger;
import java.math.RoundingMode;

/** A sloppy, nonrecursive solution to Euler 535 */
public final class Euler535 {
    /** Utility class */
    private Euler535() {}

        /** floor(squareRoot( )) for big integers
        @param x input
        @return the square root of x, rounded down
        */
        public static BigInteger flSqrt(final BigInteger x) {
            return BigIntegerMath.sqrt(x, RoundingMode.DOWN);
        }

        /** floor(log( )) for big integers
        @param x input
        @return log base 10 of x, rounded down
        */
        public static int flLog10(final BigInteger x) {
            return BigIntegerMath.log10(x, RoundingMode.DOWN);
        }

        /** A model of S(n) as a series of tracks of 1..n */
        static final class Track {
            private BigInteger i = BigInteger.ZERO;
            private BigInteger j = BigInteger.ZERO;
            private BigInteger r = BigInteger.ZERO;
            private boolean skip = false;

            /** Set latest circled number
            @param i latest circled number
            */
            public void setI(final BigInteger i) {
                this.i = i;
            }

            /** Retrieve latest circled number
            @return latest circled number
            */
            public BigInteger getI() {
                return i;
            }

            /** Set uncircled number
            @param j uncircled number
            */
            public void setJ(final BigInteger j) {
                this.j = j;
            }

            /** Retrieve uncircled number
            @return uncircled number
            */
            public BigInteger getJ() {
                return j;
            }

            /** Set current run length
            @param r current run length
            */
            public void setR(final BigInteger r) {
                this.r = r;
            }

            /** Retrieve current run length
            @return current run length
            */
            public BigInteger getR() {
                return r;
            }

            /** Set whether this track should be skipped for iteration
            @param skip whether this track should be skipped for iteration
            */
            public void setSkip(final boolean skip) {
                this.skip = skip;
            }

            /** Retrieve whether this track should be skipped for iteration
            @return whether this track should be skipped for iteration
            */
            public boolean shouldSkip() {
                return skip;
            }
        }

        /** Hop forward into S(n) by calculating the largest position
        where a new track has begun
        @param n input
        @return tracks at a late position
        */
        public static Track[] teleport(final BigInteger n) {
            BigInteger sum = BigInteger.ZERO;
            int k = 0;

            BigInteger circled = BigInteger.ONE;
            BigInteger uncircled = BigInteger.ZERO;

            Track[] tracks = new Track[5 + flLog10(n)];
            for (int i = 0; i < tracks.length; i++) {
                tracks[i] = new Track();
            }

            tracks[0].setI(BigInteger.ONE);

            while (sum.compareTo(n) < 0) {
                tracks[k].setI(circled);
                tracks[k].setJ(uncircled);

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

        /** Sum of the floored square roots up to n
        @param n input
        @return sum of the floored square roots up to n
        */
        public static BigInteger sumOfRoots(final BigInteger n) {
            BigInteger x, sum = BigInteger.ZERO;
            BigInteger sqrtN = flSqrt(n);

            for (x = BigInteger.ONE; x.compareTo(sqrtN) < 0; x = x.add(BigInteger.ONE)) {
                sum = sum.add(x.multiply(x.multiply(BigInteger.valueOf(2)).add(BigInteger.ONE)));
            }

            return sum.add(x.multiply(n.subtract(x.multiply(x)).add(BigInteger.ONE)));
        }

        /** Iterate the track vector model up to vector at n
        @param tracks some starting position
        @param n max iteration
        */
        public static void run(final Track[] tracks, final BigInteger n) {
            BigInteger m = tracks[0].getI().add(tracks[0].getJ());
            int nextTrack = 0;
            int k;

            while (m.compareTo(n) < 0) {
                k = 0;

                while (tracks[k].shouldSkip()) {
                    tracks[k].setSkip(false);
                    k++;
                }

                if (tracks[k].getR().equals(BigInteger.ZERO)) {
                    nextTrack = k + 1;

                    while (tracks[nextTrack].shouldSkip()) {
                        nextTrack++;
                    }

                    tracks[k].setR(flSqrt(tracks[nextTrack].getI().add(BigInteger.ONE)));
                }

                // Blink
                if (k == 0) {
                    m = m.add(tracks[k].getR());
                    tracks[k].setI(tracks[k].getI().add(tracks[k].getR()));
                    tracks[k].setR(BigInteger.ZERO);
                    tracks[k].setSkip(true);
                } else {
                    m = m.add(BigInteger.ONE);
                    tracks[k].setR(tracks[k].getR().subtract(BigInteger.ONE));
                    tracks[k].setI(tracks[k].getI().add(BigInteger.ONE));
                    tracks[k].setSkip(tracks[k].getR().compareTo(BigInteger.ZERO) == 0);
                }
            }

            moonwalk(tracks, n);
        }

        /** Correct the top track, in case we blinked past the correct vector for n
        @param tracks vector position at or in the middle of a blink past n
        @param n max iteration
        */
        public static void moonwalk(final Track[] tracks, final BigInteger n) {
            BigInteger subtotal = BigInteger.ZERO;

            for (int i = 1; i < tracks.length; i++) {
                subtotal = subtotal.add(tracks[i].getI());
            }

            tracks[0].setI(n.subtract(subtotal));
        }

        /** T(n) rem 10^9 in terms of a sum of sums of the track vector
        @param tracks vector model of S(n)
        @return the sum of the sums of each track
        */
        public static int sumOfSums(final Track[] tracks) {
            BigInteger total = BigInteger.ZERO;
            BigInteger i;
            BigInteger subtotal;

            for (Track track : tracks) {
                i = track.getI();
                subtotal = i.multiply(i.add(BigInteger.ONE)).divide(BigInteger.valueOf(2));
                total = total.add(subtotal);
            }

            return total.divideAndRemainder(BigInteger.valueOf(1000000000L))[1].intValue();
        }

        /** Non-recursive implementation of T(n)
        @param n max iteration
        @return T(n) rem 10^9
        */
        public static int t(final BigInteger n) {
            Track[] tracks = teleport(n);
            run(tracks, n);
            return sumOfSums(tracks);
        }

        /** A few manual assertions for S, T */
        public static void test() {
            Preconditions.checkArgument(t(BigInteger.ONE) == 1);
            Preconditions.checkArgument(t(BigInteger.valueOf(20)) == 86);
            Preconditions.checkArgument(t(BigInteger.valueOf(1000)) == 364089);
            Preconditions.checkArgument(t(BigInteger.valueOf(1000000000L)) == 978348241);

            System.out.println("Passed all assertions");
        }

        /** CLI entry point
        @param args CLI flags
        */
        public static void main(final String[] args) {
            if (args.length > 0 && args[0].equals("-t")) {
                test();
            } else {
                System.out.println("T(10^18)_(10^9):");
                System.out.println(t(BigInteger.valueOf(1000000000000000000L)));
            }
        }
    }
