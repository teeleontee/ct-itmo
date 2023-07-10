package info.kgeorgiy.ja.leontev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * Implementation of {@link ScalarIP}
 *
 * @author Leontev Taras
 */
public class IterativeParallelism implements ScalarIP {

    private final ParallelMapper parallelMapper;

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    public IterativeParallelism() {
        this.parallelMapper = null;
    }

    private <T, R> R parallelTask(int threads,
                                  List<? extends T> values,
                                  Function<Stream<? extends T>, R> function,
                                  Function<Stream<R>, R> collect)
            throws InterruptedException {
        threads = Math.min(threads, values.size());
        List<R> ranges = new ArrayList<>(threads);
        if (values.isEmpty()) {
            return collect.apply(ranges.stream());
        }
        List<Stream<? extends T>> listOfSublist = new ArrayList<>();
        List<Thread> threadList = new ArrayList<>(threads);
        int rangeSize = values.size() / threads;
        for (int i = 0; i < threads; i++) {
            ranges.add(null);
            final int start = i * rangeSize;
            final int end = i != threads - 1 ? start + rangeSize : values.size();
            final int threadIndex = i;
            Stream<? extends T> sublists = values.subList(start, end).stream();
            listOfSublist.add(sublists);
            threadList.add(new Thread(() -> {
                R value = function.apply(sublists);
                ranges.set(threadIndex, value);
            }));
        }
        if (parallelMapper != null) {
            List<R> res = parallelMapper.map(function, listOfSublist);
            return collect.apply(res.stream());
        }
        threadList.forEach(Thread::start);
        join(threadList);
        return collect.apply(ranges.stream());
    }

    private static void join(List<Thread> threadList) throws InterruptedException {
        InterruptedException exception = null;
        for (Thread thread : threadList) {
            while (true) {
                try {
                    thread.join();
                    break;
                } catch (final InterruptedException e) {
                    if (exception == null) {
                        exception = e;
                    } else {
                        exception.addSuppressed(e);

                    }
                }
            }
        }
        if (exception != null) {
            throw exception;
        }
    }

    /**
     * @param threads    number of concurrent threads.
     * @param values     values to get maximum of.
     * @param comparator value comparator.
     * @return maximum in List
     * @throws InterruptedException if thread was interrupted
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator)
            throws InterruptedException {
        return parallelTask(threads,
                values,
                s -> s.max(comparator).orElseThrow(),
                s -> s.max(comparator).orElseThrow()
        );
    }

    /**
     * @param threads    number of concurrent threads.
     * @param values     values to get minimum of.
     * @param comparator value comparator.
     * @return minimum in list
     * @throws InterruptedException if thread was interrupted
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator)
            throws InterruptedException {
        return maximum(threads,
                values,
                comparator.reversed()
        );
    }

    /**
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @return {@code true} if all elements return {@code true} on predicate, else returns {@code false}
     * @throws InterruptedException if thread was interrupted
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelTask(threads,
                values,
                s -> s.allMatch(predicate),
                s -> s.allMatch(t -> t.equals(true))
        );
    }

    /**
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @return {@code true} if at least one element in the list returns {@code true}
     * on predicate else returns {@code false}
     * @throws InterruptedException if thread was interrupted
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelTask(threads,
                values,
                s -> s.anyMatch(predicate),
                s -> s.anyMatch(t -> t.equals(true))
        );
    }

    /**
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @return The amount of elements that complete the predicate
     * @throws InterruptedException if thread was interrupted
     */
    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelTask(threads,
                values,
                s -> (int) s.filter(predicate).count(),
                s -> s.reduce(0, Integer::sum)
        );
    }
}
