package info.kgeorgiy.ja.leontev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

/**
 * Implementation of {@link ParallelMapper}
 *
 * @author Leontev Taras
 */
public class ParallelMapperImpl implements ParallelMapper {

    private static class SynchronizedCounter {

        private RuntimeException runtimeException = null;

        private int cnt;

        SynchronizedCounter() {
            this.cnt = 0;
        }

        synchronized void incr() {
            cnt++;
        }

        synchronized void checkFull(int size) throws InterruptedException {
            while (cnt != size) {
                wait();
            }
        }

        synchronized void notifyIfFull(int size) {
            if (cnt == size) {
                notify();
            }
        }
    }


    private final Queue<Runnable> tasks = new ArrayDeque<>();

    private final List<Thread> threadsList = new ArrayList<>();

    /**
     * Default constructor
     */
    public ParallelMapperImpl() {
    }

    /**
     * Constructor for ParallelMapperImpl. Creates {@code threads} workers and starts them.
     * The workers then will be waiting for tasks in {@code Queue<Runnable> tasks}
     *
     * @param threads the amount of workers we will have running and waiting for tasks
     */
    public ParallelMapperImpl(final int threads) {
        Runnable worker = () -> {
            try {
                while (!Thread.interrupted()) {
                    Runnable task;
                    synchronized (tasks) {
                        while (tasks.isEmpty()) {
                            tasks.wait();
                        }
                        task = tasks.poll();
                        tasks.notify();
                    }
                    try {
                        task.run();
                    } catch (final RuntimeException e) {
                        System.err.println(e.getMessage());
                    }
                }
            } catch (final InterruptedException ignored) {
            }
        };
        for (int i = 0; i < threads; i++) {
            threadsList.add(new Thread(worker));
            threadsList.get(i).start();
        }
    }

    /**
     * Maps function {@code f} over specified {@code args}.
     * Mapping for each element performed in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args)
            throws InterruptedException {
        SynchronizedCounter synchronizedCounter = new SynchronizedCounter();
        List<R> results = new ArrayList<>(Collections.nCopies(args.size(), null));
        List<Runnable> runnables = new ArrayList<>();
        IntStream.range(0, args.size()).forEach(i -> {
                runnables.add(() -> {
                    try {
                        results.set(i, f.apply(args.get(i)));
                    } catch (final RuntimeException e) {
                        synchronized (synchronizedCounter) {
                            if (synchronizedCounter.runtimeException == null) {
                                synchronizedCounter.runtimeException = e;
                            } else {
                                synchronizedCounter.runtimeException.addSuppressed(e);
                            }
                        }
                    }
                    synchronizedCounter.incr();
                    synchronizedCounter.notifyIfFull(args.size());
                });
            }
        );

        synchronized (tasks) {
            tasks.addAll(runnables);
            tasks.notifyAll();
        }

        synchronized (synchronizedCounter) {
            if (synchronizedCounter.runtimeException != null) {
                throw synchronizedCounter.runtimeException;
            }
        }

        synchronizedCounter.checkFull(args.size());
        return results;
    }

    /**
     * Stops all threads. All unfinished mappings are left in undefined state.
     */
    @Override
    public void close() {
        InterruptedException exception = null;
        threadsList.forEach(Thread::interrupt);
        for (Thread thread : threadsList) {
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
            System.err.println(exception.getMessage());
        }
    }

}
