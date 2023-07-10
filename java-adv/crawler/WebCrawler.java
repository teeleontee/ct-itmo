package info.kgeorgiy.ja.leontev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;

public class WebCrawler implements Crawler {

    private static class DownloaderUtility {

        private final Downloader downloader;

        private final Set<String> downloads;

        private final ConcurrentMap<String, IOException> errors;

        private final ExecutorService downloadThreadsPool;

        private final ExecutorService extractThreadsPool;

        public DownloaderUtility(Downloader downloader,
                                 ExecutorService downloadThreadsPool,
                                 ExecutorService extractThreadsPool) {
            this.downloader = downloader;
            this.downloads = ConcurrentHashMap.newKeySet();
            this.errors = new ConcurrentHashMap<>();
            this.downloadThreadsPool = downloadThreadsPool;
            this.extractThreadsPool = extractThreadsPool;
        }

        public void download(String url, int depth) {
            int level = 0;
            Queue<String> front = new ConcurrentLinkedQueue<>();
            Queue<String> next = new ConcurrentLinkedQueue<>();
            Set<String> used = ConcurrentHashMap.newKeySet();
            front.add(url);
            while (level != depth) {
                Queue<Future<List<String>>> levelLinks = new ConcurrentLinkedQueue<>();
                front.forEach((link) -> {
                    Future<Document> curDocument = downloadThreadsPool.submit(() -> {
                        if (!used.contains(link)) {
                            try {
                                used.add(link);
                                Document tempDoc = downloader.download(link);
                                downloads.add(link);
                                return tempDoc;
                            } catch (final IOException ex) {
                                errors.put(link, ex);
                            }
                        }
                        return EMPTY;
                    });
                    levelLinks.add(extractThreadsPool.submit(() -> curDocument.get()
                            .extractLinks()
                            .stream()
                            .filter(el -> !used.contains(el))
                            .toList()));
                    levelLinks.forEach(s -> used.add(s.toString()));
                });
                levelLinks.forEach(listFuture -> {
                    try {
                        next.addAll(listFuture.get().stream().filter(el -> !used.contains(el)).toList());
                    } catch (InterruptedException | ExecutionException ignored) {
                    }
                });
                front = next;
                level++;
            }
        }

        public Result createResult() {
            return new Result(downloads.stream().toList(), errors);
        }
    }


    private static void shutdownThread(ExecutorService downloadThreadsPool) {
        downloadThreadsPool.shutdown();
        try {
            if (!downloadThreadsPool.awaitTermination(800, TimeUnit.MILLISECONDS)) {
                downloadThreadsPool.shutdownNow();
            }
        } catch (InterruptedException e) {
            downloadThreadsPool.shutdownNow();
        }
    }

    private static final Document EMPTY = new EmptyDocument();

    private final Downloader downloader;

    private final ExecutorService downloadThreadsPool;

    private final ExecutorService extractThreadsPool;

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.downloadThreadsPool = Executors.newFixedThreadPool(downloaders);
        this.extractThreadsPool = Executors.newFixedThreadPool(extractors);
    }

    @Override
    public Result download(String url, int depth) {
        DownloaderUtility util = new DownloaderUtility(downloader, downloadThreadsPool, extractThreadsPool);
        util.download(url, depth);
        return util.createResult();
    }

    @Override
    public void close() {
        shutdownThread(downloadThreadsPool);
        shutdownThread(extractThreadsPool);
    }

    public static void main(String[] args) {
        if (args == null || args.length == 0 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Bad arguments given");
        }
        else {
            try (Crawler webCrawler = new WebCrawler(new CachingDownloader(200),
                    Integer.parseInt(args[2]),
                    Integer.parseInt(args[3]),
                    Integer.parseInt(args[4]))) {
                webCrawler.download(args[0], Integer.parseInt(args[1]));
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        }
    }
}
