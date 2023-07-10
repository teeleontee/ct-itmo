package info.kgeorgiy.ja.leontev.crawler;

import info.kgeorgiy.java.advanced.crawler.Document;

import java.io.IOException;
import java.util.List;

public class EmptyDocument implements Document {
    @Override
    public List<String> extractLinks() throws IOException {
        return List.of();
    }
}
