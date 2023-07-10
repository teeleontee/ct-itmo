package info.kgeorgiy.ja.leontev.i18n;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.text.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Function;


/**
 * <p>Implementation of {@link TextAnalyzer} interface</p>
 * @author teeleontee
 */
public class TextStatistics implements TextAnalyzer {

    private Locale locale;

    private ExecutorService service;

    private void init(Locale locale) {
        this.locale = locale;
        this.service = Executors.newFixedThreadPool(5);
    }

    /**
     * Implementation of {@link TextAnalyzer#analyze(Locale, String)}
     */
    @Override
    public TextStats analyze(Locale inLocale, String input) {
        init(inLocale);
        try {
            Path in  = Path.of(input);
            List<String> txt = new ArrayList<>();
            try (BufferedReader reader = Files.newBufferedReader(in, StandardCharsets.UTF_8)) {
                String line;
                while ((line = reader.readLine()) != null) {
                    txt.add(line);
                }
            } catch (IOException e) {
                System.err.println("I/O exception occurred while opening file: " + e.getMessage());
            }
            String str   = String.join(System.lineSeparator(), txt);
            return collectStatistics(str);
        } catch (InvalidPathException e) {
            System.err.println("invalid path to text given: " + e.getMessage());
        }
        return null;
    }

    /**
     * Implementation of {@link TextAnalyzer#writeAnalysis(Locale, String, TextStats)}
     */
    @Override
    public void writeAnalysis(Locale outLocale, String output, TextStats textStats) {
        try {
            Path out = Path.of(output);
            if (out.getParent() != null) {
                Files.createDirectories(out.getParent());
            }
            ResourceBundle bundle =
                    ResourceBundle.getBundle("info.kgeorgiy.ja.leontev.i18n.StatBundle", outLocale);
            try (BufferedWriter writer = Files.newBufferedWriter(out, StandardCharsets.UTF_8)) {
                writer.write(textStats.toOutput(bundle));
            } catch (final IOException e) {
                System.err.println(e.getMessage());
            }
        } catch (final IOException e) {
            System.err.println(e.getMessage());
        }
    }

    private <R> Runnable task(String text,
                              Phaser phaser,
                              final BreakIterator it,
                              List<R> rList,
                              TriFunction<String, Integer, Integer, R> function1,
                              Function<R, Boolean> function2)
    {
        return () -> {
            phaser.register();
            it.setText(text);
            for (
                    int begin = it.first(), end = it.next();
                    end != BreakIterator.DONE;
                    begin = end, end = it.next()
            ) {
                R temp = null;
                try {
                    temp = function1.apply(text, begin, end);
                } catch (final Exception e) {
                    System.err.println(e.getMessage());
                }

                if (Objects.nonNull(temp) &&function2.apply(temp)) {
                    rList.add(temp);
                }
            }
            phaser.arriveAndDeregister();
        };
    }

    /**
     * closes executorService
     */
    @Override
    public void close() throws Exception {
        service.close();
    }

    public static void main(String[] args) {
        if (args == null || args.length != 4 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Bad parameters");
            return;
        }
        Locale inLocale = Locale.forLanguageTag(args[0]);
        Locale outLocale = Locale.forLanguageTag(args[1]);

        try (TextAnalyzer analyser = new TextStatistics()) {
            TextStats stats = analyser.analyze(inLocale, args[2]);
            analyser.writeAnalysis(outLocale, args[3], stats);
        } catch (final Exception e) {
            System.err.println(e.getMessage());
        }
    }

    private TextStats collectStatistics(String text) {

        final List<String> sentences = new ArrayList<>();
        final List<String> words     = new ArrayList<>();
        final List<Double> numbers   = new ArrayList<>();
        final List<Double> money     = new ArrayList<>();
        final List<Date>   dates     = new ArrayList<>();

        Phaser phaser = new Phaser(1);

        // Sentences
        service.submit(task(text, phaser, BreakIterator.getSentenceInstance(locale), sentences,
                String::substring, s -> !s.isBlank()));

        // Words
        service.submit(task(text, phaser, BreakIterator.getWordInstance(locale), words,
                String::substring, s -> s.codePoints().anyMatch(Character::isLetter) && !s.isBlank()));

        // Dates
        service.submit(task(text, phaser, BreakIterator.getWordInstance(locale), dates,
                (s, l, r) -> DateFormat.getDateInstance(DateFormat.SHORT, locale).parse(s, new ParsePosition(l)),
                Objects::nonNull));

        // Numbers
        service.submit(task(text, phaser, BreakIterator.getWordInstance(locale), numbers,
                        (String s, Integer l, Integer r) -> {
                            Number num = NumberFormat.getNumberInstance(locale).parse(s, new ParsePosition(l));
                            return num != null ? num.doubleValue() : null;
                        }, Objects::nonNull)
        );

        // Money, Money, Money, must be funny...
        service.submit(task(text, phaser, BreakIterator.getWordInstance(locale), money,
                    (s, l, r) -> {
                        Number num = NumberFormat.getCurrencyInstance(locale).parse(s, new ParsePosition(l));
                        return num != null ? num.doubleValue() : null;
                    }, Objects::nonNull)
        );

        phaser.arriveAndAwaitAdvance();
        phaser.arriveAndDeregister();

        GeneralStats generalStats = new GeneralStats(sentences.size(), words.size(),
                numbers.size(), money.size(), dates.size());

        NumberStats numberStats     = sentences.isEmpty() ? EMPTY_NUMBER_STATS : getNumberStats(numbers);
        WordStats wordStats         = words.isEmpty() ? EMPTY_WORD_STATS  : getWordStats(words);
        SentenceStats sentenceStats = sentences.isEmpty() ? EMPTY_SENTENCE_STATS : getSentenceStats(sentences);
        MoneyStats moneyStats       = money.isEmpty() ? EMPTY_MONEY_STATS : getMoneyStats(money);
        DateStats dateStats         = dates.isEmpty() ? EMPTY_DATE_STAT : getDateStats(dates);

        return new TextStats(generalStats, sentenceStats, wordStats, numberStats, moneyStats, dateStats);
    }


    private NumberStats getNumberStats(List<Double> numbers) {
        double sum = numbers.stream().reduce(0., Double::sum);
        double avg = sum / numbers.size();
        double max = numbers.stream().max(Comparator.naturalOrder()).orElse(0.);
        double min = numbers.stream().min(Comparator.naturalOrder()).orElse(0.);
        return new NumberStats(numbers.size(), min, max, avg);
    }

    private WordStats getWordStats(List<String> words) {
        Collator collator = Collator.getInstance(locale);
        words.sort(collator);
        String max = words.get(words.size() - 1);
        String min = words.get(0);
        String minByLength = words.stream().min(Comparator.comparing(String::length)).orElse("");
        String maxByLength = words.stream().max(Comparator.comparing(String::length)).orElse("");
        double avg = words.stream().map(String::length).reduce(0, Integer::sum).doubleValue() / words.size();
        return new WordStats(words.size(), min, max, minByLength, maxByLength, avg);
    }

    private SentenceStats getSentenceStats(List<String> sentences) {
        Collator collator = Collator.getInstance(locale);
        sentences.sort(collator);
        String min = sentences.get(sentences.size() - 1);
        String max = sentences.get(0);
        String maxByLength = sentences.stream().max(Comparator.comparing(String::length)).orElse("");
        String minByLength = sentences.stream().min(Comparator.comparing(String::length)).orElse("");
        double avg = sentences.stream().map(String::length).reduce(0, Integer::sum).doubleValue() / sentences.size();
        return new SentenceStats(sentences.size(), min, max, minByLength, maxByLength, avg);
    }

    private MoneyStats getMoneyStats(List<Double> money) {
        double sum = money.stream().reduce(0., Double::sum);
        double max = money.stream().max(Comparator.naturalOrder()).orElse(0.);
        double min = money.stream().min(Comparator.naturalOrder()).orElse(0.);
        double avg = sum / money.size();
        return new MoneyStats(money.size(), min, max, avg);
    }

    private DateStats getDateStats(List<Date> dates) {
        BigInteger sum = dates.stream()
                .map(date -> BigInteger.valueOf(date.getTime()))
                .reduce(BigInteger.valueOf(0), BigInteger::add);
        BigInteger avgMs = sum.divide(BigInteger.valueOf(dates.size()));
        Date avgDate = new Date(avgMs.longValue());
        dates.sort(Comparator.comparing(Date::getTime));
        return new DateStats(dates.size(), dates.get(0), dates.get(dates.size() - 1), avgDate);
    }

    private static final Date EMPTY_DATE = new Date(0);

    private static final DateStats EMPTY_DATE_STAT = new DateStats(0, EMPTY_DATE, EMPTY_DATE, EMPTY_DATE);

    private static final NumberStats EMPTY_NUMBER_STATS = new NumberStats(0, 0, 0, 0);

    private static final WordStats EMPTY_WORD_STATS = new WordStats(0, "", "", "", "", 0);

    private static final SentenceStats EMPTY_SENTENCE_STATS = new SentenceStats(0, "", "", "", "", 0);

    private static final MoneyStats EMPTY_MONEY_STATS = new MoneyStats(0, 0, 0, 0);
}
