package info.kgeorgiy.ja.leontev.i18n;

import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.Date;
import java.util.Locale;

@DisplayName("i18n test")
public class i18nTest {

    private static final Date EMPTY_DATE = new Date();

    private static final DateStats EMPTY_DATE_STAT = new DateStats(0, EMPTY_DATE, EMPTY_DATE, EMPTY_DATE);

    private static final NumberStats EMPTY_NUMBER_STATS = new NumberStats(0, 0, 0, 0);

    private static final WordStats EMPTY_WORD_STATS = new WordStats(0, "", "", "", "", 0);

    private static final SentenceStats EMPTY_SENTENCE_STATS = new SentenceStats(0, "", "", "", "", 0);

    private static final MoneyStats EMPTY_MONEY_STATS = new MoneyStats(0, 0, 0, 0);

    private static final GeneralStats EMPTY_GEN_STATS = new GeneralStats(0, 0, 0, 0, 0);

    @Test
    @DisplayName("Testing Empty file")
    public void test_empty0() {
        System.out.println("Starting empty file test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_empty0";
        final TextStats expectedStats = new TextStats(EMPTY_GEN_STATS, EMPTY_SENTENCE_STATS, EMPTY_WORD_STATS,
                EMPTY_NUMBER_STATS, EMPTY_MONEY_STATS, EMPTY_DATE_STAT);
        try (TextAnalyzer analyzer = new TextStatistics()) {
            TextStats stats = analyzer.analyze(Locale.ENGLISH, pathToFile);
            Assert.assertEquals(stats.generalStats, expectedStats.generalStats);
            Assert.assertEquals(stats.sentenceStats, expectedStats.sentenceStats);
            Assert.assertEquals(stats.wordsStats, expectedStats.wordsStats);
            Assert.assertEquals(stats.moneyStats, expectedStats.moneyStats);
            Assert.assertEquals(stats.dateStats.count(), expectedStats.dateStats.count());
        } catch (final Exception e) {
            System.err.println(e.getMessage());
            Assert.fail();
        }
    }

    @Test
    @DisplayName("Testing Smoke")
    public void test_smoke1() {
        System.out.println("Starting smoke test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_smoke1";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats.dates(),  0);
            Assert.assertEquals(stats2.generalStats.sentences(),  3);
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }

    @Test
    @DisplayName("Testing counters")
    public void test_counters2() {
        System.out.println("Starting counters test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_counters2";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats, new GeneralStats(2, 15, 4, 1, 0));
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }

    @Test
    @DisplayName("Testing mins")
    public void test_min3() {
        System.out.println("Starting mins test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_min3";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats, new GeneralStats(5, 16, 3, 2, 0));
            Assert.assertEquals(stats2.wordsStats.min(), "apples");
            Assert.assertEquals(stats2.numberStats.min(), 2, 0.00001);
            Assert.assertEquals(stats2.moneyStats.min(), 150.0, 0.00001);
            Assert.assertEquals(stats2.sentenceStats.min(), "You have $150.");
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }

    @Test
    @DisplayName("Testing min by length")
    public void test_min_length4() {
        System.out.println("Starting mins test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_minlen4";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats, new GeneralStats(5, 12, 0, 1, 0));
            Assert.assertEquals(stats2.wordsStats.minByLength(), "we");
            Assert.assertEquals(stats2.sentenceStats.minByLength(), "Very. ");
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }

    @Test
    @DisplayName("Testing max by length")
    public void test_max_length5() {
        System.out.println("Starting maxes test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_maxlen5";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats, new GeneralStats(3, 65, 2, 0, 0));
            Assert.assertEquals(stats2.wordsStats.maxByLength(), "motivated");
            Assert.assertEquals(stats2.sentenceStats.maxByLength(), "We never give up, that's why I spent all fucking night doing this homework, because I felt really really motivated, I only went through 3 burn energy drinks tonight.");
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }


    @Test
    @DisplayName("Testing averages")
    public void test_average6() {
        System.out.println("Starting averages test...");
        final String pathToFile = "info\\kgeorgiy\\ja\\leontev\\i18n\\cases\\test_average6";
        try (TextAnalyzer analyzer2 = new TextStatistics()) {
            TextStats stats2 = analyzer2.analyze(Locale.forLanguageTag("en-US"), pathToFile);
            Assert.assertEquals(stats2.generalStats, new GeneralStats(1, 7, 10, 3, 0));
            Assert.assertEquals(stats2.numberStats.average(), 5.5, 0.00001);
            Assert.assertEquals(stats2.moneyStats.averageSum(), 10.0, 0.000001);
        } catch (final Exception e) {
            System.err.println();
            Assert.fail();
        }
    }
}
