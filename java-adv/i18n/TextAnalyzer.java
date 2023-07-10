package info.kgeorgiy.ja.leontev.i18n;

import java.util.Locale;

public interface TextAnalyzer extends AutoCloseable {

    /**
     * Analyzes the text that is at {@code input}. Gives information about the amount
     * of sentences, words, dates, numbers, and money in {@code inLocale}
     * @param inLocale The locale the text is in
     * @param input The file with the text
     * @return {@code TextStats} from the text
     */
    TextStats analyze(Locale inLocale, String input);

    /**
     * Writes the analysis {@code textStats} to {@code output} with {@code outLocale}
     * @param outLocale the locale in which to write
     * @param output where to write to
     * @param textStats the analysis
     */
    void writeAnalysis(Locale outLocale, String output, TextStats textStats);
}
