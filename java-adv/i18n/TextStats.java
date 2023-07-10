package info.kgeorgiy.ja.leontev.i18n;

import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.ResourceBundle;

record GeneralStats(int sentences, int words, int numbers, int moneys, int dates) {

}

record SentenceStats(int count, String min, String max, String minByLength, String maxByLength, double average) {

}

record WordStats(int count, String min, String max, String minByLength, String maxByLength, double average) {

}

record NumberStats(int count, double min, double max, double average) {

}

record MoneyStats(int count, double min, double max, double averageSum) {

}

record DateStats(int count, Date minDate, Date maxDate, Date averageDate) {

}

public class TextStats {
    GeneralStats generalStats;
    SentenceStats sentenceStats;
    WordStats wordsStats;
    NumberStats numberStats;
    MoneyStats moneyStats;
    DateStats dateStats;

    public TextStats(GeneralStats generalStats,
                     SentenceStats sentenceStats,
                     WordStats wordsStats,
                     NumberStats numberStats,
                     MoneyStats moneyStats,
                     DateStats dateStats)
    {
        this.generalStats = generalStats;
        this.sentenceStats = sentenceStats;
        this.wordsStats = wordsStats;
        this.numberStats = numberStats;
        this.moneyStats = moneyStats;
        this.dateStats = dateStats;
    }

    public String toOutput(ResourceBundle bundle) {
        NumberFormat moneyFormat = NumberFormat.getCurrencyInstance(bundle.getLocale());
        DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, bundle.getLocale());
        String minDate, maxDate, avgDate;
        if (dateStats.count() == 0) {
            minDate = "";
            maxDate = "";
            avgDate = "";
        } else {
            minDate = dateFormat.format(dateStats.minDate());
            maxDate = dateFormat.format(dateStats.maxDate());
            avgDate = dateFormat.format(dateStats.averageDate());
        }
        return String.format(
                "%s" + System.lineSeparator() + // genStat
                        "%s%s" + System.lineSeparator() + // senCount
                        "%s%s" + System.lineSeparator() + // wordCnt
                        "%s%s" + System.lineSeparator() + // numCnt
                        "%s%s" + System.lineSeparator() + // moneyCnt
                        "%s%s" + System.lineSeparator() + // dateCnt
                "%s" + System.lineSeparator() + // senStat
                        "%s%s" + System.lineSeparator() + // senCnt
                        "%s%s" + System.lineSeparator() + // senMin
                        "%s%s" + System.lineSeparator() + // senMax
                        "%s%s" + System.lineSeparator() + // senMinLen
                        "%s%s" + System.lineSeparator() + // senMaxLen
                        "%s%s" + System.lineSeparator() + // avgLenSen
                "%s" + System.lineSeparator() + // wordStat
                        "%s%s" + System.lineSeparator() + // wordCnt
                        "%s%s" + System.lineSeparator() + // wordMin
                        "%s%s" + System.lineSeparator() + // wordMax
                        "%s%s" + System.lineSeparator() + // wordMinLen
                        "%s%s" + System.lineSeparator() + // wordMaxLen
                        "%s%s" + System.lineSeparator() + // wordAvgLen
                "%s" + System.lineSeparator() + // numStat
                        "%s%s" + System.lineSeparator() + // numCnt
                        "%s%s" + System.lineSeparator() + // minCnt
                        "%s%s" + System.lineSeparator() + // maxCnt
                        "%s%s" + System.lineSeparator() + // avgCnt
                "%s" + System.lineSeparator() + // moneyStat
                        "%s%s" + System.lineSeparator() + // moneyCnt
                        "%s%s" + System.lineSeparator() + // moneyMin
                        "%s%s" + System.lineSeparator() + // moneyMax
                        "%s%s" + System.lineSeparator() + // moneyAvg
                "%s" + System.lineSeparator() + // date
                        "%s%s" + System.lineSeparator() + // dateCnt
                        "%s%s" + System.lineSeparator() + // dateMin
                        "%s%s" + System.lineSeparator() + // dateMax
                        "%s%s" + System.lineSeparator(),  // dateAvg
                bundle.getString("genStat"),
                bundle.getString("senCount"),     sentenceStats.count(),
                bundle.getString("wordCount"),    wordsStats.count(),
                bundle.getString("numCount"),     numberStats.count(),
                bundle.getString("moneyCount"),   moneyStats.count(),
                bundle.getString("dateCount"),    dateStats.count(),
                bundle.getString("sentenceStat"),
                bundle.getString("senCount"),     sentenceStats.count(),
                bundle.getString("minSen"),       sentenceStats.min(),
                bundle.getString("maxSen"),       sentenceStats.max(),
                bundle.getString("minByLen"),     sentenceStats.minByLength(),
                bundle.getString("maxByLen"),     sentenceStats.maxByLength(),
                bundle.getString("avgLen"),       sentenceStats.average(),
                bundle.getString("wordStat"),
                bundle.getString("wordCount"),    wordsStats.count(),
                bundle.getString("minWord"),      wordsStats.min(),
                bundle.getString("maxWord"),      wordsStats.max(),
                bundle.getString("minWordByLen"), wordsStats.minByLength(),
                bundle.getString("maxWordByLen"), wordsStats.maxByLength(),
                bundle.getString("avgWordLen"),   wordsStats.average(),
                bundle.getString("numStat"),
                bundle.getString("numCount"),     numberStats.count(),
                bundle.getString("minNum"),       numberStats.min(),
                bundle.getString("maxNum"),       numberStats.max(),
                bundle.getString("avgNum"),       numberStats.average(),
                bundle.getString("moneyStat"),
                bundle.getString("moneyCount"),   moneyStats.count(),
                bundle.getString("minSum"),       moneyFormat.format(moneyStats.min()),
                bundle.getString("maxSum"),       moneyFormat.format(moneyStats.max()),
                bundle.getString("avgSum"),       moneyFormat.format(moneyStats.averageSum()),
                bundle.getString("dateStat"),
                bundle.getString("dateCount"),    dateStats.count(),
                bundle.getString("minDate"),      minDate,
                bundle.getString("maxDate"),      maxDate,
                bundle.getString("avgDate"),      avgDate
                );
    }
}
