package info.kgeorgiy.ja.leontev.i18n;

import java.util.ListResourceBundle;

public class StatBundle_en_US extends ListResourceBundle {

    @Override
    protected Object[][] getContents() {
        return CONTENTS;
    }

    private final Object[][] CONTENTS = {
            {"genStat", "General Statistics: "},
            {"wordStat", "Word Statistics: "},
            {"moneyStat", "Money Statistics: "},
            {"sentenceStat", "Sentence Statistics"},
            {"numStat", "Number Statistics"},
            {"dateStat", "Date Statistics"},
            {"wordCount", "\tWord Count: "},
            {"senCount", "\tSentence Count: "},
            {"numCount", "\tNumber Count: "},
            {"moneyCount", "\tMoney Count: "},
            {"dateCount", "\tDates Count: "},
            {"minSen", "\tMinimal Sentence: "},
            {"maxSen", "\tMaximal Sentence: "},
            {"minByLen", "\tMinimal Sentence by length: "},
            {"maxByLen", "\tMaximal Sentence by length: "},
            {"avgLen", "\tAverage Sentence Length: "},
            {"minWord", "\tMinimal word: "},
            {"maxWord", "\tMaximal word: "},
            {"minWordByLen", "\tMinimal word by length: "},
            {"maxWordByLen", "\tMaximal word by length: "},
            {"avgWordLen", "\tAverage length of word: "},
            {"minNum", "\tSmallest Number: "},
            {"maxNum", "\tBiggest Number: "},
            {"avgNum", "\tAverage Number: "},
            {"minSum", "\tMinimal Sum: "},
            {"maxSum", "\tMaximal Sum: "},
            {"avgSum", "\tAverage Sum: "},
            {"minDate", "\tEarliest Date: "},
            {"maxDate", "\tMost Recent Date: "},
            {"avgDate", "\tAverage Date: "},
            {"unq", "unique"},
    };
}
