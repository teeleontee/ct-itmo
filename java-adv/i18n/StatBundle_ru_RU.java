package info.kgeorgiy.ja.leontev.i18n;

import java.util.ListResourceBundle;

public class StatBundle_ru_RU extends ListResourceBundle {
    @Override
    protected Object[][] getContents() {
        return CONTENTS;
    }

    private final Object[][] CONTENTS = {
            {"genStat", "Сводная статистика: "},
            {"wordStat", "Статистика по словам"},
            {"moneyStat", "Статистика по суммам денег"},
            {"sentenceStat", "Статистика по предложениям"},
            {"numStat", "Статистика по числам"},
            {"dateStat", "Статистика по датам"},
            {"wordCount", "\tЧисло слов: "},
            {"senCount", "\tЧисло предложений: "},
            {"numCount", "\tЧисло чисел: "},
            {"moneyCount", "\tЧисло сумм: "},
            {"dateCount", "\tЧисло дат: "},
            {"minSen", "\tМинимальное предложение: "},
            {"maxSen", "\tМаксимальное предложение: "},
            {"minByLen", "\tМинимальная длина предложения: "},
            {"maxByLen", "\tМаксимальная длина предложения: "},
            {"avgLen", "\tСредняя длина предложения: "},
            {"minWord", "\tМинимальное слово: "},
            {"maxWord", "\tМаксимальное слово: "},
            {"minWordByLen", "\tМинимальная длина слова: "},
            {"maxWordByLen", "\tМаксимальная длина слова: "},
            {"avgWordLen", "\tСредняя длина слова: "},
            {"minNum", "\tМинимальное число: "},
            {"maxNum", "\tМаксимальное число: "},
            {"avgNum", "\tСреднее число: "},
            {"minSum", "\tМинимальная сумма: "},
            {"maxSum", "\tМаксимальная сумма: "},
            {"avgSum", "\tСредняя сумма: "},
            {"minDate", "\tМинимальная дата: "},
            {"maxDate", "\tМаксимальная дата: "},
            {"avgDate", "\tСредняя дата: "},
            {"unq", "различных"},
    };
}
