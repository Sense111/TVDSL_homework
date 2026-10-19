# HW1: лексер Funny

Лексер для Funny на Clojure.

## Запуск

Все команды выполняются из каталога `hw1`:

```sh
lein test
lein run -m hw1.main tokens "function f() returns r:int { r = 0; }"
lein run -m hw1.main table
lein run -m hw1.main examples
```

Если не передать исходный текст после `tokens`, программа читает его из stdin. Команда `table` выводит таблицу переходов и размеры автомата, а `examples` запускает примеры.

Правила токенов, устройство автомата и результаты измерения покрытия описаны в [отчёте](REPORT.md).
