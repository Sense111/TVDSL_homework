# T1: семантика, IR, оптимизации и кодогенерация в JVM-байт-код

Контрольная точка T1 проекта (компилятор Funny). Фронтенд берётся из `t0` (лексер, парсер, типы), IR и оптимизации из `hw4`.

## Конвейер

Строка Funny -> `t0.pipeline/run` (типы hw3) -> `t1.semantic` (запись в массив только после присваивания) -> `hw4.ir/gen-module` -> `hw4.opt/optimize-module` -> `t1.codegen` (ASM 9.9.1) -> `.class` в памяти.

`t1.compile/compile-source` возвращает `{:class-name :bytes}` или `{:errors [...]}`. Третий аргумент — опции: `{:optimize false}` выключает оптимизации, флаги `hw4.opt` (`:const-prop :algebra :dce :cse`) передаются как есть.

## Рантайм

`java/t1/ImmutableIntArray.java`: `int length()`, `int get(int)`, `ImmutableIntArray with(int,int)`, статические `of(int...)` и `copyOf(int[])`. Выход за границы и деление на 0 дают штатные исключения JVM.

## Запуск

Из каталога `t1`:

```sh
lein test
```

Примеры Funny: `examples/fill_ok.funny` (корректная запись в массив), `examples/fill_bad.funny` (запись до присваивания, ошибка `:uninitialized-array`).

## Ограничения T1

- Кортеж из нескольких возвратов: только `int[]` для чисто целочисленных; если есть массив, метод возвращает `Object[]` (см. docstring `codegen.clj`).
- CLI, который выдаёт `.class` на диск, и контракты `requires/ensures/invariant/assert/assume` не реализованы: это финал и бонус.
