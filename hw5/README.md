# HW5: алгоритм Петерсона в NuSMV

Модель взаимного исключения Петерсона для двух процессов (`peterson.smv`), 5 проверок (2 CTL + 3 LTL), отчёт в [REPORT.md](REPORT.md).

## Запуск

Нужен NuSMV 2.6.0 (проверялось на `NuSMV-2.6.0-Linux`, linux64).

```sh
NUSMV=/путь/к/NuSMV ./run.sh     # если NuSMV в PATH, переменную можно не задавать
```

Скрипт сохраняет результаты проверок в `logs/`. Отдельные команды:

```sh
NuSMV peterson.smv                    # BDD, все SPEC и LTLSPEC
NuSMV -bmc -bmc_length 12 peterson.smv   # SAT-based BMC, только LTL
NuSMV -coi -dynamic peterson.smv      # cone of influence + динамический порядок переменных
```

## Файлы

| Файл | Что это |
|---|---|
| `peterson.smv` | модель и свойства |
| `run.sh` | все прогоны, пишет `logs/*` |
| `logs/` | выводы NuSMV: основной, BMC, COI, достижимые состояния |
| `REPORT.md` | свойства, результаты, трассы, обоснования |
