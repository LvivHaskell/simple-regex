## Регулярные выражения

Сброка:
```
cabal new-build
```

Запуск:
```
cabal new-exec regex <регулярное выражение>
```

Пример. Поиск процессов, запущенных браузером Google Chrome:
```
ps aux | cabal new-exec regex ".*chrome.*"
```
