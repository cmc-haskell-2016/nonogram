# nonogram

[![Build Status](https://travis-ci.org/cmc-haskell-2016/nonogram.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/nonogram)

![Iron man is waiting for you] (app/iron.png)

## Подготовка к установке и запуску

Для сборки и запуска приложения, возможно, понадобится установка следующих утилит:

`apt-get`:

```
sudo apt-get update
```

`ghci`:

```
sudo apt-get install ghc
```

`git`:

```
sudo apt-get install git
```

`LLVM`:

```
sudo apt-get install llvm
```

`cabal` и используемые библиотеки:

```
sudo apt-get install cabal-install
cabal update
cabal install gloss-examples
cabal install containers
```

Инструкция по установке `stack`:

http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu

## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/nonogram.git
cd nonogram
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec nonogram
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec nonogram
```

### Управление

Для старта игры введите путь до задачи (папка examples).

Назначение клавиш:

- <kbd>g</kbd> подсказка на основе пересекающихся блоков
- <kbd>s</kbd> подсказка на основе блоков, расположенных по периметру поля
- <kbd>с</kbd> подсказка, заполняющая крестиками верно закрашенные строки и
  столбцы  
- <kbd>p</kbd> подсказка, заполняющая крестиками клетки, которые точно не будут
  закрашены
- <kbd>a</kbd> подсказка, обрамляющая крестиками заполненные блоки 
- <kbd>k</kbd> проверка текущего состояния кроссворда на правильность
- <kbd>o</kbd> автоматическое решение кроссворда

Для выхода из игры нажмите <kbd>ESC</kbd>.
