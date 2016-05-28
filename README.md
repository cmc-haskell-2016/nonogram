# nonogram

[![Build Status](https://travis-ci.org/cmc-haskell-2016/nonogram.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/nonogram)

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

`stack`:

# Инструкция по установке:

http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu

`git`:

```
--sudo apt-get install git
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
