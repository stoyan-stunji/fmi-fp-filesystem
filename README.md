# ФАЙЛОВА СИСТЕМА

СУ - ФМИ - Зимен семестър 2024/2025
- Изготвил: Стоян Стоянов Иванов
- Специалност: Информатика, 3 курс
- Факултетен номер: 9MI0400132
- Ръководител на курса: доц. д-р Трифон Трифонов

# 1. Увод:
Основната цел на проекта е създаването на програма, която симулира файлова система, реализирана на Haskell. Тази система предлага функционалности за създаване, управление, навигиране и визуализиране на файлове и папки в йерархична структура. Допълнително, програмата интегрира персонализиран анализатор (parser) за обработка на потребителски команди, предоставяйки интуитивно взаимодействие, наподобяващо Shell интерфейс, за ефективно управление на файловата система.

# 2. Функционалност

2.1. Дефиниране на файловата система:

Може да бъде един от следните два варианта:
- File String String: Представлява файл, който съдържа две текстови стойности. Първата е низ с името на файла (напр. "readme.md"), а втората - низ със съдържанието на файла (напр. "This is a basic file system structure.").
- Root String [FileSystem]: Представлява директория/папка с две части - низ с името на директорията (напр. "projects") и [FileSystem], който е списък от поддиректории или файлове вътре в тази директория.
Чрез deriving(Show) се позволява автоматично генериране на текстово представяне на данните.

2.2. Реализация на stack:

Бележка: Aбстрактен тип данни, който съдържа подредена, линейна последователност от елементи.

-	headStack: Връща елемента на върха на stack, ако има такъв. Входът е stack от произволен тип [a]. Резултатът е Maybe a, което означава: Just x, ако stack не е празен, и Nothing, ако е празен.
```yaml
headStack [1, 2, 3] ---> Just 1
```
-	pushStack: Добавя нов елемент към върха на stack. Входът са елемент и stack. Резултатът е нов stack с елемента, добавен накрая.
```yaml
pushStack 4 [1, 2, 3] ---> [1, 2, 3, 4]
```
-	popStack: Премахва елемента на върха на stack. Входът е stack. Резултатът е нов stack без последния елемент.
```yaml
popStack [1, 2, 3] ---> [1, 2]
```
-	topStack: Връща последния елемент в stack. Входът е stack. Резултатът е елементът на върха.
```yaml
topStack [1, 2, 3] ---> 3
```
Бележка: Функциите за head, push, pop и top са взаимствани от упражнения по Логическо програмиране, проведени през летен семестър 2022/2023. Преименувани са, за да няма препокриване с Prelude модула. Идеята за използване на стек е взета от тук.

2.3. Валидация:

-	isNameFolder: Проверява дали даденото име съвпада с името на директория/папка. Първият аргумент е името, което се проверява. Вторият аргумент е файловата система. Резултатът е True, ако съответното име съвпада с името на папката, или False в противен случай.
```yaml
isNameFolder "projects" (Root "projects" []) ---> True

isNameFolder "documents" (File "resume.pdf" "My resume") ---> False
```
- isNameFile: Проверява дали даденото име съвпада с името на файл. Първият аргумент е името, което се проверява. Вторият аргумент е файловата система. Резултатът е True, ако съответното име съвпада с името на файла, или False в противен случай.
```yaml
isNameFile "resume.pdf" (File "resume.pdf" "My resume") ---> True

isNameFile "welcome.txt" (Root "archives" []) ---> False
```
- isFilePath: Проверява дали даден низ представлява път. Аргументът е низ, който трябва да се провери. Резултатът е True, ако низът започва със /, или False в противен случай.
```yaml
isFilePath "/home/user/documents" ---> Резултат: True
```
- isValidName: Проверява дали дадено име, на файл или папка, е валидно в контекста на текущата файловата система. Първият аргумент е функция (напр. isNameFile или isNameFolder), която проверява дали дадено име съществува. Вторият аргумент е името за проверка. Третият аргумент е файловата система. Резултатът е True, ако името не съвпада с името на текущата папка и не се среща сред елементите в нея. Ако името на текущата папка е равно на входното име, функцията връща False - името не е валидно, тъй като вече съществува. За останалите елементи в директорията се използва foldr от библиотеката на Haskell - входната функция проверява дали даден елемент съответства на входното име.
```yaml
isValidName isNameFile "newfile.txt" (Root "documents" [File "resume.pdf" "My resume"]) ---> True т.е. файлът newfile.txt не съществува
``` 
