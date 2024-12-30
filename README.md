# ФАЙЛОВА СИСТЕМА

СУ - ФМИ - Зимен семестър 2024/2025
- Изготвил: Стоян Стоянов Иванов
- Специалност: Информатика, 3 курс
- Факултетен номер: 9MI0400132
- Ръководител на курса: доц. д-р Трифон Трифонов

# 1. Увод:
Основната цел на проекта е създаването на програма, която симулира файлова система, реализирана на Haskell. Тази система предлага функционалности за създаване, управление, навигиране и визуализиране на файлове и папки в йерархична структура. Допълнително, програмата интегрира персонализиран анализатор (parser) за обработка на потребителски команди, предоставяйки интуитивно взаимодействие, наподобяващо Shell интерфейс, за ефективно управление на файловата система.

# 2. Функционалност

### 2.1. Дефиниране на файловата система:

Може да бъде един от следните два варианта:
- **File String String**: Представлява файл, който съдържа две текстови стойности. Първата е низ с името на файла (напр. "readme.md"), а втората - низ със съдържанието на файла (напр. "This is a basic file system structure.").
- **Root String [FileSystem]**: Представлява директория/папка с две части - низ с името на директорията (напр. "projects") и [FileSystem], който е списък от поддиректории или файлове вътре в тази директория.
Чрез deriving(Show) се позволява автоматично генериране на текстово представяне на данните.

### 2.2. Реализация на stack:

*Бележка: Aбстрактен тип данни, който съдържа подредена, линейна последователност от елементи.*

-	**headStack**: Връща елемента на върха на stack, ако има такъв. Входът е stack от произволен тип [a]. Резултатът е Maybe a, което означава: Just x, ако stack не е празен, и Nothing, ако е празен.
```yaml
headStack [1, 2, 3] ---> Just 1
```
-	**pushStack**: Добавя нов елемент към върха на stack. Входът са елемент и stack. Резултатът е нов stack с елемента, добавен накрая.
```yaml
pushStack 4 [1, 2, 3] ---> [1, 2, 3, 4]
```
-	**popStack**: Премахва елемента на върха на stack. Входът е stack. Резултатът е нов stack без последния елемент.
```yaml
popStack [1, 2, 3] ---> [1, 2]
```
-	**topStack**: Връща последния елемент в stack. Входът е stack. Резултатът е елементът на върха.
```yaml
topStack [1, 2, 3] ---> 3
```
Бележка: Функциите за head, push, pop и top са взаимствани от упражнения по Логическо програмиране, проведени през летен семестър 2022/2023. Преименувани са, за да няма препокриване с Prelude модула. Идеята за използване на стек е взета от тук.

### 2.3. Валидация:

-	**isNameFolder**: Проверява дали даденото име съвпада с името на директория/папка. Първият аргумент е името, което се проверява. Вторият аргумент е файловата система. Резултатът е True, ако съответното име съвпада с името на папката, или False в противен случай.
```yaml
isNameFolder "projects" (Root "projects" []) ---> True
isNameFolder "documents" (File "resume.pdf" "My resume") ---> False
```
- **isNameFile**: Проверява дали даденото име съвпада с името на файл. Първият аргумент е името, което се проверява. Вторият аргумент е файловата система. Резултатът е True, ако съответното име съвпада с името на файла, или False в противен случай.
```yaml
isNameFile "resume.pdf" (File "resume.pdf" "My resume") ---> True
isNameFile "welcome.txt" (Root "archives" []) ---> False
```
- **isFilePath**: Проверява дали даден низ представлява път. Аргументът е низ, който трябва да се провери. Резултатът е True, ако низът започва със /, или False в противен случай.
```yaml
isFilePath "/home/user/documents" ---> Резултат: True
```
- **isValidName**: Проверява дали дадено име, на файл или папка, е валидно в контекста на текущата файловата система. Първият аргумент е функция (напр. isNameFile или isNameFolder), която проверява дали дадено име съществува. Вторият аргумент е името за проверка. Третият аргумент е файловата система. Резултатът е True, ако името не съвпада с името на текущата папка и не се среща сред елементите в нея. Ако името на текущата папка е равно на входното име, функцията връща False - името не е валидно, тъй като вече съществува. За останалите елементи в директорията се използва foldr от библиотеката на Haskell - входната функция проверява дали даден елемент съответства на входното име.
```yaml
isValidName isNameFile "newfile.txt" (Root "documents" [File "resume.pdf" "My resume"]) ---> True
т.е. файлът newfile.txt не съществува
``` 

### 2.4. Анализатор (Parser)

*Бележка: Parser – в този документ да се разбира като „анализатор“, а „parsing“ - „разбор“, „анализ“, като най-близък превод на български език в този контекст.*

- **Дефиниция**:  Това е нов тип данни Parser, който е обвивка около функция с тип: String -> Maybe (String, prs). Ако разборът е успешeн, връща Just (remainingString, parsedResult), където remainingString е низът, който остава след като е извършен разборът, а parsedResult е резултатът от разбора. Ако анализът не успее, връща Nothing. Идеята и имплементацията е взаимствана от тук и тук.
- **Functor Parser**: Позволява прилагане на функция към резултата от анализатора, като се запази структурата му. fmap прилага функция F към резултата от анализатора P. След като е извлечен резултатът Х, прилагаме F към Х и връщаме нов анализатор P’. Линк към Data.Functor
```yaml
uppercaseParser :: Parser String 
uppercaseParser = fmap (map toUpper) wordParser
```
- **Applicative Parser**: Позволява композиция на анализатори. Това е важно за комбиниране на анализатори, които правят разбор на различни части от входния низ. pure създава анализатор, който винаги връща дадена стойност X, без да се променя входния низ. (<*>) позволява прилагане на анализатор, който връща функция, върху друг анализатор, който връща аргумент. Левият анализатор анализира функция от тип a -> b. Десният анализатор анализира аргумент от тип a. Резултатът е нов анализатор, който връща резултат от тип b. Линк към Control.Applicative и Stackoverflow.
```yaml
Анализатор за събиране на две числа:
addParser :: Parser (Int -> Int -> Int)
addParser = pure (+)

Използване на анализатора за събиране с аргументи:
sumParser :: Parser Int
sumParser = (+) <$> addParser <*> intParser <*> intParser
```
- **Alternative Parser**: Предоставя възможност за избор между два анализатора. Ако първият не успее, може да се опита вторият. empty е функция, която никога не успява и връща Nothing. (<|>) позволява да се комбинират два анализатора така, че ако единият не успее, да се опита вторият. Ако и той не успее, резултатът ще бъде Nothing. Линк към Stackoverflow.
```yaml
Анализатор, който се опитва да направи разбор на числа, ако не успее, преминава към буквени низове.
numberOrWordParser :: Parser String
numberOrWordParser = many digit <|> many letter
``` 
