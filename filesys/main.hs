import Control.Applicative ( Alternative((<|>), empty) )
import Data.Char

-- [STOYAN IVANOV, INFORMATICS, 9MI0400132, CRS_3, GRP_1]

-- [FILE_SYSTEM]
-- Explanation: File <name> <val> & Root <name> [<conts>].

data FileSystem = File String String | Root String [FileSystem]
    deriving(Show)

mySystem :: FileSystem
mySystem = Root "/"
    [ File "welcome.txt" "Welcome to the file system!",
      File "readme.md" "This is a basic file system structure.",
      Root "projects"
        [ Root "project1"
            [ File "main.cpp" "C++ project source code",
              File "README.md" "Project 1 documentation"
            ],
          Root "project2"
            [ File "index.html" "HTML project page",
              File "style.css" "CSS for the project page"
            ]
        ],
      Root "documents"
        [ Root "work"
            [ File "report.docx" "Work report for the year",
              File "budget.xlsx" "Work budget for the year"
            ],
          Root "personal"
            [ File "resume.pdf" "My resume",
              File "vacation_plans.txt" "Vacation plans for next summer"
            ]
        ],
      Root "archives"
        [ Root "2021"
            [ File "old_report.pdf" "Report from 2021",
              File "old_budget.xlsx" "Budget from 2021"
            ],
          Root "2022"
            [ File "annual_review.docx" "Review of the year 2022"
            ]
        ]
    ]

-- [STACK]
-- Explanation: Basic Data Structures implementation;
--              Renamed so not ambigious with built-in head & etc.

headStack :: [a] -> Maybe a
headStack (x:xs) = Just x
headStack _ = Nothing

pushStack :: a -> [a] -> [a]
pushStack el st = st ++ [el]

popStack :: [a] -> [a]
popStack [x] = []
popStack (x:xs) = x : popStack xs

topStack :: [a] -> a
topStack [x] = x
topStack (x:xs) = topStack xs

-- [FILE_SYSTEM_UTILS]

isNameFolder :: String -> FileSystem -> Bool
isNameFolder name1 (Root name2 _) = name1 == name2
isNameFolder _ _ = False

isNameFile :: String -> FileSystem -> Bool
isNameFile name1 (File name2 _) = name1 == name2
isNameFile _ _ = False

isFilePath :: String -> Bool
isFilePath ('/':_) = True
isFilePath _ = False

isValidName :: (String -> FileSystem -> Bool) -> String -> FileSystem -> Bool
isValidName foo name1 (Root name2 xs) = name2 /= name1 && foldr (\x results -> foo name1 x || results) False xs
isValidName _ _ _ = True

-- [PARSERS]

newtype Parser prs = Parser {runParser :: String -> Maybe (String, prs)}

-- ChatGPT: Make me an abstract Haskell data type definition. It should be Parser structure, 
--          which has a container labeled Parser that holds a function runParser, which is a 
--          transformation process. The function runParser takes a string input and return: a 
--          tuple with a remaining string and parsed result and nothing - an empty state.
-- Explanation: Renamed most of the variables.

-- Explanation: From hackage.haskell.org -> Data.Functor
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap foo (Parser p) = Parser $ \input -> do
        (inputCopy, x) <- p input
        Just (inputCopy, foo x)

-- Explanation: pure & <*> taken from hackage.haskell.org -> Control.Applicative
instance Applicative Parser where

    pure :: a -> Parser a
    pure x = Parser $ \y -> Just (y, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser left) <*> (Parser right) = Parser $ \x -> do
        (x1, foo) <- left x
        (x2, el) <- right x1
        Just (x2, foo el)

-- Explanation: <|> read from stackoverflow.com/questions/26002415/what-does-haskells-operator-do
instance Alternative Parser where

    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser left) <|> (Parser right) = Parser $ \x -> left x <|> right x

-- [INDIVIDUAL_PARSERS]

parserForChar :: Char -> Parser Char
parserForChar x = Parser foo
    where
        foo [] = Nothing
        foo (y:ys)
          | y == x = Just (ys, x)
          | otherwise = Nothing

-- Explanation: traverse taken from hackage.haskell.org -> Data.Traversable
parserForString :: String -> Parser String
parserForString = traverse parserForChar

-- Explanation: span taken from hoogle.haskell.org
parserForMany :: (Char -> Bool) -> Parser String
parserForMany foo = Parser $ \input -> let (singular, rest) = span foo input in Just (rest, singular)

-- Explanation: isSpace from Data.Char
parserForWhiteSpace :: Parser String
parserForWhiteSpace = parserForMany isSpace

-- Explanation: *> taken from hackage.haskell.org -> Control.Applicative
parserForSlash :: Parser String
parserForSlash = parserForChar '/' *> parserForMany (/= '/')

getNextDirectory :: String -> Maybe (String, String)
getNextDirectory "" = Just ("", "")
getNextDirectory a = runParser parserForSlash a

parserForCd :: Parser String
parserForCd = parserForString "cd"

parserForPwd :: Parser String
parserForPwd = parserForString "pwd"

parserForLs :: Parser String
parserForLs = parserForString "ls"

parserForShow :: Parser String
parserForShow = parserForString "show"

parserForCat :: Parser String
parserForCat = parserForString "cat"

parserForQuit :: Parser String
parserForQuit = parserForString "quit"

parserForRm :: Parser String
parserForRm = parserForString "rm"

-- Explanation: <|> read from stackoverflow.com/questions/26002415/what-does-haskells-operator-do
parserForMk :: Parser String
parserForMk = parserForString "mkdir" <|> parserForString "mkfile"

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
parserForWord :: String -> Maybe (String, String)
parserForWord = runParser $ parserForMany (/= ' ') <* parserForWhiteSpace

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
parserForEndFile :: String -> Maybe (String, String)
parserForEndFile = runParser $ parserForMany (/= '~') <* parserForChar '~' <* parserForWhiteSpace

-- Explanation: <|> read from stackoverflow.com/questions/26002415/what-does-haskells-operator-do
parserForCommand :: Parser String
parserForCommand = parserForCd <|> parserForPwd <|> parserForLs <|> parserForShow <|> parserForCat <|> parserForRm <|> parserForQuit <|> parserForMk

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
parseCommand :: String -> Maybe (String, String)
parseCommand = runParser (parserForCommand <* parserForWhiteSpace)

-- [GET_FILE_SYSTEM_UTILS]

getFile :: String -> [FileSystem] -> Maybe FileSystem
getFile name xs = case filter (isNameFile name) xs of
        [] -> Nothing
        (x:_) -> Just x

getFolder :: String -> FileSystem -> Maybe FileSystem
getFolder name (Root _ xs) = case filter (isNameFolder name) xs of
        [] -> Nothing
        (x:_) -> Just x
getFolder _ _ = Nothing

getNameOfRoot :: FileSystem -> Maybe String
getNameOfRoot (Root name _) = Just name
getNameOfRoot _ = Nothing

getFileFromRoot :: String -> FileSystem -> Maybe FileSystem
getFileFromRoot name (Root _ xs) = getFile name xs
getFileFromRoot _ _ = Nothing

getFileByDirectory :: String -> FileSystem -> Maybe FileSystem
getFileByDirectory input x@(Root _ xs) = case getNextDirectory input of
        Just ("", file) -> getFile file xs
        Just (rest, cur) -> case getFolder cur x of
            Nothing -> Nothing
            Just curFolder -> getFileByDirectory rest curFolder
getFileByDirectory _ _ = Nothing

-- [ADD_&_REMOVE]

-- Explanation: ChatGPT rewrote 'add'. Prompt was a frustrated "fix" and almost all of the code.
add :: String -> FileSystem -> Maybe FileSystem -> Maybe FileSystem
add path temp (Just old@(Root name xs)) = case getNextDirectory path of
    Just ("", "") -> Just (Root name (temp : xs))
    Just (rest, cur) -> case changeDirectories cur xs of
        Nothing -> Nothing
        Just directory@(Root path files) -> case add rest temp (Just directory) of
            Nothing -> Nothing
            Just new@(Root dir xs) -> Just (changeRoot new old)
    Nothing -> Nothing
add _ _ _ = Nothing

addFile :: String -> String -> String -> Maybe FileSystem -> Maybe FileSystem
addFile path name value = add path (File name value)

addFolder :: String -> String -> Maybe FileSystem -> Maybe FileSystem
addFolder path name = add path (Root name [])

removeFileFromRootHelper :: String -> [FileSystem] -> [FileSystem]
removeFileFromRootHelper name (x@(File otherName _) : xs)
    | name == otherName = xs
    | otherwise = x : removeFileFromRootHelper name xs
removeFileFromRootHelper name (x : xss) = x : removeFileFromRootHelper name xss
removeFileFromRootHelper _ [] = []

removeFileFromRoot :: String -> FileSystem -> FileSystem
removeFileFromRoot name (Root name1 xs) = Root name1 (removeFileFromRootHelper name xs)
removeFileFromRoot _ x = x

removeFileFromPathHelper :: String -> FileSystem -> FileSystem
removeFileFromPathHelper input root@(Root name xs) = case getNextDirectory input of
    Just ("", fileName) -> removeFileFromRoot fileName root
    Just(nextPath, curPath) -> case changeDirectories curPath xs of
        Nothing -> root
        Just next -> removeFileFromPathHelper nextPath next
removeFileFromPathHelper _ fileSystem = fileSystem

removeFileFromPath :: String -> FileSystem -> FileSystem
removeFileFromPath input root@(Root _ _) = if isFilePath input then changeRoot (removeFileFromPathHelper input root) root else removeFileFromRoot input root
removeFileFromPath _ fileSystem = fileSystem

-- [OTHER_FILE_SYSTEM_UTILS]

fancyList :: [Maybe a] -> Maybe [a]
fancyList [] = Just []
fancyList ((Just x) : xs) = case fancyList xs of
        (Just result) -> Just (x : result)
        Nothing -> Nothing
fancyList (Nothing : xs) = fancyList xs

filesToString :: [FileSystem] -> [String]
filesToString (Root name _ : xs) = foo xs : filesToString xs
    where foo :: [FileSystem] -> String
          foo (Root "/" _ : xss) = "/" ++ foo xss
          foo (Root name _ : xss) = "/" ++ name ++ foo xss
          foo _ = ""
filesToString _ = []

catFiles :: String -> FileSystem -> FileSystem -> Maybe FileSystem
catFiles newName (File _ contents1) (File _ contents2) = Just $ File newName (contents1 ++ contents2)
catFiles _ _ _ = Nothing

changeDirectories :: String -> [FileSystem] -> Maybe FileSystem
changeDirectories name xs = headStack $ filter (isNameFolder name) xs

changeRootHelper :: FileSystem -> [FileSystem] -> [FileSystem]
changeRootHelper new@(Root oldName oldXs) (old@(Root newName xss) : newXs)
        | oldName == newName  = new : newXs
        | otherwise = Root newName (changeRootHelper new xss) : changeRootHelper new newXs
changeRootHelper new (old : newXs) = old : changeRootHelper new newXs
changeRootHelper _ x = x

changeRoot :: FileSystem -> FileSystem -> FileSystem
changeRoot new old@(Root name xs) = case getNameOfRoot new of
        Nothing -> old
        Just name1 -> if name1 == name then new else Root name $ changeRootHelper new xs

printFile :: FileSystem -> String
printFile (File name content) = "file_name: " ++ name ++ "\ncontent: \n" ++ content ++ "\n"
printFile _ = "printFile::incorrect_error_type\n"

printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs)   = n ++ "/" ++ printSystem xs
printSystem _ = ""

printRoot :: FileSystem -> String
printRoot (Root n _) = "root: " ++ n ++ "\n"
printRoot (File n _) = "file: " ++ n ++ "\n"

-- [MAIN_FUNCTIONS]

-- [PWD]

pwd :: [FileSystem] -> IO()
pwd s = let system = printSystem s in
    putStrLn ("path:\n" ++ system)

-- [CD]

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input xs = case getNextDirectory input of
    Just ("", "") -> Just xs
    Just (rest, "..") -> case popStack xs of
        [] -> Nothing
        back -> cd rest back
    Just (rest, cur) -> case changeDirectories cur xs1 of
        Just result -> cd rest (pushStack result xs)
        Nothing  -> Nothing
    Nothing -> Nothing
 where system@(Root name xs1) = topStack xs

-- [LS]

lsHelper :: String -> [FileSystem] -> Maybe [FileSystem]
lsHelper input system = case parseCommand input of
    Nothing -> Nothing
    Just (rest, curr) -> case curr of
        "cd" -> cd ("/" ++ rest) system
        "mkdir" -> mkdir rest system
        "mkfile" -> mkfile rest system
        "rm" -> Just $ rm rest system
        "cat" -> Just $ cat rest system
        _ -> Nothing

ls :: String -> Maybe FileSystem -> String
ls input (Just system) = case lsHelper ("cd" ++ input) [system] of
    (Just result) -> case topStack result of
        (Root _ a) -> concatMap printRoot a
        _ -> ""
    _ -> ""
ls _ _ = ""

-- [CAT]

catHelper :: FileSystem -> String -> [FileSystem] -> [FileSystem]
catHelper cur input system = case parserForWord input of
    Just (name, ">") -> case catFiles name (File "" "") cur of
            Nothing -> system
            Just (File name1 content1) -> case mkfile (name1 ++ " " ++ content1 ++ "~") system of
                    Nothing -> system
                    Just result -> result
    Just ("", "") -> system
    Just (rest, curr) -> if isFilePath curr then
            case getFileByDirectory curr (head system) of
                Nothing -> catHelper cur rest system
                Just file -> case catFiles "" file cur of
                        Nothing -> catHelper cur rest system
                        Just result -> catHelper result rest system
        else case getFileFromRoot curr (topStack system) of
                Nothing -> catHelper cur rest system
                Just file -> case catFiles "" file cur of
                        Nothing -> catHelper cur rest system
                        Just result -> catHelper result rest system
    Nothing -> system

cat :: String -> [FileSystem] -> [FileSystem]
cat = catHelper (File "" "")

rm :: String -> [FileSystem] -> [FileSystem]
rm _ [] = []
rm input xs = case parserForWord input of
    Just ("", last) -> let paths = zip xs (map (\a -> a ++ "/" ++ last) (filesToString xs)) in
        map (\(a, b) -> removeFileFromPath b a) paths
    Just (rest, cur) -> let paths = zip xs (map (\a -> a ++ "/" ++ cur) (filesToString xs)) in
        rm rest $ map (\(a, b) -> removeFileFromPath b a) paths
    Nothing -> xs

-- [UTILS]

mkdirHelper :: String -> [FileSystem] -> Maybe [FileSystem]
mkdirHelper input system = if isValidName isNameFolder input (topStack system)
    then Nothing else let paths = zip system (filesToString system) in
         fancyList $ map (\(a, b) -> addFolder b input (Just a)) paths

mkdir :: String -> [FileSystem] -> Maybe [FileSystem]
mkdir input system = case parserForWord input of
    Just ("", last) -> case foo last system of
        Nothing -> Just system
        (Just result) -> Just result
    Just (rest1, cur1) -> case foo cur1 system of
        Nothing -> mkdir rest1 system
        (Just system1) -> mkdir rest1 system1
  where
    foo :: String -> [FileSystem] -> Maybe [FileSystem]
    foo = mkdirHelper

mkfile :: String -> [FileSystem] -> Maybe [FileSystem]
mkfile input system = case parserForWord input of
    Just (rest, name) -> case parserForEndFile rest of
        Just(_, content) -> if isValidName isNameFile name (topStack system) then Nothing
        else let paths = zip system (filesToString system) in
            fancyList $ map (\(a, b) -> addFile b name content (Just a)) paths
        Nothing -> Nothing
    Nothing -> Nothing

showFS :: String -> FileSystem -> String
showFS name (Root _ rest) = case getFile name rest of
    Nothing -> "show::no_such_file\n"
    (Just result) -> printFile result
showFS _ _ = "show::error\n"

-- [RUN]

run :: [FileSystem] -> IO()
run xs = do
    putStr $ printSystem xs ++ "^ "
    input <- getLine
    case lsHelper input xs of
        Nothing -> case parseCommand input of
            Just (_, "quit") -> putStrLn "Goodbye! :D"
            Just (_, "pwd") -> do pwd xs
                                  run xs
            Just (rest, "ls") -> case rest of
                ('/' : path)  -> do putStr $ ls path $ Just $ head xs
                                    run xs
                _             -> do putStr $ ls rest $ Just $ topStack xs
                                    run xs
            Just (l, "show") -> do putStr $ showFS l $ topStack xs
                                   run xs
            _  -> run xs
        Just result -> run result

-- [RUNNER]

main :: IO()
main = run [mySystem]

