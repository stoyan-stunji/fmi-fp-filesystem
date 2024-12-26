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

charParser :: Char -> Parser Char
charParser x = Parser foo
    where
        foo [] = Nothing
        foo (y:ys)
          | y == x = Just (ys, x)
          | otherwise = Nothing

-- Explanation: traverse taken from hackage.haskell.org -> Data.Traversable
stringParser :: String -> Parser String
stringParser = traverse charParser

-- Explanation: span taken from hoogle.haskell.org
manyParser :: (Char -> Bool) -> Parser String
manyParser foo = Parser $ \input -> let (singular, rest) = span foo input in Just (rest, singular)

-- Explanation: isSpace from Data.Char
whiteSpaceParser :: Parser String 
whiteSpaceParser = manyParser isSpace

-- Explanation: *> taken from hackage.haskell.org -> Control.Applicative
slashParser :: Parser String
slashParser = charParser '/' *> manyParser (/= '/')

getNextDirectory :: String -> Maybe (String, String)
getNextDirectory "" = Just ("", "")
getNextDirectory x = runParser slashParser x

cdParser :: Parser String
cdParser = stringParser "cd"

pwdParser :: Parser String 
pwdParser = stringParser "pwd"

lsParser :: Parser String 
lsParser = stringParser "ls"

showParser :: Parser String 
showParser = stringParser "show" 

catParser :: Parser String 
catParser = stringParser "cat"

quitParser :: Parser String 
quitParser = stringParser "quit"

rmParser :: Parser String 
rmParser = stringParser "rm"

-- Explanation: <|> read from stackoverflow.com/questions/26002415/what-does-haskells-operator-do
directoryParser :: Parser String 
directoryParser = stringParser "mkdir" <|> stringParser "mkfile"

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
wordParser :: String -> Maybe (String, String)
wordParser = runParser $ manyParser (/= ' ') <* whiteSpaceParser

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
eofParser :: String -> Maybe (String, String) 
eofParser = runParser $ manyParser (/= '~') <* charParser '~' <* whiteSpaceParser

-- Explanation: <|> read from stackoverflow.com/questions/26002415/what-does-haskells-operator-do
cmdParser :: Parser String 
cmdParser = cdParser <|> pwdParser <|> lsParser <|> catParser <|> rmParser <|> quitParser <|> directoryParser <|> showParser

-- Explanation: <* taken from hackage.haskell.org -> Control.Applicative
parseCmd :: String -> Maybe (String, String)
parseCmd = runParser (cmdParser <* whiteSpaceParser)

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
            Just new@(Root dir xs) -> Just (changeEntity new old)
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
removeFileFromPath input root@(Root _ _) = if isFilePath input then changeEntity (removeFileFromPathHelper input root) root else removeFileFromRoot input root
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

changeEntityHelper :: FileSystem -> [FileSystem] -> [FileSystem]
changeEntityHelper new@(Root oldName oldXs) (old@(Root newName xss) : newXs)
        | oldName == newName  = new : newXs
        | otherwise = Root newName (changeEntityHelper new xss) : changeEntityHelper new newXs
changeEntityHelper new (old : newXs) = old : changeEntityHelper new newXs
changeEntityHelper _ x = x

changeEntity :: FileSystem -> FileSystem -> FileSystem
changeEntity new old@(Root name xs) = case getNameOfRoot new of 
        Nothing -> old
        Just name1 -> if name1 == name then new else Root name $ changeEntityHelper new xs

printFile :: FileSystem -> String
printFile (File name content) = "file_name: " ++ name ++ "\ncontent: \n" ++ content ++ "\n"
printFile _ = "printFile::incorrect_error_type\n"

printSystem :: [FileSystem] -> String
printSystem ((Root "/" _) : xs) = "/" ++ printSystem xs
printSystem ((Root n _) : xs)   = n ++ "/" ++ printSystem xs
printSystem _ = ""

printEntity :: FileSystem -> String 
printEntity (Root n _) = "root: " ++ n ++ "\n"
printEntity (File n _) = "file: " ++ n ++ "\n"

-- [MAIN_FUNCTIONS]

cd :: String -> [FileSystem] -> Maybe [FileSystem]
cd input xs = case getNextDirectory input of
    Just ("", "")     -> Just xs
    Just (rest, "..") -> case popStack xs of 
        []   -> Nothing 
        back -> cd rest back
    Just (rest, curr) -> case changeDirectories curr xs' of
        Just res -> cd rest (pushStack res xs)
        Nothing  -> Nothing
    Nothing           -> Nothing 
 where syst@(Root name xs') = topStack xs

mkdir :: String -> [FileSystem] -> Maybe [FileSystem]
mkdir input syst = case wordParser input of 
    Just ("", last)     -> case makeDir last syst of 
        Nothing      -> Just syst
        (Just res)   -> Just res
    Just (rest', curr') -> case makeDir curr' syst of
        Nothing      -> mkdir rest' syst
        (Just syst') -> mkdir rest' syst'
 where
     makeDir :: String -> [FileSystem] -> Maybe [FileSystem]
     makeDir cInput cSystem = if isValidName isNameFolder cInput (topStack cSystem) then Nothing 
     else let paths = zip cSystem (filesToString cSystem) in 
            fancyList $ map (\(f, s) -> addFolder s cInput (Just f)) paths 

mkFile :: String -> [FileSystem] -> Maybe [FileSystem]
mkFile input syst = case wordParser input of 
    Just (rest', fileName) -> case eofParser rest' of
        Just(_, fileContent) -> if isValidName isNameFile fileName (topStack syst) then Nothing 
        else let paths = zip syst (filesToString syst) in
            fancyList $ map (\(f, s) -> addFile s fileName fileContent (Just f)) paths
        Nothing -> Nothing 
    Nothing -> Nothing

cat :: String -> [FileSystem] -> [FileSystem]
cat = catHelper (File "" "") 
    where
        catHelper :: FileSystem -> String -> [FileSystem] -> [FileSystem] 
        catHelper currFile input syst = 
            case wordParser input of
                Just (fileName, ">") -> case catFiles fileName (File "" "") currFile of
                    Nothing -> syst 
                    Just (File n' c') -> case mkFile (n' ++ " " ++ c' ++ "~") syst of
                        Nothing -> syst
                        Just res -> res
                Just ("", "") -> syst
                Just(rest, curr) -> if isFilePath curr then 
                    case getFileByDirectory curr (head syst) of
                        Nothing -> catHelper currFile rest syst
                        Just file -> case catFiles "" file currFile of
                            Nothing -> catHelper currFile rest syst
                            Just resFile -> catHelper resFile rest syst
                    else 
                        case getFileFromRoot curr (topStack syst) of 
                            Nothing -> catHelper currFile rest syst
                            Just file -> case catFiles "" file currFile of
                                Nothing -> catHelper currFile rest syst
                                Just resFile -> catHelper resFile rest syst
                Nothing -> syst 

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input syst = case parseCmd input of
    Nothing -> Nothing 
    Just (rest, curr) -> case curr of 
        "cd"     -> cd ("/" ++ rest) syst
        "mkdir"  -> mkdir rest syst
        "mkfile" -> mkFile rest syst
        "rm"     -> Just $ removeFile rest syst
        "cat"    -> Just $ cat rest syst
        _        -> Nothing

ls :: String -> Maybe FileSystem -> String 
ls input (Just syst) = case eval ("cd" ++ input) [syst] of
    (Just res) -> case topStack res of 
        (Root _ xs) -> concatMap printEntity xs
        _           -> ""
    _          -> ""
ls _ _ = ""

pwd :: [FileSystem] -> IO() 
pwd xs = let system = printSystem xs in 
    putStr $ "Path\n" ++ replicate (length system) '-' ++ "\n" ++ system ++ "\n"

showFile :: String -> FileSystem -> String 
showFile name (Root _ xs) = case getFile name xs of
    Nothing    -> "No such file\n"
    (Just res) -> printFile res
showFile _ _ = "Bad use of function showFile\n"

removeFile :: String -> [FileSystem] -> [FileSystem]
removeFile _ [] = [] -- this should never happen
removeFile input xs = 
    case wordParser input of
        Just ("", last) -> let paths = zip xs (map (\x -> x ++ "/" ++ last) (filesToString xs)) in
            map (\(f, s) -> removeFileFromPath s f) paths 
        Just (rest, curr) -> let paths = zip xs (map (\x -> x ++ "/" ++ curr) (filesToString xs)) in 
            removeFile rest $ map (\(f, s) -> removeFileFromPath s f) paths
        Nothing -> xs

repl :: [FileSystem] -> IO()
repl xs = do
    putStr $ printSystem xs ++ "^ "
    input <- getLine
    case eval input xs of
        Nothing -> case parseCmd input of
            Just (_, "quit")  -> putStrLn "Goodbye! :D"
            Just (_, "pwd") -> do pwd xs
                                  repl xs
            Just (rest, "ls") -> case rest of 
                ('/' : path)  -> do putStr $ ls path $ Just $ head xs
                                    repl xs
                _             -> do putStr $ ls rest $ Just $ topStack xs
                                    repl xs
            Just (l, "show") -> do putStr $ showFile l $ topStack xs
                                   repl xs
            _  -> repl xs
        Just res -> repl res

-- [RUNNER]

main :: IO()
main = repl [mySystem]

