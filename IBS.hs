import Data.Char (digitToInt)

-- Función para validar el código ISBN
isValidISBN :: String -> Bool
isValidISBN isbn = case filter (\c -> c /= '-') isbn of
    []  -> False  -- Si el ISBN está vacío o solo contiene guiones, es inválido
    xs  -> checkValidity $ map digitToInt xs

-- Función auxiliar para realizar la verificación de la validez del ISBN
checkValidity :: [Int] -> Bool
checkValidity digits =
    let weightedDigits = zipWith (*) digits [10,9..1]
        totalSum = sum weightedDigits
    in totalSum `mod` 11 == 0

-- Función principal que solicita el ISBN e informa sobre su validez
main :: IO ()
main = do
    putStrLn "Ingrese el código ISBN (incluyendo guiones):"
    isbn <- getLine
    let result = isValidISBN isbn
    putStrLn $ "El código ISBN ingresado es válido: " ++ show result
