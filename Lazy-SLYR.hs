import Control.Applicative
import Data.Word
import System.Environment
import Data.Attoparsec.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Binary.UTF8.String as UTF8

infixl 9 :$
data Expr = Expr :$ Expr | I | K | S | U | F | E
    | Inc | Nat !Word8 | Cons | Nil deriving Show

apply :: Expr -> Expr -> Expr
apply (S :$ x :$ y) z = apply x z `apply` apply y z
apply (K :$ x) y = x
apply I x = x
apply U x = x `apply` K `apply` S `apply` K
apply F f = f `apply` (F `apply` f)
apply Inc (Nat x) = Nat $! succ x
apply f x = f :$ x

eval :: Expr -> Expr
eval (x :$ y) = eval x `apply` eval y
eval E = error "サヨナラ！"
eval x = x

encode :: BL.ByteString -> Expr
encode = foldr cons empty . map church . BL.unpack where
    empty = K :$ I
    cons x xs = (S :$ ((S :$ (K :$ ((S :$ (K :$ S)) :$ K))) :$ ((S :$ I) :$ (K :$ x)))) :$ xs

church :: Word8 -> Expr
church 0 = K :$ I
church 1 = I
church n = S :$ (S :$ (K :$ S) :$ K) :$ church (pred n)

decode :: Expr -> BL.ByteString
decode expr = runList $ expr `apply` Cons `apply` Nil where
    runList (Cons :$ x :$ xs) = unchurch x `BL.cons` runList xs
    runList Nil = BL.empty
    runList _ = error "Result was not a list"

unchurch :: Expr -> Word8
unchurch expr = case expr `apply` Inc `apply` Nat 0 of
    Nat n -> n
    _ -> error "Result was not a number"

parseExpr :: Parser Expr
parseExpr = spaces *> choice
    [ string (BS.pack $ UTF8.encode "イヤーッ！") *> ((:$) <$> parseExpr <*> parseExpr)
    , U <$ string (BS.pack $ UTF8.encode "グワーッ！")
    , I <$ string (BS.pack $ UTF8.encode "アバーッ！")
    , F <$ string (BS.pack $ UTF8.encode "ゴウランガ！")
    , E <$ string (BS.pack $ UTF8.encode "サヨナラ！")
    , string (BS.pack $ UTF8.encode "アイ") *> do { m <- length <$> many (string (BS.pack $ UTF8.encode "エ")) ;
                           n <- pred <$> length <$> some (string (BS.pack $ UTF8.encode "！")) ;
                           return $ church $ fromIntegral $ 10 * m + n }
    ] <* spaces

spaces = skipMany $ satisfy $ inClass "\n\r "

main = do
    (path:_) <- getArgs
    Done _ prog <- parse parseExpr <$> BL.readFile path
    input <- encode <$> BL.pack <$> UTF8.encode <$> getContents
    BL.putStr $ decode $ eval $ prog :$ input