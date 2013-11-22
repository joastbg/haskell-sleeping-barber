module Main (main) where
 
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import GHC.Conc
import System.Random

data BarberShop = BarberShop {
    seats :: TVar Int,
    queue :: TBQueue Int,
    slog :: TQueue String
}

initShop :: Int -> STM (BarberShop)
initShop seats = do 
    s <- newTVar seats
    q <- newTBQueue seats
    l <- newTQueue
    return BarberShop { seats = s, queue = q, slog = l }

debug msg c = putStrLn (msg ++ (take (45-(length msg)) (repeat ' ')) ++ (show c))

enterShop' :: BarberShop -> Int -> IO(Either Int Int)
enterShop' s c = do
    ns <- atomically $ readTVar (seats s)
    if ns > 0 then atomically $ do
        writeTVar (seats s) (ns-1)
        writeTBQueue (queue s) c >> return (Right c)
    else return (Left c)

enterShop :: BarberShop -> Int -> IO ()
enterShop s c = do
    debug "(c) entering shop" c
    res <- enterShop' s c
    case res of
      Right c -> debug "(s) seat available for" c
      Left c -> debug "(s) turning away customer" c

cutHair :: BarberShop -> IO ()
cutHair s = do
    c <- atomically $ readTBQueue (queue s)
    atomically $ do
        ns <- readTVar (seats s)
        writeTVar (seats s) (ns+1)
    debug "(b) cutting hair of customer" c
    (getStdRandom $ randomR (100000,10000000)) >>= threadDelay
    debug "(b) done cutting hair of customer" c
    
barberWork s = forkIO loop
    where loop = cutHair s >> loop

createCustomers shop =
    createCustomer shop 1
        where createCustomer shop n = (getStdRandom $ randomR (100000,2000000)) >>= threadDelay >> (forkIO (enterShop shop n)) >> createCustomer shop (n+1)

main :: IO ()
main = do
    putStrLn "Barber goes to work..."
    shop <- atomically $ initShop 3
    barberWork shop
    createCustomers shop
