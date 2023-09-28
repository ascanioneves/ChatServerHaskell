import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)

main :: IO ()
main = do
    putStrLn "Digite o endereço IP do servidor (por exemplo, 127.0.0.1):"
    serverIP <- getLine
    putStrLn "Digite a porta do servidor (por exemplo, 8080):"
    serverPortStr <- getLine
    let serverPort = read serverPortStr :: Int

    -- Crie uma conexão com o servidor
    addr <- resolve serverIP serverPort
    sock <- openConnection addr

    -- Crie um MVar para receber mensagens do servidor
    serverMsgMVar <- newEmptyMVar

    -- Inicie uma thread para receber mensagens do servidor
    _ <- forkIO (receiveMessages sock serverMsgMVar)

    -- Inicie uma thread para enviar mensagens para o servidor
    _ <- forkIO (sendMessages sock)

    -- Loop principal para exibir mensagens recebidas
    putStrLn "Conectado ao servidor. Digite mensagens para enviar:"
    displayMessages serverMsgMVar

-- Função para resolver o endereço e a porta do servidor
resolve :: String -> Int -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
    return addr

-- Função para abrir uma conexão com o servidor
openConnection :: AddrInfo -> IO Socket
openConnection addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

-- Função para receber mensagens do servidor e colocá-las em um MVar
receiveMessages :: Socket -> MVar String -> IO ()
receiveMessages sock serverMsgMVar = do
    hdl <- socketToHandle sock ReadMode
    forever $ do
        message <- hGetLine hdl
        putMVar serverMsgMVar message

-- Função para enviar mensagens para o servidor
sendMessages :: Socket -> IO ()
sendMessages sock = do
    hdl <- socketToHandle sock WriteMode
    forever $ do
        message <- getLine
        hPutStrLn hdl message

-- Função para exibir mensagens recebidas do servidor
displayMessages :: MVar String -> IO ()
displayMessages serverMsgMVar = do
    message <- takeMVar serverMsgMVar
    putStrLn ("Servidor: " ++ message)
    displayMessages serverMsgMVar
