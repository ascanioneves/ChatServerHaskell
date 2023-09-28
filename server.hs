import Control.Concurrent
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Network.Socket
import System.IO
import Control.Monad (forever)

import ChatbotMain (chatbotResponse)

-- Defina a variável clientsMVar
type Clients = [Handle]
type ClientsMVar = MVar Clients

-- Função para lidar com a comunicação com um cliente
handleClient :: Handle -> ClientsMVar -> IO ()
handleClient clientHandle clientsMVar = do
    hSetBuffering clientHandle LineBuffering
    putStrLn "Cliente conectado."
    hPutStrLn clientHandle "Bem-vindo ao chat! Digite seu nome:"
    name <- hGetLine clientHandle
    putStrLn ("Nome do cliente: " ++ name)
    hPutStrLn clientHandle ("Olá, " ++ name ++ "! Você está conectado.")
    chatLoop clientHandle name clientsMVar -- Passe clientsMVar como argumento


-- Loop para a conversa
chatLoop :: Handle -> String -> ClientsMVar -> IO ()
chatLoop clientHandle name clientsMVar = do
    message <- hGetLine clientHandle
    putStrLn (name ++ ": " ++ message)

    -- Chama o chatbot para obter uma resposta
    let response = chatbotResponse message
    putStrLn ("Chatbot: " ++ response)

    -- Envie a resposta de volta ao cliente
    hPutStrLn clientHandle response

    chatLoop clientHandle name clientsMVar
    
-- Função para enviar mensagens para todos os clientes
broadcast :: ClientsMVar -> String -> IO ()
broadcast clientsMVar message = do
    clients <- readMVar clientsMVar
    mapM_ (\client -> sendMessage client message) clients

-- Função para enviar uma mensagem para um cliente específico
sendMessage :: Handle -> String -> IO ()
sendMessage clientHandle message = do
    hPutStrLn clientHandle message


main :: IO ()
main = do
    -- Crie uma MVar para manter a lista de clientes
    clientsMVar <- newMVar []

    -- Configure o servidor
    let port = 8080
    addrInfo <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just (show port))
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addrInfo)
    listen sock 5
    putStrLn $ "Servidor de chat escutando na porta " ++ show port

    -- Aguarde a conexão de clientes
    forever $ do
        (clientSock, clientAddr) <- accept sock
        putStrLn $ "Cliente conectado: " ++ show clientAddr
        clientHandle <- socketToHandle clientSock ReadWriteMode
        hSetBuffering clientHandle LineBuffering
        modifyMVar_ clientsMVar (\clients -> return (clientHandle : clients))
        forkFinally (handleClient clientHandle clientsMVar) -- Passe clientsMVar como argumento
            (\_ -> do
                putStrLn $ "Cliente desconectado: " ++ show clientAddr
                hClose clientHandle
                modifyMVar_ clientsMVar (\clients -> return (filter (/= clientHandle) clients)))