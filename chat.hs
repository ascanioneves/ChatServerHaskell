-- Main.hs

import System.IO
import ChatbotMain (chatbotResponse) -- Importe o módulo ChatbotMain

-- Função principal do chatbot
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Chatbot Haskell!"
  putStrLn "Digite 'Ajuda' para obter informações."
  chatLoop

-- Loop de conversação
chatLoop :: IO ()
chatLoop = do
  putStr "> "
  hFlush stdout
  userInput <- getLine
  if userInput == "sim"
    then putStrLn "Conectando a um atendente humano..."
  else if userInput == "nao" 
    then putStrLn "Criar chatbot"
  else do
      let response = chatbotResponse userInput
      putStrLn response
      chatLoop