-- ChatbotMain.hs

module ChatbotMain (chatbotResponse) where

chatbotResponse :: String -> String
chatbotResponse userInput =
  case userInput of
    "Olá" -> "Olá! Como posso ajudar?"
    "Qual é o seu nome?" -> "Eu sou um chatbot Haskell."
    "Ajuda" -> "Posso responder a perguntas comuns ou encaminhá-lo para um atendente humano. O que você prefere?"
    _ -> "Desculpe, não entendi. Você gostaria de falar com um atendente? (sim/não)"
    