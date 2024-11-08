{-
Definição:
Criar uma aplicação em Haskell que permita montar um Kit de sobrevivência para uma missão
espacial simulada. O programa será utilizado para listar e gerenciar os itens essenciais que
uma equipe precisaria durante uma missão espacial fictícia.
A aplicação deve iniciar com uma mensagem de boas-vindas e oferecer as seguintes opções
no Menu Principal:
1. Adicionar item ao kit: Permite ao usuário adicionar um novo item ao kit, especificando
nome e preço.
2. Listar todos os itens no kit: Exibe todos os itens adicionados até o momento.
3. Remover item do kit: Exibe a lista de itens com numeração e permite ao usuário
remover um item específico.
4. Calcular custo total do kit: Soma o preço de todos os itens e exibe o custo total do kit
de sobrevivência.
5. Sair da aplicação: Encerra o programa.
Cada item do kit deve ter um nome e um preço associado, e o programa deve permitir que o
usuário gerencie esses itens através das funcionalidades listadas acima.
**Adicionar um item que esteja relacionada a uma inovação a este trabalho.
Sugestões:
a) Primeiramente, podem fazer uma versão apenas com o menu principal e a leitura da
opção. Depois, fazer uma nova versão cadastrando os itens - utilizar apenas uma lista de String
[String].
b) Criar um tipo item e então usar uma lista de itens [Item].
Tipo item:
data Item = Item {nome:: String, preço:: Int} deriving Show
** A palavra-chave deriving Show significa que o Haskell automaticamente gera uma instância da
classe Show para o tipo Item. Isso permite que os valores do tipo Item sejam impressos de forma legível
no console.
c) Lembre que tudo são funções então não há variável ‘global’. A lista de itens e novo item
devem ser argumentos de funções.
d) Faça tudo no replit ou na ferramenta do Haskell, preferencialmente.
e) Verificar os códigos de exemplo disponíveis no moodle. Com os códigos de exemplo é
possível de fazer a aplicação.
Critérios de avalição
1) Cada funcionalidade conforme requisitos: 2,0
2) Item de “Inovação neste trabalho”: 2,0.
3) Avaliação geral (código, entrega, organização): 2,0 pontos
4) Apresentação: 4,0
Entregáveis:
a) Deve ser postado no link indicado.
b) Colocar o nome dos integrantes do grupo no código.
c) Apresentação em aula.
-}

main :: IO()
main = do
  print("Bem-vindo ao PROMOK (PROgrama para MOtagem de Kit)")
  print("Selecione o número correspondente a ação que desejada")
  print("1 - ADICIONAR item ao kit")
  print("2 - LISTAR todos os itens do kit")
  print("3 - REMOVER item do kit")
  print("4 - CALCULAR o custo total do kit")
  print("5 - SAIR")
  print("0 - SURPRESA")
  line <- getLine
  print ("The user input is:")
  print (line)
