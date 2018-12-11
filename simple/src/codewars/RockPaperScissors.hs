

rps :: String -> String -> String
rps "rock" "scissors" = "Player 1 won!"
rps "scissors" "paper" = "Player 1 won!"
rps "paper" "rock" = "Player 1 won!"
rps "scissors" "rock"  = "Player 2 won!"
rps "paper" "scissors"  = "Player 2 won!"
rps "rock" "paper"  = "Player 2 won!"
rps "rock" "rock"  = "Draw!"
rps "paper" "paper"  = "Draw!"
rps "scissors" "scissors"  = "Draw!"