open System

let deterministicDice i = ((i - 1) % 100) + 1

type PlayerState = {Position: int; Score: int}
type Game = {
    Standings: Map<int,PlayerState>
    DieRolls: int
    PlayerInTurn: int
}
module Game =
    let newGame start = 
        {
            Standings = start |> List.map (fun (player,position) -> (player, {Position=position;Score=0} )) |> Map.ofList
            DieRolls = 0
            PlayerInTurn = 1
        }
    
    let winner game = game.Standings |> Map.toList |> List.exists (fun state -> (snd state).Score >= 1000)

    let roll (game: Game) =
        let rolls = game.DieRolls + 1
        let thisRoll = deterministicDice rolls
        let newStandings = 
            game.Standings 
            |> Map.find game.PlayerInTurn
            |> fun state -> {Position = ((state.Position + thisRoll - 1) % 10) + 1 ; Score= state.Score}
            |> fun score -> Map.add game.PlayerInTurn score game.Standings
        {
            Standings = newStandings
            DieRolls = game.DieRolls + 1
            PlayerInTurn = game.PlayerInTurn
        }
    let scoreAndContinue game = 
        let newStandings = 
            game.Standings 
            |> Map.find game.PlayerInTurn
            |> fun state -> {Position = state.Position; Score= state.Score + state.Position}
            |> fun score -> Map.add game.PlayerInTurn score game.Standings
        let rolls = game.DieRolls
        let playerInTurn = (game.PlayerInTurn % 2) + 1
        {
            Standings = newStandings
            DieRolls = rolls
            PlayerInTurn = playerInTurn
        }


    let rec play game =
        if winner game then game
        else
            seq{1..3}
            |> Seq.fold (fun state _i -> roll state) game
            |> scoreAndContinue
            |> play


let startingGame = [(1,2);(2,8)] |> Game.newGame
//let startingGame = [(1,4);(2,8)] |> Game.newGame

let score game = 
    let losingScore = game.Standings |> Map.find game.PlayerInTurn |> fun state -> state.Score
    losingScore * game.DieRolls

startingGame
|> Game.play
|> score
|> printfn "Game score %i"
