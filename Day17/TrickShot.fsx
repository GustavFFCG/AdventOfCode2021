
type Velocity = {xspeed:int;yspeed:int}
type Position = {x:int;y:int}

type Area = {
    xmin:int
    ymin:int
    xmax:int
    ymax:int
}

let sampleArea = { xmin= 20; ymin= -10; xmax= 30; ymax= -5 }
let inputArea = { xmin= 169; ymin= -108; xmax= 206; ymax= -68 }

let newxspeed xspeed =
    match xspeed with
        | 0 -> 0
        | x when x > 0 -> x - 1
        | x -> x + 1

let newyspeed yspeed = yspeed - 1

module Position =
    type PositionRelativeTarget =
        | EnRoute
        | InTarget
        | Missed
    let step (position,velocity) =
        {
            x=position.x + velocity.xspeed
            y = position.y + velocity.yspeed
        },
        {
            xspeed= newxspeed velocity.xspeed
            yspeed= newyspeed velocity.yspeed
        }
    let relativeTarget area (trajectory: Position list) =
        let position = trajectory |> List.head
        if position.x >= area.xmin && position.x <= area.xmax && position.y >= area.ymin && position.y <= area.ymax then InTarget
        else if position.x > area.xmax || position.y < area.ymin then Missed
        else EnRoute
    
    let start = {x=0;y=0}

let findSolutions targetArea =
    let xspeedCandidates = 
        let workingXspeed xspeed =
            let rec hitsTarget x xspeed =
                if x >= targetArea.xmin && x <= targetArea.xmax then true
                else if xspeed = 0 || x > targetArea.xmax  then false
                else hitsTarget (x + (newxspeed xspeed)) (newxspeed xspeed)
            hitsTarget xspeed xspeed
        seq{1..targetArea.xmax}
        |> Seq.filter workingXspeed
    
    let yspeedCandidates =
        seq {targetArea.ymin..(-targetArea.ymin)}

    let rec maybeTrajectory trajectory (position,velocity) =
        let newTrajectory = position::trajectory
        newTrajectory
        |> Position.relativeTarget targetArea
        |> function
            | Position.PositionRelativeTarget.Missed -> None
            | Position.PositionRelativeTarget.InTarget -> newTrajectory |> Some
            | Position.PositionRelativeTarget.EnRoute -> 
                let next = Position.step (position,velocity)
                maybeTrajectory newTrajectory next

        

    xspeedCandidates
    |> Seq.map(fun xspeed ->
        yspeedCandidates
        |> Seq.map (fun yspeed -> {xspeed=xspeed; yspeed=yspeed})
    )
    |> Seq.concat
    |> Seq.map(fun velocity -> 
        maybeTrajectory [] (Position.start, velocity)
        )
    |> Seq.choose id
    
    
let highpoint (trajectory: Position list) =
    trajectory
    |> List.map (fun p -> p.y)
    |> List.max



let sampleSolutions = findSolutions sampleArea
let inputSolutions = findSolutions inputArea

sampleSolutions
|> Seq.map highpoint
|> Seq.max
|> printfn "Max y for sample is %i"

inputSolutions
|> Seq.map highpoint
|> Seq.max
|> printfn "Max y for input is %i"

inputSolutions
|> Seq.length
|> printfn "Number of solutions is %i"