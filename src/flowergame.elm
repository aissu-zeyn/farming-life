import String
import Assets exposing 
  (flower1, flower2, flower3, flower4, flower5, flower7, flower8, flower9, flower12, flower13, flower14, flower15, flower16, flower17, flower18, flower19, flower20, box1, box2, back, tree1, flowertrio, cow, plant1, pig3, plant)
import Random exposing (Generator, Seed, int, float, generate, initialSeed)   
import FlowerGameUI exposing (flowerGameUI)

myShapes model =
    (case model.state of
        InGame timeRemaining ->
            [
            flowerGameUI,
            box2
            |> scale 0.4
            |> move (-10, 50)
            ,
            box1
            |> scale 0.27
            |> move (-80, 49)
            ]          
          ++
          [drawTargetFlower model.targetFlower -75 48]
          ++
          [timeBoard (String.fromInt (round timeRemaining))
          |> move (-15, 42)
          ]
          ++
          [ scoreBoard (String.fromInt model.coins)
           |> move (10,42)
          ]
          ++
          List.map (\(flower, (x, y), i) -> drawFlower model flower x y i) model.flowers
        
        GameOver ->
            [
              rect 2000 2000
              |> filled (rgb 131 146 76)
              , tree1
              |> scale 0.25
              |> move (75,40)
              , tree1
              |> scale 0.25
              |> move (-70,-30)
              , tree1
              |> scale 0.25
              |> move (70,-15)
              , tree1
              |> scale 0.25
              |> move (0,50)
              , tree1
              |> scale 0.25
              |> move (-70,35)
              ,
              tree1 
              |> scale 0.25
              |> move (10, -40)
              ,
              cow 
              |> scale 0.18
              |> move (-50,-50)
              ,
              cow 
              |> scale 0.18
              |> mirrorX
              |> move (35, 35)
              ,
              pig3
              |> scale 0.18
              |> move (-30,-5)
              ,
              flowertrio
              |> scale 0.1
              |> move (10, 30)
              ,
              flower12
              |> scale 0.08
              |> move (-30, -35)
              , 
              flowertrio
              |> scale 0.1
              |> move (60, -50)
              ,
              flower12
              |> scale 0.08
              |> move (-40, 55)
              ,
              plant1
              |> scale 0.1
              |> move (-75, 60)
              ,
              plant
              |> scale 0.1
              |> move (40, -10)
              ,
              plant1
              |> scale 0.1
              |> move (-10, -20)
              ,
              plant1
              |> scale 0.1
              |> move (80, -50)
              ,
              plant1
              |> scale 0.1
              |> move (-80, 0)
              ,
              box1
              |> scale 0.3
              |> move (-30, 0)
              |> notifyTap StartGame
              ,
              text "Play"
              |> bold 
              |> sansserif
              |> size 7
              |> filled white
              |> move(-30, 0)
              |> notifyTap StartGame
              ,
              text "Again"
              |> bold
              |> sansserif
              |> size 7
              |> filled white
              |> move (-33, -10)
              |> notifyTap StartGame
              ,
              box1
              |> scale 0.3
              |> move (30, 0)
              ,
              text "score: "
              |> centered 
              |> sansserif
              |> bold
              |> size 7
              |> filled white
              |> move (38, 0)
              ,
              text (String.fromInt model.coins) 
              |> centered
              |> sansserif
              |> bold 
              |> size 7
              |> filled white
              |> move (36, -10)
            ]
        )

drawTargetFlower flower x y = 
  case flower of 
    YellowDaisy -> flower1 
             |> scale 0.1 
             |> move (x, y) 
    PurpleTulip -> flower2 
             |> scale 0.1 
             |> move (x, y) 
    RedRose -> flower3 
             |> scale 0.1 
             |> move (x, y) 
    WhiteLily -> flower4
             |> scale 0.22
             |> move (x, y)
    PurpleBell -> flower5
             |> scale 0.1
             |> move (x, y)
    RedTulip -> flower7
             |> scale 0.1
             |> move (x, y)
    PinkDaisy -> flower8
             |> scale 0.1
             |> move (x, y)
    PinkPeony -> flower12
             |> scale 0.1
             |> move (x, y)
    YellowDaffodil -> flower13
             |> scale 0.1
             |> move (x, y)
    BlueTulip -> flower14
             |> scale 0.1
             |> move (x, y)
    OrangeTulip -> flower15
             |> scale 0.1
             |> move (x, y)
    YellowTulip -> flower16
             |> scale 0.1
             |> move (x, y)
    OrangeRose -> flower17
             |> scale 0.1
             |> move (x, y)
    YellowLily -> flower18
             |> scale 0.22
             |> move (x, y)
    OrangePeony -> flower19
             |> scale 0.1
             |> move (x, y)
    YellowBell -> flower20
             |> scale 0.1
             |> move (x,y)

drawFlower model flower x y id = 
  case flower of 
    YellowDaisy -> flower1 
             |> scale 0.15
             |> move (x, y) 
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    PurpleTulip -> flower2 
             |> scale 0.15
             |> move (x, y) 
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    RedRose -> flower3 
             |> scale 0.125
             |> move (x, y) 
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    WhiteLily -> flower4
             |> scale 0.22
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    PurpleBell -> flower5
             |> scale 0.22
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    RedTulip -> flower7
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    PinkDaisy -> flower8
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    PinkPeony -> flower12
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    YellowDaffodil -> flower13
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    BlueTulip -> flower14
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    OrangeTulip -> flower15
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    YellowTulip -> flower16
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    OrangeRose -> flower17
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    YellowLily -> flower18
             |> scale 0.22
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    OrangePeony -> flower19
             |> scale 0.15
             |> move (x, y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)
    YellowBell -> flower20
             |> scale 0.22
             |> move (x,y)
             |> notifyTap (pickUpIfMatch flower model.targetFlower)
             |> notifyTap (changeTargetIfMatch flower model.targetFlower)
             |> notifyTap (changeFlowerIfMatch id flower model.targetFlower)

pickUpIfMatch tappedFlower targetFlower = 
  if tappedFlower == targetFlower then 
      PickUpCoin
  else 
      DontPickUpCoin    

changeTargetFlowerTo flowers = 
  let 
    (randomFlower, _) = 
      Random.step 
        (Random.map 
          (\index ->
            case List.drop index flowers of 
              (flower, _, _) :: _ ->
                flower
              [] -> 
                List.head(List.map (\(f,_, _) -> f) flowers)
                |> Maybe.withDefault PurpleTulip
                )
                (Random.int 0 (List.length flowers - 1))
                )
                (initialSeed 30)
   in 
   randomFlower
    
changeTargetIfMatch tappedFlower targetFlower = 
  if tappedFlower == targetFlower then 
    ChangeTargetFlower
  else 
    DontPickUpCoin


changeFlowerIfMatch id tappedFlower targetFlower = 
  if tappedFlower == targetFlower then
    ChangeFlower id 
  else 
    DontChangeFlower id


changeFlowerTo id flowers targetFlower time =
    let
        (newFlower, _) = Random.step generateRandomFlower (initialSeed time)
        updatedFlowers =
            List.map
                (\(flower, pos, flowerId) ->
                    if flowerId == id then
                        (newFlower, pos, flowerId)
                    else
                        (flower, pos, flowerId)
                )
                flowers
    in
    ( updatedFlowers, if List.member targetFlower (List.map (\(f, _, _) -> f) updatedFlowers) then
                        targetFlower
                      else
                        changeTargetFlowerTo updatedFlowers
    )

generateRandomFlower =
    let
        randomIndex : Generator Int
        randomIndex =
            Random.int 0 16
    in
    Random.map 
        (\index ->
            case index of
                0 -> PurpleTulip
                1 -> YellowDaisy
                2 -> RedRose
                3 -> WhiteLily 
                4 -> PurpleBell
                5 -> RedTulip
                6 -> PinkDaisy
                7 -> PinkPeony
                8 -> YellowDaffodil
                9 -> BlueTulip
                10 -> OrangeTulip
                11 -> YellowTulip
                12 -> OrangeRose
                13 -> YellowLily
                14 -> OrangePeony
                15 -> YellowBell
                _ -> PurpleTulip 
        )
        randomIndex
      
scoreBoard coins = text coins
              |> sansserif
              |> bold
              |> size 8
              |> filled (rgb 80 62 54) 

timeBoard time = text time 
              |> sansserif
              |> bold
              |> size 8
              |> filled (rgb 80 62 54)  

type Msg = Tick Float GetKeyState
         | StartGame
         | PickUpCoin
         | DontPickUpCoin
         | ChangeTargetFlower 
         | ChangeFlower Int 
         | DontChangeFlower Int

type State = InGame Float
           | GameOver
         
resetGame model = 
  let
    initialFlowers = [(YellowDaisy, (-72, -7), 1), (PurpleTulip, (-54, -7), 2), (PinkDaisy, (-72, -27), 3), (WhiteLily, (-48,-31), 4), (PurpleBell, (-69, -47), 5), (RedTulip, (-52, -47), 6), (BlueTulip, (-10, -7), 7), (RedRose, (7, -10), 8), (YellowBell, (-7, -30), 9), (OrangePeony, (10, -26), 10), (PinkDaisy, (-10, -48), 11), (YellowDaisy, (10, -48), 12), (BlueTulip, (38, 43), 13), (YellowDaffodil, (60,45), 14), (YellowTulip, (35, 25), 15), (OrangeTulip, (60, 25), 16), (YellowLily, (40, 3), 17), (YellowDaisy, (60, 5), 18), (RedTulip, (35, -10), 19), (OrangePeony, (63, -13), 20), (OrangeRose, (35, -28), 21), (YellowLily, (63, -28), 22), (PinkPeony, (34,-47), 23), (RedRose, (57,-48), 24)]
    initialTargetFlower = changeTargetFlowerTo initialFlowers
  in
    { model | state = InGame, time = 0, 
    coins = 0, flowers = initialFlowers, 
    targetFlower = initialTargetFlower,
    lastResetTime = model.time
    }
    
decrementTimer dt model = 
  { model | 
        state = case model.state of 
                  InGame timeRemaining ->
                    InGame <| timeRemaining - dt
                  _ -> model.state
  }

checkTimeOut model = 
  { model | state = 
              case model.state of 
                InGame timeRemaining ->
                  if timeRemaining <= 0 then 
                    GameOver
                  else 
                    model.state 
                _ -> model.state}

update msg model =
    case msg of
        Tick t _ ->
            let 
              dt = t - model.time
            in
              {model | time = t}
              |> decrementTimer dt
              |> checkTimeOut
        
        StartGame ->
          {model | state = InGame 60, coins=0}
        
        PickUpCoin ->
            { model | coins = model.coins + 1}
        
        DontPickUpCoin -> 
            {model | coins = model.coins}
        
        ChangeFlower id ->   
            let 
              (updatedFlowers, newTargetFlower) = 
                changeFlowerTo id model.flowers model.targetFlower (round model.time)
            in
            {model | flowers = updatedFlowers, targetFlower = newTargetFlower}
        
        ChangeTargetFlower ->
            {model | targetFlower = changeTargetFlowerTo model.flowers}
        
        DontChangeFlower id ->
            {model | flowers = model.flowers}
           
type alias Model =
    { time : Float
    , state : State
    , coins : Int 
    , flowers : List (Flower, (Float, Float), Int)
    , targetFlower : Flower
    }

type Flower = PurpleTulip | RedRose | YellowDaisy | WhiteLily | PurpleBell | RedTulip | PinkDaisy | PinkPeony | YellowDaffodil | BlueTulip | OrangeTulip | YellowTulip | OrangeRose | YellowLily | OrangePeony | YellowBell 

init = { time = 0 
       , state = InGame 60
       , coins = 0
       , flowers = [(YellowDaisy, (-72, -7), 1), (PurpleTulip, (-54, -7), 2), (PinkDaisy, (-72, -27), 3), (WhiteLily, (-48,-31), 4), (PurpleBell, (-69, -47), 5), (RedTulip, (-52, -47), 6), (BlueTulip, (-10, -7), 7), (RedRose, (7, -10), 8), (YellowBell, (-7, -30), 9), (OrangePeony, (10, -26), 10), (PinkDaisy, (-10, -48), 11), (YellowDaisy, (10, -48), 12), (BlueTulip, (38, 43), 13), (YellowDaffodil, (60,45), 14), (YellowTulip, (35, 25), 15), (OrangeTulip, (60, 25), 16), (YellowLily, (40, 3), 17), (YellowDaisy, (60, 5), 18), (RedTulip, (35, -10), 19), (OrangePeony, (63, -13), 20), (OrangeRose, (35, -28), 21), (YellowLily, (63, -28), 22), (PinkPeony, (34,-47), 23), (RedRose, (57,-48), 24)]
       , targetFlower = PurpleTulip 
       }
    
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

