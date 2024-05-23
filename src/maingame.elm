import Assets exposing 
  (flower1, flower2, flower3, barn, pond, pig1, pig2, pig3, cow, chicken, egg, start, housef, htp, back, tree, tree1, plant1, flowertrio)
import FlowerGameUI exposing (flowerGameMain)
import FarmAreaUI exposing (farmArea, fence)
import IntroUI exposing (introUI)
import InstructionsUI exposing (instructionsUI)
import DraftDrawingGame3
import FlowerGameDraft3

myShapes model =
    ( case model.state of
        Intro  ->
            [ introUI
            ,
              start
              |> scale 0.25
              |> move (0,-15)
              |> makeTransparent (10 * (cos(5*model.time)))
            ,
            start
              |> scale 0.24
              |> move (0,-15)
              |> notifyTap ToMain
             ,
             htp
             |> scale 0.19
             |> move (70, -50)
             |> makeTransparent (10 * (cos(5*model.time)))
             ,
             htp
             |> scale 0.18
             |> move (70, -50)
             |> notifyTap ToInstructions            
            ]
        Instructions ->
            [ instructionsUI 
            ,
            back
            |> scale 0.25
            |> move (80, 50)
            |> notifyTap ToIntro
            ]
        Main  ->
            [ 
            rect 2000 2000
            |> filled (rgb 131 146 76)
            ,
            flowerGameMain model
            |> scale 0.55
            |> move (45, -25)
            |> notifyTap (GoTo <| MiniGame2 FlowerGameDraft3.init) 
            ,
            farmArea
            |> scale 0.5
            |> scaleX 0.95
            |> move (-1, 41)
            ,
            fence 
            |> scale 0.5
            |> move (0, 42)
            ,
            barn
            |> scale 0.35
            |> move (-17.5, 60)
            ,
            pond 
            |> scale 0.5
            |> move (-80, -10)
            ,
            housef
            |> addOutline (solid 5) white
            |> scale 0.41
            |> move (-60, -30)
            |> makeTransparent (1.2 * (cos(5*model.time)))
            ,
            housef
            |> scale 0.4
            |> move (-60, -30)
            |> notifyTap (GoTo <| MiniGame1 DraftDrawingGame3.init)            
            ,
            pig1
            |> scale 0.1
            |> move (-25, 36)   
            ,
            pig2
            |> scale 0.125
            |> move (-10, 40)
            ,
            chicken
            |> scale 0.15
            |> move (30, 40)
            ,
            chicken
            |> scale 0.125
            |> move (20, 35)
            ,
            cow
            |> scale 0.12
            |> move (5, 53)
            ,
            cow
            |> scale 0.12
            |> mirrorX
            |> move (24, 62)
            ,
            egg
            |> scale 0.15
            |> move (25, 33)
            ,
             egg
            |> scale 0.15
            |> move (22, 35)
            ,
            egg
            |> scale 0.15
            |> move (20, 32)
            ,
            tree1
            |> scale 0.18
            |> move (-55, 65)
            ,
            tree1
            |> scale 0.18
            |> move (-75, 50)
            ,
            tree1
            |> scale 0.18
            |> move (-85, 30)
            , 
            tree1
            |> scale 0.18
            |> move (-55, 40)
            ,
            tree1
            |> scale 0.18
            |> move (-60, 20)
            ,
            flowertrio
            |> scale 0.1
            |> move (-70, 28)
            ,
            tree1
            |> scale 0.18
            |> move (-90, 65)
            ,
            tree1
            |> scale 0.18
            |> move (-25, 10)
            ,
            tree
            |> scale 0.2
            |> move (-10, -15)
            ,
            tree
            |> scale 0.2
            |> move (-20, -37)
            ,
            tree1
            |> scale 0.2
            |> move (-8, -55)
            ,
            tree
            |> scale 0.2
            |> move (-82, -50)
            ,
            plant1
            |> scale 0.1
            |> move (20, 15)
            ,
            flowertrio
            |> scale 0.1
            |> move (-5, 12)
            ,
            plant1
            |> scale 0.1
            |> move (-65, -58)
            ,
            flowertrio
            |> scale 0.1
            |> move (-25, -55)
            ,
            tree
            |> scale 0.2
            |> move (60, 50)
            ,
            tree
            |> scale 0.2
            |> move (82, 35)
          
            ,
            tree
            |> scale 0.2
            |> move (60, 20)
            ,
            tree
            |> scale 0.2
            |> move (87, 13)
            
            ,
            flowertrio
            |> scale 0.1
            |> move (80, 60)
            ,
            plant1
            |> scale 0.1
            |> move (87, 50)
            ,
            back
            |> scale 0.25
            |> move (80, 50)
            |> notifyTap ToIntro
            ]
            
        FlowerGame  ->
            [ text "FlowerGame"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "ToMain"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap ToMain
            ]
        DrawingGame  ->
            [ text "DrawingGame"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "ToMain"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap ToMain
            ]
        MiniGame1 mg1Model ->
          [GraphicSVG.map MG1Msg <| group <| DraftDrawingGame3.myShapes mg1Model]
          ++
          [back
          |> scale 0.25
          |> move (-80, -50)
          |> notifyTap ToMain
          ]
        MiniGame2 mg2Model ->
          [GraphicSVG.map MG2Msg <| group <| FlowerGameDraft3.myShapes mg2Model]

          ++
          [
          back
          |> scale 0.25
          |> move (80, 50)
          |> notifyTap ToMain
          ]
    )

type Msg = Tick Float GetKeyState
         | ToMain
         | ToIntro
         | ToInstructions
         | ToDrawingGame
         | ToFlowerGame
         | GoTo State
         | MG1Msg DraftDrawingGame3.Msg
         | MG2Msg FlowerGameDraft3.Msg
 

type State = Intro  
           | Instructions
           | Main 
           | FlowerGame 
           | DrawingGame
           | MiniGame1 DraftDrawingGame3.Model 
           | MiniGame2 FlowerGameDraft3.Model
           
update msg model =
    case msg of
        Tick t keys ->
            { model | time = t
                    , state = case model.state of 
                            MiniGame1 mg1Model ->
                              MiniGame1 <| DraftDrawingGame3.update (DraftDrawingGame3.Tick t keys) mg1Model
                            MiniGame2 mg2Model ->
                              MiniGame2 <| FlowerGameDraft3.update (FlowerGameDraft3.Tick t keys) mg2Model
                            _ -> model.state
            }
        GoTo state -> {model | state = state}
        MG1Msg mg1Msg ->
          { model |
                state = case model.state of 
                          MiniGame1 mg1Model ->
                            MiniGame1 <| DraftDrawingGame3.update mg1Msg mg1Model
                          _ -> model.state
          }
        MG2Msg mg2Msg ->
          { model | 
                state = case model.state of 
                          MiniGame2 mg2Model ->
                            MiniGame2 <| FlowerGameDraft3.update mg2Msg mg2Model
                          _ -> model.state
          }
        ToMain  ->
            {model | state = Main}
        ToInstructions ->
          case model.state of
              Intro ->
                {model | state = Instructions}
              otherwise ->
                model
        ToIntro ->
          case model.state of 
              Instructions ->
                {model | state = Intro}
              Main ->
                {model | state = Intro}
              otherwise ->
                model
          
        ToDrawingGame  ->
            case model.state of
                Main  ->
                    { model | state = DrawingGame  }

                otherwise ->
                    model
        ToFlowerGame  ->
            case model.state of
                Main  ->
                    { model | state = FlowerGame  }

                otherwise ->
                    model
        

type alias Model =
    { time : Float
    , state : State
    }
   
init : Model
init = { time = 0 
       , state = Intro 
       }
    
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)


