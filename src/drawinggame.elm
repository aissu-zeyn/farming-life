import Assets exposing (flower2, flower7, flower14, flower15, flower16, egg, canvas, flowertrio, plant1, tree1, eraser)

myShapes : Model -> List (Shape Msg)
myShapes model =
  ( List.reverse model.curves |>
      List.concatMap
             ( \ (pts, color, thickness) -> 
                     case pts of 
                       pt0::rest -> 
                         [ openPolygon pts |> outlined (solid thickness) (currentColor color)
                         ]
                       [] -> []
               )
  )
  ++
  [rect 90 200 
  |> filled (rgb 131 146 76) 
  |> move (76, 0)
  ,
  plant1
  |> scale 0.15
  |> move (40, 0)
  ,
  plant1
  |> scale 0.15
  |> move (80, -50)
  ,
  flowertrio
  |> scale 0.15
  |> move (85, 50)
  ]
  ++
  [ 
    case model.drawing of 
      Waiting -> group []
      Drawing tPts -> tPts |> List.map (Tuple.second) 
                           |> openPolygon
                           |> outlined (solid model.thickness) (currentColor model.color)
  , 
    rect 126 126 |> filled (rgba 0 0 0 0)
      |> move (-32, 0)
      |> addOutline (solid 0.5) black
      |> notifyMouseDownAt MouseDownAt
      |> notifyMouseMoveAt MouseMoveAt
      |> notifyMouseUp MouseUp
      |> notifyLeave MouseUp
  ]
  ++
  [
  flower16
  |> scale 0.25
  |> notifyTap changeColorToYellow
  |> move (60, 40)
  ]
  ++
  [text "brush size" 
  |> sansserif
  |> bold
  |> size 6
  |> filled black 
  |> move (-85,35)
  ]
  ++
  [text "tap on colors"
  |> sansserif
  |> bold 
  |> size 6
  |> filled black
  |> move (45, 55)
  ]
  ++
  [
  flower14
  |> scale 0.25
  |> notifyTap changeColorToBlue
  |> move (60, 20)
  ]
  ++ 
  [
  flower2
  |> scale 0.25
  |> notifyTap changeColorToPurple
  |> move (60, 0)
  ]
  ++
  [
  flower7
  |> scale 0.25
  |> notifyTap changeColorToRed
  |> move (60, -20)
  ]
  ++
  [
  flower15
  |> scale 0.25
  |> notifyTap changeColorToOrange
  |> move (60, -40)
  ]
  ++
  [egg
  |> scale 0.25
  |> notifyTap changeThicknessToThin
  |> move (-85, 50)]
  ++
  [egg
  |> scale 0.5
  |> notifyTap changeThicknessToMedium
  |> move (-72, 50)]
  ++
  [egg
  |> scale 0.75
  |> notifyTap changeThicknessToThickest
  |> move (-55, 50)]
  ++
  [eraser
  |> scale 0.35
  |> notifyTap changeColorToErase
  |> move (40, -65)
  ]

  
currentColor color = 
  case color of 
    Red -> red
    Orange -> orange
    Blue -> blue 
    Purple -> purple
    Yellow -> yellow
    Erase -> white
    
changeColorToRed = 
  ChangeToRed

changeColorToBlue =
  ChangeToBlue
  
changeColorToOrange = 
  ChangeToOrange

changeColorToPurple = 
  ChangeToPurple
  
changeColorToYellow = 
  ChangeToYellow
  
changeColorToErase = 
  ChangeToErase
  
changeThicknessToThickest = 
  ChangeToThick

changeThicknessToMedium = 
  ChangeToMedium

changeThicknessToThin = 
  ChangeToThin
 

type alias Model = { time : Float
                   , curves : List (List Point, Color, Float)
                   , drawing : DrawState
                   , color : Color
                   , thickness : Float
                   }

type Color = Red | Orange | Blue | Purple | Yellow | Erase

init : Model
init = { time = 0 
       , curves = [] 
       , drawing = Waiting
       , color = Orange
       , thickness = 0.5
       }
  
type Msg 
  = Tick Float GetKeyState
  | MouseDownAt Point
  | MouseMoveAt Point
  | MouseUp
  | ChangeToBlue
  | ChangeToRed
  | ChangeToOrange
  | ChangeToPurple
  | ChangeToYellow
  | ChangeToThick
  | ChangeToThin
  | ChangeToMedium
  | ChangeToErase
 
type DrawState = Waiting
               | Drawing (List (Time,Point))

type alias Time = Float
type alias Shaky = Float
type alias Point = (Float,Float)

update : Msg -> Model -> Model
update msg model 
  = case msg of
      Tick t _ -> { model | time = t }
      MouseDownAt pt -> 
        case model.drawing of
          Waiting -> { model | drawing = Drawing [(model.time,pt)] }
          Drawing _ -> model 
      MouseMoveAt pt -> 
        case model.drawing of
          Drawing [] ->
            { model | drawing = Drawing [ (model.time, pt) ] }
          Drawing (tPt1 :: rest) ->
            if norm (Tuple.second tPt1) pt > 1    
               || model.time - (Tuple.first tPt1) > 0.25 
            then
              { model | drawing = Drawing ( (model.time,pt) :: tPt1 :: rest ) }
            else
              model
          Waiting ->
            model 
      MouseUp -> 
        case model.drawing of 
          Drawing tPts ->
            { model | drawing = Waiting
                    , curves = ( List.map Tuple.second tPts, model.color, model.thickness) :: model.curves
                    }
          Waiting -> model
      ChangeToBlue ->
          {model | color = Blue}
      ChangeToRed ->
          {model | color = Red}
      ChangeToOrange ->
          {model | color = Orange}
      ChangeToPurple ->
          {model | color = Purple}
      ChangeToYellow ->
          {model | color = Yellow}
      ChangeToThick ->
          {model | thickness = 2}
      ChangeToMedium ->
          {model | thickness = 1}
      ChangeToThin ->
          {model | thickness = 0.5}
      ChangeToErase ->
          {model | color = Erase}
      
norm (x,y) (u,v) = (x-u)^2 + (y-v)^2


main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

