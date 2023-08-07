#r "nuget:DIKU.Canvas, 1.0.1" 

open Canvas

type vec = float * float // definition of vector type

// Functions from 2g0 used to make code more readable:

/// <summary>
/// Performs addition of 2 vectors
/// </summary>

/// <param name = "a"> 2-dimensional vector (vec type = float * float) </param>
/// <param name = "b"> 2-dimensional vector (vec type = float * float) </param>

/// <returns> 2-dimensional vector a+b (vec type = float * float) </returns>

let add (a: vec) (b: vec) = (fst(a) + fst(b), snd(a) + snd(b): vec)

//example of use: 

let vec_add1 = (3.0, 2.0 :vec)
let vec_add2 = (2.0, 3.0 :vec)

let vec_add = add vec_add1 vec_add2

///////////////////////////////////////////////////////////////////////////

/// <summary>
/// Multiplies a vector by a constant
/// </summary>

/// <param name = "a"> 2-dimensional vector (vec type = float * float) </param>
/// <param name = "c"> constant (type = float) </param>

/// <returns> 2-dimensional vector c*a (vec type = float * float) </returns>

let mul (a:vec) (c: float) = (fst(a) * c, snd(a) * c : vec) 

//example of use: 

let vec_mul_1 = (1.0, 1.0 :vec)
let const_mul = (-1.0 :float)

let vec_mul = mul vec_mul_1 const_mul

///////////////////////////////////////////////////////////////////////////

/// <summary>
/// Rotates a vector counter-clockwise around its' tail
/// </summary>

/// <param name = "a"> 2-dimensional vector (vec type = float * float) </param>
/// <param name = "angl"> rotational angle in radians (type = float) </param>

/// <returns> 2-dimensional vector rotated by given angle around it's tail (counter-clockwise) (vec type = float * float) </returns>

let rot (a:vec) (angl: float) = 
 (fst(a) * cos(angl) - snd(a) * sin(angl), fst(a) * sin(angl) + snd(a) * cos(angl): vec)

//example of use: 

let vec_rot_1 = (1.0, 0 :vec)
let angle = (System.Math.PI/2.0 :float)

let vec_rot = rot vec_rot_1 angle

///////////////////////////////////////////////////////////////////////////


/// <summary>
/// Returns vectors coordinates as a 2-tuple of integers
/// </summary>

/// <param name = "a"> 2-dimensional vector (vec type = float * float) </param>


/// <returns> 2-tuple of integers </returns>

let toInt (a: vec) = int(fst(a)),int(snd(a))

// example of use: 

let vec_toInt = (5.54, 6.1: vec)

let int_touple = toInt vec_toInt


/// <summary>
/// Draws a line on a given canvas with a shape of a given vector, from a given position
/// (uses functions "add" and "toInt")
/// </summary>

/// <param name = "C"> canvas type from DIKU.Canvas, 1.0.1 library </param>
/// <param name = "color"> color type from DIKU.Canvas, 1.0.1 library </param>
/// <param name = "p"> 2-dimensional vector (position) (vec type = float * float) </param>
/// <param name = "v"> 2-dimensional vector (endpoint of line from center) (vec type = float * float) </param>

/// <returns> unit (line drawn on canvas) </returns>


let setVector (C :canvas) (color :color) (p :vec) (v :vec) =
    let position = toInt p
    let vector_temp = add p v
    let vector = toInt vector_temp //adds vector v to position p 

    do setLine C color position vector

// example of use:

let w = 600     // Canvas is needed to let the function setVector act on something, 
let h = 600     // therefore is included in the example
let C = Canvas.create w h 

let pos_setVector = (float(w)/2.0, float(h)/2.0 : vec) //using predefined width and height to center 
let vec_setVector = (float(w)/2.0, 0.0 : vec)


do setVector C black pos_setVector vec_setVector

do show C "2g1b" //part of the assigment, not necessary for example of use


/// <summary>
/// Creates a canvas of a given size with 36 lines from center distributed equally
/// (uses functions "rot" and "setVector")
/// </summary>


/// <param name = "w"> width of canvas (type = int) </param>
/// <param name = "h"> height of canvas (type = int) </param>
/// <param name = "s"> state connected to function "react" (type = state (float)) </param>

/// <returns> type canvas with 36 lines from center distributed equally </returns>

type state = float

let draw (w: int) (h: int) (s:state) =
    
    let C = Canvas.create w h
    
    let mutable turning_vector = (float(w/2), 0.0 :vec) //used for set line
    
    
    turning_vector <- rot turning_vector (s) // this line rotates all the lines by state

    let mutable guard = 0
    while guard <= 35 do
        setVector C black (float(w)/2.0, float(h)/2.0 : vec) (turning_vector)
        turning_vector <- rot turning_vector (System.Math.PI/18.0) // 2*pi/36 = pi/18 
        
        guard <- guard + 1
    C

// example of use:

let width = 600    
let height = 600    
let state_ex = 0 

let still_canvas = draw width height state_ex

do show still_canvas "2g1c" //part of the assigment, not necessary for example of use


/// <summary>
/// React function to update state in "draw" function, reacts to left and right arrow keys
/// </summary>


/// <param name = "s"> state type connected to "draw" (type = float) </param>
/// <param name = "k"> Canvas.key type from DIKU.Canvas, 1.0.1 library </param>

/// <returns> Option type: updates s with  left (-) and right (+) arrow keys (with 0.01),
/// else None </returns>

let react (s:state) (k:Canvas.key) : state option =
  match getKey k with
    | LeftArrow ->
      Some (s-0.01)
    | RightArrow ->
      Some (s+0.01)
    | _ -> None


// example of use with "runApp" and "draw":

let width_runApp = 600    
let height_runApp = 600    
let state_runApp = 0 


do runApp "2g1d" width_runApp height_runApp draw react state_runApp






