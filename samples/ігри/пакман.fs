модуль Pacman

// Another great F# game by Phil Trelford! The code involves rendering the maze,
// AI для the ghosts, user interaction and even playing sound effects. There is
// some brief commentary, but якщо you're a beginner look at the other examples first.

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

модуль Sound =
    нехай [<Global>] Audio: obj = jsNative

    нехай origin =
        // Sample is running у an iframe, so get the location з parent
        нехай topLocation = window.top.location
        topLocation.origin + topLocation.pathname

    нехай play (fileName: string) =
        нехай audio = createNew Audio (origin + "img/pacman/" + fileName + ".wav")
        audio?play()

модуль Images =
    (**
    The following block embeds the ghosts and other parts з graphics as Base64 encoded strings.
    This way, we can load them without making additional server requests:
    *)
    нехай cyand = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAiUlEQVQoU8WSURKAIAhE8Sh6Fc/tVfQoJdqiMDTVV4wfufAAmw3kxEHUz4pA1I8OJVjAKZZ6+XiC0ATTB/gW2mEFtlpHLqaktrQ6TxUQSRCAPX2AWPMLyM0VmPOcV8palxt6uoAMpDjfWJt+o6cr0DPDnfYjyL94NwIcYjXcR/FuYklcxrZ3OO0Ep4dJ/3dR5jcAAAAASUVORK5CYII="
    нехай oranged = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAgklEQVQoU8WS0RGAIAxDZRRYhblZBUZBsBSaUk/9kj9CXlru4g7r1FxBdsFpGwoa2NwrYIFPEIeM6QS+hQQMYC70EjzuuOlt6gT5kRGGTf0Cx5qfwJYOYIw0L6W1bg+09Al2wAcCS8Y/WjqAZhluxD/B3ghZBO6n1sadzLLEbNSg8pzXIVLvbNvPwAAAAABJRU5ErkJggg=="
    нехай pinkd = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAj0lEQVQoU8WSsRWAIAxEZRQpXITGVZzIVWxYxAJHwRfwMInxqZV0XPIvgXeuM05eUuayG73TbULQwKWZGTTwCYIJphfwLcRhAW5DLfWrXFLrNLWBKAIBbOkFxJpfQDIXYAh1XoznumRo6Q0kwE8VTLN8o6UL0ArDnfYjSF/Mg4CEaA330sxD3ApHLvUdSdsBdgNkr9L8gxYAAAAASUVORK5CYII="
    нехай redd = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAkklEQVQoU8WSvRWAIAyEZRQtXIRCV3EiVtGCRSx0FHxBD5MYn1pJl0u+/PDOVcZLY5e47PrJ6TIhaOBSzBoU8AlCE0zP4FuIwwJc25Bz9TyILbVOUwuIJAjAlp5BrPkFpOYC9H6fF+O5LjW09AIS0Az7jUuQN1q6AC0z3Gk/gvTF3AhwiNYQ52Ju4pI4fKljOG0DA3tp97vN6C8AAAAASUVORK5CYII="
    нехай pu1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAWElEQVQoU62SUQoAIAhD9f6HNiYYolYi9VfzuXIxDRYbI0LCTHsfe3ldi3BgRRUY9Rnku1Rupf4NgiPeVjVU7STckphBceSvrHHtNPI21HWz4NO3eUUAgwVpmjX/zwK8KQAAAABJRU5ErkJggg=="
    нехай pu2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAW0lEQVQoU8WSwQoAIAhD9f8/2lIwdKRIl7o1e010THBESJiJXca76qnoDxFC3SD9LRpWkLnsLt4gdImtlLX/EK4iDapqr4VuI2+BauQjaOrmSz8xillDp5gQrS054jv/0fkNVAAAAABJRU5ErkJggg=="
    нехай pd1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAXElEQVQoU62SUQoAIAhD9f6HNgyMWpMs6k/XU5mqwDMTw5yq6JwbAfucwR2qAFHAu75BN11Gt6+Qz54VpMJsMV3BaS9UR8txkUzfLC9DUY0BYbOPGfpyU3g2WdwAOvU1/9KZsT4AAAAASUVORK5CYII="
    нехай pd2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAU0lEQVQoU62SUQoAIAhD9f6HNgwUGw4s6q/pc6KqwDMTQ01VtGr56ZIZvKEJEAXc9Q26cUm3r5D3zgrywHeoG3ldJrZIRz6C0I1BoR83FTBCeHsLIlw7/wOkQycAAAAASUVORK5CYII="
    нехай pl1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAVUlEQVQoU62S2woAIAhD9f8/2jAwvGRMyDfF49iQKZUISZ4xE/vZaW7LHbwhBLADqjpSUjBAdglRDQa9hxfcQi+vf5RGnpDlkB4KlMgR0N6pBIH83gIPFCb/N+MLCwAAAABJRU5ErkJggg=="
    нехай pl2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAUklEQVQoU52SUQoAIAhD3f0PbRQoZgnT/hyttYeQdFRFswYIoubD73JlPibGYA/s1Jmpk+JpDIinWxbiXP3iQslCwbhTxzhHbsWZNFsnCkTevQW2bCb/VRTuVwAAAABJRU5ErkJggg=="
    нехай pr1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAWElEQVQoU52S4Q4AIASE3fs/tKalSTHyL/O5CyAXzMQ+BxBsbj9exRE8oQqgDUS1BalNVFSuP2WQL94WIygCBEzttZWOvbz2VBnGtLXg1sgV/L8I679yewN9sScO5wcxLQAAAABJRU5ErkJggg=="
    нехай pr2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAVElEQVQoU62SWwoAIAgE9f6HNgqU3BK2R3+J48KoCjwzMaypis61+OyaK3hADOADeuoddJISaQy0iKggbEz2viah7mVPTNq7cp/ApLmcdFPVdaDJBnWdJwjk629HAAAAAElFTkSuQmCC"
    нехай blue = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAeklEQVQoU62S0Q3AIAhEyyi6UcfoRB2jG+koNkeCoVcaTaw/huMeEkS24KTUmpdrFWHbQ2CAzb5AB0eQFTFYwVnIw/+B5by0cD52vTmGhnaF25wBAb/A6HsibR0ctch5fRHi1zCigvCut4oR+wnbhrBmsZr9DlqCQfbcnfZjDyiZqCEAAAAASUVORK5CYII="
    нехай eyed = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAUElEQVQoU2NkIBMwkqmPYYA13rt37z/I6UpKSiguwSYOVwCThPkZphmXOHU0OjtD7Nu7F+FckI3YxFH8oqgI8eP9+6h+xCY+wNFBSiqiv1MBDgYsD185vj8AAAAASUVORK5CYII="
    нехай _200 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAS0lEQVQoU2NkIBMwkqmPYYA0vpVR+Q9zsvCTO4yE+CC1KE4FaYBpxEfDNWKzgWiNIIUw5xKyGa+N+PyM4UdS4nSA4pEUJ8LUku1UAMC0VA8iscBNAAAAAElFTkSuQmCC"
    нехай _400 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAASElEQVQoU2NkIBMwkqmPYYA0vpVR+S/85A4jMg3zAkwcmQ9ig52KTSO6Qch8FI3oNhClEaaJWJvhNmLTSJQfyYnLAYpHujoVAChTXA9pVJi5AAAAAElFTkSuQmCC"
    нехай _800 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAQElEQVQoU2NkIBMwkqmPYYA0vpVR+Q9zsvCTO4yE+CC1YKeCFMI0EEOjaES3EZ8BtLERn5/hNpITlwMUj3R1KgCe5lwPHtUmcwAAAABJRU5ErkJggg=="
    нехай _1600 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAQ0lEQVQoU2NkIBMwkqmPYQA0vpVR+S/85A4jiIY5mxg+WANMIYiGaUYXR+ejaES3EdlAvBrxKSTJRnx+HoDoGDopBwDHLGwPAhDgRQAAAABJRU5ErkJggg=="

    // Create image using the specified data
    нехай createImage data =
      нехай img = document.createElement("img") :?> HTMLImageElement
      img.src <- data
      img

    // Load the different Pacman images
    нехай приватний pu1Img, pu2Img =
      createImage pu1, createImage  pu2
    нехай приватний pd1Img, pd2Img =
      createImage pd1, createImage  pd2
    нехай приватний pl1Img, pl2Img =
      createImage pl1, createImage pl2
    нехай приватний pr1Img, pr2Img =
      createImage pr1, createImage pr2

    // Represent Pacman's mouth state
    нехай приватний lastp = ref pr1Img

    (**
    This функція returns the pacman image для the specified X and Y location, taking into account the
    direction у which Pacman is going. It keeps a змінливий state із current step з Pacman's
    mouth.
    *)
    нехай imageAt(x: _ ref, y: _ ref, v: _ ref) =
      нехай p1, p2 =
        співстав v.Value із
        | -1,  0 -> pl1Img, pl2Img
        |  1,  0 -> pr1Img, pr2Img
        |  0, -1 -> pu1Img, pu2Img
        |  0,  1 -> pd1Img, pd2Img
        |  _,  _ -> lastp.Value, lastp.Value
      нехай x' = int (floor(float (x.Value/6)))
      нехай y' = int (floor(float (y.Value/6)))
      нехай p = якщо (x' + y') % 2 = 0 тоді p1 інакше p2
      lastp.Value <- p
      p

модуль Keyboard =

    /// Set з currently pressed keys
    нехай змінливий keysPressed = Set.empty
    /// Update the keys as requested
    нехай reset () = keysPressed <- Set.empty
    нехай isPressed keyCode = Set.contains keyCode keysPressed

    /// Triggered when key is pressed/released
    нехай update (e : KeyboardEvent, pressed) =
      нехай key = e.key
      нехай op =  якщо pressed тоді Set.add інакше Set.remove
      keysPressed <- op key keysPressed

    /// Register DOM event handlers
    нехай init () =
      window.addEventListener("keydown", фун e -> update(e :?> _, true))
      window.addEventListener("keyup", фун e -> update(e :?> _, false))

модуль Types =
    (**
    Creating ghosts
    ===============
    Ghosts are represented by a simple F# class тип that contains the image з the ghost,
    current X, Y positions and a velocity у both directions. In Pacman, ghosts are змінливий
    and expose `Move` and `Reset` methods that change their properties.
    *)

    /// Wrap around the sides з the Maze
    нехай wrap (x,y) (dx,dy) =
      нехай x =
        якщо dx = -1 && x = 0 тоді 30 * 8
        інякщо dx = 1  && x = 30 *8 тоді 0
        інакше x
      x + dx, y + dy

    /// Mutable representation з a ghost
    тип Ghost(image: HTMLImageElement,x,y,v) =
      нехай змінливий x' = x
      нехай змінливий y' = y
      нехай змінливий v' = v
      член val Image = image
      член val IsReturning = false із get, set
      член __.X = x'
      член __.Y = y'
      член __.V = v'
      /// Move back to initial location
      член ghost.Reset() =
        x' <- x
        y' <- y
      /// Move у the current direction
      член ghost.Move(v) =
        v' <- v
        нехай dx,dy = v
        нехай x,y = wrap (x',y') (dx,dy)
        x' <- x
        y' <- y

відкрити Images
відкрити Types

(**
Here we define the maze, tile bits and blank block. The maze is defined as one big string
using ASCII-art encoding. Where `/`, `7`, `L` and `J` represent corners (upper-left, upper-right,
lower-left and lower-right), `!`, `|`, `-` and `_` represent walls (left, right, top, bottom) while
`o` and `.` represent two kinds з pills у the maze.
*)

нехай maze =
 [| "##/------------7/------------7##"
    "##|............|!............|##"
    "##|./__7./___7.|!./___7./__7.|##"
    "##|o|  !.|   !.|!.|   !.|  !o|##"
    "##|.L--J.L---J.LJ.L---J.L--J.|##"
    "##|..........................|##"
    "##|./__7./7./______7./7./__7.|##"
    "##|.L--J.|!.L--7/--J.|!.L--J.|##"
    "##|......|!....|!....|!......|##"
    "##L____7.|L__7 |! /__J!./____J##"
    "#######!.|/--J LJ L--7!.|#######"
    "#######!.|!          |!.|#######"
    "#######!.|! /__==__7 |!.|#######"
    "-------J.LJ |      ! LJ.L-------"
    "########.   | **** !   .########"
    "_______7./7 |      ! /7./_______"
    "#######!.|! L______J |!.|#######"
    "#######!.|!          |!.|#######"
    "#######!.|! /______7 |!.|#######"
    "##/----J.LJ L--7/--J LJ.L----7##"
    "##|............|!............|##"
    "##|./__7./___7.|!./___7./__7.|##"
    "##|.L-7!.L---J.LJ.L---J.|/-J.|##"
    "##|o..|!.......<>.......|!..o|##"
    "##L_7.|!./7./______7./7.|!./_J##"
    "##/-J.LJ.|!.L--7/--J.|!.LJ.L-7##"
    "##|......|!....|!....|!......|##"
    "##|./____JL__7.|!./__JL____7.|##"
    "##|.L--------J.LJ.L--------J.|##"
    "##|..........................|##"
    "##L--------------------------J##" |]

нехай tileBits =
 [| [|0b00000000;0b00000000;0b00000000;
      0b00000000;0b00000011;0b00000100;
      0b00001000;0b00001000|]

    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000|] // top
    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11000000;0b00100000;0b00010000;0b00010000|] // tr
    [|0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000|] // left
    [|0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000|] // right
    [|0b00001000;0b00001000;0b00000100;0b00000011;0b00000000;0b00000000;0b00000000;0b00000000|] // bl
    [|0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000;0b00000000|] // bottom
    [|0b00010000;0b00010000;0b00100000;0b11000000;0b00000000;0b00000000;0b00000000;0b00000000|] // br
    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000|] // door
    [|0b00000000;0b00000000;0b00000000;0b00011000;0b00011000;0b00000000;0b00000000;0b00000000|] // pill
    [|0b00000000;0b00011000;0b00111100;0b01111110;0b01111110;0b00111100;0b00011000;0b00000000|] // power
 |]

нехай blank =
  [| 0b00000000;0b00000000;0b00000000; 0b00000000;0b00000000;0b00000000;0b00000000;0b00000000 |]

(**
Check для walls:
The following functions parse the maze representation and check various properties з the maze.
Those are used для rendering, but also для checking whether Pacman can go у a given direction.
Characters _|!/7LJ represent different walls
*)

нехай isWall (c:char) =
  "_|!/7LJ-".IndexOf(c) <> -1

/// Returns ' ' для positions outside з range
нехай tileAt (x,y) =
  якщо x < 0 || x > 30 тоді ' ' інакше maze.[y].[x]

/// Is the maze tile at x,y a wall?
нехай isWallAt (x,y) =
  tileAt(x,y) |> isWall

// Is Pacman at a point where it can turn?
нехай verticallyAligned (x,y) =  (x % 8) = 5
нехай horizontallyAligned (x,y) = (y % 8) = 5
нехай isAligned n = (n % 8) = 5

// Check whether Pacman can go у given direction
нехай noWall (x,y) (ex,ey) =
  нехай bx, by = (x+6+ex) >>> 3, (y+6+ey) >>> 3
  isWallAt (bx,by) |> not

нехай canGoUp (x,y) = isAligned x && noWall (x,y) (0,-4)
нехай canGoDown (x,y) = isAligned x && noWall (x,y) (0,5)
нехай canGoLeft (x,y) = isAligned y && noWall (x,y) (-4,0)
нехай canGoRight (x,y) = isAligned y && noWall (x,y) (5,0)

(**
Background rendering
================================
To render the background, we first fill the background
and тоді iterate over the string lines that represent the maze and we draw images з
walls specified у the `tileBits` value earlier (or use `blank` tile для all other characters).

The following is used to map from tile characters to the `tileBits` values and to draw individual lines:
*)
нехай tileColors = "BBBBBBBBBYY"
нехай tileChars =  "/_7|!L-J=.o"

/// Returns tile для a given Maze character
нехай toTile (c:char) =
  нехай i = tileChars.IndexOf(c)
  якщо i = -1 тоді blank, 'B'
  інакше tileBits.[i], tileColors.[i]

/// Draw the lines specified by a wall tile
нехай draw f (lines:int[]) =
  нехай width = 8
  lines |> Array.iteri (фун y line ->
    для x = 0 to width-1 зробити
      нехай bit = (1 <<< (width - 1 - x))
      нехай pattern = line &&& bit
      якщо pattern <> 0 тоді f (x,y) )

/// Creates a brush для rendering the given RGBA color
нехай createBrush (context:CanvasRenderingContext2D) (r,g,b,a) =
  нехай id = context.createImageData(1.0, 1.0)
  нехай d = id.data
  d.[0] <- r; d.[1] <- g
  d.[2] <- b; d.[3] <- a
  id

(**
The main функція для rendering background just fills the canvas із a black color and
тоді iterates over the maze tiles and renders individual walls:
*)
нехай createBackground () =
  // Fill background із black
  нехай background = document.createElement("canvas") :?> HTMLCanvasElement
  background.width <- 256.
  background.height <- 256.
  нехай context = background.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);

  // Render individual tiles з the maze
  нехай blue = createBrush context (63uy, 63uy, 255uy, 255uy)
  нехай yellow = createBrush context (255uy, 255uy, 0uy, 255uy)
  нехай lines = maze
  для y = 0 to lines.Length-1 зробити
    нехай line = lines.[y]
    для x = 0 to line.Length-1 зробити
      нехай c = line.[x]
      нехай tile, color = toTile c
      нехай brush = співстав color із 'Y' -> yellow | _ -> blue
      нехай f (x',y') =
        context.putImageData
          (brush, float (x*8 + x'), float (y*8 + y'))
      draw f tile
  background

/// Clear whatever is rendered у the specified Maze cell
нехай clearCell (background : HTMLCanvasElement) (x,y) =
  нехай context = background.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (float (x*8), float (y*8), 8., 8.)

нехай createGhosts context =
  [| Images.redd, (16, 11), (1,0)
     Images.cyand, (14, 15), (1,0)
     Images.pinkd, (16, 13), (0,-1)
     Images.oranged, (18, 15), (-1,0) |]
  |> Array.map (фун (data,(x,y),v) ->
        Ghost(Images.createImage data, (x*8)-7, (y*8)-3, v) )

(**
Generating Ghost movement
=========================
For generating Ghost movements, we need an implementation з the [Flood fill algorithm](https://en.wikipedia.org/wiki/Flood_fill),
which we use to generate the shortest path home when Ghosts are returning. The `fillValue` функція does this, by starting
at a specified location (which can be one з the directions у which ghosts can go).
*)

/// Recursive flood fill функція
нехай flood canFill fill (x,y) =
  нехай rec f n = функція
    | [] -> ()
    | ps ->
        нехай ps = ps |> List.filter (фун (x,y) -> canFill (x,y))
        ps |> List.iter (фун (x,y) -> fill (x,y,n))
        ps |> List.collect (фун (x,y) ->
            [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]) |> f (n+1)
  f 0 [(x,y)]

/// Possible routes that take the ghost home
нехай homeRoute =
  нехай numbers =
    maze |> Array.map (фун line ->
      line.ToCharArray()
      |> Array.map (фун c -> якщо isWall c тоді 999 інакше -1) )
  нехай canFill (x:int,y:int) =
    y>=0 && y < (numbers.Length-1) &&
    x>=0 && x < (numbers.[y].Length-1) &&
    numbers.[y].[x] = -1
  нехай fill (x,y,n) = numbers.[y].[x] <- n
  flood canFill fill (16,15)
  numbers

/// Find the shortest way home from specified location
/// (adjusted by offset у which ghosts start)
нехай fillValue (x,y) (ex,ey) =
  нехай bx = int (floor(float ((x+6+ex)/8)))
  нехай by = int (floor(float ((y+6+ey)/8)))
  homeRoute.[by].[bx]

нехай fillUp (x,y) = fillValue (x,y) (0,-4)
нехай fillDown (x,y) = fillValue (x,y) (0,5)
нехай fillLeft (x,y) = fillValue (x,y) (-4,0)
нехай fillRight (x,y) = fillValue (x,y) (5,0)

(**
When choosing a direction, ghosts that are returning will go у the direction
that leads them home. Other ghosts generate a list з possible directions (the `directions` array)
and тоді filter those that are у the direction з Pacman and choose one з the options. If they
are stuck and cannot go у any way, they stay where they are.
*)
нехай chooseDirection (ghost:Ghost) =
  нехай x,y = ghost.X, ghost.Y
  нехай dx,dy = ghost.V
  // Are we facing towards the given point?
  нехай isBackwards (a,b) =
    (a <> 0 && a = -dx) || (b <> 0 && b = -dy)
  // Generate array із possible directions
  нехай directions =
    [|якщо canGoLeft(x,y) тоді поступатися (-1,0), fillLeft(x,y)
      якщо canGoDown(x,y) тоді поступатися (0,1), fillDown(x,y)
      якщо canGoRight(x,y) тоді поступатися (1,0), fillRight(x,y)
      якщо canGoUp(x,y) тоді поступатися (0,-1), fillUp(x,y) |]

  якщо ghost.IsReturning тоді
    // Returning ghosts find the shortest way home
    нехай xs = directions |> Array.sortBy snd
    нехай v, n = xs.[0]
    якщо n = 0 тоді ghost.IsReturning <- false
    v
  інакше
    // Other ghosts pick one direction twoards Pacman
    нехай xs =
      directions
      |> Array.map fst
      |> Array.filter (not << isBackwards)
    якщо xs.Length = 0 тоді 0, 0
    інакше
      нехай randomNum = System.Random().NextDouble()
      нехай i = randomNum * float xs.Length
      xs.[int (floor i)]

/// Count number з dots у the maze
нехай countDots () =
  maze |> Array.sumBy (фун line ->
    line.ToCharArray()
    |> Array.sumBy (функція '.' -> 1 | 'o' -> 1 | _ -> 0))

(**
## The game play функція

Most з the Pacman game logic is wrapped у the top-level `playLevel` функція. This takes two functions - that are called
when the game completes - and тоді it initializes the world state and runs у a loop until the end з the game.
The following outlines the structure з the функція:

    нехай playLevel (onLevelCompleted, onGameOver) =
      // (Create canvas, background and ghosts)
      // (Define the Pacman state)
      // (Move ghosts and Pacman)
      // (Detect pills and collisiions)
      // (Rendering everything у the game)
      нехай rec update () =
        logic ()
        render ()
        якщо dotsLeft.Value = 0 тоді onLevelCompleted()
        інякщо energy.Value <= 0 тоді onGameOver()
        інакше window.setTimeout(update, 1000. / 60.) |> ignore

      update()

After defining all the helpers, the `update` функція runs у a loop (via a timer) until there are no dots
left or until the Pacman is out з energy and тоді it calls one з the continuations.

In the following 5 sections, we'll look at the 5 blocks з code that define the body з the функція.
*)

нехай playLevel (onLevelCompleted, onGameOver) =
  (**
  ### Create canvas, background and ghosts
  In the first part, the функція finds the `<canvas>` element, paints it із black background and
  creates other graphical elements - namely the game background, ghosts and eyes:
  *)
  // Fill the canvas element
  нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
  canvas.width <- 256.
  canvas.height <- 256.
  нехай context = canvas.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);
  нехай bonusImages =
    [| createImage Images._200; createImage Images._400;
       createImage Images._800; createImage Images._1600 |]

  // Load images для rendering
  нехай background = createBackground()
  нехай ghosts = createGhosts(context)
  нехай blue,eyed = createImage Images.blue, createImage Images.eyed

  (**
  ### Define the Pacman state
  Next, we define the game state. Pacman game uses змінливий state, so the following uses
  F# reference cells; `ref 0` creates a змінливий cell containing `0`. Later, we will access
  the value by writing `score.Value` and mutate it by writing `score.Value <- score.Value + 1`.
  *)
  нехай pills = maze |> Array.map (фун line ->
    line.ToCharArray() |> Array.map id)
  нехай dotsLeft = ref (countDots())
  нехай score = ref 0
  нехай bonus = ref 0
  нехай bonuses = ref []
  нехай energy = ref 128
  нехай flashCountdown = ref 0
  нехай powerCountdown = ref 0
  нехай x, y = ref (16 * 8 - 7), ref (23 * 8 - 3)
  нехай v = ref (0,0)

  нехай moveGhosts () =
    ghosts |> Array.iter (фун ghost ->
      ghost.Move(chooseDirection ghost)
    )

  нехай movePacman () =
    // In which directions should pacman go?
    нехай inputs =
       [| якщо Keyboard.isPressed "ArrowUp" тоді
            поступатися canGoUp (x.Value,y.Value), (0,-1)
          якщо Keyboard.isPressed "ArrowDown" тоді
            поступатися canGoDown (x.Value,y.Value), (0,1)
          якщо Keyboard.isPressed "ArrowLeft" тоді
            поступатися canGoLeft (x.Value,y.Value), (-1,0)
          якщо Keyboard.isPressed "ArrowRight" тоді
            поступатися canGoRight (x.Value,y.Value), (1,0) |]
    // Can we continue у the same direction?
    нехай canGoForward =
      співстав v.Value із
      | 0,-1 -> canGoUp(x.Value,y.Value)
      | 0,1  -> canGoDown(x.Value,y.Value)
      | -1,0 -> canGoLeft(x.Value,y.Value)
      | 1, 0 -> canGoRight(x.Value,y.Value)
      | _ -> false
    // What новий directions can we take?
    нехай availableDirections =
      inputs
      |> Array.filter fst
      |> Array.map snd
      |> Array.sortBy (фун v' -> v' = v.Value)
    якщо availableDirections.Length > 0 тоді
      // Choose the first one, prefers no change
      v.Value <- availableDirections.[0]
    інякщо inputs.Length = 0 || not canGoForward тоді
      // There are no options - stop
      v.Value <- 0,0

    // Update X and Y accordingly
    нехай x',y' = wrap (x.Value,y.Value) v.Value
    x.Value <- x'
    y.Value <- y'

  // Check якщо Pacman eats a pill at current cell
  нехай eatPills () =
    нехай tx = int (floor(float ((x.Value+6)/8)))
    нехай ty = int (floor(float ((y.Value+6)/8)))
    нехай c = pills.[ty].[tx]
    якщо c = '.' тоді
      // Eating a small pill increments the score
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      score.Value <- score.Value + 10
      dotsLeft.Value <- dotsLeft.Value - 1
      Sound.play "Dot5"
    якщо c = 'o' тоді
      // Eating a large pill turns on the power mode
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      bonus.Value <- 0
      score.Value <- score.Value + 50
      powerCountdown.Value <- 250
      dotsLeft.Value <- dotsLeft.Value - 1
      Sound.play "Powerup"

  /// Are there any ghosts that collide із Pacman?
  нехай touchingGhosts () =
    нехай px, py = x.Value, y.Value
    ghosts |> Array.filter (фун ghost ->
      нехай x,y = ghost.X, ghost.Y
      ((px >= x && px < x + 13) ||
       (x < px + 13 && x >= px)) &&
      ((py >= y && py < y + 13) ||
       (y < py + 13 && y >= py)) )

(**
The `collisionDetection` функція implements the right response to collision із a ghost:
*)
  /// Handle collision detections between Pacman and ghosts
  нехай collisionDetection () =
    нехай touched = touchingGhosts ()
    якщо touched.Length > 0 тоді
      якщо powerCountdown.Value > 0 тоді
        // Pacman is eating ghosts!
        touched |> Array.iter (фун ghost ->
          якщо not ghost.IsReturning тоді
            Sound.play "EatGhost"
            ghost.IsReturning <- true
            нехай added = int (2. ** (float bonus.Value))
            score.Value <- score.Value + added * 200
            нехай image = bonusImages.[bonus.Value]
            bonuses.Value <- (100, (image, ghost.X, ghost.Y)) :: bonuses.Value
            bonus.Value <-  min 3 (bonus.Value + 1) )
      інакше
        // Pacman loses energy when hitting ghosts
        energy.Value <- energy.Value - 1
        якщо flashCountdown.Value = 0 тоді Sound.play "Hurt"
        flashCountdown.Value <- 30
    якщо flashCountdown.Value > 0 тоді flashCountdown.Value <- flashCountdown.Value - 1

  /// Updates bonus points
  нехай updateBonus () =
    нехай removals,remainders =
      bonuses.Value
      |> List.map (фун (count,x) -> count-1,x)
      |> List.partition (fst >> (=) 0)
    bonuses.Value <- remainders

(**
The logic is called from the following single `logic` функція that includes all the checks:
*)
  нехай logic () =
    moveGhosts()
    movePacman()
    eatPills ()
    якщо powerCountdown.Value > 0 тоді
        powerCountdown.Value <- powerCountdown.Value - 1
    collisionDetection()
    updateBonus ()

(**
### Rendering everything у the game

When rendering everything у the game, we first draw the background and тоді we render
individual components. Those include the score, remaining energy, pacman, ghosts and bonuses.
Each з those is handled by a single nested функція that are put together у `render`.
We start із Pacman and remaining energy:
*)
  нехай renderPacman () =
    нехай p = Images.imageAt(x,y,v)
    якщо (flashCountdown.Value >>> 1) % 2 = 0
    тоді context.drawImage(!^ p, float x.Value, float y.Value)

  нехай renderEnergy () =
    context.fillStyle <- !^ "yellow"
    context.fillRect(120., 250., float energy.Value, 2.)
(**
The next three functions render ghosts, current score and bonuses:
*)
  нехай renderGhosts () =
    ghosts |> Array.iter (фун ghost ->
      нехай image =
        якщо ghost.IsReturning тоді eyed
        інакше
          якщо powerCountdown.Value = 0 тоді ghost.Image
          інякщо powerCountdown.Value > 100 ||
                ((powerCountdown.Value >>> 3) % 2) <> 0 тоді blue
          інакше ghost.Image
      context.drawImage(!^ image, float ghost.X, float ghost.Y) )

  нехай renderScore () =
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    context.fillText("Score " + (score.Value).ToString(), 0., 255.)

  нехай renderBonus () =
    bonuses.Value |> List.iter (фун (_,(image,x,y)) ->
      context.drawImage(!^ image, float x, float y))

  нехай render () =
    context.drawImage(!^ background, 0., 0.)
    renderScore ()
    renderEnergy ()
    renderPacman()
    renderGhosts ()
    renderBonus ()

  нехай rec update () =
    logic ()
    render ()
    якщо dotsLeft.Value = 0 тоді onLevelCompleted()
    інякщо energy.Value <= 0 тоді onGameOver()
    інакше window.setTimeout(update, 1000 / 60) |> ignore

  update()

(**
Game entry point
================
Now we have everything we need to start the game, so the last step is to define the
`levelCompleted` and `gameOver` functions (that are called when the game ends), render
the starting state з the game (із "CLICK TO START" text) and start the game!
*)
нехай rec game () =
  // Initialize keyboard and canvas
  Keyboard.reset()
  нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
  нехай context = canvas.getContext_2d()

  // A helper функція to draw text
  нехай drawText(text,x,y) =
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    context.fillText(text, x, y)

  // Called when level is completed
  нехай levelCompleted () =
    drawText("COMPLETED",96.,96.)
    window.setTimeout(game, 5000) |> ignore

  // Called when the game ends
  нехай gameOver () =
    drawText("GAME OVER",96.,96.)
    window.setTimeout(game, 5000) |> ignore

  // Start a новий game after click!
  нехай start () =
    нехай background = createBackground()
    context.drawImage(!^ background, 0., 0.)
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    drawText("CLICK TO START", 88., 96.)
    нехай змінливий playing = false
    canvas.addEventListener("click", фун _ ->
        якщо not playing тоді
            playing <- true
            playLevel (levelCompleted, gameOver))

  // Resize canvas and get ready для a game
  нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
  canvas.width <- 256.
  canvas.height <- 256.
  start()

// At the beginning, initialize keyboard & start the first game.
Keyboard.init ()
game ()
