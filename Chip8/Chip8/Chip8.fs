module chip8
open System
open System.Windows.Forms
open System.IO
open System.Drawing

let memory = Array.create 4096 0uy
let mutable PC = 0x200us
let mutable Vx = Array.create 16 0uy
let keys = Array.create 16 0uy
let OnKeyPress (args:KeyEventArgs) =

    match args.KeyCode with
    | Keys.D1 -> keys.[0x1] <- 1uy
    | Keys.D2 -> keys.[0x2] <- 1uy
    | Keys.D3 -> keys.[0x3] <- 1uy
    | Keys.D4 -> keys.[0xC] <- 1uy
    | Keys.Q -> keys.[0x4] <- 1uy
    | Keys.W -> keys.[0x5] <- 1uy
    | Keys.E -> keys.[0x6] <- 1uy
    | Keys.R -> keys.[0xD] <- 1uy
    | Keys.A -> keys.[0x7] <- 1uy
    | Keys.S -> keys.[0x8] <- 1uy
    | Keys.D -> keys.[0x9] <- 1uy
    | Keys.F -> keys.[0xE] <- 1uy
    | Keys.Z -> keys.[0xA] <- 1uy
    | Keys.X -> keys.[0x0] <- 1uy
    | Keys.C -> keys.[0xB] <- 1uy
    | Keys.V -> keys.[0xF] <- 1uy
    | _ -> ()

let OnKeyUp (args:KeyEventArgs) =
    match args.KeyCode with
    | Keys.D1 -> keys.[0x1] <- 0uy
    | Keys.D2 -> keys.[0x2] <- 0uy
    | Keys.D3 -> keys.[0x3] <- 0uy
    | Keys.D4 -> keys.[0xC] <- 0uy
    | Keys.Q -> keys.[0x4] <- 0uy
    | Keys.W -> keys.[0x5] <- 0uy
    | Keys.E -> keys.[0x6] <- 0uy
    | Keys.R -> keys.[0xD] <- 0uy
    | Keys.A -> keys.[0x7] <- 0uy
    | Keys.S -> keys.[0x8] <- 0uy
    | Keys.D -> keys.[0x9] <- 0uy
    | Keys.F -> keys.[0xE] <- 0uy
    | Keys.Z -> keys.[0xA] <- 0uy
    | Keys.X -> keys.[0x0] <- 0uy
    | Keys.C -> keys.[0xB] <- 0uy
    | Keys.V -> keys.[0xF] <- 0uy
    | _ -> ()

let mutable I = 0us

// Array representing the value (black = 0; white = 1) of the 2048 pixels (64*32 display)
let screen = Array.create 2048 0uy

let romFile = ""
let mutable DelayTimer = 0uy
let mutable SoundTimer = 0uy

type DoubleBufferForm() =
    inherit Form()
    do base.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.DoubleBuffer, true)

let Draw (args:PaintEventArgs) =
    let whiteBrush = new SolidBrush(Color.White)  
    for row in [0..31] do
        for col in [0..63] do
            if screen.[col + (row * 64)] <> 0uy then
                args.Graphics.FillRectangle(whiteBrush, col * 16, row * 16, 16, 16)
    whiteBrush.Dispose()

let form = new DoubleBufferForm()

// Create a stack storing adresses of subroutines (= functions).
// It is used to know which program to go back to once the current one is finished.
// It can be up to 16 levels deep
let stack = Array.create 16 0us

// Pointer to the top of our stack
let mutable SP = 0

// Characters 0-F (in hexadecimal) are represented by a 4x5 font.
let fontset = [|
    0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy; //0
    0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy; //1
    0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy; //2
    0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy; //3
    0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy; //4
    0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy; //5
    0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy; //6
    0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy; //7
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy; //8
    0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy; //9
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy; //A
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy; //B
    0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy; //C
    0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy; //D
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy; //E
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy |]  //F
