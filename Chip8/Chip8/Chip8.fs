module chip8

let memory = Array.create 4096 0uy
let mutable PC = 0x200us
let mutable Vx = Array.create 16 0uy
let mutable I = 0us
// tableau representant la valeur noire = 0 blanc = 1 des 2048 pixels (affichage 64*32)
let screen = Array.create 2048 0uy

let romFile = ""


