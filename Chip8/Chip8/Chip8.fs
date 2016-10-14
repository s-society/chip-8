module chip8

let memory = Array.create 4096 0uy
let mutable PC = 0x200us
let Vx = Array.create 16 0uy
let I = 0us
// tableau representant la valeur R,G,B,A des 2048 pixels (affichage 64*32)
let pixelsvalues = Array.create 8192 0us

let romFile = ""


