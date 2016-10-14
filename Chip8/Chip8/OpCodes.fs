module OpCodes
open chip8

//35 opcodes
pixelsvalues = chip8.pixelsvalues
let opcodes (op) = match op with 
| 00E0 ->  (let i = 0 in while i < pixelsvalues.Length do
                pixelsvalues.[i] <- 0us
                pixelsvalues.[i+1] <- 0us
                pixelsvalues.[i+2] <- 0us
                pixelsvalues.[i+3] <- 255us 
                i = i + 1)
                chip8.PC = chip8.PC + 2us

