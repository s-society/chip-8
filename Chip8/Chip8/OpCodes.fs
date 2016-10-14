module OpCodes
open chip8

//35 opcodes
pixelsvalues = chip8.pixelsvalues
//Instruction pour effacer l'écran 
let  instruction_00E0 = let i = 0 in  while i < pixelsvalues.Length do
                pixelsvalues.[i] <- 0u 
                i = i + 1 ;
                chip8.PC = chip8.PC + 2us

