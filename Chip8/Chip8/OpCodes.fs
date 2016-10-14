module OpCodes
open chip8

//35 opcodes
screen = chip8.screen
//Instruction pour effacer l'écran 
let  instruction_00E0 = let i = 0 in  while i < screen.Length do
                screen.[i] <- 0u 
                i = i + 1 ;
                chip8.PC = chip8.PC + 2us

