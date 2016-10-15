﻿module OpCodes

//35 opcodes

//Instruction pour effacer l'écran
let  instruction_00E0 = let mutable i = 0 in while i < chip8.screen.Length do
                                                chip8.screen.[i] <- 0uy
                                                i <- i + 1 ;
                        chip8.PC <- chip8.PC + 2us

// ANNN - Affecter NNN à I
let instruction_ANNN opcode = chip8.I <- ( opcode &&& 0x0FFFus) // garde seulement les 3 dernières valeurs pour les assigner
                              chip8.PC <- chip8.PC + 2us