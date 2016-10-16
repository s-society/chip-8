module OpCodes

// 35 opcodes

// 00E0 - Instruction pour effacer l'écran
let  instruction_00E0 = let mutable i = 0 in while i < chip8.screen.Length do
                                                chip8.screen.[i] <- 0uy
                                                i <- i + 1 ;
                        chip8.PC <- chip8.PC + 2us

// 3XKK - Passe à l'instruction suivante si VX = KK
let instruction_3XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] = KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 4XKK - Passe à l'instruction suivante si VX != KK
let instruction_4XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] <> KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 6XKK - Affecter KK à VX
let instruction_6XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              chip8.Vx.[X] <- KK
                              chip8.PC <- chip8.PC + 2us

// 7XKK - Ajoute KK à VX
let instruction_7XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              chip8.Vx.[X] <- chip8.Vx.[X] + KK
                              chip8.PC <- chip8.PC + 2us

// 9XY0 - Passe à l'instruction suivante si VX != VY
let instruction_9XY0 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              let Y = int ((opcode &&& 0x00F0us) >>> 4) in
                              if chip8.Vx.[X] <> chip8.Vx.[Y] then chip8.PC <- 4us else chip8.PC <- 2us

// ANNN - Affecter NNN à I
let instruction_ANNN opcode = chip8.I <- ( opcode &&& 0x0FFFus) // garde seulement les 3 dernières valeurs pour les assigner
                              chip8.PC <- chip8.PC + 2us

// FX1E - Affecte VX + I à I
let instruction_FX1E opcode = chip8.I <- chip8.I + uint16 chip8.Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                              chip8.PC <- chip8.PC + 2us

// FX33 - Stock la représentation BCD de VX aux adresses I, I + 1, I + 2
let instruction_FX33 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in // number of Vx register
                              let B = chip8.Vx.[X] / 100uy in
                              let C = (chip8.Vx.[X] / 10uy) % 10uy in
                              let D = chip8.Vx.[X] % 10uy in
                              chip8.memory.[int chip8.I] <- B
                              chip8.memory.[int chip8.I + 1] <- C
                              chip8.memory.[int chip8.I + 2] <- D
                              chip8.PC <- chip8.PC + 2us

// FX65 - Stock les valeurs de de la mémoire à l'adresse I jusqu'à I + X dans VO jusqu'à VX
let instruction_FX65 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                  chip8.Vx.[i] <- chip8.memory.[int chip8.I + i]
                              chip8.PC <- chip8.PC + 2us
