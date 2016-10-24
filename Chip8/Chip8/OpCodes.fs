module OpCodes

// 35 opcodes

// 00E0 - Instruction pour effacer l'écran
let instruction_00E0 = let mutable i = 0 in while i < chip8.screen.Length do
                                                chip8.screen.[i] <- 0uy
                                                i <- i + 1 
                       chip8.PC <- chip8.PC + 2us

// 00EE - Retour depuis une sous-routine 
let instruction_00EE = chip8.SP <- chip8.SP - 1
                       chip8.PC <- chip8.stack.[chip8.SP]
                       chip8.PC <- chip8.PC + 2us

// 2NNN - Appelle la sous-routine à l'adresse NNN
let instruction_2NNN opcode = chip8.stack.[chip8.SP] <- chip8.PC
                              chip8.SP <- chip8.SP + 1
                              chip8.PC <- opcode &&& 0x0FFFus

// 3XKK - Passe à l'instruction suivante si VX = KK
let instruction_3XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] = KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 4XKK - Passe à l'instruction suivante si VX != KK
let instruction_4XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] <> KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 5XY0 - Passe l'instruction suivante si VX = VY
let instruction_5XY0 opcode = let X = int((opcode &&& 0x0F00us) >>> 8) in
                              let Y = int((opcode &&& 0x00F0us) >>> 4) in
                              if chip8.Vx.[X] = chip8.Vx.[Y] then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

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

// BNNN - Saute jusuq'à NNN + V[0]
let instruction_BNNN opcode = chip8.PC <- (opcode &&& 0x0FFFus) + uint16(chip8.Vx.[0])

// FX1E - Affecte VX + I à I
let instruction_FX1E opcode = chip8.I <- chip8.I + uint16 chip8.Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                              chip8.PC <- chip8.PC + 2us

// FX33 - Stock la représentation BCD de VX aux adresses I, I + 1, I + 2
let instruction_FX33 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in // number of Vx register
                              let B = int chip8.Vx.[X] / 100 in
                              let C = (int chip8.Vx.[X] / 10) % 10 in
                              let D = int chip8.Vx.[X] % 10 in
                              chip8.memory.[int chip8.I] <- byte B
                              chip8.memory.[int chip8.I + 1] <- byte C
                              chip8.memory.[int chip8.I + 2] <- byte D
                              chip8.PC <- chip8.PC + 2us


// FX55 charge les Vx en I -> I + 15
let instruction_FX55 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                chip8.memory.[int chip8.I + i] <- chip8.Vx.[i]
                              chip8.PC <- chip8.PC + 2us

// FX65 - Stock les valeurs de de la mémoire à l'adresse I jusqu'à I + X dans VO jusqu'à VX
let instruction_FX65 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                  chip8.Vx.[i] <- chip8.memory.[int chip8.I + i]
                              chip8.PC <- chip8.PC + 2us
