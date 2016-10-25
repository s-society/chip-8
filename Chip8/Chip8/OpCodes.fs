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

// 8XY0 Affecter VY à VX
let instruction_8XY0 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in // garde seulement la première valeur
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY1 Affecter resultat de VX ||| VY à VX
let instruction_8XY1 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] ||| chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY2 Affecter resultat de VX &&& VY à VX
let instruction_8XY2 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] &&& chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY3 Affecter VX xor VY à VX
let instruction_8XY3 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] ^^^ chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY4 Affecte VX + VY à VX si dépassemnt alors VX[F] = 1 sinon VX[F] = 0
let instruction_8XY4 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (int chip8.Vx.[X] + int chip8.Vx.[Y] > 255) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] + chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY5 Affecter VX-VY à VX.Si VX > VY alors VX[F] = 1 sinon VX[F] = 0
let instruction_8XY5 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (chip8.Vx.[X] > chip8.Vx.[Y]) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] - chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XY6 Fait un shift droit sur VX affecte le bit sorti à VX[F]
let instruction_8XY6 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              chip8.Vx.[0xF] <- chip8.Vx.[X] &&& 0b0000001uy
                              chip8.Vx.[X] <- chip8.Vx.[X] >>> 1
                              chip8.PC <- chip8.PC + 2us

//8XY7 Affecter VX-VY à VX.Si VX < VY alors VX[F] = 1 sinon VX[F] = 0
let instruction_8XY7 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (chip8.Vx.[X] < chip8.Vx.[Y]) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] - chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

//8XYE Fait un shift gauche sur VX affecte le bit sorti à VX[F] = 0
let instruction_8XYE opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              chip8.Vx.[0xF] <- (chip8.Vx.[X] &&& 0b10000000uy) >>> 7
                              chip8.Vx.[X] <- chip8.Vx.[X] <<< 1
                              chip8.PC <- chip8.PC + 2us

// 9XY0 - Passe à l'instruction suivante si VX != VY
let instruction_9XY0 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              let Y = int ((opcode &&& 0x00F0us) >>> 4) in
                              if chip8.Vx.[X] <> chip8.Vx.[Y] then chip8.PC <- 4us else chip8.PC <- 2us

// ANNN - Affecter NNN à I
let instruction_ANNN opcode = chip8.I <- ( opcode &&& 0x0FFFus) // garde seulement les 3 dernières valeurs pour les assigner
                              chip8.PC <- chip8.PC + 2us

// FX65 - Stock les valeurs de de la mémoire à l'adresse I jusqu'à I + X dans VO jusqu'à VX
let instruction_FX65 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                  chip8.Vx.[i] <- chip8.memory.[int chip8.I + i]
                              chip8.PC <- chip8.PC + 2us
