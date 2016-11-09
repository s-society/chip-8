module OpCodes

// Needed for the randomizing opcode CXKK
let random = System.Random()

// 34 opcodes needed for the execution of ROMs

// 00E0 - Instruction to clear the screen
let instruction_00E0 () = let mutable i = 0 in while i < chip8.screen.Length do
                                                chip8.screen.[i] <- 0uy
                                                i <- i + 1
                          chip8.PC <- chip8.PC + 2us

// 00EE - Go back from a subroutine
let instruction_00EE () = chip8.SP <- chip8.SP - 1
                          chip8.PC <- chip8.stack.[chip8.SP]
                          chip8.PC <- chip8.PC + 2us

// 1NNN - Jump to address NNN
let instruction_1NNN opcode = chip8.PC <- opcode &&& 0x0FFFus

// 2NNN - Call subroutine at memory address NNN
let instruction_2NNN opcode = chip8.stack.[chip8.SP] <- chip8.PC
                              chip8.SP <- chip8.SP + 1
                              chip8.PC <- opcode &&& 0x0FFFus

// 3XKK - Skip next instruction if VX = KK
let instruction_3XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] = KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 4XKK - Skip next instruction if VX != KK
let instruction_4XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              if chip8.Vx.[X] <> KK then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 5XY0 - Skip next instruction if VX = VY
let instruction_5XY0 opcode = let X = int((opcode &&& 0x0F00us) >>> 8) in
                              let Y = int((opcode &&& 0x00F0us) >>> 4) in
                              if chip8.Vx.[X] = chip8.Vx.[Y] then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// 6XKK - Store KK in VX
let instruction_6XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              chip8.Vx.[X] <- KK
                              chip8.PC <- chip8.PC + 2us

// 7XKK - Add KK to VX
let instruction_7XKK opcode = let X = int (opcode &&& 0x0F00us >>> 8) in //garde seulement la première valeur
                              let KK = byte (opcode &&& 0x00FFus) in
                              chip8.Vx.[X] <- chip8.Vx.[X] + KK
                              chip8.PC <- chip8.PC + 2us

// 8XY0 - Store VY in VX
let instruction_8XY0 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in // garde seulement la première valeur
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY1 - Store result of (VX ||| VY) in VX
let instruction_8XY1 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] ||| chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY2 - Store result of (VX &&& VY) in VX
let instruction_8XY2 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] &&& chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY3 - Store result of (VX ^^^ VY) in VX
let instruction_8XY3 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              chip8.Vx.[X] <- chip8.Vx.[X] ^^^ chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY4 - Store VX + VY in VX; if overload then VF = 1 else VF = 0
let instruction_8XY4 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (int chip8.Vx.[X] + int chip8.Vx.[Y] > 255) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] + chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY5 - Store VX - VY in VX; if VX > VY then VF = 1 else VF = 0
let instruction_8XY5 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (chip8.Vx.[X] > chip8.Vx.[Y]) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] - chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XY6 - Apply a right shift on VX and store the additional bit in VF
let instruction_8XY6 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              chip8.Vx.[0xF] <- chip8.Vx.[X] &&& 0b0000001uy
                              chip8.Vx.[X] <- chip8.Vx.[X] >>> 1
                              chip8.PC <- chip8.PC + 2us

// 8XY7 - Store VX - VY in VX; if VX < VY then VF = 1 else VF = 0
let instruction_8XY7 opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              let Y = int (opcode &&& 0x00F0us >>> 4) in
                              if (chip8.Vx.[X] < chip8.Vx.[Y]) then chip8.Vx.[0xF] <- 1uy else chip8.Vx.[0xF] <- 0uy
                              chip8.Vx.[X] <- chip8.Vx.[X] - chip8.Vx.[Y]
                              chip8.PC <- chip8.PC + 2us

// 8XYE - Apply a left shift on VX and store the additional bit in VF = 0
let instruction_8XYE opcode = let X = int (opcode &&& 0x0F00us >>> 8) in
                              chip8.Vx.[0xF] <- (chip8.Vx.[X] &&& 0b10000000uy) >>> 7
                              chip8.Vx.[X] <- chip8.Vx.[X] <<< 1
                              chip8.PC <- chip8.PC + 2us

// 9XY0 - Skip next instruction if VX != VY
let instruction_9XY0 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              let Y = int ((opcode &&& 0x00F0us) >>> 4) in
                              if chip8.Vx.[X] <> chip8.Vx.[Y] then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// ANNN - Store NNN in I
let instruction_ANNN opcode = chip8.I <- ( opcode &&& 0x0FFFus) // garde seulement les 3 dernières valeurs pour les assigner
                              chip8.PC <- chip8.PC + 2us

// BNNN - Jump to NNN + V0
let instruction_BNNN opcode = chip8.PC <- (opcode &&& 0x0FFFus) + uint16(chip8.Vx.[0])

// CXKK - Randomizer through XOR between KK and a random number
let instruction_CXKK opcode = let KK = int (opcode &&& 0x00FFus) in
                              let NN = random.Next() in
                              let X = int(opcode &&& 0x0F00us >>> 8) in
                              chip8.Vx.[X] <- byte(KK ^^^ NN)
                              chip8.PC<-chip8.PC + 2us

//DXYK - Draw a sprite at coordinates X,Y of size K
let instruction_DXYK opcode = let X = chip8.Vx.[ int ((opcode &&& 0x0F00us) >>> 8) ]  in
                              let Y = chip8.Vx.[ int ((opcode &&& 0x00F0us) >>> 4)] in
                              let Height = byte (opcode &&& 0x000Fus) in
                              chip8.Vx.[0xF] <- 0uy
                              for lines in [0..(int Height-1)] do
                                  let linedata = chip8.memory.[int chip8.I + lines]
                                  let mutable bit = 0b10000000uy
                                  for width in [0..7] do
                                      let pixelIndex = int X + width + (int Y + lines) * 64 in
                                      if pixelIndex < chip8.screen.Length then
                                        let screenPixel = chip8.screen.[pixelIndex] in
                                          if linedata &&& bit > 0uy then do 
                                              if screenPixel = 1uy then chip8.Vx.[0xF] <- 1uy
                                              chip8.screen.[pixelIndex] <- screenPixel ^^^ 1uy
                                        bit <- bit >>> 1

                              chip8.form.Invalidate()
                              chip8.PC <- chip8.PC + 2us
                                  
// EX9E - Skip next instruction if VX key is pressed
let instruction_EX9E opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              if chip8.keys.[int chip8.Vx.[X]] <> 0uy then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// EXA1 - Skip next instruction if VX key isn't pressed
let instruction_EXA1 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              if chip8.keys.[int chip8.Vx.[X]] = 0uy then chip8.PC <- chip8.PC + 4us else chip8.PC <- chip8.PC + 2us

// FX0A - Wait for a key press
let rec instruction_FX0Ar opcode index = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                                         if index < 16 then
                                           if chip8.keys.[index] = 1uy then
                                             chip8.Vx.[X] <- 1uy
                                             chip8.PC <- chip8.PC + 2us
                                           instruction_FX0Ar opcode (index + 1)  
                                         ()

let instruction_FX0A opcode = instruction_FX0Ar opcode 0

// FX07 - Set VX to the value of the delay timer
let instruction_FX07 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              chip8.Vx.[X] <- chip8.DelayTimer
                              chip8.PC <- chip8.PC + 2us

// FX15 - Set the delay timer to the value of VX
let instruction_FX15 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              chip8.DelayTimer <- chip8.Vx.[X]
                              chip8.PC <- chip8.PC + 2us

// FX18 - Set the sound timer to the value of VX
let instruction_FX18 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              chip8.SoundTimer <- chip8.Vx.[X]
                              chip8.PC <- chip8.PC + 2us

// FX1E - Store VX + I in I
let instruction_FX1E opcode = chip8.I <- chip8.I + uint16 chip8.Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                              chip8.PC <- chip8.PC + 2us

// FX29 - Store in I the memory address of the character of index VX
let instruction_FX29 opcode = let X = (opcode &&& 0x0F00us) >>> 8 in
                              chip8.I <- uint16 (chip8.Vx.[int X]*5uy)
                              chip8.PC <- chip8.PC + 2us

// FX33 - Store the BCD representation of VX in memory addresses I, I + 1, I + 2
let instruction_FX33 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in // number of Vx register
                              let B = int chip8.Vx.[X] / 100 in
                              let C = (int chip8.Vx.[X] / 10) % 10 in
                              let D = int chip8.Vx.[X] % 10 in
                              chip8.memory.[int chip8.I] <- byte B
                              chip8.memory.[int chip8.I + 1] <- byte C
                              chip8.memory.[int chip8.I + 2] <- byte D
                              chip8.PC <- chip8.PC + 2us


// FX55 - Load the registers from V0 to VX in memory from address I to I + X
let instruction_FX55 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                chip8.memory.[int chip8.I + i] <- chip8.Vx.[i]
                              chip8.PC <- chip8.PC + 2us

// FX65 - Store the values of memory at address I to I + X in registers from VO to VX
let instruction_FX65 opcode = let X = int ((opcode &&& 0x0F00us) >>> 8) in
                              for i in [0..X] do
                                  chip8.Vx.[i] <- chip8.memory.[int chip8.I + i]
                              chip8.PC <- chip8.PC + 2us
