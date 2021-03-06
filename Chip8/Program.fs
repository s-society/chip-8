﻿open System
open System.Windows.Forms
open System.IO
open System.Drawing

[<EntryPoint>][<STAThread>]
let main argv = 

    // Filename for the ROM
    let mutable romName = String.Empty

    // File picker dialog
    let openBinDialog = new OpenFileDialog()
    openBinDialog.Title <- "Open Chip-8 ROM File"
    openBinDialog.Filter <- "Chip-8 ROM Files|*.c8|All files|*.*"
    match openBinDialog.ShowDialog() with
        | DialogResult.OK -> do let rom = File.ReadAllBytes(openBinDialog.FileName)
                                if rom.Length = 0 || rom.Length > (0xFFF - 0x200) then
                                    ignore(MessageBox.Show("Invalid","FATAL",MessageBoxButtons.OK, MessageBoxIcon.Error))
                                    Environment.Exit(1)
                                else
                                    romName <- openBinDialog.FileName
                                    rom.CopyTo(chip8.memory, int chip8.PC) // ROM is copied on memory at 0x200
        |_ -> Environment.Exit(1)


    // Current opcode
    let mutable opcode = 0us

    let mutable d_s_timer = DateTime.Now
    let mutable instruction_timer = DateTime.Now
           
    
    // Main loop function for the chip-8 emulator :
    let main_loop = 
        async { // So the program continues further as the loop still runs 
        while true do  
            if ((DateTime.Now - d_s_timer).Milliseconds >= 1000/60) then do
                d_s_timer <- DateTime.Now
                if chip8.SoundTimer > 0uy then do
                    if(chip8.SoundTimer = 1uy) then do
                        Console.Beep()
                    chip8.SoundTimer <- chip8.SoundTimer - 1uy
                                                    
                if chip8.DelayTimer > 0uy then chip8.DelayTimer <- chip8.DelayTimer - 1uy


            if ((DateTime.Now - instruction_timer).Milliseconds >= 1) then do
                instruction_timer <- DateTime.Now
                opcode <- (uint16 (chip8.memory.[int chip8.PC]) <<< 8) ||| (uint16 (chip8.memory.[int chip8.PC + 1]))
                match (opcode &&& 0xF000us) with 
                | 0x0000us -> match opcode &&& 0x00FFus with

                                | 0xE0us -> OpCodes.instruction_00E0()

                                | 0xEEus -> OpCodes.instruction_00EE()

                                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                          Environment.Exit(1)

                | 0x1000us -> OpCodes.instruction_1NNN opcode

                | 0x2000us -> OpCodes.instruction_2NNN opcode

                | 0x3000us -> OpCodes.instruction_3XKK opcode

                | 0x4000us -> OpCodes.instruction_4XKK opcode

                | 0x5000us -> OpCodes.instruction_5XY0 opcode

                | 0x6000us -> OpCodes.instruction_6XKK opcode

                | 0x7000us -> OpCodes.instruction_7XKK opcode

                | 0x8000us -> match (opcode &&& 0x000Fus) with 

                                | 0x0us -> OpCodes.instruction_8XY0 opcode

                                | 0x1us -> OpCodes.instruction_8XY1 opcode

                                | 0x2us -> OpCodes.instruction_8XY2 opcode 

                                | 0x3us -> OpCodes.instruction_8XY3 opcode

                                | 0x4us -> OpCodes.instruction_8XY4 opcode

                                | 0x5us -> OpCodes.instruction_8XY5 opcode

                                | 0x6us -> OpCodes.instruction_8XY6 opcode 

                                | 0x7us -> OpCodes.instruction_8XY7 opcode

                                | 0xEus -> OpCodes.instruction_8XYE opcode    

                                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                          Environment.Exit(1)

                | 0x9000us -> OpCodes.instruction_9XY0 opcode

                | 0xA000us -> OpCodes.instruction_ANNN opcode

                | 0xB000us -> OpCodes.instruction_BNNN opcode

                | 0xC000us -> OpCodes.instruction_CXKK opcode

                | 0xD000us -> OpCodes.instruction_DXYK opcode

                | 0xE000us -> match opcode &&& 0x00FFus with

                                | 0x9Eus -> OpCodes.instruction_EX9E opcode

                                | 0xA1us -> OpCodes.instruction_EXA1 opcode

                                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                          Environment.Exit(1)

                | 0xF000us -> match opcode &&& 0x00FFus with
                                               
                                | 0x07us -> OpCodes.instruction_FX07 opcode

                                | 0x0Aus -> OpCodes.instruction_FX0A opcode

                                | 0x15us -> OpCodes.instruction_FX15 opcode

                                | 0x18us -> OpCodes.instruction_FX18 opcode

                                | 0x1Eus -> OpCodes.instruction_FX1E opcode

                                | 0x29us -> OpCodes.instruction_FX29 opcode

                                | 0x33us -> OpCodes.instruction_FX33 opcode 

                                | 0x55us -> OpCodes.instruction_FX55 opcode

                                | 0x65us -> OpCodes.instruction_FX65 opcode

                                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                          Environment.Exit(1)

                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                          Environment.Exit(1)

                Console.WriteLine("0x{0:X4}", opcode)

                ()}
            
    chip8.fontset.CopyTo(chip8.memory, 0)
                                            
    chip8.form.ClientSize <- new System.Drawing.Size(64 * 16, 32 * 16)
    chip8.form.Load.Add(fun e -> chip8.form.BackColor <- Color.Black
                                 Async.Start(main_loop))
    chip8.form.Paint.Add(chip8.Draw)

    // Handle keys
    chip8.form.KeyDown.Add(chip8.OnKeyPress)
    chip8.form.KeyUp.Add(chip8.OnKeyUp)

    chip8.form.Text <- String.Format("{0} - Chip-F Emulator", romName)
    chip8.form.MaximizeBox <- false
    chip8.form.FormBorderStyle <- FormBorderStyle.FixedSingle
    Application.Run(chip8.form)

    0
