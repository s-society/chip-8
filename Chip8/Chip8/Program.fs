// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Windows.Forms
open System.IO
open System.Drawing

[<EntryPoint>][<STAThread>]
let main argv = 
    let mutable romName = String.Empty
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
                                    rom.CopyTo(chip8.memory, int chip8.PC)
        |_ -> Environment.Exit(1)
    0

     