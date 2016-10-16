﻿module chip8

let memory = Array.create 4096 0uy
let mutable PC = 0x200us
let Vx = Array.create 16 0uy
let mutable I = 0us
// tableau representant la valeur noire = 0 blanc = 1 des 2048 pixels (affichage 64*32)
let screen = Array.create 2048 0uy

let romFile = ""

//Characters 0-F (in hexadecimal) are represented by a 4x5 font.
let fontset = [| 
    0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy; //0
    0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy; //1
    0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy; //2
    0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy; //3
    0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy; //4
    0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy; //5
    0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy; //6
    0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy; //7
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy; //8
    0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy; //9
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy; //A
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy; //B
    0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy; //C
    0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy; //D
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy; //E
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy |]  //F  
