module Demoinput

open Solution

let demoinput = "nop +0
                 acc +1
                 jmp +4
                 acc +3
                 jmp -3
                 acc -99
                 acc +1
                 jmp -4
                 acc +6"

let democode = parse demoinput

let democode2 = parse "nop +0
                       acc +1
                       jmp +4
                       acc +3
                       jmp -3
                       acc -99
                       acc +1
                       jmp -4
                       acc +6"
