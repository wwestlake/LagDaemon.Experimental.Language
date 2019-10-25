﻿(*
    Yadsl (Yet Another Domain Specific Language)
    Copyright (C) 2019, William W. Westlake
    Licensed un the MIT License
*)

namespace LagDaemon.Yadsl.Compiler



module Model=

    open System
    open FParsec

    type YValue =
        | TInteger of int64
        | TLong of uint64
        | TFloat of float
        | TString of string
        | TBool of bool
        | TChar of char
        | TByte of byte
        | TIdentifier of string


    type UserState = unit // doesn't have to be unit, of course
    type Parser<'t> = Parser<'t, UserState>
