type position_encoding = [ `UTF8 | `UTF16 ]

type position = { line : int; character : int }

type t

val clamp_offset : string -> int -> int

val make : ?position_encoding:position_encoding -> string -> t
val of_file : ?position_encoding:position_encoding -> string -> t

val line : t -> int -> int

val position : t -> int -> position
val offset : t -> line:int -> character:int -> int
