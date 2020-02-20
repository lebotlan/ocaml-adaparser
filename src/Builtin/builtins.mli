open Adavalues.Adavalue
open Astlib.Ast

exception Bad_arguments of string

type v = expr adavalue

val delay: v 

val rem: v (* Not implemented yet *)
val araise: v (* Not implemented yet *)
val goto: v (* Not implemented yet *)

val times: v
val div: v
val plus: v
val minus: v
val neg: v
val idplus: v
val power: v
val modu: v
val abs: v
  
val geq: v
val leq: v
val gt: v
val lt: v
val equal: v
val notequal: v

val band: v
val bor: v
val bnot: v
val bxor: v

val sconcat: v
