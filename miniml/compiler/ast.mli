type pattern =
  | PVar of string
  | PConstructor of string * string list

type typerepr =
  | ISum of (string * bool) list
  | IRecord of string list

type constant =
  | CString of string
  | CUnit
  | CInt of string

type label =
  | Nolabel
  | Labelled of string
  | Optional of string

type expr =
  | EVar of string
  | EConstant of constant
  | EConstr of string * expr list
  | EGetfield of expr * string
  | ERecord of (string * expr) list
  | ERecordwith of expr * (string * expr) list
  | EApply of string * (expr * label) list
  | EIf of expr * expr * expr
  | EChain of expr * expr
  | EMatch of expr * (pattern * expr) list
  | ELet of pattern * expr * expr

type definition =
  | MLet of string * (string * label) list * expr
  | MTypedef of string * typerepr
