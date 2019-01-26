type longident =
  | Lident of string
  | Ldot of longident * string

type pattern =
  | PVar of string
  | PInt of string
  | PConstructor of longident * string list

type typerepr =
  | ISum of (string * bool) list
  | IRecord of string list
  | IRebind

type constant =
  | CString of string
  | CUnit
  | CInt of string

type label =
  | Nolabel
  | Labelled of string
  | Optional of string

type expr =
  | EVar of longident
  | EConstant of constant
  | EConstr of longident * expr list
  | EGetfield of expr * longident
  | ESetfield of expr * longident * expr
  | ERecord of (longident * expr) list
  | ERecordwith of expr * (longident * expr) list
  | EApply of longident * (expr * label) list
  | EIf of expr * expr * expr
  | EChain of expr * expr
  | EMatch of expr * (pattern * expr) list
  | ETry of expr * (pattern * expr) list
  | ELet of (pattern * expr) list * expr
  | ELambda of string list * expr

type definition =
  | MLet of bool * (string * (string * label) list * expr) list
  | MTypedef of (string * typerepr) list
  | MException of string * bool
  | MOpen of longident
  | MStruct of string * definition list
