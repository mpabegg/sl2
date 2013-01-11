(* Programa escrito por Peter Sewell - Universidade de Cambridge *)                                           

(* Este arquivo cont�m um interpretador, um verificador de tipo e um 
   pretty-printer para a linguagem L1. Para us�-lo copiei o arquivo 
   para um diret�rio de trabalho e prompt digite

       mosml -P full l1.ml 

   Isto inicia uma sess�o em Moscow ML. Digitando
      
      doit ();

   aparece a sequencia de passos para  < l1:=3;!l1 , {l1=0 } >, e digitando

      doit2 ();

   executa o verificador de tipos para o mesmo exemplo.

   Para testar outras express�es de L1 a �rvore de sintaxe abstrata deve 
   ser construida explicitamente, por exemplo para avaliar a express�o
   l1 := 3 ; !l1 na mem�ria com endere�o l1 contendo o inteiro 0:

      prettyreduce (Seq( Assign ("l1",Integer 3), Deref "l1"), [("l1",0)]);

 *)


(* *********************)
(* sintaxe abstrata    *)
(* *********************)

type loc = string

datatype oper = Plus | GTEQ

datatype expr = 
         Integer of int
       | Boolean of bool
       | Op of expr * oper * expr
       | If of expr * expr * expr
       | Assign of loc * expr
       | Deref of loc
       | Skip
       | Seq of expr * expr
       | While of expr * expr


(* **********************************)
(* interpretador para a sem�ntica   *)
(* **********************************)

fun is_value (Integer n) = true
  | is_value (Boolean b) = true
  | is_value (Skip) = true
  | is_value _ = false

  (* A mem�ria � representada por uma lista de pares loc*int. As opera��es

    lookup : store * loc         -> int option
    update : store * (loc * int) -> store option

   retornam NONE se o endere�o n�o est� no dom�nio da mem�ria.
  *)

type store = (loc * int) list

fun lookup ( [], l ) = NONE
  | lookup ( (l',n')::pairs, l) = 
       if l=l' then SOME n' else lookup (pairs,l)

fun update'  front [] (l,n) = NONE
 |  update'  front ((l',n')::pairs) (l,n) = 
    if l=l' then 
        SOME(front @ ((l,n)::pairs) )
    else 
        update' ((l',n')::front) pairs (l,n)

fun update (s, (l,n)) = update' [] s (l,n)




  (* Defini��o da fun��o que implementa um passo de avalia��o

     reduce :  expr * store -> (expr * store) option 

  que recebe uma configura��o (e,s) e retorna ou NONE, caso n�o
  haja transi��o, ou SOME (e',s'), se h� transi��o (e,s) --> (e',s'). 
  *)

fun reduce (Integer n,s) = NONE
  | reduce (Boolean b,s) = NONE
  | reduce (Op (e1,opr,e2),s) = 
    (case (e1,opr,e2) of
         (Integer n1, Plus, Integer n2) => SOME(Integer (n1+n2), s)   (*op + *)
       | (Integer n1, GTEQ, Integer n2) => SOME(Boolean (n1 >= n2), s)(*op >=*)
       | (e1,opr,e2) => (                                               
         if (is_value e1) then (                                        
             case reduce (e2,s) of 
                 SOME (e2',s') => SOME (Op(e1,opr,e2'),s')     (* (op2) *)
               | NONE => NONE )                           
         else (                                                 
             case reduce (e1,s) of 
                 SOME (e1',s') => SOME(Op(e1',opr,e2),s')      (* (op1) *)
               | NONE => NONE ) ) )
  | reduce (If (e1,e2,e3),s) =
    (case e1 of
         Boolean(true) => SOME(e2,s)                           (* (if1) *)
       | Boolean(false) => SOME(e3,s)                          (* (if2) *)
       | _ => (case reduce (e1,s) of
                   SOME(e1',s') => SOME(If(e1',e2,e3),s')      (* (if3) *)
                 | NONE => NONE ))
  | reduce (Deref l,s) = 
    (case lookup  (s,l) of                
          SOME n => SOME(Integer n,s)                          (* (deref) *)
        | NONE => NONE )                  
  | reduce (Assign (l,e),s) =                                  
    (case e of                                                 
         Integer n => (case update (s,(l,n)) of 
                           SOME s' => SOME(Skip, s')           (* (assign1) *)
                         | NONE => NONE)                                   
       | _ => (case reduce (e,s) of                           
                   SOME (e',s') => SOME(Assign (l,e'), s')     (* (assign2) *)
                 | NONE => NONE  ) )                          
  | reduce (While (e1,e2),s) = SOME( If(e1,Seq(e2,While(e1,e2)),Skip),s) (* (while) *)
  | reduce (Skip,s) = NONE                                     
  | reduce (Seq (e1,e2),s) =                                   
    (case e1 of                                                 
        Skip => SOME(e2,s)                                     (* (seq1) *)
      | _ => ( case reduce (e1,s) of                           
                 SOME (e1',s') => SOME(Seq (e1',e2), s')       (* (seq2) *)
               | NONE => NONE ) )                                        


  (* Fun�ao de avalia��o em v�rios passos

     evaluate :  expr * store -> (expr * store)  

  que recebe uma configura��o (e,s) e retorna (e',s')
  tal que (e,s) -->* (e',s') -/->.  
  
  *)

fun evaluate (e,s) = case reduce (e,s) of 
                         NONE => (e,s)
                       | SOME (e',s') => evaluate (e',s')


(* **********************************)
(* verifica��o de tipos             *)
(* **********************************)

(* tipos *)

datatype type_L1 =
         int
       | unit
       | bool

datatype type_loc =
         intref

type typeEnv = (loc*type_loc) list 



(* ******************* *)
(* infer�ncia de tipos *)
(* ******************* *)

(* infertype : typeEnv -> expr -> type_L1 option *)


fun infertype gamma (Integer n) = SOME int  (* (TInt)  *)

  | infertype gamma (Boolean b) = SOME bool (* (TBool) *)

  | infertype gamma (Op (e1,Plus,e2))       (* (T+)    *)
    = (case (infertype gamma e1, infertype gamma e2) of
          (SOME int, SOME int) => SOME int
        | _ => NONE)

  | infertype gamma (Op (e1,GTEQ,e2))       (* (T>=)   *)
    = (case (infertype gamma e1, infertype gamma e2) of
          (SOME int, SOME int) => SOME bool
        | _ => NONE)

  | infertype gamma (If (e1,e2,e3)) 
    = (case (infertype gamma e1, infertype gamma e2, infertype gamma e3) of
           (SOME bool, SOME t2, SOME t3) => (if t2=t3 then SOME t2 else NONE)
         | _ => NONE)

  | infertype gamma (Deref l) 
    = (case lookup (gamma,l) of
           SOME intref => SOME int
         | NONE => NONE)

  | infertype gamma (Assign (l,e)) 
    = (case (lookup (gamma,l), infertype gamma e) of
           (SOME intref,SOME int) => SOME unit
         | _ => NONE)

  | infertype gamma (Skip) = SOME unit

  | infertype gamma (Seq (e1,e2))  
    = (case (infertype gamma e1, infertype gamma e2) of
           (SOME unit, SOME t2) => SOME t2
         | _ => NONE )

  | infertype gamma (While (e1,e2)) 
    = (case (infertype gamma e1, infertype gamma e2) of
           (SOME bool, SOME unit) => SOME unit 
         | _ => NONE )



(* ****************************** *)
(* fun��es auxiliares para teste  *)
(* ****************************** *)
;
load "Listsort";
load "Int";
load "Bool";

(* pretty print de express�es *)

fun prettyprintop Plus = "+"
  | prettyprintop GTEQ = ">="
                         
fun prettyprintexpr (Integer n) = Int.toString n
  | prettyprintexpr (Boolean b) = Bool.toString b
  | prettyprintexpr (Op (e1,opr,e2)) 
    = "(" ^ (prettyprintexpr e1) ^ (prettyprintop opr) ^ (prettyprintexpr e2) ^ ")"
  | prettyprintexpr (If (e1,e2,e3)) 
    = "if " ^ (prettyprintexpr e1 ) ^ " then " ^ (prettyprintexpr e2)
      ^ " else " ^ (prettyprintexpr e3)
  | prettyprintexpr (Deref l) = "!" ^ l
  | prettyprintexpr (Assign (l,e)) =  l ^ ":=" ^ (prettyprintexpr e )
  | prettyprintexpr (Skip) = "skip"
  | prettyprintexpr (Seq (e1,e2)) =  (prettyprintexpr e1 ) ^ ";" 
                                     ^ (prettyprintexpr e2)
  | prettyprintexpr (While (e1,e2)) =  "while " ^ (prettyprintexpr e1 ) 
                                       ^ " do " ^ (prettyprintexpr e2)

(* pretty print da mem�ria, ordenada por endere�os *)

fun rawprintstore [] = ""
  | rawprintstore ((l,n)::pairs) = l ^ "=" ^ (Int.toString n) 
                                   ^ " " ^ (rawprintstore pairs)

fun prettyprintstore pairs = 
    let val pairs' = Listsort.sort 
                         (fn ((l,n),(l',n')) => String.compare (l,l'))
                         pairs
    in
        "{" ^ rawprintstore pairs' ^ "}" end

(* pretty print de configura��es *)

fun prettyprintconfig (e,s) = 
 "< " ^ (prettyprintexpr e) ^ " , " ^ (prettyprintstore s) ^ " >"

(* realiza um sequencia de redu��es imprimindo o estado inicial e o estado ap�s
   cada passo
*)


fun prettyreduce' (e,s) = 
    case reduce (e,s) of 
        SOME (e',s') => 
        ( TextIO.print ("\n -->  " ^ prettyprintconfig (e',s') ) ;
          prettyreduce' (e',s'))
      | NONE => (TextIO.print "\n -/->  " ; 
                 if is_value e then 
                     TextIO.print "(valor)\n" 
                 else 
                     TextIO.print "(erro)" )

fun prettyreduce (e,s) = 
    ( TextIO.print ("      "^(prettyprintconfig (e,s)) ) ;
      prettyreduce' (e,s)
    )

(* **************)
(* testes       *)
(* **************)

fun doit () = prettyreduce (Seq( Assign ("l1",Integer 3), Deref "l1"), [("l1",0)] )
(*
 infertype [("l1",intref)] (Seq( Assign ("l1",Integer 3), Deref "l1"));;
*)
fun doit2 () = infertype [("l1",intref)] (Seq( Assign ("l1",Integer 3), Deref "l1"));

