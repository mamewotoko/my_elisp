/*
%{
%} 
*/


/* 字句の宣言         */
/* %token <int> INT   */           /* 型付きの値 */
/* %token PLUS MINUS  */
/* TIMES DIV          */
/* %start main        */           /* エントリポイントの宣言 */
/* %type <int> main   */         /* エントリポイントの型 */
%%

/* CFG */

/*
main:
  expr { $1 }
;

expr:
  INT {$1}
| LPAREN PLUS expr expr RPAREN  { $3 + $4 }
| LPAREN MINUS expr expr RPAREN { $3 - $4 }
;
*/

/*
%{
%} 
*/
