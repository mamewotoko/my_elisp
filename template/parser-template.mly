/*
%{
%} 
*/


/* ��������         */
/* %token <int> INT   */           /* ���դ����� */
/* %token PLUS MINUS  */
/* TIMES DIV          */
/* %start main        */           /* ����ȥ�ݥ���Ȥ���� */
/* %type <int> main   */         /* ����ȥ�ݥ���Ȥη� */
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
