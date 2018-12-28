{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 André Ferreira de Moraes               }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Cirilo Veloso  -  www.veloso.adm.br            }
{                               Aroldo Zanella -  www.forumweb.com.b           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 06/01/2005: André Ferreira de Moraes
|*  - Primeira Versao ACBrCMC7
|* 29/05/2006: André Ferreira de Moraes
|*  - Corrigido Bug no calculo do digito Verificador 2
|* 29/05/2006: Diogo Augusto Pereira
|*  - Calculo de CMC7 compatiblizado com o Banrisul 
******************************************************************************}
unit ACBrCMC7;

interface

uses
  SysUtils, Classes, ACBrBase, ACBrUtil;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrCMC7 = class(TACBrComponent)
  private
    { Private declarations }
    FCMC7        : String;
    FCMC7Bloco1  : String;
    FCMC7Bloco3  : String;
    FCMC7Bloco2  : String;
    FBanco       : String;
    FAgencia     : String;
    FDvCCT       : Char;
    FComp        : String;
    FNumero      : String;
    FConta       : String;
    FTipificacao : Char;
    FDvBcoAg     : Char;
    FDvCMC7      : Char;
    FC1          : Integer;
    FC2          : Integer;
    FC3          : Integer;
    function DigitosaIgnorarConta(const Banco: String) : integer;
    procedure SetCMC7(Banda: String);
    procedure ZeraCampos ;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override ;
    Procedure MontaCMC7(pBanco, pAgencia, pConta, pNrCheque, pCamaraCompesacao: string;
      const pTipificacao : String = '5') ; overload ;
    Procedure MontaCMC7(Bloco1, Bloco2, Bloco3 : String) ; overload ;
  published
    { Published declarations }
    property CMC7        : String  read FCMC7  write SetCMC7 stored false ;
    property CMC7Bloco1  : String  read FCMC7Bloco1  stored false ;
    property CMC7Bloco2  : String  read FCMC7Bloco2  stored false ;
    property CMC7Bloco3  : String  read FCMC7Bloco3  stored false ;
    property Banco       : String  read FBanco       stored false ;
    property Agencia     : String  read FAgencia     stored false ;
    property DvCCT       : Char    read FDvCCT       stored false ; { Díg.Verif. Comp+Cheque+Tipificação }
    property Comp        : String  read FComp        stored false ;
    property Numero      : String  read FNumero      stored false ;
    property Conta       : String  read FConta       stored false ;
    property Tipificacao : Char    read FTipificacao stored false ; { Tipificação(5-Comum 6-Bancário 7-Salário 8-Administr. 9-CPMF) }
    property DvBcoAg     : Char    read FDvBcoAg     stored false ; { Dígito verificador do Banco+Agência: }
    property DvCMC7      : Char    read FDvCMC7      stored false ;
    property C1          : Integer read FC1          stored false ;
    property C2          : Integer read FC2          stored false ;
    property C3          : Integer read FC3          stored false ;
  end;

function ValidaCMC7(CMC7: String) : Boolean;
function FormataCMC7(const ACMC7: String): String;
function CalculaC1(const Chave: String): Integer;
function CalculaC2(const Chave: String): Integer;
function CalculaC3(const Chave: String): Integer;
function CalcDigitoCMC7(const Documento : String; Inicial, Final : integer) : String;

implementation

uses
  ACBrValidador;

function FormataCMC7(const ACMC7: String): String;
var
  CMC7: String;
begin
  CMC7 := ACBrUtil.OnlyNumber(ACMC7);

  if Length(CMC7) <> 30 then
    raise Exception.Create('Código CMC7 Inválido!');

  Result := '<' +
    Copy(CMC7, 1, 8)   + '<' +
    Copy(CMC7, 9, 10)  + '>' +
    Copy(CMC7, 19, 12) + ':';
end;

function CalculaC1(const Chave: String): Integer;
begin
  if Length(Chave) <> 10 then
    raise Exception.Create('Parâmetros inválidos para o cálculo do C1.');

  Result := StrToInt( Modulo11(Chave) );
end;

function CalculaC2(const Chave: String): Integer;
begin
  if Length(Chave) <> 10 then
    raise Exception.Create('Parâmetros inválidos para o cálculo do C2.');

  Result := StrToInt( Modulo11(Chave) );
end;

function CalculaC3(const Chave: String): Integer;
begin
  if Length(Chave) <> 6 then
    raise Exception.Create('Parâmetros inválidos para o cálculo do C3.');

  Result := StrToInt( Modulo11(Chave) );
end;

function CalcDigitoCMC7(const Documento : String; Inicial, Final : integer) : String;
var
  I: Integer;
  vVal1, vVal2, vVal3, vSoma, vPeso : Real;
begin
  vSoma := 0;
  for I := 1 to Length(Documento) do
  begin
    if Odd(I) then
       vPeso := Inicial
    else
       vPeso := Final;

    if CharInSet(Documento[I], ['0'..'9']) then
    begin
       vVal1 := StrToFloat(Documento[I])*vPeso;
       if (vVal1 > 9) then
          vVal2 := StrToFloat(copy(formatFloat('0',vVal1),1,1)) + StrToFloat(copy(formatFloat('0',vVal1),length(formatFloat('0',vVal1)),1))
       else
          vVal2 := vVal1;
       vSoma := vSoma+vVal2;
    end;
  end;
  vVal3 := round((10-(vSoma/10))*100)/100;

  Result := copy(formatFloat('0.000',frac(vVal3)),3,1);
end;

///Dica retirada do site www.delphi.eti.br e otimizada
function ValidaCMC7(CMC7: String): Boolean;
var Dv : string;
begin
  // contador: 123 4567 8 901 234567 8  9 0123456789 0
  // conteudo: 745 0030 2 018 000379 5  7 0030079144 9
  //           --- ---- - --- ------ -  - ---------- -
  //           |   |    |  |  |      |  | |          |
  //           |   |    |  |  |      |  | |          ---> digito verificador 3
  //           |   |    |  |  |      |  | -------------> conta corrente
  //           |   |    |  |  |      |  ---------------> digito verificador 1
  //           |   |    |  |  |      ------------------> Tipificação ( 5 padrao/normal, 8 ch tributário, 9 administrativo )
  //           |   |    |  |  -------------------------> cheque
  //           |   |    |  ----------------------------> compe ( camara de compensação )
  //           |   |    -------------------------------> digito verificador 2
  //           |   ------------------------------------> agência
  //           ----------------------------------------> banco

  CMC7   := OnlyNumber(CMC7) ;  { Retirando marcadores }
  Result := (Length(CMC7) = 30) ;

  // calculo do digito (2)
  if Result then
  begin
     Dv     := CalcDigitoCMC7(copy(CMC7,9,3)+copy(CMC7,12,6)+copy(CMC7,18,1),1,2);
     Result := (Dv = copy(CMC7,8,1)) ;
  end ;

  // calculo do digito (1)
  if Result then
  begin
    Dv     := CalcDigitoCMC7(copy(CMC7,1,7),2,1);
    Result := (Dv = copy(CMC7,19,1)) ;
  end;

  // calculo do digito (3)
  if Result then
  begin
    Dv     := CalcDigitoCMC7(copy(CMC7,20,10),1,2);
    Result := (Dv = copy(CMC7,30,1)) ;
  end;
end;


constructor TACBrCMC7.Create(AOwner: TComponent);
begin
 inherited Create( AOwner );

 ZeraCampos ;
end;

procedure TACBrCMC7.ZeraCampos;
begin
  FCMC7       := '';
  FCMC7Bloco1 := '';
  FCMC7Bloco2 := '';
  FCMC7Bloco3 := '';
  FBanco      := '';
  FAgencia    := '';
  FComp       := '';
  FNumero     := '';
  FConta      := '';
  FDvCCT      := ' ';
  FTipificacao:= ' ';
  FDvBcoAg    := ' ';
  FDvCMC7     := ' ';
  FC1         := 0;
  FC2         := 0;
  FC3         := 0;
end;

destructor TACBrCMC7.Destroy;
begin
  { Nada a fazer aqui por enquanto :) }
  
  inherited Destroy ;
end;

function TACBrCMC7.DigitosaIgnorarConta(const Banco: String) : integer;
var
  CodBanco : Integer;
begin
  CodBanco := StrToIntDef(Banco,0);
  case CodBanco of
      1: Result := 2;    // 001 - Banco do Brasil
     33: Result := 2;    // 033 - Santander / Banespa
     41: Result := 0;    // 041 - Banrisul Obs: Este banco utiliza todo o campo para o número da conta
    104: Result := 0;    // 104 - CEF. Utiliza apenas 7, mas os 3 primeiros são necessários para calcular o dv
//  237: Result := 3;    // 237 - Bradesco
    341: Result := 4;    // 341 - Itau
    389: Result := 1;    // 389 - Mercantil
    399: Result := 4;    // 399 - HSBC
//  409: Result := 3;    // 409 - Unibanco
    479: Result := 2;    // 479 - Bank of Boston
  else
    Result := 3;
  end;
end;

procedure TACBrCMC7.SetCMC7(Banda: String);
Const  vDigitos = '<99999999<9999999999>999999999999:' ;
var
  Ignorar : Integer;
  I : Integer ;
begin
  ZeraCampos ;

  Banda := Trim(Banda) ;
  if Banda = '' then
    exit ;

  if Length( Banda ) <> 34 then
     raise Exception.Create(ACBrStr('Banda CMC7 deve ter 34 caracteres'));

// 1234567890123456789012345678901234
// <00100049<0030000061>900000000109:

  for I := 1 to 33 do // Desprezando último caracter
  begin
    if vDigitos[I] = '9' then
     begin
       if not CharIsNum(Banda[I]) then
           raise Exception.CreateFmt(ACBrStr('Caracter da posição %d da Banda deve ser numérico'),[I]);
     end
    else
       if vDigitos[I] <> Banda[I] then
          raise Exception.CreateFmt(ACBrStr('Caracter da posição %d da Banda deve ser %s'),[I,vDigitos[I]]);

  end ;

  if not ValidaCMC7(Banda) then
     raise Exception.Create(ACBrStr('CMC7 Inválido'));

  try
  // '<' + Banco(3) + Agencia(4) + DV2(1) + '<' + CamaraCompesacao(3) +
  //    NrCheque(6) + Tipificacao(1) + '>' + DV1(1) + Conta(10) + DV3(1) + ':'
  // 1234567890123456789012345678901234
  // <00100049<0030000065>900000000109:

     FCMC7Bloco1 := copy(Banda,2 , 8) ;
     FCMC7Bloco2 := copy(Banda,11,10) ;
     FCMC7Bloco3 := copy(Banda,22,12) ;

     FBanco      := Copy(Banda,2 ,3);
     FAgencia    := Copy(Banda,5 ,4);
     FDvCCT      := Copy(Banda,9 ,1)[1];
     FComp       := Copy(Banda,11,3);
     FNumero     := Copy(Banda,14,6);
     FTipificacao:= Copy(Banda,20,1)[1];
     FDvBcoAg    := Copy(Banda,22,1)[1];

     Ignorar     := DigitosaIgnorarConta(FBanco);
     FConta      := Copy(Banda,23+Ignorar,10-Ignorar);

     FDvCMC7     := Copy(Banda,33,1)[1];
     FC1         := CalculaC1(FComp + FBanco + FAgencia);
     FC2         := CalculaC2(PadLeft(FConta, 10, '0'));
     FC3         := CalculaC3(PadLeft(FNumero, 6, '0'));
     FCMC7       := Banda
  except
     ZeraCampos ;
  end ;
end;


Procedure TACBrCMC7.MontaCMC7(pBanco, pAgencia, pConta, pNrCheque, pCamaraCompesacao: string;
      const pTipificacao : String = '5') ;
// Dica retirada do site http://www.ramosdainformatica.com.br/art_recentes01.php?CDA=297 e ajustado conforme necessidade
var
  vDv1, vDv2, vDv3 : string;
  Tip : Integer ;
begin
  // zeros a esquerda do banco
  pBanco := Poem_Zeros(pBanco,3);
  // zeros a esquerda da agencia
  pAgencia := Poem_Zeros(pAgencia,4);
  // zeros a esquerda da conta
  pConta := Poem_Zeros(pConta,10);
  // zeros a esquerda do NrCheque
  pNrCheque := Poem_Zeros(pNrCheque,6);
  // zeros a esquerda do CamaraCompesacao
  pCamaraCompesacao := Poem_Zeros(pCamaraCompesacao,3);
  Tip := StrToIntDef(pTipificacao,0) ;
  if (Tip < 5) or (Tip > 9) then
     raise Exception.Create(ACBrStr('Campo Tipificação deve estar na faixa 5..9')) ;

  // calculo do digito (2)
  vDv2 := CalcDigitoCMC7(pCamaraCompesacao+pNrCheque+pTipificacao,1,2);
   // calculo do digito (1)
  vDv1 := CalcDigitoCMC7(pBanco+pAgencia,2,1);
  // calculo do digito (3)
  vDv3 := CalcDigitoCMC7(pConta,1,2);

  CMC7 := '<'+pBanco+pAgencia+vDV2+'<'+pCamaraCompesacao+pNrCheque+pTipificacao+'>'+vDV1+pConta+vDV3+':';
  // '<' + Banco(3) + Agencia(4) + DV2(1) + '<' + CamaraCompesacao(3) +
  //    NrCheque(6) + Tipificacao(1) + '>' + DV1(1) + Conta(10) + DV3(1) + ':'
  // 1234567890123456789012345678901234
  // <00100049<0030000065>900000000109:
end;

procedure TACBrCMC7.MontaCMC7(Bloco1, Bloco2, Bloco3: String);
begin
  Bloco1 := Poem_Zeros(Bloco1, 8) ;
  Bloco2 := Poem_Zeros(Bloco2,10) ;
  Bloco3 := Poem_Zeros(Bloco3,12) ;
  
  CMC7 := '<'+Bloco1+'<'+Bloco2+'>'+Bloco3+':';
end;

end.
