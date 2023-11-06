{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrValidador;

interface

uses
 SysUtils, Classes,
 ACBrBase;

const
  cIgnorarChar = './-' ;
  cUFsValidas = ',AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,'+
                'RJ,RN,RS,RO,RR,SC,SP,SE,TO,EX,';

  cMaskCNPJ = '**.***.***/****-**';

type
  TACBrValTipoDocto = ( docCPF, docCNPJ, docUF, docInscEst, docNumCheque, docPIS,
                        docCEP, docCartaoCredito, docSuframa, docGTIN, docRenavam, 
                        docEmail, docCNH, docPrefixoGTIN, docCAEPF, docPlacaMercosul,
                        docCNS ) ;

type
  TACBrCalcDigFormula = (frModulo11, frModulo10PIS, frModulo10) ;
  TACBrValidadorMsg = procedure(Mensagem : String) of object ;

  TACBrCalcDigito = class
  private
    fsMultIni: Integer;
    fsMultFim: Integer;
    fsMultAtu: Integer;
    fsFormulaDigito: TACBrCalcDigFormula;
    fsDocto: String;
    fsDigitoFinal: Integer;
    fsSomaDigitos: Integer;
    fsModuloFinal: Integer;
  public
    constructor Create;

    Procedure Calcular;
    Procedure CalculoPadrao;

    Property Documento: String read fsDocto write fsDocto;
    Property MultiplicadorInicial: Integer read fsMultIni write fsMultIni;
    Property MultiplicadorFinal: Integer read fsMultFim write fsMultFim;
    property MultiplicadorAtual: Integer read fsMultAtu write fsMultAtu;
    Property DigitoFinal: Integer read fsDigitoFinal;
    property ModuloFinal: Integer read fsModuloFinal;
    Property SomaDigitos: Integer read fsSomaDigitos;
    Property FormulaDigito: TACBrCalcDigFormula read fsFormulaDigito write fsFormulaDigito;
  end;

  { TACBrValidador }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrValidador = class( TACBrComponent )
  private
    { Propriedades do Componente ACBrValidador }
    fsIgnorarChar: String;
    fsDocumento: String;
    fsComplemento: String;
    fsDocto    : String;
    fsMsgErro: String;
    fsRaiseExcept: Boolean;
    fsOnMsgErro: TACBrValidadorMsg;
    fsTipoDocto: TACBrValTipoDocto;
    fsPermiteVazio: Boolean;
    fsAjustarTamanho: Boolean;
    fsModulo: TACBrCalcDigito;
    fsExibeDigitoCorreto: Boolean;
    fsDigitoCalculado: String;

    function GetMsgErro : String;
    procedure SetDocumento(const Value: String);
    procedure SetComplemento(const Value: String);
    Function LimpaDocto(const AString : String) : String ;
    
    Procedure ValidarCPF  ;
    Procedure ValidarCNPJ ;
    Procedure ValidarCAEPF ;
    Procedure ValidarUF(const UF : String) ;
    Procedure ValidarIE ;
    Procedure ValidarCheque ;
    Procedure ValidarPIS  ;
    Procedure ValidarCEP ;
    procedure ValidarCartaoCredito ;
    procedure ValidarSuframa ;
    procedure ValidarGTIN;
    procedure ValidarRenavam;
    Procedure ValidarEmail;
    Procedure ValidarCNH ;
    Procedure ValidarPrefixoGTIN ;
    Procedure ValidarPlaca ;
    Procedure ValidarCNS ;

  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override;

    property DoctoValidado : String read fsDocto ;

    Property MsgErro : String read GetMsgErro ;
    Property Modulo  : TACBrCalcDigito read fsModulo write fsModulo ;
    Property DigitoCalculado : String read fsDigitoCalculado ;

    Function Validar  : Boolean;
    Function Formatar : String ;

  published
    property TipoDocto : TACBrValTipoDocto read fsTipoDocto write fsTipoDocto
       default docCPF ;
    property Documento : String read fsDocumento write SetDocumento
       stored false;
    property Complemento : String read fsComplemento write SetComplemento
       stored false;
    property ExibeDigitoCorreto : Boolean read fsExibeDigitoCorreto
       write fsExibeDigitoCorreto default false ;
    property IgnorarChar : String read fsIgnorarChar write fsIgnorarChar ;
    property AjustarTamanho : Boolean read fsAjustarTamanho
       write fsAjustarTamanho default false ;
    property PermiteVazio : Boolean read fsPermiteVazio write fsPermiteVazio
       default false ;
    property RaiseExcept : Boolean read fsRaiseExcept write fsRaiseExcept
       default false ;
    property OnMsgErro : TACBrValidadorMsg read fsOnMsgErro write fsOnMsgErro;

  end ;

function ValidarCPF( const Documento : String ) : String ;
function ValidarCNPJ( const Documento : String ) : String ;
function ValidarCAEPF( const Documento : String ) : String ;
function ValidarCNPJouCPF( const Documento : String ) : String ;
function ValidarIE(const AIE, AUF: String): String ;
function ValidarSuframa( const Documento : String ) : String ;
function ValidarGTIN( const Documento : String ) : String ;
function ValidarPrefixoGTIN( const Documento : String ) : String ;
function ValidarRenavam( const Documento : String ) : String ;
function ValidarEmail (const Documento : string ) : String;
function ValidarCEP(const ACEP, AUF: String): String; overload;
function ValidarCEP(const ACEP: Integer; const AUF: String): String; overload;
function ValidarCNH(const Documento: String) : String ;
function ValidarUF(const AUF: String): String;
function ValidarPlaca(const APlaca: String): String;
function ValidarCNS(const Documento: String): String;

Function FormatarFone( const AValue : String; const DDDPadrao: String = '' ): String;
Function FormatarCPF( const AValue : String )    : String ;
Function FormatarCNPJ( const AValue : String )   : String ;
Function FormatarCAEPF( const AValue : String )   : String ;
function FormatarCNPJouCPF(const AValue: String)    : String;
function FormatarPlaca(const AValue: string): string;
Function FormatarIE( const AValue: String; const UF : String ) : String ;
Function FormatarCheque( const AValue : String ) : String ;
Function FormatarPIS( const AValue : String )    : String ;
Function FormatarCEP( const AValue: String )     : String ; overload;
Function FormatarCEP( const AValue: Integer )    : String ; overload;
function FormatarSUFRAMA( const AValue: String ) : String ;

Function FormatarMascaraNumerica(const ANumValue: string; const Mascara: String): String;
Function FormatarMascaraDinamica(const AValue: String; const Mascara: String): String;
Function RemoverMascara(const AValue: String; const Mascara: String): String;

Function OnlyCNPJorCPF( const Documento : String ) : String ;

function ValidarDocumento( const TipoDocto : TACBrValTipoDocto;
  const Documento : String; const Complemento : String = '') : String ;
function FormatarDocumento( const TipoDocto : TACBrValTipoDocto;
  const Documento : String) : String ;

function Modulo11(const Documento: string; const Peso: Integer = 2; const Base: Integer = 9): String;
function MascaraIE(const AValue : String; UF : String) : String;

implementation

uses
 {$IfDef COMPILER6_UP} Variants , Math, StrUtils, {$EndIf}
  ACBrUtil.Base,
  ACBrUtil.Compatibilidade,
  ACBrUtil.Strings;

function ValidarCPF(const Documento : String) : String ;
begin
   Result := ValidarDocumento( docCPF, Documento );
end;

function ValidarCNPJ(const Documento : String) : String ;
begin
  Result := ValidarDocumento( docCNPJ, Documento );
end;

function ValidarCAEPF(const Documento : String) : String ;
begin
   Result := ValidarDocumento( docCAEPF, Documento );
end;

function ValidarIE(const AIE, AUF: String): String;
begin
  Result := ValidarDocumento(docInscEst, AIE, AUF);
end;

function ValidarSuframa( const Documento : String ) : String ;
begin
  Result := ValidarDocumento( docSuframa, Documento );
end;

function ValidarGTIN( const Documento : String ) : String ;
begin
  Result := ValidarDocumento( docGTIN, Documento );
end;

function ValidarPrefixoGTIN( const Documento : String ) : String ;
begin
  Result := ValidarDocumento( docPrefixoGTIN, Documento );
end;

function ValidarRenavam( const Documento : String ) : String ;
begin
  Result := ValidarDocumento( docRenavam, Documento );
end;

function ValidarEmail (const Documento : string ) : String;
var
  SL: TStringList;
  sDelimiter: Char;
  I: Integer;
begin
  Result := '';
  sDelimiter := FindDelimiterInText(Documento);

  if (sDelimiter = ' ') then
    Result := ValidarDocumento( docEmail, Documento )
  else
  begin
    SL := TStringList.Create;
    try
      AddDelimitedTextToList(Documento, sDelimiter, SL);

      I := 0;
      while (Result = '') and (I < SL.Count) do
      begin
        Result := ValidarDocumento( docEmail, SL[I] );
        Inc( I );
      end;
    finally
      SL.Free;
    end;
  end;
end;

function ValidarCEP(const ACEP, AUF: String): String;
begin
  Result := ValidarDocumento( docCEP, ACEP, AUF);
end;

function ValidarCEP(const ACEP: Integer; const AUF: String): String;
begin
  Result := ValidarCEP( FormatarCEP(ACEP), AUF );
end;

function ValidarCNH(const Documento: String): String ;
begin
  Result := ValidarDocumento( docCNH, Documento );
end;

function ValidarUF(const AUF: String): String;
begin
Result := ValidarDocumento( docUF, AUF );
end;

function ValidarCNPJouCPF(const Documento : String) : String ;
Var
  NumDocto : String ;
begin
   NumDocto := OnlyNumber(Documento) ;
   if Length(NumDocto) < 12 then
      Result := ValidarCPF( Documento )
   else
      Result := ValidarCNPJ( Documento ) ;
end;

function ValidarPlaca(const APlaca: String): String;
begin
  Result := ValidarDocumento( docPlacaMercosul, APlaca );
end;

function ValidarCNS(const Documento: String): String ;
begin
  Result := ValidarDocumento( docCNS, Documento );
end;

{ Retorna apenas números, e apenas se o conteúdo for CPF ou CNPJ, caso contrário
    retorna vazio }
function OnlyCNPJorCPF(const Documento: String): String;
var
  NumDocto: String;
begin
  NumDocto := OnlyNumber(Documento);
  if EstaVazio( ValidarCNPJouCPF(NumDocto) ) then
    Result := NumDocto
  else
    Result := '';
end;

function ValidarDocumento(const TipoDocto : TACBrValTipoDocto ;
  const Documento: String; const Complemento : String = '') : String ;
Var
  ACBrVal : TACBrValidador ;
begin
  ACBrVal := TACBrValidador.Create(nil);
  try
    ACBrVal.RaiseExcept := False;
    ACBrVal.PermiteVazio:= False ;
    ACBrVal.TipoDocto   := TipoDocto;
    ACBrVal.Documento   := Documento;
    ACBrVal.Complemento := Complemento;

    if ACBrVal.Validar then
       Result := ''
    else
       Result := ACBrVal.MsgErro;
  finally
    ACBrVal.Free;
  end;
end;

function FormatarFone(const AValue : String; const DDDPadrao: String = '') : String ;
var
  FoneNum, Mascara : string;
  ComecaComZero: Boolean;
  LenFoneNum: Integer;
begin
  Result := '';
  FoneNum := OnlyNumber(AValue);
  ComecaComZero := (LeftStr(FoneNum,1) = '0');
  FoneNum := RemoveZerosEsquerda(FoneNum);

  LenFoneNum := length(FoneNum);
  if (LenFoneNum = 0) or (FoneNum = '0') then
    exit;

  if (LenFoneNum <= 9) and NaoEstaVazio(DDDPadrao) then
  begin
     FoneNum := LeftStr(DDDPadrao,2) + FoneNum;
     LenFoneNum := LenFoneNum + 2;
  end;

  if LenFoneNum > 12 then
  begin
    FoneNum := LeftStr(FoneNum,2) + RemoveZerosEsquerda(copy(FoneNum,3,Length(FoneNum)));
    LenFoneNum := length(FoneNum);
  end;

  case LenFoneNum of
    9: Mascara := '*****-****';
    10:
      begin
        if ComecaComZero and (copy(FoneNum,2,2) = '00') then  // 0300,0500,0800,0900
          Mascara := '****-***-****'
        else
          Mascara := '(**)****-****';
      end;
    11: Mascara := '(**)*****-****';
    12: Mascara := '+**(**)****-****';
  else
    if LenFoneNum > 12 then
      Mascara := '+**(**)*****-****'
    else
      Mascara := '****-****';
  end;

  Result := FormatarMascaraNumerica( FoneNum, Mascara );
end;

function FormatarCPF(const AValue: String): String;
Var S : String ;
begin
  S := PadLeft( OnlyNumber(AValue), 11, '0') ;
  Result := copy(S,1,3) + '.' + copy(S,4 ,3) + '.' +
            copy(S,7,3) + '-' + copy(S,10,2) ;
end;

function FormatarCNPJ(const AValue: String): String;
Var S : String ;
begin
  S := PadLeft( OnlyNumber(AValue), 14, '0') ;
  Result := copy(S,1,2) + '.' + copy(S,3,3) + '.' +
            copy(S,6,3) + '/' + copy(S,9,4) + '-' + copy(S,13,2) ;
end;

function FormatarCAEPF(const AValue: String): String;
Var S : String ;
begin
  S := PadLeft( OnlyNumber(AValue), 14, '0') ;
  Result := copy(S,1,3) + '.' + copy(S,4,3) + '.' +
            copy(S,7,3) + '/' + copy(S,10,3) + '-' + copy(S,13,2) ;
end;

function FormatarCNPJouCPF(const AValue: String): String;
var
  S: String;
begin
  S := OnlyNumber(AValue);
  if Length(S) = 0 then
     Result := S
  else
  begin
    if Length(S) = 14 then
      Result := FormatarCNPJ(S)
    else
      Result := FormatarCPF(S);
  end;
end;

function FormatarPlaca(const AValue: string): string;
Var S : String ;
begin
 S := UpperCase( Trim(AValue) );
 Result := Copy(S, 1, 3) + '-' + Copy(S, 4, 4);
end;

function MascaraIE(const AValue : String; UF : String) : String;
var
 LenDoc : Integer;
 Mascara : String;
begin
  UF      := UpperCase( UF ) ;
  LenDoc  := Length( AValue ) ;
  Mascara := StringOfChar('*', LenDoc) ;

  IF UF = 'AC' Then Mascara := '**.***.***/***-**';
  IF UF = 'AL' Then Mascara := '*********';
  IF UF = 'AP' Then Mascara := '*********';
  IF UF = 'AM' Then Mascara := '**.***.***-*';
  IF UF = 'BA' Then Mascara := '*******-**';
  IF UF = 'CE' Then Mascara := '********-*';
  IF UF = 'DF' Then Mascara := '***********-**';
  IF UF = 'ES' Then Mascara := '*********';
  IF UF = 'GO' Then Mascara := '**.***.***-*';
  IF UF = 'MA' Then Mascara := '*********';
  IF UF = 'MT' Then Mascara := '**********-*';
  IF UF = 'MS' Then Mascara := '**.***.***-*';
  IF UF = 'MG' Then Mascara := '***.***.***/****';
  IF UF = 'PA' Then Mascara := '**-******-*';
  IF UF = 'PB' Then Mascara := '********-*';
  IF UF = 'PR' Then Mascara := '***.*****-**';
  IF UF = 'PE' Then Mascara := IfThen((LenDoc>9),'**.*.***.*******-*','*******-**');
  IF UF = 'PI' Then Mascara := '*********';
  IF UF = 'RJ' Then Mascara := '**.***.**-*';
  IF UF = 'RN' Then Mascara := IfThen((LenDoc>9),'**.*.***.***-*','**.***.***-*');
  IF UF = 'RS' Then Mascara := '***/*******';
  IF UF = 'RO' Then Mascara := IfThen((LenDoc>13),'*************-*','***.*****-*');
  IF UF = 'RR' Then Mascara := '********-*';
  IF UF = 'SC' Then Mascara := '***.***.***';
  IF UF = 'SP' Then Mascara := ifthen((LenDoc>1) and (AValue[1]='P'),'*-********.*/***', '***.***.***.***');
  IF UF = 'SE' Then Mascara := '**.***.***-*';
  IF UF = 'TO' Then Mascara := IfThen((LenDoc=11),'**.**.******-*','**.***.***-*');

  Result := Mascara;

end;

function FormatarIE(const AValue: String; const UF: String): String;
Var
  Mascara : String ;
Begin
  Result := AValue ;
  if UpperCase( Trim(AValue) ) = 'ISENTO' then
     exit ;

  Mascara := MascaraIE( AValue, UF);
  Result := FormatarMascaraNumerica( OnlyAlphaNum( AValue ), Mascara);
end;

function FormatarCheque(const AValue: String): String;
Var S : String ;
begin
  S := PadLeft( Trim(AValue), 7, '0') ; { Prenche zeros a esquerda }
  Result := copy(S,1,6) + '-' + copy(S,7,1) ;
end;

function FormatarPIS(const AValue: String): String;
Var S : String ;
begin
  S := PadLeft( Trim(AValue), 11, '0') ;
  Result := copy(S,1,2) + '.' + copy(S,3,5) + '.' +
            copy(S,8,3) + '.' + copy(S,11,1)
end;

function FormatarCEP(const AValue: String): String;
Var
  S : String ;
begin
  S := OnlyNumber(AValue);
  if Length(S) < 5 then
    S := PadLeft( S, 5, '0');    // "9876" -> "09876"

  if Length(S) = 5 then
    S := PadRight( S, 8, '0')    // "09876" -> "09876-000"; "18270" -> "18270-000"
  else
    S := PadLeft( S, 8, '0') ;    // "9876000" -> "09876-000"

  Result := copy(S,1,5) + '-' + copy(S,6,3) ;
end;

function FormatarCEP(const AValue: Integer): String;
begin
  Result := FormatarCEP(IntToStr(AValue));
end;

function FormatarSUFRAMA(const AValue: String): String;
begin
  Result := AValue;
end;

function FormatarMascaraNumerica(const ANumValue: string; const Mascara: String): String;
var
  LenMas, LenDoc: Integer;
  I, J: Integer;
  C: Char;
  wNumValue: String;
begin
  Result := '';
  wNumValue := Trim( ANumValue );
  LenMas := Length( Mascara ) ;
  LenDoc := Length( wNumValue );

  J := LenMas ;
  For I := LenMas downto 1 do
  begin
    C := Mascara[I] ;

    if C = '*' then
    begin
      if J <= ( LenMas - LenDoc ) then
        C := '0'
      else
        C := wNumValue[( J - ( LenMas - LenDoc ) )] ;

      Dec( J ) ;
    end;

    Result := C + Result;
  end;
end;

Function FormatarMascaraDinamica(const AValue: String; const Mascara: String): String;
var
  LenMas, LenDoc: Integer;
  i, j: Integer;
  c: Char;
  wValue: String;
begin
  Result := '';
  wValue := Trim( AValue );
  LenMas := Length( Mascara ) ;
  LenDoc := Length( wValue );

  i := 1; j := 1 ;
  while (i <= LenMas) and (j <= LenDoc) do
  begin
    c := Mascara[i];
    if c = '*' then
    begin
      c := wValue[j];
      Inc(j);
    end;

    Result := Result + c;
    Inc(i);
  end;
end;

function RemoverMascara(const AValue: String; const Mascara: String): String;
var
  i, m: Integer;
  cm, cv: Char;
  wValue: String;
begin
  Result := '';
  wValue := Trim(AValue);
  m := min(Length(Mascara), Length(wValue));

  i := 1;
  while (i <= m) do
  begin
    cm := Mascara[i];
    cv := wValue[i];
    if (cm = '*') or (cm <> cv) then
      Result := Result + cv;

    Inc(i);
  end;
end;

function FormatarDocumento(const TipoDocto : TACBrValTipoDocto ;
  const Documento : String) : String ;
Var
  ACBrVal : TACBrValidador ;
begin
  ACBrVal := TACBrValidador.Create(nil);
  try
    ACBrVal.RaiseExcept := False;
    ACBrVal.TipoDocto   := TipoDocto;
    ACBrVal.Documento   := Documento;
    Result := ACBrVal.Formatar;
  finally
    ACBrVal.Free;
  end;
end;

function Modulo11(const Documento: string; const Peso: Integer;
  const Base: Integer): String;
Var
  ACBrVal : TACBrValidador ;
begin
  ACBrVal := TACBrValidador.Create(nil);
  try
    ACBrVal.Modulo.Documento            := Documento ;
    ACBrVal.Modulo.MultiplicadorInicial := Peso  ;
    ACBrVal.Modulo.MultiplicadorFinal   := Base ;
    ACBrVal.Modulo.FormulaDigito        := frModulo11 ;
    ACBrVal.Modulo.Calcular ;
    Result := IntToStr( ACBrVal.Modulo.DigitoFinal ) ;
  finally
    ACBrVal.Free;
  end;
end;

{ TACBrValidador }

constructor TACBrValidador.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

  fsIgnorarChar        := cIgnorarChar ;
  fsDocumento          := '' ;
  fsComplemento        := '' ;
  fsDocto              := '' ;
  fsMsgErro            := '' ;
  fsDigitoCalculado    := '' ;
  fsAjustarTamanho     := false ;
  fsExibeDigitoCorreto := false ;
  fsRaiseExcept        := false ;
  fsPermiteVazio       := false ;
  fsOnMsgErro          := nil ;
  fsTipoDocto          := docCPF ;
  fsModulo             := TACBrCalcDigito.Create ;
end;

destructor TACBrValidador.Destroy;
begin
  FreeAndNil( fsModulo ) ;

  inherited Destroy ;
end;

procedure TACBrValidador.SetDocumento(const Value: String);
begin
  if fsDocumento = Value then exit ;

  fsDocumento := Value;
  fsMsgErro   := 'Função Validar não foi chamada' ;
  fsDigitoCalculado := '' ;

  fsDocto := LimpaDocto( fsDocumento ) ;
end;

function TACBrValidador.GetMsgErro : String;
begin
   Result := ACBrStr(fsMsgErro);
end;

Function TACBrValidador.LimpaDocto(const AString : String) : String ;
Var A : Integer ;
begin
  if fsTipoDocto = docEmail then
  begin
    Result := Trim(AString);
    exit;
  end;

  Result := '' ;
  For A := 1 to length( AString ) do
  begin
     if pos(AString[A], fsIgnorarChar) = 0 then
        Result := Result + UpperCase(AString[A]) ;
  end ;

  Result := Trim(Result) ;
end ;

procedure TACBrValidador.SetComplemento(const Value: String);
begin
  fsComplemento := Value;
end;

function TACBrValidador.Validar : Boolean;
Var
  NomeDocto : String ;
begin
  Result    := true ;
  fsMsgErro := '' ;
  fsDocto   := LimpaDocto( fsDocumento ) ;
  fsDigitoCalculado := '' ;

  if (fsDocto = '') then
  begin
     if fsPermiteVazio then
        exit
     else
      begin
        NomeDocto := 'Documento' ;

        case fsTipoDocto of
          docCPF           : NomeDocto := 'CPF'  ;
          docCNPJ          : NomeDocto := 'CNPJ' ;
          docCAEPF         : NomeDocto := 'CAEPF' ;
          docUF            : NomeDocto := 'UF' ;
          docInscEst       : NomeDocto := 'Inscrição Estadual' ;
          docNumCheque     : NomeDocto := 'Número de Cheque' ;
          docPIS           : NomeDocto := 'PIS' ;
          docCEP           : NomeDocto := 'CEP' ;
          docCartaoCredito : NomeDocto := 'Número de Cartão' ;
          docSuframa       : NomeDocto := 'SUFRAMA';
          docGTIN          : NomeDocto := 'GTIN';
          docRenavam       : NomeDocto := 'Renavam';
          docEmail         : NomeDocto := 'E-Mail';
          docCNH           : NomeDocto := 'Carteira Nacional de Habilitação' ;
          docPrefixoGTIN   : NomeDocto := 'Prefixo do Código GTIN' ;
          docPlacaMercosul         : NomeDocto := 'Placa' ;
          docCNS           : NomeDocto := 'Número Cartão Nacional do SUS';
        end;

        fsMsgErro := NomeDocto + ' não pode ser vazio.' ;
      end ;
  end ;

  if fsMsgErro = '' then
     case fsTipoDocto of
       docCPF           : ValidarCPF  ;
       docCNPJ          : ValidarCNPJ ;
       docCAEPF         : ValidarCAEPF ;
       docUF            : ValidarUF( fsDocto ) ;
       docInscEst       : ValidarIE ;
       docNumCheque     : ValidarCheque ;
       docPIS           : ValidarPIS ;
       docCEP           : ValidarCEP ;
       docCartaoCredito : ValidarCartaoCredito ;
       docSuframa       : ValidarSuframa ;
       docGTIN          : ValidarGTIN ;
       docRenavam       : ValidarRenavam ;
       docEmail         : ValidarEmail ;
       docCNH           : ValidarCNH ;
       docPrefixoGTIN   : ValidarPrefixoGTIN ;
       docPlacaMercosul         : ValidarPlaca ;
       docCNS           : ValidarCNS;
     else
      raise Exception.Create('Tipo de documento informado inválido!');
     end;

  if fsMsgErro <> '' then
  begin
     Result := false ;
     
     if Assigned( fsOnMsgErro ) then
        fsOnMsgErro( fsMsgErro ) ;

     if fsRaiseExcept then
        raise Exception.Create( ACBrStr(fsMsgErro) );
  end ;

end;

function TACBrValidador.Formatar: String;
begin
  Result := LimpaDocto(fsDocumento)  ;

  case fsTipoDocto of
    docCPF      : Result := FormatarCPF( Result ) ;
    docCNPJ     : Result := FormatarCNPJ( Result ) ;
    docCAEPF    : Result := FormatarCAEPF( Result ) ;
    docInscEst  : Result := FormatarIE( Result, fsComplemento ) ;
    docNumCheque: Result := FormatarCheque( Result ) ;
    docPIS      : Result := FormatarPIS( Result ) ;
    docCEP      : Result := FormatarCEP( Result ) ;
    docSuframa  : Result := FormatarSUFRAMA( Result ) ;
    docPlacaMercosul    : Result := FormatarPlaca( Result ) ;
  end;
end;

Procedure TACBrValidador.ValidarCheque ;
begin
  if not StrIsNumber( fsDocto ) then
  begin
     fsMsgErro := 'Digite apenas os números do Cheque' ;
     exit ;
  end ;

  Modulo.CalculoPadrao ;
  Modulo.Documento := copy(fsDocto, 1, length(fsDocto)-1) ;
  Modulo.Calcular ;
  fsDigitoCalculado := IntToStr( Modulo.DigitoFinal ) ;

  if fsDigitoCalculado <> copy(fsDocto, length(fsDocto), 1) then
  begin
     fsMsgErro := 'Número de Cheque inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + '.. Dígito correto: '+fsDigitoCalculado ;
  end ;
end;

Procedure TACBrValidador.ValidarCNPJ ;
Var DV1, DV2 : String ;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, 14, '0') ;

  if (Length( fsDocto ) <> 14) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'CNPJ deve ter 14 dígitos. (Apenas números)' ;
     exit
  end ;

  if fsDocto = StringOfChar('0',14) then  // Prevenção contra 00000000000000
  begin
     fsMsgErro := 'CNPJ inválido.' ;
     exit ;
  end ;

  Modulo.CalculoPadrao ;
  Modulo.Documento := copy(fsDocto, 1, 12) ;
  Modulo.Calcular ;
  DV1 := IntToStr( Modulo.DigitoFinal ) ;

  Modulo.Documento := copy(fsDocto, 1, 12)+DV1 ;
  Modulo.Calcular ;
  DV2 := IntToStr( Modulo.DigitoFinal ) ;

  fsDigitoCalculado := DV1+DV2 ;

  if (DV1 <> fsDocto[13]) or (DV2 <> fsDocto[14]) then
  begin
     fsMsgErro := 'CNPJ inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro +  '.. Digito calculado: '+fsDigitoCalculado ;
  end ;
end;

Procedure TACBrValidador.ValidarCPF ;
Var DV1, DV2 : String ;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, 11, '0') ;

  if (Length( fsDocto ) <> 11) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'CPF deve ter 11 dígitos. (Apenas números)' ;
     exit
  end ;

  if pos(fsDocto,'11111111111.22222222222.33333333333.44444444444.55555555555.'+
         '66666666666.77777777777.88888888888.99999999999.00000000000') > 0 then
  begin
     fsMsgErro := 'CPF inválido !' ;
     exit ;
  end ;

  Modulo.MultiplicadorInicial := 2  ;
  Modulo.MultiplicadorFinal   := 11 ;
  Modulo.FormulaDigito        := frModulo11 ;
  Modulo.Documento := copy(fsDocto, 1, 9) ;
  Modulo.Calcular ;
  DV1 := IntToStr( Modulo.DigitoFinal ) ;

  Modulo.Documento := copy(fsDocto, 1, 9)+DV1 ;
  Modulo.Calcular ;
  DV2 := IntToStr( Modulo.DigitoFinal ) ;

  fsDigitoCalculado := DV1+DV2 ;

  if (DV1 <> fsDocto[10]) or (DV2 <> fsDocto[11]) then
  begin
     fsMsgErro := 'CPF inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + '.. Dígito calculado: '+fsDigitoCalculado ;
  end ;
end;

procedure TACBrValidador.ValidarEmail;
const
  InvalidChar = ' àâêôûãõáéíóúçüñýÀÂÊÔÛÃÕÁÉÍÓÚÇÜÑÝ*;:\|#$%&*§!()][{}<>˜ˆ´ªº+¹²³';
var
  i: Integer;
begin
  // se estiver vazio
  if Documento = '' then
  begin
    fsMsgErro := 'e-mail não pode ser vazio!' ;
    exit;
  end;

  // Não existe email com menos de 8 caracteres.
  if Length(Documento) < 8 then
  begin
    fsMsgErro := 'e-mail não pode conter menos do que 8 caracteres!' ;
    exit;
  end;

  fsMsgErro := 'e-mail inválido!' ;

  // Verificando se há somente um @
  if ((Pos('@', Documento) = 0) or (PosEx('@', Documento, Pos('@', Documento) + 1) > 0)) then
    exit;

  // Verificando se no mínimo há um ponto
  if (Pos('.', Documento) = 0) then
    exit;

  // Verificando se há dois pontos juntos
  if (Pos('..', Documento) > 0) then
    exit;

  // Não pode começar ou terminar com @ ou ponto
  if CharInSet(Documento[1], ['@', '.']) or CharInSet(Documento[Length(Documento)], ['@', '.']) then
    exit;

  // O @ e o ponto não podem estar juntos
  if (Documento[Pos('@', Documento) + 1] = '.') or (Documento[Pos('@', Documento) - 1] = '.') then
    exit;

  // O último ponto tem que estar depois do @
  if (PosEx('.', Documento, Pos('@', Documento) + 1) < Pos('@', Documento)) then
    exit;

  // Testa se tem algum caracter inválido.
  for i := 1 to Length(Documento) do
  begin
    if pos( Documento[i], InvalidChar ) > 0 then
      exit;
  end;

  // Tudo OK
  fsMsgErro := '' ;
end;

Procedure TACBrValidador.ValidarCEP ;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( Trim(fsDocto), 8, '0') ;

  if (Length( fsDocto ) <> 8) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'CEP deve ter 8 dígitos. (Apenas números)' ;
     exit
  end ;

  { Passou o UF em Complemento ? Se SIM, verifica o UF }
  fsComplemento := trim(fsComplemento) ;
  if fsComplemento <> '' then
  begin
     ValidarUF( fsComplemento ) ;
     if fsMsgErro <> '' then
        exit ;
   end ;

  if ((fsDocto >= '01000000') and (fsDocto <= '19999999')) and { SP }
     ((fsComplemento = 'SP')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '20000000') and (fsDocto <= '28999999')) and { RJ }
     ((fsComplemento = 'RJ')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '29000000') and (fsDocto <= '29999999')) and { ES }
     ((fsComplemento = 'ES')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '30000000') and (fsDocto <= '39999999')) and { MG }
     ((fsComplemento = 'MG')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '40000000') and (fsDocto <= '48999999')) and { BA }
     ((fsComplemento = 'BA')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '49000000') and (fsDocto <= '49999999')) and { SE }
     ((fsComplemento = 'SE')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '50000000') and (fsDocto <= '56999999')) and { PE }
     ((fsComplemento = 'PE')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '57000000') and (fsDocto <= '57999999')) and { AL }
     ((fsComplemento = 'AL')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '58000000') and (fsDocto <= '58999999')) and { PB }
     ((fsComplemento = 'PB')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '59000000') and (fsDocto <= '59999999')) and { RN }
     ((fsComplemento = 'RN')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '60000000') and (fsDocto <= '63999999')) and { CE }
     ((fsComplemento = 'CE')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '64000000') and (fsDocto <= '64999999')) and { PI }
     ((fsComplemento = 'PI')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '65000000') and (fsDocto <= '65999999')) and { MA }
     ((fsComplemento = 'MA')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '66000000') and (fsDocto <= '68899999')) and { PA }
     ((fsComplemento = 'PA')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '68900000') and (fsDocto <= '68999999')) and { AP }
     ((fsComplemento = 'AP')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '69000000') and (fsDocto <= '69299999')) and { AM }
     ((fsComplemento = 'AM')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '69300000') and (fsDocto <= '69399999')) and { RR }
     ((fsComplemento = 'RR')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '69400000') and (fsDocto <= '69899999')) and { AM }
     ((fsComplemento = 'AM')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '69900000') and (fsDocto <= '69999999')) and { AC }
     ((fsComplemento = 'AC')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '70000000') and (fsDocto <= '72799999')) and { DF }
     ((fsComplemento = 'DF')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '72800000') and (fsDocto <= '72999999')) and { GO }
     ((fsComplemento = 'GO')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '73000000') and (fsDocto <= '73699999')) and { DF }
     ((fsComplemento = 'DF')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '73700000') and (fsDocto <= '76799999')) and { GO }
     ((fsComplemento = 'GO')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '77000000') and (fsDocto <= '77999999')) and { TO }
     ((fsComplemento = 'TO')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '78000000') and (fsDocto <= '78899999')) and { MT }
     ((fsComplemento = 'MT')  or  (fsComplemento = '')) then exit ;

(* if ((fsDocto >= '78900000') and (fsDocto <= '78999999')) and { RO }
 Faixa antiga foi removida: http://acbr.sourceforge.net/mantis/view.php?id=19 *)  
  if ((fsDocto >= '76800000') and (fsDocto <= '76999999')) and { RO }
     ((fsComplemento = 'RO')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '79000000') and (fsDocto <= '79999999')) and { MS }
     ((fsComplemento = 'MS')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '80000000') and (fsDocto <= '87999999')) and { PR }
     ((fsComplemento = 'PR')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '88000000') and (fsDocto <= '89999999')) and { SC }
     ((fsComplemento = 'SC')  or  (fsComplemento = '')) then exit ;

  if ((fsDocto >= '90000000') and (fsDocto <= '99999999')) and { RS }
     ((fsComplemento = 'RS')  or  (fsComplemento = '')) then exit ;

  if fsComplemento <> '' then
     fsMsgErro := 'CEP inválido para '+fsComplemento+' !' 
  else
     fsMsgErro := 'CEP inválido !' ;

end;

Procedure TACBrValidador.ValidarIE ;
Const
   c0_9 : String = '0-9' ;
   cPesos : array[1..14] of array[1..14] of Integer =
          {1} {2} {3} {4} {5} {6} {7} {8} {9} {10} {11} {12} {13} {14}
  {1}    ((0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ,2   ,3   ,4   ,5   ,6 ),
  {2}     (0  ,0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9   ,2   ,3   ,4   ,5 ),
  {3}     (2  ,0  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ,2   ,3   ,4   ,5   ,6 ),
  {4}     (0  ,2  ,3  ,4  ,5  ,6  ,0  ,0  ,0  ,0   ,0   ,0   ,0   ,0 ),
  {5}     (0  ,8  ,7  ,6  ,5  ,4  ,3  ,2  ,1  ,0   ,0   ,0   ,0   ,0 ),
  {6}     (0  ,2  ,3  ,4  ,5  ,6  ,7  ,0  ,0  ,8   ,9   ,0   ,0   ,0 ),
  {7}     (0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ,1   ,2   ,3   ,4   ,5 ),
  {8}     (0  ,2  ,3  ,4  ,5  ,6  ,7  ,2  ,3  ,4   ,5   ,6   ,7   ,8 ),
  {9}     (0  ,0  ,2  ,3  ,4  ,5  ,6  ,7  ,2  ,3   ,4   ,5   ,6   ,7 ),
  {10}    (0  ,0  ,2  ,1  ,2  ,1  ,2  ,1  ,2  ,1   ,1   ,2   ,1   ,0 ),
  {11}    (0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ,10  ,11  ,2   ,3   ,0 ),
  {12}    (0  ,0  ,0  ,0  ,10 ,8  ,7  ,6  ,5  ,4   ,3   ,1   ,0   ,0 ),
  {13}    (0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ,10  ,2   ,3   ,0   ,0 ),
  {14}    (0  ,0  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,3   ,4   ,5   ,6   ,7 ));

Var
   vDigitos : array of Variant;
   xROT, yROT :  String ;
   Tamanho, FatorF, FatorG, I, xMD, xTP, yMD, yTP, DV, DVX, DVY : Integer ;
   SOMA, SOMAq, nD, M : Integer ;
   OK : Boolean ;
   Passo, D : Char;
   LPrefixo : String;

begin
  if UpperCase( Trim(fsDocto) ) = 'ISENTO' then
     exit ;
     
  if fsComplemento = '' then
  begin
     fsMsgErro := 'Informe a UF no campo Complemento' ;
     exit ;
  end ;

  ValidarUF( fsComplemento ) ;
  if fsMsgErro <> '' then
     exit ;

  { Somente digitos ou letra P na primeira posicao }
  { P é usado pela Insc.Estadual de Produtor Rural de SP }
  if ( not StrIsNumber( copy(fsDocto,2,length(fsDocto) ))) or
     ( not CharIsNum(fsDocto[1]) and (fsDocto[1] <> 'P')) then
  begin
     fsMsgErro := 'Caracteres inválidos na Inscrição Estadual' ;
     exit
  end ;

  Tamanho := 0  ;
  xROT    := 'E';
  xMD     := 11 ;
  xTP     := 1  ;
  yROT    := '' ;
  yMD     := 0  ;
  yTP     := 0  ;
  FatorF  := 0  ;
  FatorG  := 0  ;

  SetLength( vDigitos, 13);
  vDigitos := VarArrayOf(['','','','','','','','','','','','','','']) ;

  if fsComplemento = 'AC' then
  begin
     if Length(fsDocto) = 9 then
      begin
        Tamanho := 9 ;
        vDigitos := VarArrayOf(
           ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1','0','','','','',''] ) ;
      end
     else
     begin
       Tamanho  := 13;
       xTP      := 2;
       yROT     := 'E';
       yMD      := 11;
       yTP      := 1 ;
       vDigitos := VarArrayOf(
         ['DVY','DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1','0','']);
     end ;
  end ;

  if fsComplemento = 'AL' then
  begin
     Tamanho := 9 ;
     xROT := 'BD' ;
     vDigitos   := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'4','2','','','','',''] ) ;
  end ;

  if fsComplemento = 'AP' then
  begin
    Tamanho := 9 ;
    xROT := 'CE' ;
    vDigitos   := VarArrayOf(
       ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'3','0','','','','',''] ) ;

    if (fsDocto >= '030170010') and (fsDocto <= '030190229') then
       FatorF := 1
    else if fsDocto >= '030190230' then
       xROT := 'E' ;
  end ;

  if fsComplemento = 'AM' then
  begin
     Tamanho := 9 ;
     vDigitos  := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] ) ;
  end ;

  if fsComplemento = 'BA' then
  begin
    if Length(fsDocto) < 9 then
       fsDocto := PadLeft(fsDocto,9,'0') ;

    Tamanho := 9 ;
    xTP := 2   ;   yTP  := 3   ;   yROT := 'E' ;
    vDigitos := VarArrayOf(
       ['DVX','DVY',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] ) ;

    if pos(fsDocto[2],'0123458') > 0 then
     begin
       xMD := 10   ;   yMD := 10 ;
     end
    else
     begin
       xMD := 11   ;   yMD := 11 ;
     end ;
  end ;

  if fsComplemento = 'CE' then
  begin
     Tamanho := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'0','','','','',''] ) ;
  end ;

  if fsComplemento = 'DF' then
  begin
     Tamanho := 13 ;
     xTP := 2   ;   yROT := 'E'  ;   yMD  := 11   ;   yTP  := 1 ;
     vDigitos  := VarArrayOf(
        ['DVY','DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'7,8','0','']);
  end ;

  if fsComplemento = 'ES' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] ) ;
  end ;

  if fsComplemento = 'GO' then
  begin
    Tamanho  := 9 ;
    vDigitos := VarArrayOf(
       [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1,2','','','','',''] ) ;
    LPrefixo := fsDocto[1] + fsDocto[2];

    if not (StrToIntDef(LPrefixo,0) in [10,11,20..29]) then
      fsMsgErro := ACBrStr('Prefixo IE Inválido 10 ou 11 / 20 a 29');

    if (fsDocto >= '101031050') and (fsDocto <= '101199979') then
       FatorG := 1 ;
  end ;

  if fsComplemento = 'MA' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'2','1','','','','',''] ) ;
  end ;

  if fsComplemento = 'MT' then
  begin
     if Length(fsDocto) = 9 then
        fsDocto := PadLeft(fsDocto,11,'0') ;

     Tamanho := 11 ;
     vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','',''] ) ;
  end ;

  if fsComplemento = 'MS' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'8','2','','','','',''] ) ;
  end ;

  if fsComplemento = 'MG' then
  begin
     Tamanho  := 13 ;
     xROT := 'AE'    ;   xMD := 10   ;   xTP := 10 ;
     yROT := 'E'     ;   yMD := 11   ;   yTP := 11 ;
     vDigitos := VarArrayOf(
       ['DVY','DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'']);
  end ;

  if fsComplemento = 'PA' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'5','1','','','','',''] ) ;
  end ;

  if fsComplemento = 'PB' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'6','1','','','','',''] ) ;
  end ;

  if fsComplemento = 'PR' then
  begin
     Tamanho := 10 ;
     xTP := 9   ;   yROT := 'E'   ;   yMD := 11   ;   yTP := 8 ;
     vDigitos := VarArrayOf(
        [ 'DVY','DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','',''] ) ;
  end ;

  if fsComplemento = 'PE' then
  begin
    if Length(fsDocto) = 14 then
    begin
      //Antigo CACEPE
      Tamanho := 14;
      xTP     := 7;
      FatorF  := 1;
      vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1-9','8','1']);
    end
    else
    begin
      Tamanho := 9;
      xTP     := 14;
      yROT    := 'E';
      yMD     := 11;
      yTP     := 7;
      vDigitos := VarArrayOf(
        [ 'DVY','DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] );
    end;
  end;

  if fsComplemento = 'PI' then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'9','1','','','','',''] ) ;
  end ;

  if fsComplemento = 'RJ' then
  begin
     Tamanho := 8 ;
     xTP := 8 ;
     vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1,7,8,9','','','','','',''] ) ;
  end ;

  if fsComplemento = 'RN' then
  begin
    if Length(fsDocto) = 9 then
    begin
      Tamanho := 9 ;
      xROT := 'BD' ;
      vDigitos := VarArrayOf(
         [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'0','2','','','','',''] ) ;
    end
    else
    begin
      Tamanho  := 10 ;
      xROT     := 'BD' ;
      xTP      := 11 ;
      vDigitos := VarArrayOf(
         [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'0','2','','','',''] ) ;
    end;
  end ;

  if fsComplemento = 'RS' then
  begin
     Tamanho := 10 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','',''] ) ;
  end ;

  if fsComplemento = 'RO' then
  begin
     FatorF := 1 ;
     if Length(fsDocto) = 9 then
     begin
        Tamanho := 9 ;
        xTP := 4 ;
        vDigitos := VarArrayOf(
          [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1-9','','','','',''] ) ;
     end 
     else
     begin
        Tamanho  := 14 ;
        vDigitos := VarArrayOf(
        ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9]);
     end ;
  end ;
  
  if fsComplemento = 'RR' then
  begin
     Tamanho  := 9 ;
     xROT := 'D'   ;   xMD := 9   ;   xTP := 5 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'4','2','','','','',''] ) ;
  end ;

  if (fsComplemento = 'SC') or (fsComplemento = 'SE') then
  begin
     Tamanho  := 9 ;
     vDigitos := VarArrayOf(
        [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] ) ;
  end;

  if fsComplemento = 'SP' then
  begin
     xROT := 'D'   ;   xTP := 12 ;
     if fsDocto[1] = 'P' then
      begin
        Tamanho  := 13 ;
        vDigitos := VarArrayOf(
         [c0_9,c0_9,c0_9,'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'P','']);
      end
     else
      begin
        Tamanho  := 12 ;
        yROT := 'D'   ;   yMD := 11   ;   yTP := 13 ;
        vDigitos := VarArrayOf(
         ['DVY',c0_9,c0_9,'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','']);
      end ;
  end ;

  if fsComplemento = 'TO' then
  begin
     if Length(fsDocto)=11 then
      begin
        Tamanho := 11 ;
        xTP := 6 ;
        vDigitos := VarArrayOf(
          ['DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'1,2,3,9','0,9','9','2','','','']);
      end
     else
      begin
{        Tamanho := 10 ;
        vDigitos := VarArrayOf(
          [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'0-4','','','',''] ) ; }
        Tamanho := 9 ;
        vDigitos := VarArrayOf(
          [ 'DVX',c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,c0_9,'','','','',''] )
      end;
  end ;

  { Verificando se o tamanho Total está correto }
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, Tamanho, '0') ;

  OK := (Tamanho > 0) and (Length(fsDocto) = Tamanho) ;
  if not OK then
     fsMsgErro := Format('Tamanho inválido: Esperado %d caracteres, mas foram digitados somente %d caracteres.', [Tamanho, Length(fsDocto)]) ;

  { Verificando os digitos nas posicoes são permitidos }
  fsDocto := PadLeft(fsDocto,14) ;
  DVX := 0  ;
  DVY := 0  ;
  I   := 13 ;
  while OK and (I >= 0) do
  begin
     D := fsDocto[14-I] ;

     if vDigitos[I] = '' then
        OK := (D = ' ')

     else if (vDigitos[I] = 'DVX') or (vDigitos[I] = 'DVY') or
             (vDigitos[I] = c0_9) then
      begin
        OK := CharIsNum( D ) ;

        if vDigitos[I] = 'DVX' then
           DVX := StrToIntDef( D, 0 )
        else
           if vDigitos[I] = 'DVY' then
              DVY := StrToIntDef( D, 0 ) ;
      end

     else if pos(',',vDigitos[I]) > 0 then   { Ex: '2,5,7,8' Apenas os da lista}
        OK := (pos( D, vDigitos[I] ) > 0)

     else if pos('-',vDigitos[I]) > 0 then
        OK := ( (D >= copy(vDigitos[I],1,1)) and (D <= copy(vDigitos[I],3,1)) )

     else
        OK := ( D = vDigitos[I] ) ;

     if not OK then
        fsMsgErro := Format('Dígito %d deveria ser %s ',
         [14-I-(14-Tamanho), vDigitos[I]]) ;

     I := I - 1 ;
  end ;

  Passo := 'X' ;
  while OK and (xTP > 0) do
  begin
     SOMA := 0  ;
     SOMAq:= 0  ;
     I    := 14 ;

     while OK and (I > 0) do
     begin
        D := fsDocto[15-I] ;

        if CharIsNum(D) then
        begin
           nD := StrToIntDef(D,0) ;
           M  := nD * cPesos[xTP,I] ;
           SOMA := SOMA + M ;

           if pos('A',xROT) > 0 then
              SOMAq := SOMAq + Trunc(M / 10) ;
        end ;

        I := I - 1 ;
     end ;

     if pos('A',xROT) > 0 then
        SOMA := SOMA + SOMAq

     else if pos('B',xROT) > 0 then
        SOMA := SOMA * 10

     else if pos('C',xROT) > 0 then
        SOMA := SOMA + (5 + (4 * FatorF) ) ;

     { Calculando digito verificador }
     DV := Trunc(SOMA mod xMD) ;
     if pos('E',xROT) > 0 then
        DV := Trunc(xMD - DV) ;

     if DV = 10 then
        DV := FatorG   { Apenas GO modifica o FatorG para diferente de 0 }
     else if DV = 11 then
        DV := FatorF ;

     if Passo = 'X' then
        OK := (DVX = DV)
     else
        OK := (DVY = DV) ;

     fsDigitoCalculado := IntToStr(DV) ;
     if not OK then
     begin
        fsMsgErro := 'Dígito verificador inválido.' ;

        if fsExibeDigitoCorreto then
           fsMsgErro := fsMsgErro + '.. Calculado: '+fsDigitoCalculado ;
     end ;

     if PASSO = 'X' then
      begin
        PASSO := 'Y'  ;
        xROT  := yROT ;
        xMD   := yMD  ;
        xTP   := yTP  ;
      end
     else
        break ;
  end ;

  fsDocto := Trim( fsDocto ) ;
  if (fsMsgErro <> '') then
     fsMsgErro := 'Insc.Estadual inválida para '+fsComplemento +' '+ fsMsgErro ;

end;

Procedure TACBrValidador.ValidarUF(const UF: String) ;
begin
 if pos( ','+UF+',', cUFsValidas) = 0 then
    fsMsgErro := 'UF inválido: '+UF ;
end;

procedure TACBrValidador.ValidarPIS;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, 11, '0') ;

  if (Length( fsDocto ) <> 11) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'PIS deve ter 11 dígitos. (Apenas números)' ;
     exit;
  end ;

  Modulo.CalculoPadrao ;
  Modulo.FormulaDigito := frModulo10PIS ;
  Modulo.Documento     := copy(fsDocto, 1, 10) ;
  Modulo.Calcular ;
  fsDigitoCalculado := IntToStr( Modulo.DigitoFinal ) ;

  if (fsDigitoCalculado <> fsDocto[11]) then
  begin
     fsMsgErro := 'PIS inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + '.. Dígito calculado: '+fsDigitoCalculado ;
  end ;
end;

procedure TACBrValidador.ValidarPlaca;
begin
  // RegExp = [A-Z]{2,3}[0-9]{4}|[A-Z]{3,4}[0-9]{3}|[A-Z0-9]{7}

  if (Length( fsDocto ) <> 7) or ( not StrIsAlphaNum( fsDocto ) ) then
  begin
     fsMsgErro := 'Placa deve ter 7 dígitos. (Somente letras e números)' ;
     exit;
  end ;

end;

procedure TACBrValidador.ValidarCNS;

  function validaCnsInic12: boolean;
  var
   soma, resto, dv: integer;
   resultado: string;
  begin
    Modulo.MultiplicadorInicial := 5;
    Modulo.MultiplicadorFinal := 15;
    Modulo.FormulaDigito := frModulo11;
    Modulo.Documento := PadLeft(fsDocto, 11, '0'); // pis
    Modulo.Calcular;

    if Modulo.ModuloFinal = 10 then
    begin
      soma := Modulo.SomaDigitos + 2;
      resto := soma mod 11;
      dv := 11 - resto;
      resultado := Modulo.fsDocto + '001' + IntToStr(dv);
    end
    else
      resultado := Modulo.fsDocto + '000' + IntToStr(Modulo.DigitoFinal);

    result := fsDocto = resultado;
  end;

  function validaCnsInic789: boolean;
  begin
    Modulo.MultiplicadorInicial := 1;
    Modulo.MultiplicadorFinal := 15;
    Modulo.FormulaDigito := frModulo11;
    Modulo.Documento := fsDocto;
    Modulo.Calcular;
    Result := Modulo.ModuloFinal = 0;
  end;

var
  resultado: boolean;

begin
   resultado:=false;

   if fsAjustarTamanho then
     fsDocto := PadLeft(fsDocto, 15, '0');

   if Length(fsDocto) <> 15 then
   begin
      fsMsgErro := 'O número do Cartão Nacional do SUS deve ter 15 dígitos';
      exit;
   end;

   case PChar(fsDocto)^ of
     '1'..'2': resultado := validaCnsInic12;
     '7'..'9': resultado := validaCnsInic789;
   end;

  if not resultado then
  begin
     fsMsgErro := 'O número do Cartão Nacional do SUS informado é inválido!' ;
     exit;
  end;
end;


procedure TACBrValidador.ValidarPrefixoGTIN;
type
  TRangePrefixGTIN = record
    fxPrefixIni: Integer;
    fxPrefixFim: Integer;
    indEsp: Integer;
    fxPaisNome: String;
  end;

var
  I: Integer;
  CodigoNormalizado: String;
  sPrefixo: String;
  iPrefixo: Integer;
  bEncontrado: Boolean;

const
  ARRAY_PREFIX_GTIN: array[0..126] of TRangePrefixGTIN = (
    (fxPrefixIni: 000;	fxPrefixFim: 019; indEsp: 0; fxPaisNome: 'GS1 US'),
    (fxPrefixIni: 020;	fxPrefixFim: 029; indEsp: 1; fxPaisNome: 'Números de circulação restrita dentro da região'),
    (fxPrefixIni: 030;	fxPrefixFim: 039; indEsp: 0; fxPaisNome: 'GS1 US'),
    (fxPrefixIni: 040;	fxPrefixFim: 049; indEsp: 1; fxPaisNome: 'GS1 Números de circulação restrita dentro da empresa'),
    (fxPrefixIni: 050;	fxPrefixFim: 059; indEsp: 1; fxPaisNome: 'GS1 US reserved for future use'),
    (fxPrefixIni: 060;	fxPrefixFim: 139; indEsp: 0; fxPaisNome: 'GS1 US'),
    (fxPrefixIni: 200;	fxPrefixFim: 299; indEsp: 1; fxPaisNome: 'GS1 Números de circulação restrita dentro da região'),
    (fxPrefixIni: 300;	fxPrefixFim: 379; indEsp: 0; fxPaisNome: 'GS1 France'),
    (fxPrefixIni: 380;	fxPrefixFim: 380; indEsp: 0; fxPaisNome: 'GS1 Bulgaria'),
    (fxPrefixIni: 383;	fxPrefixFim: 383; indEsp: 0; fxPaisNome: 'GS1 Slovenija'),
    (fxPrefixIni: 385;	fxPrefixFim: 385; indEsp: 0; fxPaisNome: 'GS1 Croatia'),
    (fxPrefixIni: 387;	fxPrefixFim: 387; indEsp: 0; fxPaisNome: 'GS1 BIH (Bosnia-Herzegovina)'),
    (fxPrefixIni: 389;	fxPrefixFim: 389; indEsp: 0; fxPaisNome: 'GS1 Montenegro'),
    (fxPrefixIni: 400;	fxPrefixFim: 440; indEsp: 0; fxPaisNome: 'GS1 Germany'),
    (fxPrefixIni: 450;	fxPrefixFim: 459; indEsp: 0; fxPaisNome: 'GS1 Japan'),
    (fxPrefixIni: 490;	fxPrefixFim: 499; indEsp: 0; fxPaisNome: 'GS1 Japan'),
    (fxPrefixIni: 460;	fxPrefixFim: 469; indEsp: 0; fxPaisNome: 'GS1 Russia'),
    (fxPrefixIni: 470;	fxPrefixFim: 470; indEsp: 0; fxPaisNome: 'GS1 Kyrgyzstan'),
    (fxPrefixIni: 471;	fxPrefixFim: 471; indEsp: 0; fxPaisNome: 'GS1 Taiwan'),
    (fxPrefixIni: 474;	fxPrefixFim: 474; indEsp: 0; fxPaisNome: 'GS1 Estonia'),
    (fxPrefixIni: 475;	fxPrefixFim: 475; indEsp: 0; fxPaisNome: 'GS1 Latvia'),
    (fxPrefixIni: 476;	fxPrefixFim: 476; indEsp: 0; fxPaisNome: 'GS1 Azerbaijan'),
    (fxPrefixIni: 477;	fxPrefixFim: 477; indEsp: 0; fxPaisNome: 'GS1 Lithuania'),
    (fxPrefixIni: 478;	fxPrefixFim: 478; indEsp: 0; fxPaisNome: 'GS1 Uzbekistan'),
    (fxPrefixIni: 479;	fxPrefixFim: 479; indEsp: 0; fxPaisNome: 'GS1 Sri Lanka'),
    (fxPrefixIni: 480;	fxPrefixFim: 480; indEsp: 0; fxPaisNome: 'GS1 Philippines'),
    (fxPrefixIni: 481;	fxPrefixFim: 481; indEsp: 0; fxPaisNome: 'GS1 Belarus'),
    (fxPrefixIni: 482;	fxPrefixFim: 482; indEsp: 0; fxPaisNome: 'GS1 Ukraine'),
    (fxPrefixIni: 483;	fxPrefixFim: 483; indEsp: 0; fxPaisNome: 'GS1 Turkmenistan'),
    (fxPrefixIni: 484;	fxPrefixFim: 484; indEsp: 0; fxPaisNome: 'GS1 Moldova'),
    (fxPrefixIni: 485;	fxPrefixFim: 485; indEsp: 0; fxPaisNome: 'GS1 Armenia'),
    (fxPrefixIni: 486;	fxPrefixFim: 486; indEsp: 0; fxPaisNome: 'GS1 Georgia'),
    (fxPrefixIni: 487;	fxPrefixFim: 487; indEsp: 0; fxPaisNome: 'GS1 Kazakstan'),
    (fxPrefixIni: 488;	fxPrefixFim: 488; indEsp: 0; fxPaisNome: 'GS1 Tajikistan'),
    (fxPrefixIni: 489;	fxPrefixFim: 489; indEsp: 0; fxPaisNome: 'GS1 Hong Kong'),
    (fxPrefixIni: 500;	fxPrefixFim: 509; indEsp: 0; fxPaisNome: 'GS1 UK'),
    (fxPrefixIni: 520;	fxPrefixFim: 521; indEsp: 0; fxPaisNome: 'GS1 Association Greece'),
    (fxPrefixIni: 528;	fxPrefixFim: 528; indEsp: 0; fxPaisNome: 'GS1 Lebanon'),
    (fxPrefixIni: 529;	fxPrefixFim: 529; indEsp: 0; fxPaisNome: 'GS1 Cyprus'),
    (fxPrefixIni: 530;	fxPrefixFim: 530; indEsp: 0; fxPaisNome: 'GS1 Albania'),
    (fxPrefixIni: 531;	fxPrefixFim: 531; indEsp: 0; fxPaisNome: 'GS1 Macedonia'),
    (fxPrefixIni: 535;	fxPrefixFim: 535; indEsp: 0; fxPaisNome: 'GS1 Malta'),
    (fxPrefixIni: 539;	fxPrefixFim: 539; indEsp: 0; fxPaisNome: 'GS1 Ireland'),
    (fxPrefixIni: 540;	fxPrefixFim: 549; indEsp: 0; fxPaisNome: 'GS1 Belgium & Luxembourg'),
    (fxPrefixIni: 560;	fxPrefixFim: 560; indEsp: 0; fxPaisNome: 'GS1 Portugal'),
    (fxPrefixIni: 569;	fxPrefixFim: 569; indEsp: 0; fxPaisNome: 'GS1 Iceland'),
    (fxPrefixIni: 570;	fxPrefixFim: 579; indEsp: 0; fxPaisNome: 'GS1 Denmark'),
    (fxPrefixIni: 590;	fxPrefixFim: 590; indEsp: 0; fxPaisNome: 'GS1 Poland'),
    (fxPrefixIni: 594;	fxPrefixFim: 594; indEsp: 0; fxPaisNome: 'GS1 Romania'),
    (fxPrefixIni: 599;	fxPrefixFim: 599; indEsp: 0; fxPaisNome: 'GS1 Hungary'),
    (fxPrefixIni: 600;	fxPrefixFim: 601; indEsp: 0; fxPaisNome: 'GS1 South Africa'),
    (fxPrefixIni: 603;	fxPrefixFim: 603; indEsp: 0; fxPaisNome: 'GS1 Ghana'),
    (fxPrefixIni: 604;	fxPrefixFim: 604; indEsp: 0; fxPaisNome: 'GS1 Senegal'),
    (fxPrefixIni: 608;	fxPrefixFim: 608; indEsp: 0; fxPaisNome: 'GS1 Bahrain'),
    (fxPrefixIni: 609;	fxPrefixFim: 609; indEsp: 0; fxPaisNome: 'GS1 Mauritius'),
    (fxPrefixIni: 611;	fxPrefixFim: 611; indEsp: 0; fxPaisNome: 'GS1 Morocco'),
    (fxPrefixIni: 613;	fxPrefixFim: 613; indEsp: 0; fxPaisNome: 'GS1 Algeria'),
    (fxPrefixIni: 615;	fxPrefixFim: 615; indEsp: 0; fxPaisNome: 'GS1 Nigeria'),
    (fxPrefixIni: 616;	fxPrefixFim: 616; indEsp: 0; fxPaisNome: 'GS1 Kenya'),
    (fxPrefixIni: 618;	fxPrefixFim: 618; indEsp: 0; fxPaisNome: 'GS1 Ivory Coast'),
    (fxPrefixIni: 619;	fxPrefixFim: 619; indEsp: 0; fxPaisNome: 'GS1 Tunisia'),
    (fxPrefixIni: 620;	fxPrefixFim: 620; indEsp: 0; fxPaisNome: 'GS1 Tanzania'),
    (fxPrefixIni: 621;	fxPrefixFim: 621; indEsp: 0; fxPaisNome: 'GS1 Syria'),
    (fxPrefixIni: 622;	fxPrefixFim: 622; indEsp: 0; fxPaisNome: 'GS1 Egypt'),
    (fxPrefixIni: 623;	fxPrefixFim: 623; indEsp: 0; fxPaisNome: 'GS1 Brunei'),
    (fxPrefixIni: 624;	fxPrefixFim: 624; indEsp: 0; fxPaisNome: 'GS1 Libya'),
    (fxPrefixIni: 625;	fxPrefixFim: 625; indEsp: 0; fxPaisNome: 'GS1 Jordan'),
    (fxPrefixIni: 626;	fxPrefixFim: 626; indEsp: 0; fxPaisNome: 'GS1 Iran'),
    (fxPrefixIni: 627;	fxPrefixFim: 627; indEsp: 0; fxPaisNome: 'GS1 Kuwait'),
    (fxPrefixIni: 628;	fxPrefixFim: 628; indEsp: 0; fxPaisNome: 'GS1 Saudi Arabia'),
    (fxPrefixIni: 629;	fxPrefixFim: 629; indEsp: 0; fxPaisNome: 'GS1 Emirates'),
    (fxPrefixIni: 640;	fxPrefixFim: 649; indEsp: 0; fxPaisNome: 'GS1 Finland'),
    (fxPrefixIni: 690;	fxPrefixFim: 699; indEsp: 0; fxPaisNome: 'GS1 China'),
    (fxPrefixIni: 700;	fxPrefixFim: 709; indEsp: 0; fxPaisNome: 'GS1 Norway'),
    (fxPrefixIni: 729;	fxPrefixFim: 729; indEsp: 0; fxPaisNome: 'GS1 Israel'),
    (fxPrefixIni: 730;	fxPrefixFim: 739; indEsp: 0; fxPaisNome: 'GS1 Sweden'),
    (fxPrefixIni: 740;	fxPrefixFim: 740; indEsp: 0; fxPaisNome: 'GS1 Guatemala'),
    (fxPrefixIni: 741;	fxPrefixFim: 741; indEsp: 0; fxPaisNome: 'GS1 El Salvador'),
    (fxPrefixIni: 742;	fxPrefixFim: 742; indEsp: 0; fxPaisNome: 'GS1 Honduras'),
    (fxPrefixIni: 743;	fxPrefixFim: 743; indEsp: 0; fxPaisNome: 'GS1 Nicaragua'),
    (fxPrefixIni: 744;	fxPrefixFim: 744; indEsp: 0; fxPaisNome: 'GS1 Costa Rica'),
    (fxPrefixIni: 745;	fxPrefixFim: 745; indEsp: 0; fxPaisNome: 'GS1 Panama'),
    (fxPrefixIni: 746;	fxPrefixFim: 746; indEsp: 0; fxPaisNome: 'GS1 Republica Dominicana'),
    (fxPrefixIni: 750;	fxPrefixFim: 750; indEsp: 0; fxPaisNome: 'GS1 Mexico'),
    (fxPrefixIni: 754;	fxPrefixFim: 755; indEsp: 0; fxPaisNome: 'GS1 Canada'),
    (fxPrefixIni: 759;	fxPrefixFim: 759; indEsp: 0; fxPaisNome: 'GS1 Venezuela'),
    (fxPrefixIni: 760;	fxPrefixFim: 769; indEsp: 0; fxPaisNome: 'GS1 Schweiz; Suisse; Svizzera'),
    (fxPrefixIni: 770;	fxPrefixFim: 771; indEsp: 0; fxPaisNome: 'GS1 Colombia'),
    (fxPrefixIni: 773;	fxPrefixFim: 773; indEsp: 0; fxPaisNome: 'GS1 Uruguay'),
    (fxPrefixIni: 775;	fxPrefixFim: 775; indEsp: 0; fxPaisNome: 'GS1 Peru'),
    (fxPrefixIni: 777;	fxPrefixFim: 777; indEsp: 0; fxPaisNome: 'GS1 Bolivia'),
    (fxPrefixIni: 778;	fxPrefixFim: 779; indEsp: 0; fxPaisNome: 'GS1 Argentina'),
    (fxPrefixIni: 780;	fxPrefixFim: 780; indEsp: 0; fxPaisNome: 'GS1 Chile'),
    (fxPrefixIni: 784;	fxPrefixFim: 784; indEsp: 0; fxPaisNome: 'GS1 Paraguay'),
    (fxPrefixIni: 786;	fxPrefixFim: 786; indEsp: 0; fxPaisNome: 'GS1 Ecuador'),
    (fxPrefixIni: 789;	fxPrefixFim: 790; indEsp: 0; fxPaisNome: 'GS1 Brasil'),
    (fxPrefixIni: 800;	fxPrefixFim: 839; indEsp: 0; fxPaisNome: 'GS1 Italy'),
    (fxPrefixIni: 840;	fxPrefixFim: 849; indEsp: 0; fxPaisNome: 'GS1 Spain'),
    (fxPrefixIni: 850;	fxPrefixFim: 850; indEsp: 0; fxPaisNome: 'GS1 Cuba'),
    (fxPrefixIni: 858;	fxPrefixFim: 858; indEsp: 0; fxPaisNome: 'GS1 Slovakia'),
    (fxPrefixIni: 859;	fxPrefixFim: 859; indEsp: 0; fxPaisNome: 'GS1 Czech'),
    (fxPrefixIni: 860;	fxPrefixFim: 860; indEsp: 0; fxPaisNome: 'GS1 Serbia'),
    (fxPrefixIni: 865;	fxPrefixFim: 865; indEsp: 0; fxPaisNome: 'GS1 Mongolia'),
    (fxPrefixIni: 867;	fxPrefixFim: 867; indEsp: 0; fxPaisNome: 'GS1 North Korea'),
    (fxPrefixIni: 868;	fxPrefixFim: 869; indEsp: 0; fxPaisNome: 'GS1 Turkey'),
    (fxPrefixIni: 870;	fxPrefixFim: 879; indEsp: 0; fxPaisNome: 'GS1 Netherlands'),
    (fxPrefixIni: 880;	fxPrefixFim: 880; indEsp: 0; fxPaisNome: 'GS1 South Korea'),
    (fxPrefixIni: 884;	fxPrefixFim: 884; indEsp: 0; fxPaisNome: 'GS1 Cambodia'),
    (fxPrefixIni: 885;	fxPrefixFim: 885; indEsp: 0; fxPaisNome: 'GS1 Thailand'),
    (fxPrefixIni: 888;	fxPrefixFim: 888; indEsp: 0; fxPaisNome: 'GS1 Singapore'),
    (fxPrefixIni: 890;	fxPrefixFim: 890; indEsp: 0; fxPaisNome: 'GS1 India'),
    (fxPrefixIni: 893;	fxPrefixFim: 893; indEsp: 0; fxPaisNome: 'GS1 Vietnam'),
    (fxPrefixIni: 896;	fxPrefixFim: 896; indEsp: 0; fxPaisNome: 'GS1 Pakistan'),
    (fxPrefixIni: 899;	fxPrefixFim: 899; indEsp: 0; fxPaisNome: 'GS1 Indonesia'),
    (fxPrefixIni: 900;	fxPrefixFim: 919; indEsp: 0; fxPaisNome: 'GS1 Austria'),
    (fxPrefixIni: 930;	fxPrefixFim: 939; indEsp: 0; fxPaisNome: 'GS1 Australia'),
    (fxPrefixIni: 940;	fxPrefixFim: 949; indEsp: 0; fxPaisNome: 'GS1 New Zealand'),
    (fxPrefixIni: 950;	fxPrefixFim: 950; indEsp: 1; fxPaisNome: 'GS1 Global Office'),
    (fxPrefixIni: 951;	fxPrefixFim: 951; indEsp: 1; fxPaisNome: 'Numeracao para EPC Tag Data Standard'),
    (fxPrefixIni: 955;	fxPrefixFim: 955; indEsp: 0; fxPaisNome: 'GS1 Malaysia'),
    (fxPrefixIni: 958;	fxPrefixFim: 958; indEsp: 0; fxPaisNome: 'GS1 Macau'),
    (fxPrefixIni: 960;	fxPrefixFim: 969; indEsp: 1; fxPaisNome: 'Global Office (GTIN-8s)'),
    (fxPrefixIni: 977;	fxPrefixFim: 977; indEsp: 1; fxPaisNome: 'Serial publications (ISSN)'),
    (fxPrefixIni: 978;	fxPrefixFim: 979; indEsp: 1; fxPaisNome: 'Bookland (ISBN)'),
    (fxPrefixIni: 980;	fxPrefixFim: 980; indEsp: 1; fxPaisNome: 'Refund receipts'),
    (fxPrefixIni: 981;	fxPrefixFim: 984; indEsp: 1; fxPaisNome: 'GS1 Coupon identification for common currency areas'),
    (fxPrefixIni: 990;	fxPrefixFim: 999; indEsp: 1; fxPaisNome: 'GS1 Coupon identification')
  );
begin
  ValidarGTIN;
  if NaoEstaVazio(fsMsgErro) then
    Exit;

  CodigoNormalizado := PadLeft(Trim(fsDocto), 14, '0');

  if (StrToInt(Copy(CodigoNormalizado, 1, 6)) = 0) then //gtin8
    sPrefixo := copy(CodigoNormalizado, 7, 3)
  else
    sPrefixo := copy(CodigoNormalizado, 2, 3);

  iPrefixo := StrtoIntDef(sPrefixo, 0);
{$IFNDEF COMPILER23_UP}
  bEncontrado := False;
{$ENDIF}
  for I := Low(ARRAY_PREFIX_GTIN) to High(ARRAY_PREFIX_GTIN) do
  begin
    bEncontrado := InRange(iPrefixo, ARRAY_PREFIX_GTIN[I].fxPrefixIni, ARRAY_PREFIX_GTIN[I].fxPrefixFim);
    if bEncontrado then
      Break;
  end;

  if bEncontrado then
    fsMsgErro := ''
  else
    fsMsgErro := Format(
      'Prefixo "%d" do GTIN "%s" informado inválido', [iPrefixo, Documento]
    );
end;

procedure TACBrValidador.ValidarRenavam;
const
  vSequencia = '3298765432';
var
  iFor, vSoma, vDV: integer;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, 11, '0') ;

  if (Length( fsDocto ) <> 11) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'RENAVAM deve ter 11 dígitos. (Apenas números)' ;
     exit;
  end ;

  vSoma := 0;
  for iFor := 1 to 10 do
     vSoma := vSoma + (StrtoInt(fsDocto[iFor]) * StrToInt(vSequencia[iFor]));

  vDV := (vSoma * 10) mod 11;
  if vDV = 10 then
     vDV := 0;

  fsDigitoCalculado := IntToStr(vDV);

  if (fsDigitoCalculado <> fsDocto[11]) then
  begin
     fsMsgErro := 'RENAVAM inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + '.. Dígito calculado: '+fsDigitoCalculado ;
  end ;
end;

procedure TACBrValidador.ValidarSuframa;
begin
  if ( Length( fsDocto ) < 9 ) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'Código SUFRAMA deve ter no mínimo 9 dígitos. (Apenas números)' ;
     exit
  end ;

  Modulo.CalculoPadrao ;
  Modulo.FormulaDigito := frModulo11 ;
  Modulo.Documento     := copy(fsDocto, 1, 8) ;
  Modulo.Calcular;

  fsDigitoCalculado := IntToStr( Modulo.DigitoFinal ) ;
  if (fsDigitoCalculado <> fsDocto[9]) then
  begin
     fsMsgErro := 'Número SUFRAMA inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + ' Dígito calculado: ' + fsDigitoCalculado ;
  end;
end;

procedure TACBrValidador.ValidarGTIN;
var
  DigOriginal, DigCalculado, Codigo: String;

  function CalcularDV(ACodigoGTIN: String): String;
  var
    Dig, I, DV: Integer;
  begin
    DV := 0;
    Result := '' ;

    // adicionar os zeros a esquerda, se não fizer isso o cálculo não bate
    // limite = tamanho maior codigo (gtin14) - 1 (digito)
    ACodigoGTIN := PadLeft(ACodigoGTIN, 13, '0');

    for I := Length(ACodigoGTIN) downto 1 do
    begin
      Dig := StrToInt(ACodigoGTIN[I]);
      DV  := DV + (Dig * IfThen(odd(I), 3, 1));
    end;

    DV := (Ceil(DV / 10) * 10) - DV ;
    Result := IntToStr(DV);
  end;

begin
  if not StrIsNumber(fsDocto) then
  begin
    fsMsgErro := 'Código GTIN inválido, o código GTIN deve conter somente números.' ;
    Exit;
  end;

  if not(Length(fsDocto) in [8, 12, 13, 14]) then
  begin
    fsMsgErro := 'Código GTIN inválido, o código GTIN deve ter 8, 12, 13 ou 14 caracteres.' ;
    Exit;
  end;

  Codigo       := Copy(fsDocto, 1, Length(fsDocto) - 1);
  DigOriginal  := fsDocto[Length(fsDocto)];
  DigCalculado := CalcularDV(Codigo);

  fsDigitoCalculado := DigCalculado;
  if DigOriginal <> DigCalculado then
  begin
   fsMsgErro := 'Código GTIN inválido.' ;

   if fsExibeDigitoCorreto then
     fsMsgErro := fsMsgErro + ' Dígito calculado: ' + fsDigitoCalculado ;
  end;
end;

procedure TACBrValidador.ValidarCAEPF;
Var DV1, DV2, DVFinal : String ;
begin
  if fsAjustarTamanho then
     fsDocto := PadLeft( fsDocto, 14, '0') ;

  if (Length( fsDocto ) <> 14) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'CAEPF deve ter 14 dígitos. (Apenas números)' ;
     exit
  end ;

  if fsDocto = StringOfChar('0',14) then  // Prevenção contra 00000000000000
  begin
     fsMsgErro := 'CAEPF inválido.' ;
     exit ;
  end ;

  Modulo.CalculoPadrao ;
  Modulo.Documento := copy(fsDocto, 1, 12) ;
  Modulo.Calcular ;
  DV1 := IntToStr( Modulo.DigitoFinal ) ;

  Modulo.Documento := copy(fsDocto, 1, 12)+DV1 ;
  Modulo.Calcular ;
  DV2 := IntToStr( Modulo.DigitoFinal ) ;

  Modulo.fsDigitoFinal := StrToInt(DV1) * 10 + StrToInt(DV2)+12;
  if Modulo.DigitoFinal > 99 then
    Modulo.fsDigitoFinal := Modulo.fsDigitoFinal - 100;

  DVFinal := PadLeft(IntToStr(Modulo.DigitoFinal),2,'0');

  DV1 := DVFinal[1];
  DV2 := DVFinal[2];

  if (DV1 <> fsDocto[13]) or (DV2 <> fsDocto[14]) then
  begin
     fsMsgErro := 'CAEPF inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro +  '.. Digito calculado: '+fsDigitoCalculado ;
  end ;
end;

{ Rotina extraida do site:   www.tcsystems.com.br   }
Procedure TACBrValidador.ValidarCartaoCredito ;
Var
  Valor, Soma, Multiplicador, Tamanho, i : Integer;
begin
  if not StrIsNumber( fsDocto ) then
  begin
     fsMsgErro := 'Cartão deve ter apenas Números' ;
     exit
  end ;

  Multiplicador := 2;
  Soma          := 0;
  Tamanho       := Length(fsDocto) ;

  For i := 1 to Tamanho - 1 do
  begin
    Try
      Valor := StrToInt (Copy (fsDocto, i, 1)) * Multiplicador;
    Except
      Valor := 0;
    End;

    Soma := Soma + (Valor DIV 10) + (Valor mod 10);
    if Multiplicador = 1 Then
       Multiplicador := 2
    else
       Multiplicador := 1;
  end;

  fsDigitoCalculado := IntToStr((10 - (Soma mod 10)) mod 10) ;
  if fsDigitoCalculado <> Copy (fsDocto, Tamanho, 1) Then
  begin
     fsMsgErro := 'Numero do Cartão Inválido.' ;

     if fsExibeDigitoCorreto then
        fsMsgErro := fsMsgErro + '.. Dígito calculado: '+fsDigitoCalculado ;
  end ;
end;

// Rotina para Validação da C.N.H. Modelo 11 digitos (nova CNH)
procedure TACBrValidador.ValidarCNH ;
var
  I, vFator, vSoma, vResto, vBase: integer;
  sResultado : string;
begin
  if (Length( fsDocto ) <> 11) or ( not StrIsNumber( fsDocto ) ) then
  begin
     fsMsgErro := 'C.N.H. deve ter 11 dígitos. (Apenas números)' ;
     exit
  end ;

  if pos(fsDocto,'11111111111.22222222222.33333333333.44444444444.55555555555.'+
         '66666666666.77777777777.88888888888.99999999999.00000000000') > 0 then
  begin
     fsMsgErro := 'C.N.H. não deve conter números repetidos !' ;
     exit ;
  end ;

  vBase := 0;

  vSoma := 0;
  vFator := 9;
  for I := 1 to 9 do
  begin
    vSoma := vSoma + (StrToInt(fsDocto[I]) * vFator);
    dec(vFator) ;
  end;
  vResto := vSoma Mod 11;
  if vResto = 10 then
    vBase := -2;

  if vResto > 9 then
    vResto := 0;
  sResultado := IntToStr(vResto) ;

  vSoma := 0;
  vFator := 1;
  for I := 1 to 9 do
  begin
    vSoma := vSoma + (StrToInt(fsDocto[I]) * vFator);
    Inc(vFator) ;
  end;

  if (vSoma Mod 11) + vBase < 0 then
    vResto := 11 + (vSoma Mod 11) + vBase;

  if (vSoma Mod 11) + vBase >= 0 then
    vResto := (vSoma Mod 11) + vBase;

  if vResto > 9 then
    vResto := 0;

  sResultado := sResultado + IntToStr(vResto) ;

  if Copy(fsDocto,10,2) <> sResultado then
    fsMsgErro := 'C.N.H. Inválida !!' ;
end;

{------------------------------ TACBrCalcDigito ------------------------------}
constructor TACBrCalcDigito.Create;
begin
  fsDocto         := '' ;
  fsDigitoFinal   := 0 ;
  fsSomaDigitos   := 0 ;
  fsMultIni       := 2 ;
  fsMultFim       := 9 ;
  fsFormulaDigito := frModulo11 ;
end;

procedure TACBrCalcDigito.Calcular;
Var
  A,N,Base,Tamanho,ValorCalc : Integer ;
  ValorCalcSTR: String;
begin
  fsSomaDigitos := 0 ;
  fsDigitoFinal := 0 ;
  fsModuloFinal := 0 ;

  if (fsMultAtu >= fsMultIni) and (fsMultAtu <= fsMultFim) then
     Base:= fsMultAtu
  else
     Base:= fsMultIni ;
  Tamanho := Length(fsDocto) ;

  { Calculando a Soma dos digitos de traz para diante, multiplicadas por BASE }
  For A := 1 to Tamanho do
  begin
     N := StrToIntDef( fsDocto[ Tamanho - A + 1 ], 0 ) ;
     ValorCalc := (N * Base);

     if (fsFormulaDigito = frModulo10) and ( ValorCalc > 9) then
     begin
       ValorCalcSTR := IntToStr(ValorCalc);
       ValorCalc    := StrToInt(ValorCalcSTR[1])+StrToInt(ValorCalcSTR[2]);
     end;

     fsSomaDigitos := fsSomaDigitos + ValorCalc ;

     if fsMultIni > fsMultFim then
      begin
        Dec( Base ) ;
        if Base < fsMultFim then
           Base := fsMultIni;
      end
     else
      begin
        Inc( Base ) ;
        if Base > fsMultFim then
           Base := fsMultIni ;
      end ;
  end ;

  case fsFormulaDigito of
    frModulo11 :
      begin
        fsModuloFinal := fsSomaDigitos mod 11 ;

        if fsModuloFinal < 2 then
           fsDigitoFinal := 0
        else
           fsDigitoFinal := 11 - fsModuloFinal;
      end ;

    frModulo10PIS :
      begin
        fsModuloFinal := (fsSomaDigitos mod 11);
        fsDigitoFinal := 11 - fsModuloFinal;

        if (fsDigitoFinal >= 10) then
           fsDigitoFinal := 0;
      end ;

    frModulo10 :
      begin
        fsModuloFinal := (fsSomaDigitos mod 10);
        fsDigitoFinal := 10 - fsModuloFinal;

        if (fsDigitoFinal >= 10) then
           fsDigitoFinal := 0;
      end ;
  end;
end;

procedure TACBrCalcDigito.CalculoPadrao;
begin
  fsMultIni       := 2 ;
  fsMultFim       := 9 ;
  fsMultAtu       := 0;
  fsFormulaDigito := frModulo11 ;
end;

(*{$IfDef HAS_REGEXPR}
const
  cEmailRegex = '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
                '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
                '.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$';

Uses
  {$IfDef HAS_REGEXPR}
   {$IfDef FPC} RegExpr, {$Else} RegularExpressions,{$EndIf}
  {$EndIf}

Implementation

{$IfDef FPC}
procedure TACBrValidador.ValidarEmail;
var
  vRegex: TRegExpr;
begin
  vRegex := TRegExpr.Create;
  try
    vRegex.Expression := cEmailRegex;
    if not vRegex.Exec(Documento) then
      fsMsgErro := 'e-mail inválido!'
    else
      fsMsgErro := '';
  finally
    vRegex.Free;
  end;
end;
{$Else}
procedure TACBrValidador.ValidarEmail;
var
  vRegex: TRegEx;
begin
  if not vRegex.IsMatch(Documento, cEmailRegex) then
    fsMsgErro := 'e-mail inválido!'
  else
    fsMsgErro := '';
end;
{$EndIf}
{$EndIf}
*)
end.


