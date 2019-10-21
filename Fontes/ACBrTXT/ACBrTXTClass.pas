{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

{$I ACBr.inc}

unit ACBrTXTClass;

interface

uses
  SysUtils, Classes, DateUtils, Math, Variants;

type
  EACBrTXTClassErro            = class(Exception) ;

  TErrorEvent = procedure(const MsnError: String) of object;

  { TACBrTXTClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrTXTClass = class
  private
    FLinhasBuffer: Integer;
    FNomeArquivo: String;
    FOnError: TErrorEvent;
    FDelimitador: String;     /// Caracter delimitador de campos
    FTrimString: boolean;     /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      /// Mascara para valores tipo currency

    FConteudo : TStringList;

    procedure AssignError(const MsnError: String);
    procedure SetLinhasBuffer(const AValue: Integer);
    procedure SetNomeArquivo(const AValue: String);
  public
    constructor Create ;
    destructor Destroy ; override ;

    procedure WriteBuffer ;
    procedure SaveToFile ;
    procedure LoadFromFile ;
    procedure Reset ;
    function Add( const AString : String; AddDelimiter : Boolean = True ) : Integer;
    function DFill(Value: Double;
                   Decimal: Integer = 2;
                   Nulo: Boolean = false): String;
    function LFill(const Value: String;
                   Size: Integer = 0;
                   Nulo: Boolean = false;
                   Caracter: Char = '0'): String; overload;
    function LFill(Value: Extended;
                   Size: Integer;
                   Decimal: Integer = 2;
                   Nulo: Boolean = false;
                   Caracter: Char = '0';
                   const Mascara: String = ''): String; overload;
    function LFill(Value: Int64; Size: Integer; Nulo: Boolean = false; Caracter: Char = '0'): String; overload;
    function LFill(Value: TDateTime; const Mask: String = 'ddmmyyyy'; Nulo: Boolean = True): String; overload;
    function RFill(const Value: String;
                   Size: Integer = 0;
                   Caracter: Char = ' '): String;

    function VDFill(Value: Variant;
                    Decimal: Integer = 2): String;
    function VLFill(Value: Variant;
                    Size: Integer;
                    Decimal: Integer = 2;
                    Caracter: Char = '0';
                    const Mascara: String = ''): String;
    ///
    procedure Check(Condicao: Boolean; const Msg: String); overload;
    procedure Check(Condicao: Boolean; const Msg: String; Fmt: array of const); overload;
    ///
    property NomeArquivo : String read FNomeArquivo write SetNomeArquivo ;
    property LinhasBuffer : Integer read FLinhasBuffer write SetLinhasBuffer ;
    property Delimitador: String read FDelimitador write FDelimitador;
    property TrimString: boolean read FTrimString write FTrimString;
    property CurMascara: String read FCurMascara write FCurMascara;
    property OnError: TErrorEvent read FOnError write FOnError;

    property Conteudo : TStringList read FConteudo write FConteudo;
  end;

implementation

Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  ACBrUtil ;

(* TACBrTXTClass *)

constructor TACBrTXTClass.Create;
begin
   FConteudo     := TStringList.Create ;
   FOnError      := Nil;
   FNomeArquivo  := '';
   FDelimitador  := '';
   FTrimString   := False;
   FCurMascara   := '';
   FLinhasBuffer := 0 ; // 0 = Sem tratamento de buffer
end;

destructor TACBrTXTClass.Destroy;
begin
  FConteudo.Free;

  inherited destroy;
end;

procedure TACBrTXTClass.WriteBuffer;
var
  FS : TFileStream ;
begin
  if NomeArquivo = '' then
     raise EACBrTXTClassErro.Create( ACBrStr('"NomeArquivo" não especificado') ) ;

  if (not FileExists( NomeArquivo )) then
     {$IFDEF UNICODE}
      WriteToTXT( NomeArquivo, FConteudo.Text, False, False )
     {$ELSE}
      FConteudo.SaveToFile( NomeArquivo ) // SaveToFile nativo deixa arquivo como UTF-8
     {$ENDIF}
  else
   begin
      FS := TFileStream.Create( NomeArquivo, fmOpenReadWrite or fmShareExclusive );
      try
         FS.Seek(0, soFromEnd);  // vai para EOF
         FConteudo.SaveToStream( FS );
      finally
         FS.Free ;
      end;
   end;

  if (FLinhasBuffer > 0) then
     FConteudo.Clear;
end;

procedure TACBrTXTClass.SaveToFile ;
begin
  WriteBuffer;
end;

procedure TACBrTXTClass.LoadFromFile ;
begin
   if NomeArquivo = '' then
      raise EACBrTXTClassErro.Create( ACBrStr('"Nome do Arquivo" não especificado') ) ;

   FConteudo.LoadFromFile( NomeArquivo );
end;

procedure TACBrTXTClass.Reset;
begin
   FConteudo.Clear;

   if FNomeArquivo <> '' then
      if FileExists( FNomeArquivo ) then
         SysUtils.DeleteFile( FNomeArquivo );
end;

function TACBrTXTClass.Add(const AString: String; AddDelimiter: Boolean
   ): Integer;
Var
  S : String ;
begin
   if TrimString then
     S := Trim( AString )
   else
     S := AString;

   Result := -1 ;
   if S = '' then
     exit ;

   if AddDelimiter then
     S := S + Delimitador;

   Result := FConteudo.Add( S );

   if FLinhasBuffer > 0 then
      if FConteudo.Count >= FLinhasBuffer then
         WriteBuffer ;
end;

procedure TACBrTXTClass.Check(Condicao: Boolean; const Msg: String);
begin
  if not Condicao then AssignError(Msg);
end;

procedure TACBrTXTClass.Check(Condicao: Boolean; const Msg: String; Fmt: array of const);
begin
  Check(Condicao, Format(Msg, Fmt));
end;

function TACBrTXTClass.RFill(const Value: String;
                             Size: Integer = 0;
                             Caracter: Char = ' '): String;
begin
  if (Size > 0) and (Length(Value) > Size) then
     Result := Copy(Value, 1, Size)
  else
     Result := Value + StringOfChar(Caracter, Size - Length(Value));

  if Caracter = '?' then
     Result := FDelimitador + StringReplace(Result, ' ', Caracter, [rfReplaceAll])
  else
     Result := FDelimitador + Result;

  /// Se a propriedade TrimString = true, Result retorna sem espaços em branco
  /// iniciais e finais.
  if FTrimString then
     Result := Trim(Result);
end;

function TACBrTXTClass.LFill(const Value: String;
                             Size: Integer = 0;
                             Nulo: Boolean = false;
                             Caracter: Char = '0'): String;
begin
  if (Nulo) and (Length(Value) = 0) then
  begin
     Result := FDelimitador;
     Exit;
  end;

  if (Size > 0) and (Length(Value) > Size) then
     Result := Copy(Value, 1, Size)
  else
     Result := StringOfChar(Caracter, Size - length(Value)) + Value;

  Result := FDelimitador + Result;

  /// Se a propriedade TrimString = true, Result retorna sem espaços em branco
  /// iniciais e finais.
  if FTrimString then
     Result := Trim(Result);
end;

function TACBrTXTClass.LFill(Value: Extended;
                        Size: Integer;
                        Decimal: Integer = 2;
                        Nulo: Boolean = false;
                        Caracter: Char = '0';
                        const Mascara: String = ''): String;
var
strCurMascara: string;
AStr: String;
begin
  strCurMascara := FCurMascara;
  // Se recebeu uma mascara como parametro substitue a principal
  if Mascara <> '' then
     strCurMascara := Mascara;

  /// Se o parametro Nulo = true e Value = 0, será retornado '|'
  if (Nulo) and (Value = 0) then
  begin
     Result := FDelimitador;
     Exit;
  end;

  if (strCurMascara <> '#') and (strCurMascara <> '') then
     Result := FDelimitador + FormatCurr(strCurMascara, Value)
  else
  begin
     AStr := FormatFloatBr(Value, FloatMask(Decimal, False));
     if Decimal > 0 then
       Delete( AStr, Length(AStr)-Decimal, 1) ;

     if Nulo then
       Result := LFill(StrToInt64(AStr), Size, Nulo, Caracter)
     else
       Result := LFill(AStr, Size, Nulo, Caracter);
  end;
end;

function TACBrTXTClass.DFill(Value: Double;
                        Decimal: Integer = 2;
                        Nulo: Boolean = false): String;
begin
  /// Se o parametro Nulo = true e Value = 0, será retornado '|'
  if (Nulo) and (Value = 0) then
  begin
     Result := FDelimitador;
     Exit;
  end;
  Result := FDelimitador + FormatFloatBr(Value, FloatMask(Decimal, False)); //FormatCurr não permite precisão acima de 4 casas decimais
end;

function TACBrTXTClass.VDFill(Value: Variant;
                        Decimal: Integer = 2): String;
begin
  /// Se o parametro Value é Null ou Undefined será retornado '|'
  if VarIsNull(Value) or VarIsEmpty(Value) then
  begin
     Result := FDelimitador;
     Exit;
  end;
  Result := FDelimitador + FormatFloatBr(Value, FloatMask(Decimal, False)); //FormatCurr não permite precisão acima de 4 casas decimais
end;

function TACBrTXTClass.LFill(Value: Int64; Size: Integer; Nulo: Boolean;
  Caracter: Char): String;
begin
  /// Se o parametro Nulo = true e Value = 0, será retornado '|'
  if (Nulo) and (Value = 0) then
  begin
     Result := FDelimitador;
     Exit;
  end;
  Result := LFill(IntToStr(Value), Size, False, Caracter);
end;

function TACBrTXTClass.LFill(Value: TDateTime; const Mask: String = 'ddmmyyyy'; Nulo: Boolean = True): String;
begin
  /// Se o parametro Value = 0, será retornado '|'
  if (Nulo) and (Value = 0) then
  begin
     Result := FDelimitador;
     Exit;
  end;
  Result := FDelimitador + FormatDateTime(Mask, Value);
end;

procedure TACBrTXTClass.AssignError(const MsnError: String);
begin
  if Assigned(FOnError) then FOnError( ACBrStr(MsnError) );
end;

procedure TACBrTXTClass.SetLinhasBuffer(const AValue: Integer);
begin
   if FLinhasBuffer = AValue then exit;
   FLinhasBuffer := max(AValue,0);   // Sem valores negativos
end;

procedure TACBrTXTClass.SetNomeArquivo(const AValue: String);
begin
   if FNomeArquivo = AValue then exit;
   FNomeArquivo := AValue;
end;

function TACBrTXTClass.VLFill(Value: Variant;
                             Size: Integer;
                             Decimal: Integer;
                             Caracter: Char;
                             const Mascara: String): String;
var
AExt: Extended;
begin
  // Se o parametro Value = Null ou não foi preenchido será retornado '|'
  if VarIsNull(Value) or VarIsEmpty(Value) then
  begin
     Result := FDelimitador;
     Exit;
  end;

  // Checa se é um valor numérico
  if not VarIsNumeric(Value) then
     raise EACBrTXTClassErro.Create( ACBrStr('Parâmetro "Value" não possui um valor numérico.'));

  AExt := Value;

  Result := LFill(AExt, Size, Decimal, False, Caracter, Mascara);
end;

end.
