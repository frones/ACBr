{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:     Elias César Vieira                          }
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

{******************************************************************************
|* Historico
|*
|* 17/05/2016: Elias César Vieira
|*  - Primeira Versao ACBrMTerClass
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTerClass;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase;

type
  { TACBrMTerComando }

  TACBrMTerComando = class
  private
    FComando: AnsiString;
    FTag: Integer;
    FTimeOut: Integer;
  public
    constructor Create(const AComando: AnsiString; ATimeOut: Integer = 0);

    property Comando: AnsiString read FComando;
    property TimeOut: Integer read FTimeOut write FTimeOut;  // Valores negativos, forçam a Pausa
    property Tag: Integer read FTag write FTag;
  end;

  { TACBrMTerComandos }

  TACBrMTerComandos = class(TObjectList{$IfDef NEXTGEN}<TACBrMTerComando>{$EndIf})
  private
    function GetObject(aIndex: Integer): TACBrMTerComando;
    procedure SetObject(aIndex: Integer; aItem: TACBrMTerComando);
  public
    function New(const AComando: AnsiString; ATimeOut: Integer = 0): Integer;
    function Add(aObj: TACBrMTerComando): Integer;
    procedure Insert(aIndex: Integer; aObj: TACBrMTerComando);

    property Objects[aIndex: Integer]: TACBrMTerComando read GetObject
      write SetObject; default;
  end;

  { Classe generica de MicroTerminal, nao implementa nenhum modelo especifico,
  apenas declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de ACBrMTer, como por exemplo a classe TACBrMTerVT100 }

  { TACBrMTerClass }

  TACBrMTerClass = class
  protected
    fpModeloStr: String;
    fpOwner: TComponent;

    procedure DisparaErroNaoImplementado( const NomeMetodo: String );
    function TimeOut: Integer;

  public
    constructor Create(aOwner: TComponent);

    procedure ComandoBackSpace(Comandos: TACBrMTerComandos); virtual;
    procedure ComandoBeep(Comandos: TACBrMTerComandos; const aTempo: Integer = 0); virtual;
    procedure ComandoBoasVindas(Comandos: TACBrMTerComandos); virtual;
    procedure ComandoDeslocarCursor(Comandos: TACBrMTerComandos; const aValue: Integer); virtual;
    procedure ComandoDeslocarLinha(Comandos: TACBrMTerComandos; aValue: Integer); virtual;
    procedure ComandoEco(Comandos: TACBrMTerComandos; const aValue: AnsiString); virtual;
    procedure ComandoEnviarParaParalela(Comandos: TACBrMTerComandos; const aDados: AnsiString); virtual;
    procedure ComandoEnviarParaSerial(Comandos: TACBrMTerComandos; const aDados: AnsiString; aSerial: Byte = 0); virtual;
    procedure ComandoEnviarTexto(Comandos: TACBrMTerComandos; const aTexto: AnsiString); virtual;
    procedure ComandoOnline(Comandos: TACBrMTerComandos); virtual;
    procedure ComandoPosicionarCursor(Comandos: TACBrMTerComandos; const aLinha, aColuna: Integer); virtual;
    procedure ComandoLimparDisplay(Comandos: TACBrMTerComandos); virtual;
    procedure ComandoLimparLinha(Comandos: TACBrMTerComandos; const aLinha: Integer); virtual;

    function InterpretarResposta(const aRecebido: AnsiString): AnsiString; virtual;
    function LimparConteudoParaEnviarEcho(const aString: AnsiString): AnsiString;
    function ExtrairResposta( var ABuffer: Ansistring; LendoPeso: Boolean = False ): AnsiString; virtual;

    property ModeloStr: String read fpModeloStr;
  end;


implementation

uses
  strutils,
  ACBrMTer, ACBrConsts, ACBrUtil;

{ TACBrMTerComando }

constructor TACBrMTerComando.Create(const AComando: AnsiString;
  ATimeOut: Integer);
begin
  inherited Create;

  FComando := AComando;
  FTimeOut := ATimeOut;
  FTag := 0;
end;


{ TACBrMTerComandos }

function TACBrMTerComandos.GetObject(aIndex: Integer): TACBrMTerComando;
begin
  Result := TACBrMTerComando(inherited Items[aIndex]);
end;

procedure TACBrMTerComandos.SetObject(aIndex: Integer; aItem: TACBrMTerComando);
begin
  inherited Items[aIndex] := aItem;
end;

function TACBrMTerComandos.New(const AComando: AnsiString; ATimeOut: Integer
  ): Integer;
begin
  Result := Add( TACBrMTerComando.Create(AComando, ATimeOut) );
end;

function TACBrMTerComandos.Add(aObj: TACBrMTerComando): Integer;
begin
  Result := inherited Add(aObj);
end;

procedure TACBrMTerComandos.Insert(aIndex: Integer; aObj: TACBrMTerComando);
begin
  inherited Insert(aIndex, aObj);
end;

{ TACBrMTerClass }

procedure TACBrMTerClass.DisparaErroNaoImplementado(const NomeMetodo: String);
begin
  raise Exception.Create(ACBrStr('Metodo: '+NomeMetodo+', não implementada em: '+ModeloStr));
end;

function TACBrMTerClass.TimeOut: Integer;
begin
  Result := TACBrMTer(fpOwner).TimeOut;
end;

constructor TACBrMTerClass.Create(aOwner: TComponent);
begin
  if not (aOwner is TACBrMTer) then
    raise Exception.Create(ACBrStr('Essa Classe deve ser instanciada por TACBrMTer'));

  fpOwner := aOwner;
  fpModeloStr := 'Não Definido';
end;

procedure TACBrMTerClass.ComandoBackSpace(Comandos: TACBrMTerComandos);
begin
  ComandoEnviarTexto(Comandos, BS);
end;

procedure TACBrMTerClass.ComandoBeep(Comandos: TACBrMTerComandos;
  const aTempo: Integer);
begin
  DisparaErroNaoImplementado('ComandoBeep');
end;

procedure TACBrMTerClass.ComandoBoasVindas(Comandos: TACBrMTerComandos);
begin
  { Sobreescrever apenas se necessário }
end;

procedure TACBrMTerClass.ComandoDeslocarCursor(Comandos: TACBrMTerComandos;
  const aValue: Integer);
begin
  DisparaErroNaoImplementado('ComandoDeslocarCursor');
end;

procedure TACBrMTerClass.ComandoDeslocarLinha(Comandos: TACBrMTerComandos;
  aValue: Integer);
begin
  DisparaErroNaoImplementado('ComandoDeslocarCursor');
end;

procedure TACBrMTerClass.ComandoEco(Comandos: TACBrMTerComandos;
  const aValue: AnsiString);
begin
  ComandoEnviarTexto(Comandos, LimparConteudoParaEnviarEcho(aValue));
end;

procedure TACBrMTerClass.ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
  const aDados: AnsiString);
begin
  DisparaErroNaoImplementado('ComandoEnviarParaParalela');
end;

procedure TACBrMTerClass.ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
  const aDados: AnsiString; aSerial: Byte);
begin
  DisparaErroNaoImplementado('ComandoEnviarParaSerial');
end;

procedure TACBrMTerClass.ComandoEnviarTexto(Comandos: TACBrMTerComandos;
  const aTexto: AnsiString);
begin
  DisparaErroNaoImplementado('ComandoEnviarTexto');
end;

procedure TACBrMTerClass.ComandoOnline(Comandos: TACBrMTerComandos);
begin
  {}
end;

procedure TACBrMTerClass.ComandoPosicionarCursor(Comandos: TACBrMTerComandos;
  const aLinha, aColuna: Integer);
begin
  DisparaErroNaoImplementado('ComandoPosicionarCursor');
end;

procedure TACBrMTerClass.ComandoLimparDisplay(Comandos: TACBrMTerComandos);
begin
  DisparaErroNaoImplementado('ComandoLimparDisplay');
end;

procedure TACBrMTerClass.ComandoLimparLinha(Comandos: TACBrMTerComandos;
  const aLinha: Integer);
begin
  ComandoPosicionarCursor(Comandos, aLinha, 0);
  ComandoEnviarTexto(Comandos, Space(TACBrMTer(fpOwner).DisplayColunas) );
  ComandoPosicionarCursor(Comandos, aLinha, 0);
end;

function TACBrMTerClass.InterpretarResposta(const aRecebido: AnsiString): AnsiString;
var
  ALen: Integer;
begin
  Result := '';
  ALen := Length(aRecebido);
  if (ALen < 1) then
    Exit;

  if CharInSet(Char(aRecebido[1]), [ACK,NAK]) then
    Exit;

  Result := aRecebido;

  if (aRecebido[1] = STX) then
  begin
    Result := copy(aRecebido, 2, ALen);
    Dec(ALen);
  end;

  if (ALen > 0) and (aRecebido[ALen] = ETX) then
    Result := copy(aRecebido, 1, ALen-1);
end;

function TACBrMTerClass.LimparConteudoParaEnviarEcho(const aString: AnsiString): AnsiString;
var
  aChar: AnsiChar;
  I: Integer;
begin
  // Função retira os caracteres estranhos da String,
  // Usada para enviar o eco ao Micro Terminal.
  Result := '';

  if (aString = EmptyStr) then
    Exit;

  for I := 1 to Length(aString) do
  begin
    aChar := aString[I];
    { Mantem apenas Letras/Numeros/Pontos/Sinais e BackSpace (#8) }
    if not CharInSet(Char(aChar), [#32..#126,#8]) then
      Continue;

    Result := Result + aChar;
  end;
end;

function TACBrMTerClass.ExtrairResposta(var ABuffer: Ansistring;
  LendoPeso: Boolean): AnsiString;
var
  P, LenTer, LenResp: Integer;
  TerminadorResposta: AnsiString;
begin
  Result := '';

  if LendoPeso then
    TerminadorResposta := TACBrMTer(fpOwner).TerminadorBalancaAsc
  else
    TerminadorResposta := TACBrMTer(fpOwner).TerminadorAsc;

  LenTer := Length(TerminadorResposta);
  if (LenTer > 0) then     // Com Terminador, pegue inicio do Buffer até o Terminador
  begin
    P := pos(TerminadorResposta, ABuffer);
    if (P > 0) then
      Result := copy(ABuffer, 1, P+(LenTer-1));;
  end
  else
     Result := ABuffer;    // Sem Terminador, Pegue tudo disponível...

  LenResp := Length(Result);
  if (LenResp > 0) then
    Delete(ABuffer, 1, LenResp);
end;

end.

