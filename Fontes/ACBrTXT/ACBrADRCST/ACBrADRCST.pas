{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }
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

unit ACBrADRCST;

interface

uses
  SysUtils,
  Classes,
  ACBrBase,
     {$IFNDEF NOGUI}
      {$IFDEF FPC}
       LResources,
      {$ENDIF}
     {$ENDIF}
  ACBrTXTClass,
  ACBrADRCST_Bloco0_Class,
  ACBrADRCST_Bloco1_Class,
  ACBrADRCST_Bloco9_Class,

  ACBrADRCSTConversao;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  { TACBrADRCST }

  TACBrADRCST = class(TACBrComponent)
  private
    FLayout : TADRCSTLayout;
    FACBrTXT: TACBrTXTClass;
    FArquivo: ansistring;
    FInicializado: boolean;
    FOnError: TErrorEvent;

    FPath: ansistring;
    FDelimitador: ansistring;
    FReplaceDelimitador: Boolean;
    FTrimString: boolean;          // Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: ansistring;       // Mascara para valores tipo currency

    fBloco_0: TBloco_0;
    fBloco_1: TBloco_1;
    fBloco_9: TBloco_9;

    function GetConteudo: TStringList;
    function GetDelimitador: ansistring;
    function GetReplaceDelimitador: Boolean;
    function GetLinhasBuffer: integer;
    function GetTrimString: boolean;
    function GetCurMascara: ansistring;
    procedure InicializaBloco(Bloco: TACBrTXTClass);
    procedure SetArquivo(const Value: ansistring);
    procedure SetDelimitador(const Value: ansistring);
    procedure SetReplaceDelimitador(const Value: Boolean);
    procedure SetLinhasBuffer(const Value: integer);
    procedure SetPath(const Value: ansistring);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: ansistring);

    function GetOnError: TErrorEvent;
    procedure SetOnError(const Value: TErrorEvent);

    function GetLayout: TADRCSTLayout;
    procedure SetLayout(const Value: TADRCSTLayout);

  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;

    /// BLOCO 1
    procedure WriteRegistro1000;
    procedure WriteRegistro1001;

    /// BLOCO 9
    procedure WriteRegistro9000;
    procedure WriteRegistro9999;

  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override;

    procedure SaveFileTXT;

    procedure IniciaGeracao;
    procedure WriteBloco_0;
    procedure WriteBloco_1;
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property Bloco_0: TBloco_0 read fBloco_0 write fBloco_0;
    property Bloco_1: TBloco_1 read fBloco_1 write fBloco_1;
    property Bloco_9: TBloco_9 read fBloco_9 write fBloco_9;
  published

    property Layout : TADRCSTLayout read GetLayout write SetLayout default lyADRCST;
    property Path: ansistring read fPath write SetPath;
    property Arquivo: ansistring read FArquivo write SetArquivo;
    property LinhasBuffer: integer read GetLinhasBuffer write SetLinhasBuffer default 1000;
    property Delimitador: ansistring read GetDelimitador write SetDelimitador;
    property ReplaceDelimitador: Boolean  read GetReplaceDelimitador write SetReplaceDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: ansistring read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

  end;

implementation

Uses
  DateUtils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{$IFNDEF FPC}
 {$R ACBrADRCST.dcr}
{$ENDIF}

{ TACBrADRCST }

constructor TACBrADRCST.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLayout := lyADRCST;

  fACBrTXT := TACBrTXTClass.Create;
  fACBrTXT.LinhasBuffer := 1000;

  fInicializado := False;

  fBloco_0 := TBloco_0.Create;
  fBloco_1 := TBloco_1.Create;
  fBloco_9 := TBloco_9.Create;

  FPath := ExtractFilePath(ParamStr(0));

  Delimitador        := '|'; //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  ReplaceDelimitador := False; //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  CurMascara         := '#0.00'; //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  TrimString         := True; //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
end;

destructor TACBrADRCST.Destroy;
begin
  fACBrTXT.Free;

  fBloco_0.Free;
  fBloco_1.Free;
  fBloco_9.Free;

  inherited;
end;

function TACBrADRCST.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrADRCST.GetCurMascara: ansistring;
begin
  Result := FCurMascara;
end;

function TACBrADRCST.GetDelimitador: ansistring;
begin
  Result := fDelimitador;
end;

function TACBrADRCST.GetLayout: TADRCSTLayout;
begin
  Result := FLayout;
end;

function TACBrADRCST.GetLinhasBuffer: integer;
begin
  Result := fACBrTXT.LinhasBuffer;
end;

function TACBrADRCST.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TACBrADRCST.GetReplaceDelimitador: Boolean;
begin
  Result := FReplaceDelimitador;
end;

function TACBrADRCST.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrADRCST.IniciaGeracao;
begin
  if fInicializado then
    exit;

  if (Trim(fArquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  fACBrTXT.NomeArquivo := FPath + FArquivo;
  {Apaga o Arquivo existente e limpa memória}
  fACBrTXT.Reset;
  InicializaBloco(Bloco_0);
  InicializaBloco(Bloco_1);
  InicializaBloco(Bloco_9);
  fInicializado := True;

  Bloco_1.Registro1000List.Clear;
  Bloco_1.Registro1001List.Clear;
end;

procedure TACBrADRCST.InicializaBloco(Bloco: TACBrTXTClass);
begin
  Bloco.NomeArquivo := FACBrTXT.NomeArquivo;
  Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
  //Bloco.Gravado := False;
  Bloco.Conteudo.Clear;
end;

procedure TACBrADRCST.SaveFileTXT;
begin
  try
    IniciaGeracao;
    WriteBloco_0;
    WriteBloco_1;
    WriteBloco_9;
  finally
    fACBrTXT.Conteudo.Clear;
    fInicializado := False;
  end;
end;

procedure TACBrADRCST.SetArquivo(const Value: ansistring);
var
  aPath: ansistring;
begin
  if fArquivo = Value then
    exit;

  fArquivo := ExtractFileName(Value);
  aPath := ExtractFilePath(Value);

  if aPath <> '' then
    Path := aPath;
end;

procedure TACBrADRCST.SetCurMascara(const Value: ansistring);
begin
  fCurMascara := Value;

  fBloco_0.CurMascara := Value;
  fBloco_1.CurMascara := Value;
  fBloco_9.CurMascara := Value;
end;

procedure TACBrADRCST.SetDelimitador(const Value: ansistring);
begin
  fDelimitador := Value;

  fBloco_0.Delimitador := Value;
  fBloco_1.Delimitador := Value;
  fBloco_9.Delimitador := Value;
end;

procedure TACBrADRCST.SetLayout(const Value: TADRCSTLayout);
begin
  FLayout := Value;
  fBloco_1.Layout := Value;
end;

procedure TACBrADRCST.SetLinhasBuffer(const Value: integer);
begin
  fACBrTXT.LinhasBuffer := Value;
end;

procedure TACBrADRCST.SetOnError(const Value: TErrorEvent);
begin
  fOnError := Value;

  fBloco_0.OnError := Value;
  fBloco_1.OnError := Value;
  fBloco_9.OnError := Value;
end;

procedure TACBrADRCST.SetPath(const Value: ansistring);
begin
  fPath := PathWithDelim(Value);
end;

procedure TACBrADRCST.SetReplaceDelimitador(const Value: Boolean);
begin
  FReplaceDelimitador := Value;

  fBloco_0.ReplaceDelimitador := Value;
  fBloco_1.ReplaceDelimitador := Value;
  fBloco_9.ReplaceDelimitador := Value;
end;

procedure TACBrADRCST.SetTrimString(const Value: boolean);
begin
  fTrimString := Value;

  fBloco_0.TrimString := Value;
  fBloco_1.TrimString := Value;
  fBloco_9.TrimString := Value;
end;

procedure TACBrADRCST.WriteBloco_0;
begin
  //if Bloco_0.Gravado then
    //Exit;

  if not FInicializado then
    raise Exception.Create('Métodos "IniciaGeracao" não foi executado');

  /// BLOCO 0
  if Layout = lyADRCST then
    WriteRegistro0000
  else
    WriteRegistro0001;

  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  //Bloco_0.Gravado := True;
end;

procedure TACBrADRCST.WriteBloco_1;
begin
  if Layout = lyADRCST then
    WriteRegistro1000
  else
    WriteRegistro1001;

  Bloco_1.WriteBuffer;
  Bloco_1.Conteudo.Clear;
end;

procedure TACBrADRCST.WriteBloco_9;
begin
  //if Bloco_9.Gravado then
    //exit;


  /// BLOCO 9
  if Layout = lyADRCST then
    WriteRegistro9000;
  WriteRegistro9999;

  Bloco_9.WriteBuffer;
  Bloco_9.Conteudo.Clear;
  //Bloco_9.Gravado := True;
end;

procedure TACBrADRCST.WriteRegistro0000;
begin
  Bloco_0.WriteRegistro0000;
end;

procedure TACBrADRCST.WriteRegistro0001;
begin
  Bloco_0.WriteRegistro0001;
end;

procedure TACBrADRCST.WriteRegistro9000;
begin
  Bloco_9.WriteRegistro9000;
end;

procedure TACBrADRCST.WriteRegistro9999;
begin
  Bloco_9.WriteRegistro9999(Bloco_1.Registro1999.QTD_LIN);
end;

procedure TACBrADRCST.WriteRegistro1000;
begin
  Bloco_1.WriteRegistro1000;
end;

procedure TACBrADRCST.WriteRegistro1001;
begin
  Bloco_1.WriteRegistro1001;
end;

{$IFNDEF NOGUI}
 {$IFDEF FPC}
initialization
	{$i ACBrADRCST.lrs}
 {$ENDIF}
{$ENDIF}

end.
