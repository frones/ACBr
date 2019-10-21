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
|* 21/01/2013: Juliana Tamizou
|*  - Adaptação para o Ato Cotepe 35/05 DF
*******************************************************************************}

{$I ACBr.inc}

unit ACBrLFD;

interface

uses
  SysUtils, Math, Classes, ACBrBase,
{$IFNDEF Framework}
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
{$ENDIF}
  DateUtils, ACBrLFD3505 ,ACBrTXTClass, ACBrLFDBlocos, 
  ACBrLFDBloco_0_Class,
  ACBrLFDBloco_A_Class, ACBrLFDBloco_B_Class, ACBrLFDBloco_C_Class, ACBrLFDBloco_D_Class,
  ACBrLFDBloco_E_Class, ACBrLFDBloco_H_Class, ACBrLFDBloco_I_Class, ACBrLFDBloco_J_Class,
  ACBrLFDBloco_K_Class, ACBrLFDBloco_L_Class, ACBrLFDBloco_Z_Class, ACBrLFDBloco_8_Class,
  ACBrLFDBloco_9_Class, ACBrLFDBloco_0_Events,
  ACBrLFDBloco_A_Events, ACBrLFDBloco_B_Events, ACBrLFDBloco_C_Events, ACBrLFDBloco_D_Events,
  ACBrLFDBloco_E_Events, ACBrLFDBloco_H_Events, ACBrLFDBloco_I_Events, ACBrLFDBloco_J_Events,
  ACBrLFDBloco_K_Events, ACBrLFDBloco_L_Events, ACBrLFDBloco_Z_Events, ACBrLFDBloco_8_Events,
  ACBrLFDBloco_9_Events;

type

  { TACBrLFD }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
  TACBrLFD = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: ansistring;
    FInicializado : boolean;
    FOnError: TErrorEvent;

    FEventsBloco_0: TEventsBloco_0;
    FEventsBloco_A: TEventsBloco_A;
    FEventsBloco_B: TEventsBloco_B;
    FEventsBloco_C: TEventsBloco_C;
    FEventsBloco_D: TEventsBloco_D;
    FEventsBloco_E: TEventsBloco_E;
    FEventsBloco_H: TEventsBloco_H;
    FEventsBloco_I: TEventsBloco_I;
    FEventsBloco_J: TEventsBloco_J;
    FEventsBloco_K: TEventsBloco_K;
    FEventsBloco_L: TEventsBloco_L;
    FEventsBloco_Z: TEventsBloco_Z;
    FEventsBloco_8: TEventsBloco_8;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: ansistring;            /// Path do arquivo a ser gerado
    FDelimitador: ansistring;     /// Caracter delimitador de campos
    FTrimString: boolean;
    /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: ansistring;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_A: TBloco_A;
    FBloco_B: TBloco_B;
    FBloco_C: TBloco_C;
    FBloco_D: TBloco_D;
    FBloco_E: TBloco_E;
    FBloco_H: TBloco_H;
    FBloco_I: TBloco_I;
    FBloco_J: TBloco_J;
    FBloco_K: TBloco_K;
    FBloco_L: TBloco_L;
    FBloco_Z: TBloco_Z;
    FBloco_8: TBloco_8;
    FBloco_9: TBloco_9;

    function GetConteudo: TStringList;
    function GetDelimitador: ansistring;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: ansistring;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrLFD3505);
    procedure SetArquivo(const AValue: ansistring);
    procedure SetDelimitador(const Value: ansistring);
    procedure SetLinhasBuffer(const AValue: Integer);
    procedure SetPath(const AValue: ansistring);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: ansistring);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure LimpaRegistros;
  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;
    /// BLOCO A
    procedure WriteRegistroA001;
    procedure WriteRegistroA990;
    /// BLOCO B
    procedure WriteRegistroB001;
    procedure WriteRegistroB990;
    /// BLOCO C
    procedure WriteRegistroC001;
    procedure WriteRegistroC990;
    /// BLOCO D
    procedure WriteRegistroD001;
    procedure WriteRegistroD990;
    /// BLOCO E
    procedure WriteRegistroE001;
    procedure WriteRegistroE990;
    /// BLOCO H
    procedure WriteRegistroH001;
    procedure WriteRegistroH990;
    /// BLOCO I
    procedure WriteRegistroI001;
    procedure WriteRegistroI990;
    /// BLOCO J
    procedure WriteRegistroJ001;
    procedure WriteRegistroJ990;
    /// BLOCO K
    procedure WriteRegistroK001;
    procedure WriteRegistroK990;
    /// BLOCO L
    procedure WriteRegistroL001;
    procedure WriteRegistroL990;
    /// BLOCO Z
    procedure WriteRegistroZ001;
    procedure WriteRegistroZ990;
    /// BLOCO 8
    procedure WriteRegistro8001;
    procedure WriteRegistro8990;
    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy

    procedure SaveFileTXT;

    procedure IniciaGeracao;
    procedure WriteBloco_0;
    procedure WriteBloco_A;
    procedure WriteBloco_B;
    procedure WriteBloco_C( FechaBloco: Boolean );
    procedure WriteBloco_D;
    procedure WriteBloco_E;
    procedure WriteBloco_H;
    procedure WriteBloco_I;
    procedure WriteBloco_J;
    procedure WriteBloco_K;
    procedure WriteBloco_L;
    procedure WriteBloco_Z;
    procedure WriteBloco_8;
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_A: TBloco_A read FBloco_A write FBloco_A;
    property Bloco_B: TBloco_B read FBloco_B write FBloco_B;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_D: TBloco_D read FBloco_D write FBloco_D;
    property Bloco_E: TBloco_E read FBloco_E write FBloco_E;
    property Bloco_H: TBloco_H read FBloco_H write FBloco_H;
    property Bloco_I: TBloco_I read FBloco_I write FBloco_I;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
    property Bloco_L: TBloco_L read FBloco_L write FBloco_L;
    property Bloco_Z: TBloco_Z read FBloco_Z write FBloco_Z;
    property Bloco_8: TBloco_8 read FBloco_8 write FBloco_8;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
  published
    property Path: ansistring read FPath write SetPath;
    property Arquivo: ansistring read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer
      default 1000 ;

    ///
    property Delimitador: ansistring read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: ansistring read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

    property EventsBloco_0: TEventsBloco_0 read FEventsBloco_0;
    property EventsBloco_C: TEventsBloco_C read FEventsBloco_C;
    property EventsBloco_D: TEventsBloco_D read FEventsBloco_D;
    property EventsBloco_E: TEventsBloco_E read FEventsBloco_E;
    property EventsBloco_H: TEventsBloco_H read FEventsBloco_H;
    property EventsBloco_I: TEventsBloco_I read FEventsBloco_I;
    property EventsBloco_J: TEventsBloco_J read FEventsBloco_J;
    property EventsBloco_K: TEventsBloco_K read FEventsBloco_K;
    property EventsBloco_L: TEventsBloco_L read FEventsBloco_L;
    property EventsBloco_Z: TEventsBloco_Z read FEventsBloco_Z;
    property EventsBloco_8: TEventsBloco_8 read FEventsBloco_8;

  end;

procedure Register;

implementation

uses ACBrUtil;

{$IFNDEF FPC}
 {$R ACBrLFD.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrLFD]);
end;

(* TACBrLFD *)

constructor TACBrLFD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_A := TBloco_A.Create;
  FBloco_B := TBloco_B.Create;
  FBloco_C := TBloco_C.Create;
  FBloco_D := TBloco_D.Create;
  FBloco_E := TBloco_E.Create;
  FBloco_H := TBloco_H.Create;
  FBloco_I := TBloco_I.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_L := TBloco_L.Create;
  FBloco_Z := TBloco_Z.Create;
  FBloco_8 := TBloco_8.Create;
  FBloco_9 := TBloco_9.Create;

  FPath := ExtractFilePath(ParamStr(0));
  FDelimitador := '|';
  FCurMascara := '#0.00';
  FTrimString := True;

  // Seta os valores defaults para todos os cdaBlocos
  SetDelimitador(FDelimitador);
  SetCurMascara(FCurMascara);
  SetTrimString(FTrimString);

  FEventsBloco_0 := TEventsBloco_0.Create(Self);
  FEventsBloco_0.Name := 'EventsBloco_0';
  FEventsBloco_0.SetSubComponent(True);

  FEventsBloco_C := TEventsBloco_C.Create(Self);
  FEventsBloco_C.Name := 'EventsBloco_C';
  FEventsBloco_C.SetSubComponent(True);

  FEventsBloco_D := TEventsBloco_D.Create(Self);
  FEventsBloco_D.Name := 'EventsBloco_D';
  FEventsBloco_D.SetSubComponent(True);

  FEventsBloco_E := TEventsBloco_E.Create(Self);
  FEventsBloco_E.Name := 'EventsBloco_E';
  FEventsBloco_E.SetSubComponent(True);

  FEventsBloco_H := TEventsBloco_H.Create(Self);
  FEventsBloco_H.Name := 'EventsBloco_H';
  FEventsBloco_H.SetSubComponent(True);

  FEventsBloco_I := TEventsBloco_I.Create(Self);
  FEventsBloco_I.Name := 'EventsBloco_I';
  FEventsBloco_I.SetSubComponent(True);

  FEventsBloco_J := TEventsBloco_J.Create(Self);
  FEventsBloco_J.Name := 'EventsBloco_J';
  FEventsBloco_J.SetSubComponent(True);

  FEventsBloco_K := TEventsBloco_K.Create(Self);
  FEventsBloco_K.Name := 'EventsBloco_K';
  FEventsBloco_K.SetSubComponent(True);

  FEventsBloco_L := TEventsBloco_L.Create(Self);
  FEventsBloco_L.Name := 'EventsBloco_L';
  FEventsBloco_L.SetSubComponent(True);

  FEventsBloco_Z := TEventsBloco_Z.Create(Self);
  FEventsBloco_Z.Name := 'EventsBloco_Z';
  FEventsBloco_Z.SetSubComponent(True);

  FEventsBloco_8 := TEventsBloco_8.Create(Self);
  FEventsBloco_8.Name := 'EventsBloco_8';
  FEventsBloco_8.SetSubComponent(True);
end;

destructor TACBrLFD.Destroy;
begin
  FACBrTXT.Free;

  FEventsBloco_0.Free;
  FEventsBloco_C.Free;
  FEventsBloco_D.Free;
  FEventsBloco_E.Free;
  FEventsBloco_H.Free;
  FEventsBloco_I.Free;
  FEventsBloco_J.Free;
  FEventsBloco_K.Free;
  FEventsBloco_L.Free;
  FEventsBloco_Z.Free;
  FEventsBloco_8.Free;

  FBloco_0.Free;
  FBloco_A.Free;
  FBloco_B.Free;
  FBloco_C.Free;
  FBloco_D.Free;
  FBloco_E.Free;
  FBloco_H.Free;
  FBloco_I.Free;
  FBloco_J.Free;
  FBloco_K.Free;
  FBloco_L.Free;
  FBloco_Z.Free;
  FBloco_8.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrLFD.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_A.LimpaRegistros;
  FBloco_B.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_D.LimpaRegistros;
  FBloco_E.LimpaRegistros;
  FBloco_H.LimpaRegistros;
  FBloco_I.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_L.LimpaRegistros;
  FBloco_Z.LimpaRegistros;
  FBloco_8.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrLFD.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrLFD.GetDelimitador: ansistring;
begin
   Result := FDelimitador;
end;

function TACBrLFD.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrLFD.SetDelimitador(const Value: ansistring);
begin
  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_A.Delimitador := Value;
  FBloco_B.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_D.Delimitador := Value;
  FBloco_E.Delimitador := Value;
  FBloco_H.Delimitador := Value;
  FBloco_I.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_L.Delimitador := Value;
  FBloco_Z.Delimitador := Value;
  FBloco_8.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

procedure TACBrLFD.SetLinhasBuffer(const AValue: Integer);
begin
   FACBrTXT.LinhasBuffer := AValue ;
end;

procedure TACBrLFD.SetPath(const AValue: ansistring);
begin
  FPath := PathWithDelim( AValue );
end;

function TACBrLFD.GetCurMascara: ansistring;
begin
  Result := FCurMascara;
end;

procedure TACBrLFD.SetCurMascara(const Value: ansistring);
begin
  FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_A.CurMascara := Value;
  FBloco_B.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_D.CurMascara := Value;
  FBloco_E.CurMascara := Value;
  FBloco_H.CurMascara := Value;
  FBloco_I.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_L.CurMascara := Value;
  FBloco_Z.CurMascara := Value;
  FBloco_8.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrLFD.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrLFD.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_A.TrimString := Value;
  FBloco_B.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_D.TrimString := Value;
  FBloco_E.TrimString := Value;
  FBloco_H.TrimString := Value;
  FBloco_I.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_L.TrimString := Value;
  FBloco_Z.TrimString := Value;
  FBloco_8.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrLFD.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

procedure TACBrLFD.InicializaBloco( Bloco: TACBrLFD3505 ) ;
begin
     Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
     Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
     Bloco.Gravado      := False ;
     Bloco.Conteudo.Clear;

end;

procedure TACBrLFD.IniciaGeracao;
begin
  if FInicializado then exit ;

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_A ) ;
  InicializaBloco( Bloco_B ) ;
  InicializaBloco( Bloco_C ) ;
  InicializaBloco( Bloco_D ) ;

  InicializaBloco( Bloco_E ) ;
  InicializaBloco( Bloco_H ) ;
  InicializaBloco( Bloco_I ) ;
  InicializaBloco( Bloco_J ) ;
  InicializaBloco( Bloco_K ) ;
  InicializaBloco( Bloco_L ) ;
  InicializaBloco( Bloco_Z ) ;
  InicializaBloco( Bloco_8 ) ;
  InicializaBloco( Bloco_9 ) ;

  ///
  FACBrTXT.Check(FDT_INI > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'inicial das informações contidas no arquivo!');
  FACBrTXT.Check(FDT_FIN > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'final das informações contidas no arquivo!');
  FACBrTXT.Check(DayOf(FDT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve '
    + 'corresponder ao primeiro dia do mês informado!');
  FACBrTXT.Check(FDT_FIN >= FDT_INI, 'CHECAGEM INICIAL: A data final deve se '
    + 'maior que a data inicial!');
  FACBrTXT.Check(FDT_FIN <= Date, 'CHECAGEM INICIAL: A data final "%s" '
    + 'não pode ser superior a data atual "%s"!',
    [DateToStr(FDT_FIN), DateToStr(Date)]);
  FACBrTXT.Check(DateOf(EndOfTheMonth(FDT_FIN)) = DateOf(FDT_FIN),
    'CHECAGEM ' + 'INICIAL: A data final deve corresponder ao último dia do mês '
    + 'informado!');

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_A.RegistroA990.QTD_LIN_A := 0;
  Bloco_B.RegistroB990.QTD_LIN_B := 0;
  Bloco_C.RegistroC990.QTD_LIN_C := 0;
  Bloco_D.RegistroD990.QTD_LIN_D := 0;
  Bloco_E.RegistroE990.QTD_LIN_E := 0;
  Bloco_H.RegistroH990.QTD_LIN_H := 0;
  Bloco_I.RegistroI990.QTD_LIN_I := 0;
  Bloco_J.RegistroJ990.QTD_LIN_J := 0;
  Bloco_K.RegistroK990.QTD_LIN_K := 0;
  Bloco_L.RegistroL990.QTD_LIN_L := 0;
  Bloco_Z.RegistroZ990.QTD_LIN_Z := 0;
  Bloco_8.Registro8990.QTD_LIN_8 := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;

  FInicializado := True ;
end;

procedure TACBrLFD.SetArquivo(const AValue: ansistring);
var
  APath : AnsiString;
begin
  if FArquivo = AValue then
     exit;

  FArquivo := ExtractFileName( AValue );
  APath    := ExtractFilePath( AValue );

  if APath <> '' then
     Path := APath;
end;

procedure TACBrLFD.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_A.DT_INI := Value;
  FBloco_B.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_D.DT_INI := Value;
  FBloco_E.DT_INI := Value;
  FBloco_H.DT_INI := Value;
  FBloco_I.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_K.DT_INI := Value;
  FBloco_L.DT_INI := Value;
  FBloco_Z.DT_INI := Value;
  FBloco_8.DT_INI := Value;
  FBloco_9.DT_INI := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_INI := Value;
    {FBloco_E.RegistroE100.DT_INI := Value;}
  end;
end;

function TACBrLFD.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

procedure TACBrLFD.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_A.DT_FIN := Value;
  FBloco_B.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_D.DT_FIN := Value;
  FBloco_E.DT_FIN := Value;
  FBloco_H.DT_FIN := Value;
  FBloco_I.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;
  FBloco_L.DT_FIN := Value;
  FBloco_Z.DT_FIN := Value;
  FBloco_8.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_FIN := Value;
    {FBloco_E.RegistroE100.DT_FIN := Value;}
  end;
end;

function TACBrLFD.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrLFD.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_A.OnError := Value;
  FBloco_B.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_D.OnError := Value;
  FBloco_E.OnError := Value;
  FBloco_H.OnError := Value;
  FBloco_I.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_L.OnError := Value;
  FBloco_Z.OnError := Value;
  FBloco_8.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrLFD.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;

    WriteBloco_A;
    WriteBloco_B;
    WriteBloco_C( True );    // True = Fecha o Bloco
    WriteBloco_D;
    WriteBloco_E;
    WriteBloco_H;
    //WriteBloco_I;
    //WriteBloco_J;
    //WriteBloco_K;
   //WriteBloco_L;
   //WriteBloco_Z;

    WriteBloco_8;
    WriteBloco_9;
  finally
    /// Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;

    FInicializado := False ;
  end;
end;

procedure TACBrLFD.WriteBloco_0;
begin
  if Bloco_0.Gravado then Exit;

  if not FInicializado then
     raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  /// BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;
  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_A;
begin
  if Bloco_A.Gravado then Exit;

  if not Bloco_0.Gravado then
    WriteBloco_0;

  /// BLOCO A
  WriteRegistroA001;
  WriteRegistroA990;
  Bloco_A.WriteBuffer;
  Bloco_A.Conteudo.Clear;
  Bloco_A.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_B;
begin
  if Bloco_B.Gravado then Exit;

  if not Bloco_A.Gravado then
    WriteBloco_A;

  /// BLOCO B
  WriteRegistroB001;
  WriteRegistroB990;
  Bloco_B.WriteBuffer;
  Bloco_B.Conteudo.Clear;
  Bloco_B.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_C( FechaBloco : Boolean );
begin
  if Bloco_C.Gravado then Exit;

  if not Bloco_B.Gravado then
    WriteBloco_B;

  /// BLOCO C
  WriteRegistroC001;

  if Bloco_C.RegistroC001.IND_MOV = imlSemDados then
    FechaBloco := True;

  if FechaBloco then
    WriteRegistroC990;

  Bloco_C.WriteBuffer;
  Bloco_C.Conteudo.Clear;
  Bloco_C.Gravado := FechaBloco;
end;

procedure TACBrLFD.WriteBloco_D;
begin
  if Bloco_D.Gravado then Exit;

  if not Bloco_C.Gravado then
    WriteBloco_C(True);

  /// BLOCO D
  WriteRegistroD001;
  WriteRegistroD990;
  Bloco_D.WriteBuffer;
  Bloco_D.Conteudo.Clear;
  Bloco_D.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_E;
begin
  if Bloco_E.Gravado then Exit;

  if not Bloco_D.Gravado then
    WriteBloco_D;

  /// BLOCO E
  WriteRegistroE001;
  WriteRegistroE990;
  Bloco_E.WriteBuffer;
  Bloco_E.Conteudo.Clear;
  Bloco_E.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_I;
begin
  if Bloco_I.Gravado then Exit;

  if not Bloco_I.Gravado then
    WriteBloco_H;

  /// BLOCO F
  WriteRegistroI001;
  WriteRegistroI990;
  Bloco_I.WriteBuffer;
  Bloco_I.Conteudo.Clear;
  Bloco_I.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_J;
begin
  if Bloco_J.Gravado then Exit;

  if not Bloco_I.Gravado then
    WriteBloco_I;

  /// BLOCO J
  WriteRegistroJ001;
  WriteRegistroJ990;
  Bloco_J.WriteBuffer;
  Bloco_J.Conteudo.Clear;
  Bloco_J.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_K;
begin
  if Bloco_K.Gravado then Exit;

  if not Bloco_J.Gravado then
     WriteBloco_J;

  /// BLOCO K
  WriteRegistroK001;
  WriteRegistroK990;
  Bloco_K.WriteBuffer;
  Bloco_K.Conteudo.Clear;
  Bloco_K.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_L;
begin
  if Bloco_L.Gravado then Exit;

  if not Bloco_K.Gravado then
     WriteBloco_K;

  /// BLOCO L
  WriteRegistroL001;
  WriteRegistroL990;
  Bloco_L.WriteBuffer;
  Bloco_L.Conteudo.Clear;
  Bloco_L.Gravado := True ;
end;

procedure TACBrLFD.WriteBloco_H;
begin
  if Bloco_H.Gravado then Exit;

  {if not Bloco_G.Gravado then
    WriteBloco_G;}

  /// BLOCO H
  WriteRegistroH001;
  WriteRegistroH990;
  Bloco_H.WriteBuffer;
  Bloco_H.Conteudo.Clear;
  Bloco_H.Gravado := True;
end;

procedure TACBrLFD.WriteBloco_Z;
begin
  if Bloco_Z.Gravado then Exit;

  if not Bloco_H.Gravado then
    WriteBloco_H;

  /// BLOCO Z
  WriteRegistroZ001;
  WriteRegistroZ990;
  Bloco_Z.WriteBuffer;
  Bloco_Z.Conteudo.Clear;
  Bloco_Z.Gravado := True;
end;

procedure TACBrLFD.WriteBloco_8;
begin
  if Bloco_8.Gravado then Exit;

  {if not Bloco_Z.Gravado then
    WriteBloco_Z;}

  /// BLOCO 8
  WriteRegistro8001;
  WriteRegistro8990;
  Bloco_8.WriteBuffer;
  Bloco_8.Conteudo.Clear;
  Bloco_8.Gravado := True;
end;

procedure TACBrLFD.WriteBloco_9;
begin
   if Bloco_9.Gravado then exit ;

   if not Bloco_8.Gravado then
      WriteBloco_8;

   /// BLOCO 9
   WriteRegistro9001;
   WriteRegistro9900;
   WriteRegistro9990;
   WriteRegistro9999;

   Bloco_9.WriteBuffer;
   Bloco_9.Conteudo.Clear;
   Bloco_9.Gravado := True ;
end;

procedure TACBrLFD.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrLFD.WriteRegistro0001;
begin
   Bloco_0.WriteRegistro0001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := '0001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_0.Registro0001.IND_MOV = imlComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_0.Registro0005Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0005';
               QTD_REG_BLC := Bloco_0.Registro0005Count;
            end;
         end;
         with New do
         begin
            REG_BLC := '0100';
            QTD_REG_BLC := 1;
         end;
         with New do
         begin
            REG_BLC := '0125';
            QTD_REG_BLC := 1;
         end;
         if Bloco_0.Registro0150Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0150';
               QTD_REG_BLC := Bloco_0.Registro0150Count;
            end;
         end;
         if Bloco_0.Registro0175Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0175';
               QTD_REG_BLC := Bloco_0.Registro0175Count;
            end;
         end;
         if Bloco_0.Registro0200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0200';
               QTD_REG_BLC := Bloco_0.Registro0200Count;
            end;
         end;
         if Bloco_0.Registro0205Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0205';
               QTD_REG_BLC := Bloco_0.Registro0205Count;
            end;
         end;
         /// Exigência do Art. 3º do AC 09/08
         ///
         /// Prorrogado para 01/01/2011 conforme Guia Prático da EFD 2.01
         if Bloco_0.Registro0400Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0400';
               QTD_REG_BLC := Bloco_0.Registro0400Count;
            end;
         end;
         if Bloco_0.Registro0450Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0450';
               QTD_REG_BLC := Bloco_0.Registro0450Count;
            end;
         end;
         if Bloco_0.Registro0460Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0460';
               QTD_REG_BLC := Bloco_0.Registro0460Count;
            end;
         end;
         if Bloco_0.Registro0465Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0465';
               QTD_REG_BLC := Bloco_0.Registro0465Count;
            end;
         end;

      end;
   end;
end;

procedure TACBrLFD.WriteRegistro0990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0990;
end;

procedure TACBrLFD.WriteRegistroA001;
begin
  Bloco_A.WriteRegistroA001;

  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
        REG_BLC := 'A001';
        QTD_REG_BLC := 1;
     end;
  end;

  if Bloco_A.RegistroA001.IND_MOV = imlComDados then
    with Bloco_9.Registro9900 do
    begin
       if Bloco_A.RegistroA020Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A020';
             QTD_REG_BLC := Bloco_A.RegistroA020Count;
          end;
       end;

       if Bloco_A.RegistroA025Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A025';
             QTD_REG_BLC := Bloco_A.RegistroA025Count;
          end;
       end;

       if Bloco_A.RegistroA035Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A035';
             QTD_REG_BLC := Bloco_A.RegistroA035Count;
          end;
       end;

       if Bloco_A.RegistroA040Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A040';
             QTD_REG_BLC := Bloco_A.RegistroA040Count;
          end;
       end;

       if Bloco_A.RegistroA045Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A045';
             QTD_REG_BLC := Bloco_A.RegistroA045Count;
          end;
       end;

       if Bloco_A.RegistroA050Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A050';
             QTD_REG_BLC := Bloco_A.RegistroA050Count;
          end;
       end;

       if Bloco_A.RegistroA055Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A055';
             QTD_REG_BLC := Bloco_A.RegistroA055Count;
          end;
       end;

       if Bloco_A.RegistroA200Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A200';
             QTD_REG_BLC := Bloco_A.RegistroA200Count;
          end;
       end;

       if Bloco_A.RegistroA300Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A300';
             QTD_REG_BLC := Bloco_A.RegistroA300Count;
          end;
       end;

       if Bloco_A.RegistroA310Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A310';
             QTD_REG_BLC := Bloco_A.RegistroA310Count;
          end;
       end;

       if Bloco_A.RegistroA320Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A320';
             QTD_REG_BLC := Bloco_A.RegistroA320Count;
          end;
       end;

       if Bloco_A.RegistroA330Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A330';
             QTD_REG_BLC := Bloco_A.RegistroA330Count;
          end;
       end;

       if Bloco_A.RegistroA350Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A350';
             QTD_REG_BLC := Bloco_A.RegistroA350Count;
          end;
       end;

       if Bloco_A.RegistroA355Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A355';
             QTD_REG_BLC := Bloco_A.RegistroA355Count;
          end;
       end;

       if Bloco_A.RegistroA360Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A360';
             QTD_REG_BLC := Bloco_A.RegistroA360Count;
          end;
       end;

       if Bloco_A.RegistroA365Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A365';
             QTD_REG_BLC := Bloco_A.RegistroA365Count;
          end;
       end;

       if Bloco_A.RegistroA370Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A370';
             QTD_REG_BLC := Bloco_A.RegistroA370Count;
          end;
       end;

       if Bloco_A.RegistroA380Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'A380';
             QTD_REG_BLC := Bloco_A.RegistroA380Count;
          end;
       end;
    end;
end;

procedure TACBrLFD.WriteRegistroA990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'A990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_A.WriteRegistroA990;
end;

procedure TACBrLFD.WriteRegistroB001;
begin
  Bloco_B.WriteRegistroB001;

  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'B001';
     QTD_REG_BLC:= 1;
  end;

  if Bloco_B.RegistroB001.IND_MOV = imlComDados then
    with Bloco_9.Registro9900 do
    begin
       if Bloco_B.RegistroB020Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B020';
             QTD_REG_BLC := Bloco_B.RegistroB020Count;
          end;
       end;

       if Bloco_B.RegistroB025Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B025';
             QTD_REG_BLC := Bloco_B.RegistroB025Count;
          end;
       end;

       if Bloco_B.RegistroB030Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B030';
             QTD_REG_BLC := Bloco_B.RegistroB030Count;
          end;
       end;

       if Bloco_B.RegistroB035Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B035';
             QTD_REG_BLC := Bloco_B.RegistroB035Count;
          end;
       end;

       if Bloco_B.RegistroB040Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B040';
             QTD_REG_BLC := Bloco_B.RegistroB040Count;
          end;
       end;

       if Bloco_B.RegistroB045Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B045';
             QTD_REG_BLC := Bloco_B.RegistroB045Count;
          end;
       end;

       if Bloco_B.RegistroB050Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B050';
             QTD_REG_BLC := Bloco_B.RegistroB050Count;
          end;
       end;

       if Bloco_B.RegistroB055Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B055';
             QTD_REG_BLC := Bloco_B.RegistroB055Count;
          end;
       end;

       if Bloco_B.RegistroB350Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B350';
             QTD_REG_BLC := Bloco_B.RegistroB350Count;
          end;
       end;

       if Bloco_B.RegistroB400Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B400';
             QTD_REG_BLC := Bloco_B.RegistroB400Count;
          end;
       end;

       if Bloco_B.RegistroB410Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B410';
             QTD_REG_BLC := Bloco_B.RegistroB410Count;
          end;
       end;

       if Bloco_B.RegistroB420Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B420';
             QTD_REG_BLC := Bloco_B.RegistroB420Count;
          end;
       end;

       if Bloco_B.RegistroB430Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B430';
             QTD_REG_BLC := Bloco_B.RegistroB430Count;
          end;
       end;

       if Bloco_B.RegistroB440Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B440';
             QTD_REG_BLC := Bloco_B.RegistroB440Count;
          end;
       end;

       if Bloco_B.RegistroB450Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B450';
             QTD_REG_BLC := Bloco_B.RegistroB450Count;
          end;
       end;

       if Bloco_B.RegistroB460Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B460';
             QTD_REG_BLC := Bloco_B.RegistroB460Count;
          end;
       end;

       if Bloco_B.RegistroB465Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B465';
             QTD_REG_BLC := Bloco_B.RegistroB465Count;
          end;
       end;

       if Bloco_B.RegistroB470Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B470';
             QTD_REG_BLC := Bloco_B.RegistroB470Count;
          end;
       end;

       if Bloco_B.RegistroB475Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B475';
             QTD_REG_BLC := Bloco_B.RegistroB475Count;
          end;
       end;

       if Bloco_B.RegistroB480Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B480';
             QTD_REG_BLC := Bloco_B.RegistroB480Count;
          end;
       end;

       if Bloco_B.RegistroB490Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B490';
             QTD_REG_BLC := Bloco_B.RegistroB490Count;
          end;
       end;

       if Bloco_B.RegistroB500Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B500';
             QTD_REG_BLC := Bloco_B.RegistroB500Count;
          end;
       end;

       if Bloco_B.RegistroB510Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B510';
             QTD_REG_BLC := Bloco_B.RegistroB510Count;
          end;
       end;

       if Bloco_B.RegistroB600Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B600';
             QTD_REG_BLC := Bloco_B.RegistroB600Count;
          end;
       end;

       if Bloco_B.RegistroB700Count > 0 then
       begin
          with New do
          begin
             REG_BLC := 'B700';
             QTD_REG_BLC := Bloco_B.RegistroB700Count;
          end;
       end;
    end;
end;

procedure TACBrLFD.WriteRegistroB990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'B990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_B.WriteRegistroB990;
end;

procedure TACBrLFD.WriteRegistroC001;
begin
  Bloco_C.WriteRegistroC001;
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'C001';
     QTD_REG_BLC:= 1;
  end;

  if Bloco_C.RegistroC001.IND_MOV = imlComDados then
  begin
     with Bloco_9.Registro9900 do
     begin
        if Bloco_C.RegistroC020Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C020';
              QTD_REG_BLC := Bloco_C.RegistroC020Count;
           end;
        end;

        if Bloco_C.RegistroC030Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C030';
              QTD_REG_BLC := Bloco_C.RegistroC030Count;
           end;
        end;

        if Bloco_C.RegistroC035Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C035';
              QTD_REG_BLC := Bloco_C.RegistroC035Count;
           end;
        end;

        if Bloco_C.RegistroC040Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C040';
              QTD_REG_BLC := Bloco_C.RegistroC040Count;
           end;
        end;

        if Bloco_C.RegistroC250Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C250';
              QTD_REG_BLC := Bloco_C.RegistroC250Count;
           end;
        end;

        if Bloco_C.RegistroC255Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C255';
              QTD_REG_BLC := Bloco_C.RegistroC255Count;
           end;
        end;

        if Bloco_C.RegistroC260Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C260';
              QTD_REG_BLC := Bloco_C.RegistroC260Count;
           end;
        end;

        if Bloco_C.RegistroC265Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C265';
              QTD_REG_BLC := Bloco_C.RegistroC265Count;
           end;
        end;

        if Bloco_C.RegistroC300Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C300';
              QTD_REG_BLC := Bloco_C.RegistroC300Count;
           end;
        end;

        if Bloco_C.RegistroC305Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C305';
              QTD_REG_BLC := Bloco_C.RegistroC305Count;
           end;
        end;

        if Bloco_C.RegistroC310Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C310';
              QTD_REG_BLC := Bloco_C.RegistroC310Count;
           end;
        end;

        if Bloco_C.RegistroC315Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C315';
              QTD_REG_BLC := Bloco_C.RegistroC315Count;
           end;
        end;

        if Bloco_C.RegistroC320Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C320';
              QTD_REG_BLC := Bloco_C.RegistroC320Count;
           end;
        end;

        if Bloco_C.RegistroC325Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C325';
              QTD_REG_BLC := Bloco_C.RegistroC325Count;
           end;
        end;

        if Bloco_C.RegistroC500Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C500';
              QTD_REG_BLC := Bloco_C.RegistroC500Count;
           end;
        end;

        if Bloco_C.RegistroC550Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C550';
              QTD_REG_BLC := Bloco_C.RegistroC550Count;
           end;
        end;

        if Bloco_C.RegistroC555Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C555';
              QTD_REG_BLC := Bloco_C.RegistroC555Count;
           end;
        end;

        if Bloco_C.RegistroC560Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C560';
              QTD_REG_BLC := Bloco_C.RegistroC560Count;
           end;
        end;

        if Bloco_C.RegistroC570Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C570';
              QTD_REG_BLC := Bloco_C.RegistroC570Count;
           end;
        end;

        if Bloco_C.RegistroC575Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C575';
              QTD_REG_BLC := Bloco_C.RegistroC575Count;
           end;
        end;

        if Bloco_C.RegistroC600Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C600';
              QTD_REG_BLC := Bloco_C.RegistroC600Count;
           end;
        end;

        if Bloco_C.RegistroC605Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C605';
              QTD_REG_BLC := Bloco_C.RegistroC605Count;
           end;
        end;

        if Bloco_C.RegistroC610Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C610';
              QTD_REG_BLC := Bloco_C.RegistroC610Count;
           end;
        end;

        if Bloco_C.RegistroC615Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C615';
              QTD_REG_BLC := Bloco_C.RegistroC615Count;
           end;
        end;

        if Bloco_C.RegistroC620Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C620';
              QTD_REG_BLC := Bloco_C.RegistroC620Count;
           end;
        end;

        if Bloco_C.RegistroC625Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C625';
              QTD_REG_BLC := Bloco_C.RegistroC625Count;
           end;
        end;

        if Bloco_C.RegistroC700Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C700';
              QTD_REG_BLC := Bloco_C.RegistroC700Count;
           end;
        end;

        if Bloco_C.RegistroC705Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C705';
              QTD_REG_BLC := Bloco_C.RegistroC705Count;
           end;
        end;

        if Bloco_C.RegistroC710Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C710';
              QTD_REG_BLC := Bloco_C.RegistroC710Count;
           end;
        end;

        if Bloco_C.RegistroC715Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C715';
              QTD_REG_BLC := Bloco_C.RegistroC715Count;
           end;
        end;

        if Bloco_C.RegistroC720Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C720';
              QTD_REG_BLC := Bloco_C.RegistroC720Count;
           end;
        end;

        if Bloco_C.RegistroC750Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C750';
              QTD_REG_BLC := Bloco_C.RegistroC750Count;
           end;
        end;

        if Bloco_C.RegistroC755Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C755';
              QTD_REG_BLC := Bloco_C.RegistroC755Count;
           end;
        end;

        if Bloco_C.RegistroC770Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C770';
              QTD_REG_BLC := Bloco_C.RegistroC770Count;
           end;
        end;

        if Bloco_C.RegistroC775Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'C775';
              QTD_REG_BLC := Bloco_C.RegistroC775Count;
           end;
        end;
     end;
  end;
end;

procedure TACBrLFD.WriteRegistroC990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'C990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_C.WriteRegistroC990;
end;

procedure TACBrLFD.WriteRegistroD001;
begin
  Bloco_D.WriteRegistroD001;

  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'D001';
     QTD_REG_BLC:= 1;
  end;

  if Bloco_D.RegistroD001.IND_MOV = imlComDados then
  begin
     with Bloco_9.Registro9900 do
     begin
        {if Bloco_D.RegistroD100Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D100';
              QTD_REG_BLC := Bloco_D.RegistroD100Count;
           end;
        end;

        if Bloco_D.RegistroD110Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D110';
              QTD_REG_BLC := Bloco_D.RegistroD110Count;
           end;
        end;

        if Bloco_D.RegistroD120Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D120';
              QTD_REG_BLC := Bloco_D.RegistroD120Count;
           end;
        end;

        if Bloco_D.RegistroD130Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D130';
              QTD_REG_BLC := Bloco_D.RegistroD130Count;
           end;
        end;

        if Bloco_D.RegistroD140Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D140';
              QTD_REG_BLC := Bloco_D.RegistroD140Count;
           end;
        end;

        if Bloco_D.RegistroD150Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D150';
              QTD_REG_BLC := Bloco_D.RegistroD150Count;
           end;
        end;

        if Bloco_D.RegistroD160Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D160';
              QTD_REG_BLC := Bloco_D.RegistroD160Count;
           end;
        end;

        if Bloco_D.RegistroD161Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D161';
              QTD_REG_BLC := Bloco_D.RegistroD161Count;
           end;
        end;

        if Bloco_D.RegistroD162Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D162';
              QTD_REG_BLC := Bloco_D.RegistroD162Count;
           end;
        end;

        if Bloco_D.RegistroD170Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D170';
              QTD_REG_BLC := Bloco_D.RegistroD170Count;
           end;
        end;

        if Bloco_D.RegistroD180Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D180';
              QTD_REG_BLC := Bloco_D.RegistroD180Count;
           end;
        end;

        if Bloco_D.RegistroD190Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D190';
              QTD_REG_BLC := Bloco_D.RegistroD190Count;
           end;
        end;

        if Bloco_D.RegistroD195Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D195';
              QTD_REG_BLC := Bloco_D.RegistroD195Count;
           end;
        end;

        if Bloco_D.RegistroD197Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D197';
              QTD_REG_BLC := Bloco_D.RegistroD197Count;
           end;
        end;
		 
        if Bloco_D.RegistroD300Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D300';
              QTD_REG_BLC := Bloco_D.RegistroD300Count;
           end;
        end;

        if Bloco_D.RegistroD301Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D301';
              QTD_REG_BLC := Bloco_D.RegistroD301Count;
           end;
        end;

        if Bloco_D.RegistroD310Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D310';
              QTD_REG_BLC := Bloco_D.RegistroD310Count;
           end;
        end;

        if Bloco_D.RegistroD350Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D350';
              QTD_REG_BLC := Bloco_D.RegistroD350Count;
           end;
        end;

        if Bloco_D.RegistroD355Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D355';
              QTD_REG_BLC := Bloco_D.RegistroD355Count;
           end;
        end;

        if Bloco_D.RegistroD360Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D360';
              QTD_REG_BLC := Bloco_D.RegistroD360Count;
           end;
        end;

        if Bloco_D.RegistroD365Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D365';
              QTD_REG_BLC := Bloco_D.RegistroD365Count;
           end;
        end;

        if Bloco_D.RegistroD370Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D370';
              QTD_REG_BLC := Bloco_D.RegistroD370Count;
           end;
        end;

        if Bloco_D.RegistroD390Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D390';
              QTD_REG_BLC := Bloco_D.RegistroD390Count;
           end;
        end;

        if Bloco_D.RegistroD400Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D400';
              QTD_REG_BLC := Bloco_D.RegistroD400Count;
           end;
        end;

        if Bloco_D.RegistroD410Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D410';
              QTD_REG_BLC := Bloco_D.RegistroD410Count;
           end;
        end;

        if Bloco_D.RegistroD411Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D411';
              QTD_REG_BLC := Bloco_D.RegistroD411Count;
           end;
        end;

        if Bloco_D.RegistroD420Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D420';
              QTD_REG_BLC := Bloco_D.RegistroD420Count;
           end;
        end;

        if Bloco_D.RegistroD500Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D500';
              QTD_REG_BLC := Bloco_D.RegistroD500Count;
           end;
        end;

        if Bloco_D.RegistroD510Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D510';
              QTD_REG_BLC := Bloco_D.RegistroD510Count;
           end;
        end;

        if Bloco_D.RegistroD530Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D530';
              QTD_REG_BLC := Bloco_D.RegistroD530Count;
           end;
        end;

        if Bloco_D.RegistroD590Count > 0 then
        begin
           with New do
           begin
             REG_BLC := 'D590';
             QTD_REG_BLC := Bloco_D.RegistroD590Count;
           end;
        end;

        if Bloco_D.RegistroD600Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D600';
              QTD_REG_BLC := Bloco_D.RegistroD600Count;
           end;
        end;

        if Bloco_D.RegistroD610Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D610';
              QTD_REG_BLC := Bloco_D.RegistroD610Count;
            end;
        end;

        if Bloco_D.RegistroD690Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D690';
              QTD_REG_BLC := Bloco_D.RegistroD690Count;
           end;
        end;

        if Bloco_D.RegistroD695Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D695';
              QTD_REG_BLC := Bloco_D.RegistroD695Count;
           end;
        end;

        if Bloco_D.RegistroD696Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D696';
              QTD_REG_BLC := Bloco_D.RegistroD696Count;
           end;
        end;

        if Bloco_D.RegistroD697Count > 0 then
        begin
           with New do
           begin
              REG_BLC := 'D697';
              QTD_REG_BLC := Bloco_D.RegistroD697Count;
           end;
        end;}
    end;
  end;
end;

procedure TACBrLFD.WriteRegistroD990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'D990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_D.WriteRegistroD990;
end;

procedure TACBrLFD.WriteRegistroE001;
begin
   Bloco_E.WriteRegistroE001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC    := 'E001';
      QTD_REG_BLC:= 1;
   end;

   if Bloco_E.RegistroE001.IND_MOV = imlComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_E.RegistroE020Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E020';
               QTD_REG_BLC := Bloco_E.RegistroE020Count;
            end;
         end;
         if Bloco_E.RegistroE025Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E025';
               QTD_REG_BLC := Bloco_E.RegistroE025Count;
            end;
         end;
         if Bloco_E.RegistroE050Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E050';
               QTD_REG_BLC := Bloco_E.RegistroE050Count;
            end;
         end;
         if Bloco_E.RegistroE055Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E055';
               QTD_REG_BLC := Bloco_E.RegistroE055Count;
            end;
         end;
         with New do
         begin
            REG_BLC := 'E300';
            QTD_REG_BLC := 1;
         end;
         if Bloco_E.RegistroE310Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E310';
               QTD_REG_BLC := Bloco_E.RegistroE310Count;
            end;
         end;
         with New do
         begin
            REG_BLC := 'E360';
            QTD_REG_BLC := 1;
         end;
         if Bloco_E.RegistroE365Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E365';
               QTD_REG_BLC := Bloco_E.RegistroE365Count;
            end;
         end;
         with New do
         begin
            REG_BLC := 'E500';
            QTD_REG_BLC := 1;
         end;
         with New do
         begin
            REG_BLC := 'E530';
            QTD_REG_BLC := 1;
         end;
         {if Bloco_E.RegistroE112Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E112';
               QTD_REG_BLC := Bloco_E.RegistroE112Count;
            end;
         end;
         if Bloco_E.RegistroE113Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E113';
               QTD_REG_BLC := Bloco_E.RegistroE113Count;
            end;
         end;
         if Bloco_E.RegistroE115Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E115';
               QTD_REG_BLC := Bloco_E.RegistroE115Count;
            end;
         end;
         if Bloco_E.RegistroE116Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E116';
               QTD_REG_BLC := Bloco_E.RegistroE116Count;
            end;
         end;
         if Bloco_E.RegistroE200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E200';
               QTD_REG_BLC := Bloco_E.RegistroE200Count;
            end;
         end;
         if Bloco_E.RegistroE210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E210';
               QTD_REG_BLC := Bloco_E.RegistroE210Count;
            end;
         end;
         if Bloco_E.RegistroE220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E220';
               QTD_REG_BLC := Bloco_E.RegistroE220Count;
            end;
         end;
         if Bloco_E.RegistroE230Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E230';
               QTD_REG_BLC := Bloco_E.RegistroE230Count;
            end;
         end;
         if Bloco_E.RegistroE240Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E240';
               QTD_REG_BLC := Bloco_E.RegistroE240Count;
            end;
         end;
         if Bloco_E.RegistroE250Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E250';
               QTD_REG_BLC := Bloco_E.RegistroE250Count;
            end;
         end;
         if Bloco_E.RegistroE500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E500';
               QTD_REG_BLC := Bloco_E.RegistroE500Count;
            end;
         end;
         if Bloco_E.RegistroE510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E510';
               QTD_REG_BLC := Bloco_E.RegistroE510Count;
            end;
         end;
         if Bloco_E.RegistroE520Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E520';
               QTD_REG_BLC := Bloco_E.RegistroE520Count;
            end;
         end;
         if Bloco_E.RegistroE530Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E530';
               QTD_REG_BLC := Bloco_E.RegistroE530Count;
            end;
         end; *}
      end;
   end;
end;

procedure TACBrLFD.WriteRegistroE990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'E990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_E.WriteRegistroE990;
end;

procedure TACBrLFD.WriteRegistroI001;
begin
  Bloco_I.WriteRegistroI001;

  with Bloco_9.Registro9900.New do
  begin
      REG_BLC    := 'I001';
      QTD_REG_BLC:= 1;
  end;

  if Bloco_I.RegistroI001.IND_MOV = imlComDados then
    with Bloco_9.Registro9900, Bloco_I do
    begin

    end;
end;

procedure TACBrLFD.WriteRegistroI990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'I990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_I.WriteRegistroI990;
end;

procedure TACBrLFD.WriteRegistroJ001;
begin
  Bloco_J.WriteRegistroJ001;
  with Bloco_9.Registro9900.New do
  begin
      REG_BLC    := 'J001';
      QTD_REG_BLC:= 1;
  end;

  {if Bloco_G.RegistroG001.IND_MOV = imComDados then
    with Bloco_9.Registro9900, Bloco_G do
    begin
      New('G110', RegistroG110Count);
      New('G125', RegistroG125Count);
      New('G126', RegistroG126Count);
      New('G130', RegistroG130Count);
      New('G140', RegistroG140Count);
    end; }
end;

procedure TACBrLFD.WriteRegistroK001;
begin
  Bloco_K.WriteRegistroK001;
  with Bloco_9.Registro9900.New do
  begin
      REG_BLC    := 'K001';
      QTD_REG_BLC:= 1;
  end;
end;

procedure TACBrLFD.WriteRegistroL001;
begin
  Bloco_L.WriteRegistroL001;
  with Bloco_9.Registro9900.New do
  begin
      REG_BLC    := 'L001';
      QTD_REG_BLC:= 1;
  end;
end;

procedure TACBrLFD.WriteRegistroJ990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'J990';
    QTD_REG_BLC := 1;
  end;
  Bloco_J.WriteRegistroJ990;
end;

procedure TACBrLFD.WriteRegistroK990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'K990';
    QTD_REG_BLC := 1;
  end;
  Bloco_K.WriteRegistroK990;
end;

procedure TACBrLFD.WriteRegistroL990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'L990';
    QTD_REG_BLC := 1;
  end;
  Bloco_L.WriteRegistroL990;
end;


procedure TACBrLFD.WriteRegistroH001;
begin
   Bloco_H.WriteRegistroH001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC    := 'H001';
      QTD_REG_BLC:= 1;
   end;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'H020';
      QTD_REG_BLC := 1;
   end;

   if Bloco_H.RegistroH001.IND_MOV = imlComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_H.RegistroH001Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H001';
               QTD_REG_BLC := Bloco_H.RegistroH001Count;
            end;
         end;

         if Bloco_H.RegistroH030Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H030';
               QTD_REG_BLC := Bloco_H.RegistroH030Count;
            end;
         end;

         if Bloco_H.RegistroH040Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H040';
               QTD_REG_BLC := Bloco_H.RegistroH040Count;
            end;
         end;

         if Bloco_H.RegistroH050Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H050';
               QTD_REG_BLC := Bloco_H.RegistroH050Count;
            end;
         end;

         if Bloco_H.RegistroH060Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H060';
               QTD_REG_BLC := Bloco_H.RegistroH060Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrLFD.WriteRegistroH990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'H990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_H.WriteRegistroH990;
end;

procedure TACBrLFD.WriteRegistroZ001;
begin
  Bloco_Z.WriteRegistroZ001;
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'Z001';
     QTD_REG_BLC:= 1;
  end;
end;

procedure TACBrLFD.WriteRegistroZ990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'Z990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_Z.WriteRegistroZ990;
end;

procedure TACBrLFD.WriteRegistro8001;
begin
  Bloco_8.WriteRegistro8001;
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := '8001';
     QTD_REG_BLC:= 1;
  end;

  if Bloco_8.Registro8001.IND_MOV = imlComDados then
    with Bloco_9.Registro9900 do
    begin
       if Bloco_8.Registro8020Count > 0 then
       begin
          with New do
          begin
             REG_BLC := '8020';
             QTD_REG_BLC := Bloco_8.Registro8020Count;
          end;
       end;

       if Bloco_8.Registro8025Count > 0 then
       begin
          with New do
          begin
             REG_BLC := '8025';
             QTD_REG_BLC := Bloco_8.Registro8025Count;
          end;
       end;

       if Bloco_8.Registro8030Count > 0 then
       begin
          with New do
          begin
             REG_BLC := '8030';
             QTD_REG_BLC := Bloco_8.Registro8030Count;
          end;
       end;
    end;
end;

procedure TACBrLFD.WriteRegistro8990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := '8990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_8.WriteRegistro8990;
end;

procedure TACBrLFD.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := '9001';
     QTD_REG_BLC:= 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrLFD.WriteRegistro9900;
begin
  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
        REG_BLC    := '9900';
        QTD_REG_BLC:= Count + 2;
     end;

     with New do
     begin
        REG_BLC    := '9990';
        QTD_REG_BLC:= 1;
     end;

     with New do
     begin
        REG_BLC    := '9999';
        QTD_REG_BLC:= 1;
     end;
  end;
  Bloco_9.WriteRegistro9900;
end;

procedure TACBrLFD.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrLFD.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                 Bloco_A.RegistroA990.QTD_LIN_A +
                                                                 Bloco_B.RegistroB990.QTD_LIN_B +
                                                                 Bloco_C.RegistroC990.QTD_LIN_C +
                                                                 Bloco_D.RegistroD990.QTD_LIN_D +
                                                                 Bloco_E.RegistroE990.QTD_LIN_E +
                                                                 Bloco_H.RegistroH990.QTD_LIN_H +
                                                                 Bloco_I.RegistroI990.QTD_LIN_I +
                                                                 Bloco_J.RegistroJ990.QTD_LIN_J +
                                                                 Bloco_K.RegistroK990.QTD_LIN_K +
                                                                 Bloco_L.RegistroL990.QTD_LIN_L +
                                                                 Bloco_Z.RegistroZ990.QTD_LIN_Z +
                                                                 Bloco_8.Registro8990.QTD_LIN_8 +
                                                                 Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;

{$IFNDEF Framework}
{$IFDEF FPC}
initialization
   {$I ACBrLFD.lrs}
{$ENDIF}
{$ENDIF}

end.

