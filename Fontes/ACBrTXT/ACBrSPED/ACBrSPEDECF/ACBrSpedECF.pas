{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	               }
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

{******************************************************************************}
{* Historico                                                                   }
{*                                                                             }
{* 18/08/2015 - Ariel Guareschi - Alterado a geração do arquivo bloco K        }
{* 27/08/2015 - Ariel Guareschi - Alterado a geração do arquivo bloco X, Y     }
{* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr        }
{******************************************************************************}
{$I ACBr.inc}
unit ACBrSpedECF;

interface

uses
  SysUtils, Math, Classes, ACBrBase, 
  {$IFNDEF Framework}
    {$IFDEF FPC}
      LResources,
    {$ENDIF}
  {$ENDIF}
  DateUtils, ACBrSped, ACBrTXTClass, ACBrECFBlocos,
  ACBrECFBloco_0_Class, ACBrECFBloco_C_Class, ACBrECFBloco_E_Class,
  ACBrECFBloco_J_Class, ACBrECFBloco_K_Class, ACBrECFBloco_L_Class,
  ACBrECFBloco_M_Class, ACBrECFBloco_N_Class, ACBrECFBloco_P_Class,
  ACBrECFBloco_Q_Class, ACBrECFBloco_T_Class, ACBrECFBloco_U_Class,
  ACBrECFBloco_X_Class, ACBrECFBloco_Y_Class, ACBrECFBloco_9_Class ;

type
  /// ACBrSpedECF - Sitema Publico de Escrituração Contábil Fiscal

  { TACBrSPEDECF }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSPEDECF = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: String;
    FInicializado : boolean;
    FOnError: TErrorEvent;
    FDT_INI: TDateTime;
    FDT_FIN: TDateTime;
    FPath: String;
    FDelimitador: String;
    FTrimString: boolean;
    FCurMascara: String;

    FBloco_0: TBloco_0;
    FBloco_C: TBloco_C;
    FBloco_E: TBloco_E;
    FBloco_J: TBloco_J;
    FBloco_K: TBloco_K;
    FBloco_L: TBloco_L;
    FBloco_M: TBloco_M;
    FBloco_N: TBloco_N;
    FBloco_P: TBloco_P;
    FBloco_Q: TBloco_Q;
    FBloco_T: TBloco_T;
    FBloco_U: TBloco_U;
    FBloco_X: TBloco_X;
    FBloco_Y: TBloco_Y;
    FBloco_9: TBloco_9;

    function GetConteudo: TStringList;
    function GetDelimitador: String;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const Value: String);
    procedure SetDelimitador(const Value: String);
    procedure SetLinhasBuffer(const Value: Integer);
    procedure SetPath(const Value: String);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);
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
    /// BLOCO C
    procedure WriteRegistroC001;
    procedure WriteRegistroC990;
    /// BLOCO E
    procedure WriteRegistroE001;
    procedure WriteRegistroE990;
    /// BLOCO J
    procedure WriteRegistroJ001;
    procedure WriteRegistroJ990;
    /// BLOCO K
    procedure WriteRegistroK001;
    procedure WriteRegistroK990;
		/// BLOCO L
    procedure WriteRegistroL001;
    procedure WriteRegistroL990;
		/// BLOCO M
    procedure WriteRegistroM001;
    procedure WriteRegistroM990;
		/// BLOCO N
    procedure WriteRegistroN001;
    procedure WriteRegistroN990;
		/// BLOCO P
    procedure WriteRegistroP001;
    procedure WriteRegistroP990;
		/// BLOCO Q
    procedure WriteRegistroQ001;
    procedure WriteRegistroQ990;
		/// BLOCO T
    procedure WriteRegistroT001;
    procedure WriteRegistroT990;
		/// BLOCO U
    procedure WriteRegistroU001;
    procedure WriteRegistroU990;
		/// BLOCO X
    procedure WriteRegistroX001;
    procedure WriteRegistroX990;
		/// BLOCO Y
    procedure WriteRegistroY001;
    procedure WriteRegistroY990;
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
    procedure CancelaGeracao;

    procedure WriteBloco_0;
    procedure WriteBloco_C;
    procedure WriteBloco_E;
    procedure WriteBloco_J;
    procedure WriteBloco_K;
    procedure WriteBloco_L;
    procedure WriteBloco_M;
    procedure WriteBloco_N;
    procedure WriteBloco_P;
    procedure WriteBloco_Q;
    procedure WriteBloco_T;
    procedure WriteBloco_U;
    procedure WriteBloco_X;
    procedure WriteBloco_Y;
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_E: TBloco_E read FBloco_E write FBloco_E;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
    property Bloco_L: TBloco_L read FBloco_L write FBloco_L;
    property Bloco_M: TBloco_M read FBloco_M write FBloco_M;
    property Bloco_N: TBloco_N read FBloco_N write FBloco_N;
    property Bloco_P: TBloco_P read FBloco_P write FBloco_P;
    property Bloco_Q: TBloco_Q read FBloco_Q write FBloco_Q;
    property Bloco_T: TBloco_T read FBloco_T write FBloco_T;
    property Bloco_U: TBloco_U read FBloco_U write FBloco_U;
    property Bloco_X: TBloco_X read FBloco_X write FBloco_X;
    property Bloco_Y: TBloco_Y read FBloco_Y write FBloco_Y;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;

  published
    property Path: String read FPath write SetPath;
    property Arquivo: String read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer default 1000 ;
    property Delimitador: String read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

  end;

procedure Register;

implementation

uses ACBrUtil;

{$IFNDEF FPC}
 {$R ACBr_SPEDECF.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDECF]);
end;

(* TACBrSpedECF *)

procedure TACBrSPEDECF.CancelaGeracao;
begin
  LimpaRegistros;
  FInicializado := False;
end;

constructor TACBrSPEDECF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_C := TBloco_C.Create;
  FBloco_E := TBloco_E.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_L := TBloco_L.Create;
  FBloco_M := TBloco_M.Create;
  FBloco_N := TBloco_N.Create;
  FBloco_P := TBloco_P.Create;
  FBloco_Q := TBloco_Q.Create;
  FBloco_T := TBloco_T.Create;
  FBloco_U := TBloco_U.Create;
  FBloco_X := TBloco_X.Create;
  FBloco_Y := TBloco_Y.Create;
  FBloco_9 := TBloco_9.Create;

  /// Objeto passado por referência para que possamos usa-lo para fazer pesquisa
  /// em seus registros.
  FBloco_C.Bloco_0 := FBloco_0;
  FBloco_E.Bloco_0 := FBloco_0;
  FBloco_J.Bloco_0 := FBloco_0;
  FBloco_K.Bloco_0 := FBloco_0;
  FBloco_L.Bloco_0 := FBloco_0;
  FBloco_M.Bloco_0 := FBloco_0;
  FBloco_N.Bloco_0 := FBloco_0;
  FBloco_P.Bloco_0 := FBloco_0;
  FBloco_Q.Bloco_0 := FBloco_0;
  FBloco_T.Bloco_0 := FBloco_0;
  FBloco_U.Bloco_0 := FBloco_0;
  FBloco_X.Bloco_0 := FBloco_0;
  FBloco_Y.Bloco_0 := FBloco_0;

  FPath := ExtractFilePath(ParamStr(0));
  FDelimitador := '|';
  FCurMascara := '#0.00';
  FTrimString := True;

  SetDelimitador(FDelimitador);
  SetCurMascara(FCurMascara);
  SetTrimString(FTrimString);
end;

destructor TACBrSPEDECF.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_C.Free;
  FBloco_E.Free;
  FBloco_J.Free;
  FBloco_K.Free;
  FBloco_L.Free;
  FBloco_M.Free;
  FBloco_N.Free;
  FBloco_P.Free;
  FBloco_Q.Free;
  FBloco_T.Free;
  FBloco_U.Free;
  FBloco_X.Free;
  FBloco_Y.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDECF.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_E.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_L.LimpaRegistros;
  FBloco_M.LimpaRegistros;
  FBloco_N.LimpaRegistros;
  FBloco_P.LimpaRegistros;
  FBloco_Q.LimpaRegistros;
  FBloco_T.LimpaRegistros;
  FBloco_U.LimpaRegistros;
  FBloco_X.LimpaRegistros;
  FBloco_Y.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDECF.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrSPEDECF.GetDelimitador: String;
begin
   Result := FDelimitador;
end;

function TACBrSPEDECF.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDECF.SetDelimitador(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio!');

  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_E.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_L.Delimitador := Value;
  FBloco_M.Delimitador := Value;
  FBloco_N.Delimitador := Value;
  FBloco_P.Delimitador := Value;
  FBloco_Q.Delimitador := Value;
  FBloco_T.Delimitador := Value;
  FBloco_U.Delimitador := Value;
  FBloco_X.Delimitador := Value;
  FBloco_Y.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

procedure TACBrSPEDECF.SetLinhasBuffer(const Value: Integer);
begin
   FACBrTXT.LinhasBuffer := Value ;
end;

procedure TACBrSPEDECF.SetPath(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio!');

  FPath := PathWithDelim( Value );
end;

function TACBrSPEDECF.GetCurMascara: String;
begin
  Result := FCurMascara;
end;

procedure TACBrSPEDECF.SetCurMascara(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio! Para deixar sem mascara digite #');

  FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_E.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_L.CurMascara := Value;
  FBloco_M.CurMascara := Value;
  FBloco_N.CurMascara := Value;
  FBloco_P.CurMascara := Value;
  FBloco_Q.CurMascara := Value;
  FBloco_T.CurMascara := Value;
  FBloco_U.CurMascara := Value;
  FBloco_X.CurMascara := Value;
  FBloco_Y.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDECF.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrSPEDECF.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_E.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_L.TrimString := Value;
  FBloco_M.TrimString := Value;
  FBloco_N.TrimString := Value;
  FBloco_P.TrimString := Value;
  FBloco_Q.TrimString := Value;
  FBloco_T.TrimString := Value;
  FBloco_U.TrimString := Value;
  FBloco_X.TrimString := Value;
  FBloco_Y.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDECF.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

procedure TACBrSPEDECF.InicializaBloco( Bloco: TACBrSPED ) ;
begin
  Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
  Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
  Bloco.Gravado      := False ;

  if not assigned(Bloco.Conteudo) then
    Bloco.Conteudo := TStringList.Create;

  Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDECF.IniciaGeracao;
begin
  if FInicializado then
    exit;

  if FDT_INI = 0 then
    raise Exception.Create(ACBrStr('Informe a data inicial das informações contidas no arquivo!'));

  if FDT_FIN = 0 then
    raise Exception.Create(ACBrStr('Informe a data final das informações contidas no arquivo!'));

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 );
  InicializaBloco( Bloco_C );
  InicializaBloco( Bloco_E );
  InicializaBloco( Bloco_J );
  InicializaBloco( Bloco_K );
  InicializaBloco( Bloco_L );
  InicializaBloco( Bloco_M );
  InicializaBloco( Bloco_N );
  InicializaBloco( Bloco_P );
  InicializaBloco( Bloco_Q );
  InicializaBloco( Bloco_T );
  InicializaBloco( Bloco_U );
  InicializaBloco( Bloco_X );
  InicializaBloco( Bloco_Y );
  InicializaBloco( Bloco_9 );

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
  Bloco_0.Registro0990.QTD_LIN := 0;
  Bloco_C.RegistroC990.QTD_LIN := 0;
  Bloco_E.RegistroE990.QTD_LIN := 0;
  Bloco_J.RegistroJ990.QTD_LIN := 0;
  Bloco_K.RegistroK990.QTD_LIN := 0;
  Bloco_L.RegistroL990.QTD_LIN := 0;
  Bloco_M.RegistroM990.QTD_LIN := 0;
  Bloco_N.RegistroN990.QTD_LIN := 0;
  Bloco_P.RegistroP990.QTD_LIN := 0;
  Bloco_Q.RegistroQ990.QTD_LIN := 0;
  Bloco_T.RegistroT990.QTD_LIN := 0;
  Bloco_U.RegistroU990.QTD_LIN := 0;
  Bloco_X.RegistroX990.QTD_LIN := 0;
  Bloco_Y.RegistroY990.QTD_LIN := 0;
  Bloco_9.Registro9990.QTD_LIN := 0;
  Bloco_9.Registro9999.QTD_LIN := 0;
  /// Limpa a lista
  Bloco_9.Registro9900.Clear;
  FInicializado := True ;
end;

procedure TACBrSPEDECF.SetArquivo(const Value: String);
var
  APath : String;
begin
  if FArquivo = Value then
    Exit;
  FArquivo := ExtractFileName( Value );
  APath    := ExtractFilePath( Value );
  if APath <> '' then
    Path := APath;
end;

procedure TACBrSPEDECF.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_E.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_K.DT_INI := Value;
  FBloco_L.DT_INI := Value;
  FBloco_M.DT_INI := Value;
  FBloco_N.DT_INI := Value;
  FBloco_P.DT_INI := Value;
  FBloco_Q.DT_INI := Value;
  FBloco_T.DT_INI := Value;
  FBloco_U.DT_INI := Value;
  FBloco_X.DT_INI := Value;
  FBloco_Y.DT_INI := Value;
  FBloco_9.DT_INI := Value;

  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_INI := Value;
end;

function TACBrSPEDECF.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

procedure TACBrSPEDECF.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_E.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;
  FBloco_L.DT_FIN := Value;
  FBloco_M.DT_FIN := Value;
  FBloco_N.DT_FIN := Value;
  FBloco_P.DT_FIN := Value;
  FBloco_Q.DT_FIN := Value;
  FBloco_T.DT_FIN := Value;
  FBloco_U.DT_FIN := Value;
  FBloco_X.DT_FIN := Value;
  FBloco_Y.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_FIN := Value;
end;

function TACBrSPEDECF.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrSPEDECF.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_E.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_L.OnError := Value;
  FBloco_M.OnError := Value;
  FBloco_N.OnError := Value;
  FBloco_P.OnError := Value;
  FBloco_Q.OnError := Value;
  FBloco_T.OnError := Value;
  FBloco_U.OnError := Value;
  FBloco_X.OnError := Value;
  FBloco_Y.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrSPEDECF.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_C;
    WriteBloco_E;
    WriteBloco_J;
    WriteBloco_K;
    WriteBloco_L;
    WriteBloco_M;
    WriteBloco_N;
    WriteBloco_P;
    WriteBloco_Q;
    WriteBloco_T;
    WriteBloco_U;
    WriteBloco_X;
    WriteBloco_Y;
    WriteBloco_9;

  finally
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;
    FInicializado := False ;
  end;
end;

procedure TACBrSPEDECF.WriteBloco_0;
begin
  if Bloco_0.Gravado then
    exit;

  if not FInicializado then
    raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;

  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_C;
begin
  if Bloco_C.Gravado then
    exit;

  if not Bloco_0.Gravado then
    WriteBloco_0;

  WriteRegistroC001;
  WriteRegistroC990;

  Bloco_C.WriteBuffer;
  Bloco_C.Conteudo.Clear;
  Bloco_C.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_E;
begin
  if Bloco_E.Gravado then
    exit;

  if not Bloco_C.Gravado then
    WriteBloco_C;

  WriteRegistroE001;
  WriteRegistroE990;

  Bloco_E.WriteBuffer;
  Bloco_E.Conteudo.Clear;
  Bloco_E.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_J;
begin
  if Bloco_J.Gravado then
    exit;

  if not Bloco_E.Gravado then
    WriteBloco_E;

  WriteRegistroJ001;
  WriteRegistroJ990;

  Bloco_J.WriteBuffer;
  Bloco_J.Conteudo.Clear;
  Bloco_J.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_K;
begin
  if Bloco_K.Gravado then
    exit;

  if not Bloco_J.Gravado then
    WriteBloco_J;

  WriteRegistroK001;
  WriteRegistroK990;

  Bloco_K.WriteBuffer;
  Bloco_K.Conteudo.Clear;
  Bloco_K.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_L;
begin
  if Bloco_L.Gravado then
    exit;

  if not Bloco_K.Gravado then
    WriteBloco_K;

  WriteRegistroL001;
  WriteRegistroL990;

  Bloco_L.WriteBuffer;
  Bloco_L.Conteudo.Clear;
  Bloco_L.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_M;
begin
  if Bloco_M.Gravado then
    exit;

  if not Bloco_L.Gravado then
    WriteBloco_L;

  WriteRegistroM001;
  WriteRegistroM990;

  Bloco_M.WriteBuffer;
  Bloco_M.Conteudo.Clear;
  Bloco_M.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_N;
begin
  if Bloco_N.Gravado then
    exit;

  if not Bloco_M.Gravado then
    WriteBloco_M;

  WriteRegistroN001;
  WriteRegistroN990;

  Bloco_N.WriteBuffer;
  Bloco_N.Conteudo.Clear;
  Bloco_N.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_P;
begin
  if Bloco_P.Gravado then
    exit;

  if not Bloco_N.Gravado then
    WriteBloco_N;

  WriteRegistroP001;
  WriteRegistroP990;

  Bloco_P.WriteBuffer;
  Bloco_P.Conteudo.Clear;
  Bloco_P.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_Q;
begin
  if Bloco_Q.Gravado then
    exit;

  if Bloco_0.Registro0000.COD_VER=ECFVersao100 then 
    exit;

  if not Bloco_P.Gravado then
    WriteBloco_P;

  WriteRegistroQ001;
  WriteRegistroQ990;

  Bloco_Q.WriteBuffer;
  Bloco_Q.Conteudo.Clear;
  Bloco_Q.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_T;
begin
  if Bloco_T.Gravado then
    exit;

  if not Bloco_Q.Gravado then
    WriteBloco_Q;

  WriteRegistroT001;
  WriteRegistroT990;

  Bloco_T.WriteBuffer;
  Bloco_T.Conteudo.Clear;
  Bloco_T.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_U;
begin
  if Bloco_U.Gravado then
    exit;

  if not Bloco_T.Gravado then
    WriteBloco_T;

  WriteRegistroU001;
  WriteRegistroU990;

  Bloco_U.WriteBuffer;
  Bloco_U.Conteudo.Clear;
  Bloco_U.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_X;
begin
  if Bloco_X.Gravado then
    exit;

  if not Bloco_U.Gravado then
    WriteBloco_U;

  WriteRegistroX001;
  WriteRegistroX990;

  Bloco_X.WriteBuffer;
  Bloco_X.Conteudo.Clear;
  Bloco_X.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_Y;
begin
  if Bloco_Y.Gravado then
    exit;

  if not Bloco_X.Gravado then
    WriteBloco_X;

  WriteRegistroY001;
  WriteRegistroY990;

  Bloco_Y.WriteBuffer;
  Bloco_Y.Conteudo.Clear;
  Bloco_Y.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_9;
begin
  if Bloco_9.Gravado then
    exit;

  if not Bloco_Y.Gravado then
    WriteBloco_Y;

  WriteRegistro9001;
  WriteRegistro9900;
  WriteRegistro9990;
  WriteRegistro9999;
  Bloco_9.WriteBuffer;
  Bloco_9.Conteudo.Clear;
  Bloco_9.Gravado := True;
end;

procedure TACBrSPEDECF.WriteRegistro0000;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '0000';
    QTD_REG_BLC := 1;
  end;
  Bloco_0.WriteRegistro0000;
end;

procedure TACBrSPEDECF.WriteRegistro0001;
begin
  Bloco_0.WriteRegistro0001;
end;

procedure TACBrSPEDECF.WriteRegistro0990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '0001';
      QTD_REG_BLC := 1;
    end;
    if Bloco_0.Registro0001.IND_DAD = idComDados then
    begin
      if Bloco_0.Registro0010Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0010';
          QTD_REG_BLC := Bloco_0.Registro0010Count;
        end;
      end;
      if Bloco_0.Registro0020Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0020';
          QTD_REG_BLC := Bloco_0.Registro0020Count;
        end;
      end;
      if Bloco_0.Registro0030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0030';
          QTD_REG_BLC := Bloco_0.Registro0030Count;
        end;
      end;
      if Bloco_0.Registro0035Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0035';
          QTD_REG_BLC := Bloco_0.Registro0035Count;
        end;
      end;
      if Bloco_0.Registro0930Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0930';
          QTD_REG_BLC := Bloco_0.Registro0930Count;
        end;
      end;
    end;
   with New do
    begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_0.WriteRegistro0990;
end;

procedure TACBrSPEDECF.WriteRegistroC001;
begin
  Bloco_C.WriteRegistroC001;
end;

procedure TACBrSPEDECF.WriteRegistroC990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'C001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_C.RegistroC001.IND_DAD = idComDados) then
    begin
      if Bloco_C.RegistroC040Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C040';
          QTD_REG_BLC := Bloco_C.RegistroC040Count;
        end;
      end;
      if Bloco_C.RegistroC050Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C050';
          QTD_REG_BLC := Bloco_C.RegistroC050Count;
        end;
      end;
      if Bloco_C.RegistroC051Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C051';
          QTD_REG_BLC := Bloco_C.RegistroC051Count;
        end;
      end;
      if Bloco_C.RegistroC053Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C053';
          QTD_REG_BLC := Bloco_C.RegistroC053Count;
        end;
      end;
      if Bloco_C.RegistroC100Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C100';
          QTD_REG_BLC := Bloco_C.RegistroC100Count;
        end;
      end;
      if Bloco_C.RegistroC150Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C150';
          QTD_REG_BLC := Bloco_C.RegistroC150Count;
        end;
      end;
      if Bloco_C.RegistroC155Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C155';
          QTD_REG_BLC := Bloco_C.RegistroC155Count;
        end;
      end;
      if Bloco_C.RegistroC157Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C157';
          QTD_REG_BLC := Bloco_C.RegistroC157Count;
        end;
      end;
      if Bloco_C.RegistroC350Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C350';
          QTD_REG_BLC := Bloco_C.RegistroC350Count;
        end;
      end;
      if Bloco_C.RegistroC355Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C355';
          QTD_REG_BLC := Bloco_C.RegistroC355Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'C990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_C.WriteRegistroC990;
end;

procedure TACBrSPEDECF.WriteRegistroE001;
begin
  Bloco_E.WriteRegistroE001;
end;

procedure TACBrSPEDECF.WriteRegistroE990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'E001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_E.RegistroE001.IND_DAD = idComDados) then
    begin
      if Bloco_E.RegistroE010Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E010';
          QTD_REG_BLC := Bloco_E.RegistroE010Count;
        end;
      end;
      if Bloco_E.RegistroE015Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E015';
          QTD_REG_BLC := Bloco_E.RegistroE015Count;
        end;
      end;
      if Bloco_E.RegistroE020Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E020';
          QTD_REG_BLC := Bloco_E.RegistroE020Count;
        end;
      end;
      if Bloco_E.RegistroE030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E030';
          QTD_REG_BLC := Bloco_E.RegistroE030Count;
        end;
      end;
      if Bloco_E.RegistroE155Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E155';
          QTD_REG_BLC := Bloco_E.RegistroE155Count;
        end;
      end;
      if Bloco_E.RegistroE355Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'E355';
          QTD_REG_BLC := Bloco_E.RegistroE355Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'E990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_E.WriteRegistroE990;
end;

procedure TACBrSPEDECF.WriteRegistroJ001;
begin
  Bloco_J.WriteRegistroJ001;
end;

procedure TACBrSPEDECF.WriteRegistroJ990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'J001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_J.RegistroJ001.IND_DAD = idComDados) then
    begin
      if Bloco_J.RegistroJ050Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'J050';
          QTD_REG_BLC := Bloco_J.RegistroJ050Count;
        end;
      end;
      if Bloco_J.RegistroJ051Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'J051';
          QTD_REG_BLC := Bloco_J.RegistroJ051Count;
        end;
      end;
      if Bloco_J.RegistroJ053Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'J053';
          QTD_REG_BLC := Bloco_J.RegistroJ053Count;
        end;
      end;
      if Bloco_J.RegistroJ100Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'J100';
          QTD_REG_BLC := Bloco_J.RegistroJ100Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'J990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_J.WriteRegistroJ990;
end;

procedure TACBrSPEDECF.WriteRegistroK001;
begin
  Bloco_K.WriteRegistroK001;
end;

procedure TACBrSPEDECF.WriteRegistroK990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'K001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_K.RegistroK001.IND_DAD = idComDados) then
    begin
      if (Bloco_K.RegistroK030Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'K030';
          QTD_REG_BLC := Bloco_K.RegistroK030Count;
        end;
      end;
      if (Bloco_K.RegistroK155Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'K155';
          QTD_REG_BLC := Bloco_K.RegistroK155Count;
        end;
      end;
      if (Bloco_K.RegistroK156Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'K156';
          QTD_REG_BLC := Bloco_K.RegistroK156Count;
        end;
      end;
      if (Bloco_K.RegistroK355Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'K355';
          QTD_REG_BLC := Bloco_K.RegistroK355Count;
        end;
      end;
      if (Bloco_K.RegistroK356Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'K356';
          QTD_REG_BLC := Bloco_K.RegistroK356Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'K990';
      QTD_REG_BLC := Bloco_K.RegistroK356Count;
    end;
  end;
  Bloco_K.WriteRegistroK990;
end;

procedure TACBrSPEDECF.WriteRegistroL001;
begin
  Bloco_L.WriteRegistroL001;
end;

procedure TACBrSPEDECF.WriteRegistroL990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'L001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_L.RegistroL001.IND_DAD = idComDados) then
    begin
      if Bloco_L.RegistroL030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'L030';
          QTD_REG_BLC := Bloco_L.RegistroL030Count;
        end;
      end;
      if Bloco_L.RegistroL100Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'L100';
          QTD_REG_BLC := Bloco_L.RegistroL100Count;
        end;
      end;
      if Bloco_L.RegistroL200Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'L200';
          QTD_REG_BLC := Bloco_L.RegistroL200Count;
        end;
      end;
      if Bloco_L.RegistroL210Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'L210';
          QTD_REG_BLC := Bloco_L.RegistroL210Count;
        end;
      end;
      if Bloco_L.RegistroL300Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'L300';
          QTD_REG_BLC := Bloco_L.RegistroL300Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'L990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_L.WriteRegistroL990;
end;

procedure TACBrSPEDECF.WriteRegistroM001;
begin
  Bloco_M.WriteRegistroM001;
end;

procedure TACBrSPEDECF.WriteRegistroM990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'M001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_M.RegistroM001.IND_DAD = idComDados) then
    begin
      if (Bloco_M.RegistroM010Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'M010';
          QTD_REG_BLC := Bloco_M.RegistroM010Count;
        end;
      end;
      if (Bloco_M.RegistroM030Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'M030';
          QTD_REG_BLC := Bloco_M.RegistroM030Count;
        end;
      end;
      if (Bloco_M.RegistroM300Count > 0) then
      begin
        with New do
        begin
          REG_BLC := 'M300';
          QTD_REG_BLC := Bloco_M.RegistroM300Count;
        end;
      end;
      if Bloco_M.RegistroM305Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M305';
          QTD_REG_BLC := Bloco_M.RegistroM305Count;
        end;
      end;
      if Bloco_M.RegistroM310Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M310';
          QTD_REG_BLC := Bloco_M.RegistroM310Count;
        end;
      end;
      if Bloco_M.RegistroM312Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M312';
          QTD_REG_BLC := Bloco_M.RegistroM312Count;
        end;
      end;
      if Bloco_M.RegistroM315Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M315';
          QTD_REG_BLC := Bloco_M.RegistroM315Count;
        end;
      end;
      if Bloco_M.RegistroM350Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M350';
          QTD_REG_BLC := Bloco_M.RegistroM350Count;
        end;
      end;
      if Bloco_M.RegistroM355Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M355';
          QTD_REG_BLC := Bloco_M.RegistroM355Count;
        end;
      end;
      if Bloco_M.RegistroM360Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M360';
          QTD_REG_BLC := Bloco_M.RegistroM360Count;
        end;
      end;
      if Bloco_M.RegistroM362Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M362';
          QTD_REG_BLC := Bloco_M.RegistroM362Count;
        end;
      end;
      if Bloco_M.RegistroM365Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M365';
          QTD_REG_BLC := Bloco_M.RegistroM365Count;
        end;
      end;
      if Bloco_M.RegistroM410Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M410';
          QTD_REG_BLC := Bloco_M.RegistroM410Count;
        end;
      end;
      if Bloco_M.RegistroM415Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M415';
          QTD_REG_BLC := Bloco_M.RegistroM415Count;
        end;
      end;
      if Bloco_M.RegistroM500Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'M500';
          QTD_REG_BLC := Bloco_M.RegistroM500Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'M990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_M.WriteRegistroM990;
end;

procedure TACBrSPEDECF.WriteRegistroN001;
begin
  Bloco_N.WriteRegistroN001;
end;

procedure TACBrSPEDECF.WriteRegistroN990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'N001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_N.RegistroN001.IND_DAD = idComDados) then
    begin
      if Bloco_N.RegistroN030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N030';
          QTD_REG_BLC := Bloco_N.RegistroN030Count;
        end;
      end;
      if Bloco_N.RegistroN500Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N500';
          QTD_REG_BLC := Bloco_N.RegistroN500Count;
        end;
      end;
      if Bloco_N.RegistroN600Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N600';
          QTD_REG_BLC := Bloco_N.RegistroN600Count;
        end;
      end;
      if Bloco_N.RegistroN610Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N610';
          QTD_REG_BLC := Bloco_N.RegistroN610Count;
        end;
      end;
      if Bloco_N.RegistroN615Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N615';
          QTD_REG_BLC := Bloco_N.RegistroN615Count;
        end;
      end;
      if Bloco_N.RegistroN620Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N620';
          QTD_REG_BLC := Bloco_N.RegistroN620Count;
        end;
      end;
      if Bloco_N.RegistroN630Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N630';
          QTD_REG_BLC := Bloco_N.RegistroN630Count;
        end;
      end;
      if Bloco_N.RegistroN650Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N650';
          QTD_REG_BLC := Bloco_N.RegistroN650Count;
        end;
      end;
      if Bloco_N.RegistroN660Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N660';
          QTD_REG_BLC := Bloco_N.RegistroN660Count;
        end;
      end;
      if Bloco_N.RegistroN670Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'N670';
          QTD_REG_BLC := Bloco_N.RegistroN670Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'N990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_N.WriteRegistroN990;
end;

procedure TACBrSPEDECF.WriteRegistroP001;
begin
  Bloco_P.WriteRegistroP001;
end;

procedure TACBrSPEDECF.WriteRegistroP990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'P001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_P.RegistroP001.IND_DAD = idComDados) then
    begin
      if Bloco_P.RegistroP001.RegistroP030.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P030';
          QTD_REG_BLC := Bloco_P.RegistroP001.RegistroP030.Count;
        end;
      end;
      if Bloco_P.RegistroP100Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P100';
          QTD_REG_BLC := Bloco_P.RegistroP100Count;
        end;
      end;
      if Bloco_P.RegistroP130Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P130';
          QTD_REG_BLC := Bloco_P.RegistroP130Count;
        end;
      end;
      if Bloco_P.RegistroP150Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P150';
          QTD_REG_BLC := Bloco_P.RegistroP150Count;
        end;
      end;
      if Bloco_P.RegistroP200Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P200';
          QTD_REG_BLC := Bloco_P.RegistroP200Count;
        end;
      end;
      if Bloco_P.RegistroP230Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P230';
          QTD_REG_BLC := Bloco_P.RegistroP230Count;
        end;
      end;
      if Bloco_P.RegistroP300Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P300';
          QTD_REG_BLC := Bloco_P.RegistroP300Count;
        end;
      end;
      if Bloco_P.RegistroP400Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P400';
          QTD_REG_BLC := Bloco_P.RegistroP400Count;
        end;
      end;
      if Bloco_P.RegistroP500Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'P500';
          QTD_REG_BLC := Bloco_P.RegistroP500Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'P990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_P.WriteRegistroP990;
end;

procedure TACBrSPEDECF.WriteRegistroQ001;
begin
  Bloco_Q.WriteRegistroQ001;
end;

procedure TACBrSPEDECF.WriteRegistroQ990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'Q001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_Q.RegistroQ001.IND_DAD = idComDados) then
    begin
      if Bloco_Q.RegistroQ100.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Q100';
          QTD_REG_BLC := Bloco_Q.RegistroQ100.Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'Q990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_Q.WriteRegistroQ990;
end;

procedure TACBrSPEDECF.WriteRegistroT001;
begin
  Bloco_T.WriteRegistroT001;
end;

procedure TACBrSPEDECF.WriteRegistroT990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'T001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_T.RegistroT001.IND_DAD = idComDados) then
    begin
      if Bloco_T.RegistroT030.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'T030';
          QTD_REG_BLC := Bloco_T.RegistroT030.Count;
        end;
      end;
      if Bloco_T.RegistroT120Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'T120';
          QTD_REG_BLC := Bloco_T.RegistroT120Count;
        end;
      end;
      if Bloco_T.RegistroT150Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'T150';
          QTD_REG_BLC := Bloco_T.RegistroT150Count;
        end;
      end;
      if Bloco_T.RegistroT170Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'T170';
          QTD_REG_BLC := Bloco_T.RegistroT170Count;
        end;
      end;
      if Bloco_T.RegistroT181Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'T181';
          QTD_REG_BLC := Bloco_T.RegistroT181Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'T990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_T.WriteRegistroT990;
end;

procedure TACBrSPEDECF.WriteRegistroU001;
begin
  Bloco_U.WriteRegistroU001;
end;

procedure TACBrSPEDECF.WriteRegistroU990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'U001';
      QTD_REG_BLC := 1;
    end;
    with New do
    begin
      REG_BLC := 'U990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_U.WriteRegistroU990;
end;

procedure TACBrSPEDECF.WriteRegistroX001;
begin
  Bloco_X.WriteRegistroX001;
end;

procedure TACBrSPEDECF.WriteRegistroX990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'X001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_X.RegistroX001.IND_DAD = idComDados) then
    begin
      if Bloco_X.RegistroX001.RegistroX280.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X280';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX280.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX291.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X291';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX291.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX292.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X292';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX292.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX300.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X300';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX300.Count;
        end;
      end;
      if Bloco_X.RegistroX310Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X310';
          QTD_REG_BLC := Bloco_X.RegistroX310Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX320.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X320';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX320.Count;
        end;
      end;
      if Bloco_X.RegistroX330Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X330';
          QTD_REG_BLC := Bloco_X.RegistroX330Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX340.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X340';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX340.Count;
        end;
      end;
      if Bloco_X.RegistroX350Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X350';
          QTD_REG_BLC := Bloco_X.RegistroX350Count;
        end;
      end;
      if Bloco_X.RegistroX351Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X351';
          QTD_REG_BLC := Bloco_X.RegistroX351Count;
        end;
      end;
      if Bloco_X.RegistroX352Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X352';
          QTD_REG_BLC := Bloco_X.RegistroX352Count;
        end;
      end;
      if Bloco_X.RegistroX353Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X353';
          QTD_REG_BLC := Bloco_X.RegistroX353Count;
        end;
      end;
      if Bloco_X.RegistroX354Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X354';
          QTD_REG_BLC := Bloco_X.RegistroX354Count;
        end;
      end;
      if Bloco_X.RegistroX355Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X355';
          QTD_REG_BLC := Bloco_X.RegistroX355Count;
        end;
      end;
      if Bloco_X.RegistroX356Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X356';
          QTD_REG_BLC := Bloco_X.RegistroX356Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX390.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X390';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX390.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX400.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X400';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX400.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX410.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X410';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX410.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX420.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X420';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX420.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX430.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X430';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX430.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX450.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X450';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX450.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX400.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X400';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX400.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX460.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X460';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX460.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX470.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X470';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX470.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX480.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X480';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX480.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX490.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X490';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX490.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX500.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X500';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX500.Count;
        end;
      end;
      if Bloco_X.RegistroX001.RegistroX510.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'X510';
          QTD_REG_BLC := Bloco_X.RegistroX001.RegistroX510.Count;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'X990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_X.WriteRegistroX990;
end;

procedure TACBrSPEDECF.WriteRegistroY001;
begin
  Bloco_Y.WriteRegistroY001;
end;

procedure TACBrSPEDECF.WriteRegistroY990;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'Y001';
      QTD_REG_BLC := 1;
    end;
    if (Bloco_Y.RegistroY001.IND_DAD = idComDados) then
    begin
      if Bloco_Y.RegistroY001.RegistroY520.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y520';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY520.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY540.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y540';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY540.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY550.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y550';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY550.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY560.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y560';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY560.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY570.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y570';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY570.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY580.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y580';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY580.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY590.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y590';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY590.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY600.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y600';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY600.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY611.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y611';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY611.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY612.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y612';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY612.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY620.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y620';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY620.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY630.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y630';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY630.Count;
        end;
      end;
      if Bloco_Y.RegistroY650Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y650';
          QTD_REG_BLC := Bloco_Y.RegistroY650Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY660.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y660';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY660.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY665.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y665';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY665.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY671.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y671';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY671.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY680.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y680';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY680.Count;
        end;
      end;
      if Bloco_Y.RegistroY681Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y681';
          QTD_REG_BLC := Bloco_Y.RegistroY681Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY682.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y682';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY682.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY690.Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'Y690';
          QTD_REG_BLC := Bloco_Y.RegistroY001.RegistroY690.Count;
        end;
      end;
      if Bloco_Y.RegistroY001.RegistroY800.ARQ_RTF <> '' then
      begin
        with New do
        begin
          REG_BLC := 'Y800';
          QTD_REG_BLC := 1;
        end;
      end;
    end;
    with New do
    begin
      REG_BLC := 'Y990';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_Y.WriteRegistroY990;
end;

procedure TACBrSPEDECF.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '9001';
    QTD_REG_BLC := 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDECF.WriteRegistro9900;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '9900';
      QTD_REG_BLC := Bloco_9.Registro9900.Count + 2;
    end;
    with New do
    begin
      REG_BLC := '9990';
      QTD_REG_BLC := 1;
    end;
    with New do
    begin
      REG_BLC := '9999';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_9.WriteRegistro9900;
end;

procedure TACBrSPEDECF.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDECF.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                 Bloco_C.RegistroC990.QTD_LIN +
                                                                 Bloco_E.RegistroE990.QTD_LIN +
                                                                 Bloco_J.RegistroJ990.QTD_LIN +
                                                                 Bloco_K.RegistroK990.QTD_LIN +
                                                                 Bloco_L.RegistroL990.QTD_LIN +
                                                                 Bloco_M.RegistroM990.QTD_LIN +
                                                                 Bloco_N.RegistroN990.QTD_LIN +
                                                                 Bloco_P.RegistroP990.QTD_LIN +
                                                                 Bloco_Q.RegistroQ990.QTD_LIN +
                                                                 Bloco_T.RegistroT990.QTD_LIN +
                                                                 Bloco_U.RegistroU990.QTD_LIN +
                                                                 Bloco_X.RegistroX990.QTD_LIN +
                                                                 Bloco_Y.RegistroY990.QTD_LIN +
                                                                 Bloco_9.Registro9990.QTD_LIN;
  Bloco_9.WriteRegistro9999;
end;

{$IFNDEF Framework}
{$IFDEF FPC}

initialization
   {$I ACBrSpedECF.lrs}
{$ENDIF}
{$ENDIF}

end.

